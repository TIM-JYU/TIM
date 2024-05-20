import itertools
from collections import defaultdict
from concurrent.futures import Future
from dataclasses import dataclass, replace, field, fields, Field
from datetime import datetime, timedelta
from pathlib import Path
from typing import Literal, Union, DefaultDict, Callable, TypeVar, Any, get_args
from urllib.parse import urlparse

import filelock
from flask import Response, flash, request
from isodate import Duration
from marshmallow import Schema
from sqlalchemy import select
from urllib3 import Retry
from werkzeug.utils import secure_filename

from timApp.auth.accesshelper import AccessDenied, verify_admin
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import get_duration_now, do_confirm
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder
from timApp.item.dist_right_network import get_dist_right_network
from timApp.item.item import Item
from timApp.tim_app import app, csrf
from timApp.tim_celery import relay_dist_rights
from timApp.timdb.sqa import db, run_sql
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import grant_access
from timApp.util.flask.requesthelper import RouteException, NotExist
from timApp.util.flask.responsehelper import (
    ok_response,
    to_json_str,
    json_response,
    safe_redirect,
)
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.logger import log_warning
from timApp.util.secret import check_secret, get_secret_or_abort
from timApp.util.utils import (
    read_json_lines,
    collect_errors_from_hosts,
    get_current_time,
)
from tim_common.marshmallow_dataclass import field_for_schema, class_schema
from tim_common.utils import DurationSchema
from tim_common.vendor.requests_futures import FuturesSession

dist_bp = TypedBlueprint("dist_rights", __name__, url_prefix="/distRights")

# Custom retry logic for distributing rights
# From tests, it seems that sometimes distribution might fail with 502, in that case we should retry.
dist_retry = Retry(
    total=5,
    connect=3,
    status_forcelist=[429, 500, 502, 503, 504],
)


@dataclass(slots=True)
class ConfirmOp:
    type: Literal["confirm"]
    email: str
    timestamp: datetime


@dataclass(slots=True)
class ConfirmGroupOp:
    type: Literal["confirmgroup"]
    group: str
    timestamp: datetime


@dataclass(slots=True)
class QuitOp:
    type: Literal["quit"]
    email: str
    timestamp: datetime


@dataclass(slots=True)
class UnlockOp:
    type: Literal["unlock"]
    email: str
    timestamp: datetime


@dataclass(slots=True)
class ChangeTimeOp:
    type: Literal["changetime"]
    email: str
    secs: int
    timestamp: datetime


@dataclass(slots=True)
class ChangeTimeGroupOp:
    type: Literal["changetimegroup"]
    group: str
    secs: int
    timestamp: datetime


@dataclass(slots=True)
class UndoConfirmOp:
    type: Literal["undoconfirm"]
    email: str
    timestamp: datetime


@dataclass(slots=True)
class UndoQuitOp:
    type: Literal["undoquit"]
    email: str
    timestamp: datetime


@dataclass(slots=True)
class ChangeStartTimeGroupOp:
    type: Literal["changestarttimegroup"]
    group: str
    starttime: datetime
    timestamp: datetime


@dataclass(slots=True)
class Right:
    require_confirm: bool
    duration_from: datetime | None
    duration_to: datetime | None
    duration: Duration | None
    accessible_from: datetime | None
    accessible_to: datetime | None


RightOp = Union[
    ConfirmOp,
    UnlockOp,
    ChangeTimeOp,
    QuitOp,
    UndoConfirmOp,
    UndoQuitOp,
    ChangeTimeGroupOp,
    ChangeStartTimeGroupOp,
    ConfirmGroupOp,
]
GroupOp = Union[
    ChangeTimeGroupOp,
    ChangeStartTimeGroupOp,
    ConfirmGroupOp,
]
GroupOps = (
    ChangeTimeGroupOp,
    ChangeStartTimeGroupOp,
    ConfirmGroupOp,
)


def _get_op_name(op_type: RightOp) -> str:
    type_field: Field = next((f for f in fields(op_type) if f.name == "type"))
    return get_args(type_field.type)[0]


right_op_types: dict[str, Schema] = {
    _get_op_name(op_type): class_schema(op_type)() for op_type in get_args(RightOp)
}


# It's faster to deserialize using class schemas instead of generating a field schema for RightOp
def _deserialize_right(line: dict[str, Any]) -> RightOp:
    return right_op_types[line["type"]].load(line)


RightOpSchema = field_for_schema(RightOp)  # type: ignore[arg-type]

RightSchema = class_schema(Right, base_schema=DurationSchema)()

Email = str


@dataclass(frozen=True)
class RightLogEntry:
    op: RightOp
    right: Right


T = TypeVar("T", bound=RightOp)


@dataclass
class RightLog:
    initial_right: Right
    group_cache: dict[str, list[Email]] = field(default_factory=dict)
    op_history: DefaultDict[Email, list[RightLogEntry]] = field(
        default_factory=lambda: defaultdict(list)
    )

    def add_op(self, r: RightOp) -> None:
        if isinstance(r, ChangeTimeGroupOp):
            emails = self.get_group_emails(r)
            self.process_group_rights(emails, change_time, r)
            return
        if isinstance(r, ChangeStartTimeGroupOp):
            emails = self.get_group_emails(r)
            self.process_group_rights(emails, change_starttime, r)
            return
        if isinstance(r, ConfirmGroupOp):
            emails = self.get_group_emails(r)
            self.process_group_rights(emails, confirm_group, r)
            return
        email = r.email
        curr_right = self.get_right(email)
        curr_hist = self.op_history[email]
        if isinstance(r, ConfirmOp):
            do_confirm(curr_right, r.timestamp)
        elif isinstance(r, UnlockOp):
            if curr_right.duration:
                curr_right.accessible_from = r.timestamp
                curr_right.accessible_to = (
                    curr_right.accessible_from
                    + get_duration_now(curr_right, r.timestamp)
                )
            else:
                # TODO: This shouldn't happen in practice because non-duration rights cannot be unlocked.
                #  Perhaps log a warning etc.
                pass
        elif isinstance(r, ChangeTimeOp):
            change_time(curr_right, r)
        elif isinstance(r, QuitOp):
            curr_right.accessible_to = r.timestamp
        elif isinstance(r, UndoConfirmOp):
            # We _don't_ want to assign "accessible_from = None" here.
            # Otherwise, if the right is reconfirmed, the start time will be wrong (it gets current timestamp).
            # If a Right with require_confirm = True is distributed, accessible_from will be saved as None in the
            # receiving end, meaning that the right is not active.
            # curr_right.accessible_from = None
            curr_right.require_confirm = True
        elif isinstance(r, UndoQuitOp):
            if isinstance(curr_hist[-1].op, QuitOp):
                try:
                    last_active = curr_hist[
                        -2
                    ].right  # -1 is the QuitOp, so one before that
                except IndexError:
                    last_active = self.initial_right
                curr_right.accessible_to = last_active.accessible_to
        else:
            raise Exception("unknown op")
        curr_hist.append(RightLogEntry(r, curr_right))

    def process_group_rights(
        self,
        emails: list[Email],
        fn: Callable[[Right, T], None],
        r: T,
    ) -> None:
        op_history = self.op_history
        for e in emails:
            l_op = self.latest_op(e)
            if l_op and isinstance(l_op.op, QuitOp):
                continue
            rig = self.get_right(e)
            fn(rig, r)
            op_history[e].append(RightLogEntry(r, rig))

    def get_group_emails(self, r: GroupOp) -> list[Email]:
        emails = self.group_cache.get(r.group)
        if not emails:
            emails = [
                e
                for e in run_sql(
                    select(User.email)
                    .join(User, UserGroup.users)
                    .filter(UserGroup.name == r.group)
                ).scalars()
            ]
        if not emails:
            if not UserGroup.get_by_name(r.group):
                raise Exception(f"Usergroup {r.group} not found")
        self.group_cache[r.group] = [e for e in emails]
        return emails

    def get_right(self, email: Email) -> Right:
        latest = self.latest_op(email)
        # Make a copy of the right so we keep it immutable.
        return replace(latest.right if latest else self.initial_right)

    def latest_op(self, email: Email) -> RightLogEntry | None:
        try:
            return self.op_history[email][-1]
        except IndexError:
            return None


def change_time(right: Right, op: ChangeTimeOp | ChangeTimeGroupOp) -> None:
    if right.accessible_to:
        right.accessible_to += timedelta(seconds=op.secs)
    if right.duration and not right.accessible_from:
        right.duration += timedelta(seconds=op.secs)


def change_starttime(right: Right, op: ChangeStartTimeGroupOp) -> None:
    if right.accessible_from:
        # non-duration right (or an unlocked duration)
        old_acc_from = right.accessible_from
        right.accessible_from = op.starttime
        # Keep the difference (accessible_to - accessible_from) constant.
        # It's not necessarily always desired, but makes sense in exams with non-duration rights.
        if right.accessible_to:
            dur = right.accessible_to - old_acc_from
            right.accessible_to = right.accessible_from + dur
    elif right.duration_from:
        dur_to = right.duration_to
        # Keep the difference (duration_to - duration_from) constant.
        unlock_period = dur_to - right.duration_from if dur_to else None
        right.duration_from = op.starttime
        right.duration_to = (
            right.duration_from + unlock_period if unlock_period else None
        )


def confirm_group(right: Right, op: ConfirmGroupOp) -> None:
    do_confirm(right, op.timestamp)


def get_current_rights(target: str) -> tuple[RightLog, Path]:
    fp = Path(app.config["FILES_PATH"])
    initial_rights, lines = read_rights(fp / f"{target}.rights.initial", 1)
    rights_log_path = fp / f"{target}.rights.log"
    try:
        logged_rights, _ = read_rights(rights_log_path, 0)
    except FileNotFoundError:
        logged_rights = []
    initial_right: Right = RightSchema.load(lines[0])
    rights = RightLog(initial_right)
    for r in itertools.chain(initial_rights, logged_rights):
        rights.add_op(r)
    return rights, rights_log_path


def read_rights(path: Path, index: int) -> tuple[list[RightOp], list[dict]]:
    lines = read_json_lines(path)
    return [_deserialize_right(line) for line in lines[index:]], lines


def do_register_right(op: RightOp, target: str) -> tuple[RightLog | None, str | None]:
    rights, right_log_path = get_current_rights(target)
    if not isinstance(op, GroupOps):
        latest_op = rights.latest_op(op.email)
        if (
            latest_op
            and isinstance(latest_op.op, QuitOp)
            and not isinstance(op, UndoQuitOp)
        ):
            return None, f"{target}: Cannot register a non-UndoQuitOp after QuitOp"
        if isinstance(op, UndoQuitOp) and (
            not latest_op or not isinstance(latest_op.op, QuitOp)
        ):
            return None, f"{target}: There is no QuitOp to undo"
    rights.add_op(op)
    with right_log_path.open("a") as f:
        f.write(to_json_str(op) + "\n")
    return rights, None


def _get_dist_hosts(
    target: str, distribute_target: str | None = None
) -> list[tuple[str, dict[str, Any] | None]]:
    host_id = app.config["DIST_RIGHTS_HOST_ID"]
    if host_id and distribute_target:
        dist_config = get_dist_right_network()
        targets = dist_config.distribute_targets.get(distribute_target)
        if targets:
            return [
                (
                    dist_config.hosts[t.target],
                    {
                        "distribute_group": t.distribute_group,
                        "distribute_from": host_id,
                    },
                )
                for t in targets
            ]

    return [(h, None) for h in app.config["DIST_RIGHTS_HOSTS"][target]["hosts"]]


def do_dist_rights(
    op: RightOp,
    rights: RightLog,
    target: str,
    distribute_target: str | None = None,
) -> list[str]:
    emails = rights.group_cache[op.group] if isinstance(op, GroupOps) else [op.email]
    session = FuturesSession(
        max_workers=app.config["DIST_RIGHTS_WORKER_THREADS"],
        adapter_kwargs={"max_retries": dist_retry},
    )
    futures = []
    host_config = app.config["DIST_RIGHTS_HOSTS"][target]
    dist_rights_send_secret = get_secret_or_abort("DIST_RIGHTS_SEND_SECRET")
    hosts = _get_dist_hosts(target, distribute_target)
    hostnames = [h for h, _ in hosts]
    rights_to_send = [{"email": e, "right": rights.get_right(e)} for e in emails]
    for m, extra in hosts:
        data = {
            "rights": rights_to_send,
            "secret": dist_rights_send_secret,
            "item_path": host_config["item"],
        }
        if extra:
            data |= extra
        r = session.put(
            f"{m}/distRights/receive",
            data=to_json_str(data),
            headers={"Content-Type": "application/json"},
            timeout=10,
        )
        futures.append(r)
    return collect_errors_from_hosts(futures, hostnames)


def register_right_impl(
    op: RightOp,
    target: str | list[str],
    backup: bool = True,
    distribute: bool = True,
    distribute_network_target: str | None = None,
) -> list[str]:
    targets = [target] if isinstance(target, str) else target
    errors = []
    for tgt in targets:
        target_s = secure_filename(tgt)
        if not target_s:
            raise RouteException(f"invalid target: {tgt}")
        with filelock.FileLock(f"/tmp/log_right_{target_s}"):
            rights, err = do_register_right(op, target_s)
            if err:
                errors.append(err)
        if distribute and rights:
            with filelock.FileLock(f"/tmp/dist_right_{target_s}"):
                errors.extend(
                    do_dist_rights(op, rights, target_s, distribute_network_target)
                )
    if backup:
        backup_errors = register_op_to_hosts(op, target, is_receiving_backup=True)
        if backup_errors:
            log_warning(f"Right backup failed for some hosts: {backup_errors}")
    return errors


@dist_bp.post("/register")
@csrf.exempt
def register_right(
    op: RightOp,
    target: str | list[str],
    secret: str,
    is_receiving_backup: bool = False,
) -> Response:
    check_secret(secret, "DIST_RIGHTS_REGISTER_SECRET")
    is_active_distributor = app.config["DIST_RIGHTS_IS_DISTRIBUTOR"]
    errors = register_right_impl(
        op,
        target,
        backup=False,
        distribute=not is_receiving_backup and is_active_distributor,
    )
    return json_response({"host_errors": errors})


@dataclass
class RightEntry:
    email: Email
    right: Right


def _resolve_relay_hosts(
    distribute_group: str, distribute_from: str | None
) -> list[str]:
    host_id = app.config["DIST_RIGHTS_HOST_ID"]
    dist_network = get_dist_right_network()
    except_targets = set()
    if distribute_from:
        except_targets.add(distribute_from)
    if host_id:
        except_targets.add(host_id)
    return dist_network.get_group_hosts(distribute_group, except_targets)


@dist_bp.put("/receive")
@csrf.exempt
def receive_right(
    rights: list[RightEntry],
    item_path: str,
    secret: str,
    distribute_group: str | None = None,
    distribute_from: str | None = None,
) -> Response:
    check_secret(secret, "DIST_RIGHTS_RECEIVE_SECRET")
    uges = run_sql(
        select(UserGroup, User.email)
        .join(User, UserGroup.name == User.name)
        .filter(User.email.in_(re.email for re in rights))
    )
    group_map = {}
    for ug, email in uges:
        group_map[email] = ug
    item: Item | None = Folder.find_by_path(item_path)
    if not item:
        item = DocEntry.find_by_path(item_path)
    if not item:
        raise RouteException(f"Item not found: {item_path}")
    for r in rights:
        ug = group_map[r.email]
        right = r.right
        grant_access(
            ug,
            item,
            AccessType.view,
            # In TIM, a right is considered active whenever accessible_from is set, so if the right still requires
            # confirmation, we must set accessible_from to be null.
            accessible_from=right.accessible_from
            if not right.require_confirm
            else None,
            accessible_to=right.accessible_to,
            duration=right.duration,
            duration_from=right.duration_from,
            duration_to=right.duration_to,
            require_confirm=right.require_confirm,
        )
    db.session.commit()
    if distribute_group:
        relay_hosts = _resolve_relay_hosts(distribute_group, distribute_from)
        if relay_hosts:
            relay_dist_rights.delay(
                to_json_str(
                    {"rights": rights, "secret": secret, "item_path": item_path}
                ),
                relay_hosts,
            )
    return ok_response()


def relay_dist_rights_impl(
    dist_json: str,
    dist_hosts: list[str],
) -> None:
    session = FuturesSession(
        max_workers=app.config["DIST_RIGHTS_WORKER_THREADS"],
        adapter_kwargs={"max_retries": dist_retry},
    )
    futures = []
    for h in dist_hosts:
        r = session.put(
            f"{h}/distRights/receive",
            data=dist_json,
            headers={"Content-Type": "application/json"},
            timeout=10,
        )
        futures.append(r)
    errors = collect_errors_from_hosts(futures, dist_hosts)
    if errors:
        for e in errors:
            log_warning(f"Right distribution failed for host: {e}")


@dist_bp.get("/changeStartTime")
def change_starttime_route(
    group: str,
    target: str,  # comma-separated; TODO: List[str] doesn't work for GET requests
    minutes: int,
    redir: str,
) -> Response:
    targets = target.split(",")
    u = get_current_user_object()
    conf_name = "DIST_RIGHTS_START_TIME_GROUP"
    start_time_group = app.config[conf_name]
    if not start_time_group:
        raise RouteException(f"{conf_name} not configured.")
    ug = UserGroup.get_by_name(start_time_group)
    if u not in ug.users and not u.is_admin:
        raise AccessDenied("You are not in the group that can change the start time.")
    curr_time = get_current_time()
    op = ChangeStartTimeGroupOp(
        type="changestarttimegroup",
        timestamp=curr_time,
        group=group,
        starttime=curr_time + timedelta(minutes=minutes),
    )
    errors = register_right_impl(op, targets)
    if errors:
        flash(str(errors))
    parsed = urlparse(redir)
    if parsed.scheme or parsed.netloc:
        raise RouteException("redir must be relative")
    return safe_redirect(request.host_url + redir)


def register_op_to_hosts(
    op: RightOp, target: str | list[str], is_receiving_backup: bool
) -> list[str]:
    curr_host = app.config["TIM_HOST"]
    register_hosts = [
        h for h in app.config["DIST_RIGHTS_REGISTER_HOSTS"] if h != curr_host
    ]
    session = FuturesSession(
        max_workers=app.config["DIST_RIGHTS_WORKER_THREADS"],
        adapter_kwargs={"max_retries": dist_retry},
    )
    futures: list[Future] = []
    for h in register_hosts:
        f = session.post(
            f"{h}/distRights/register",
            to_json_str(
                {
                    "op": op,
                    "target": target,
                    "secret": app.config["DIST_RIGHTS_REGISTER_SEND_SECRET"],
                    "is_receiving_backup": is_receiving_backup,
                }
            ),
            headers={"Content-type": "application/json"},
            timeout=10,
        )
        futures.append(f)
    return collect_errors_from_hosts(futures, register_hosts)


@dist_bp.get("/current")
def get_current_rights_route(
    groups: str,  # comma-separated; TODO: List[str] doesn't work for GET requests
    target: str,
) -> Response:
    verify_admin()
    try:
        rights, _ = get_current_rights(target)
    except FileNotFoundError:
        raise RouteException(f"Unknown target: {target}")
    groups_list = groups.split(",")
    emails = run_sql(
        select(User.email)
        .join(UserGroup, User.groups)
        .filter(UserGroup.name.in_(groups_list))
        .order_by(User.email)
    ).scalars()
    return json_response([{"email": e, "right": rights.get_right(e)} for e in emails])


@dist_bp.post("/distributeCurrentRights")
def distribute_current_rights(target: str, hosts: list[str], group: str) -> Response:
    """
    Distributes all current rights for a group to all hosts in the network.

    :param target: Target to distribute rights for.
    :param hosts: List of hosts to distribute to.
    :param group: The name of the user group for which to apply the rights.
    :return: OK response if successful.
    """
    verify_admin()

    ug = UserGroup.get_by_name(group)
    if not ug:
        raise NotExist(f"User group {group} does not exist.")

    rights, _ = get_current_rights(target)
    host_config = app.config["DIST_RIGHTS_HOSTS"][target]
    dist_rights_send_secret = get_secret_or_abort("DIST_RIGHTS_SEND_SECRET")
    rights_to_send = []
    # Unlike in do_dist_rights, we should apply the rights only to those users who have some operation in the log.
    # That is, if there was no operation applied to the user, don't distribute their right.
    for u in ug.users:
        op = rights.latest_op(u.email)
        if op is None:
            continue
        rights_to_send.append({"email": u.email, "right": op.right})

    dist_config = get_dist_right_network()
    host_urls = [hu for h in hosts if (hu := dist_config.hosts.get(h))]

    session = FuturesSession(
        max_workers=app.config["DIST_RIGHTS_WORKER_THREADS"],
        adapter_kwargs={"max_retries": dist_retry},
    )
    futures = []
    data = {
        "rights": rights_to_send,
        "secret": dist_rights_send_secret,
        "item_path": host_config["item"],
    }
    data_str = to_json_str(data)
    for h in host_urls:
        r = session.put(
            f"{h}/distRights/receive",
            data=data_str,
            headers={"Content-Type": "application/json"},
            timeout=10,
        )
        futures.append(r)

    errors = collect_errors_from_hosts(futures, host_urls)

    if errors:
        log_warning(f"Right distribution failed for some hosts: {errors}")
        raise RouteException(f"Right distribution failed for some hosts: {errors}")

    return json_response(
        {"applied_count": len([r["email"] for r in rights_to_send]), "hosts": host_urls}
    )
