import itertools
from collections import defaultdict
from dataclasses import dataclass, replace, field
from datetime import datetime, timedelta
from pathlib import Path
from typing import Literal, Union, Tuple, List, Dict, Optional, DefaultDict

import filelock
from flask import Response
from isodate import Duration
from werkzeug.utils import secure_filename

from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import get_duration_now, do_confirm
from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder
from timApp.item.item import Item
from timApp.tim_app import app, csrf
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import grant_access
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import ok_response, to_json_str, json_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.secret import check_secret, get_secret_or_abort
from timApp.util.utils import read_json_lines, wait_response_and_collect_error
from tim_common.marshmallow_dataclass import field_for_schema, class_schema
from tim_common.utils import DurationSchema
from tim_common.vendor.requests_futures import FuturesSession

dist_bp = TypedBlueprint('dist_rights', __name__, url_prefix='/distRights')


@dataclass
class ConfirmOp:
    type: Literal['confirm']
    email: str
    timestamp: datetime


@dataclass
class QuitOp:
    type: Literal['quit']
    email: str
    timestamp: datetime


@dataclass
class UnlockOp:
    type: Literal['unlock']
    email: str
    timestamp: datetime


@dataclass
class ChangeTimeOp:
    type: Literal['changetime']
    email: str
    secs: int
    timestamp: datetime


@dataclass
class ChangeTimeGroupOp:
    type: Literal['changetimegroup']
    group: str
    secs: int
    timestamp: datetime


@dataclass
class UndoConfirmOp:
    type: Literal['undoconfirm']
    email: str
    timestamp: datetime


@dataclass
class UndoQuitOp:
    type: Literal['undoquit']
    email: str
    timestamp: datetime


@dataclass
class Right:
    require_confirm: bool
    duration_from: Optional[datetime]
    duration_to: Optional[datetime]
    duration: Optional[Duration]
    accessible_from: Optional[datetime]
    accessible_to: Optional[datetime]


RightOp = Union[ConfirmOp, UnlockOp, ChangeTimeOp, QuitOp, UndoConfirmOp, UndoQuitOp, ChangeTimeGroupOp]
RightOpSchema = field_for_schema(RightOp)  # type: ignore[arg-type]

RightSchema = class_schema(Right, base_schema=DurationSchema)()

Email = str


@dataclass(frozen=True)
class RightLogEntry:
    op: RightOp
    right: Right


@dataclass
class RightLog:
    initial_right: Right
    group_cache: Dict[str, List[Email]] = field(default_factory=dict)
    op_history: DefaultDict[Email, List[RightLogEntry]] = field(default_factory=lambda: defaultdict(list))

    def add_op(self, r: RightOp) -> None:
        op_history = self.op_history
        if isinstance(r, ChangeTimeGroupOp):
            emails = self.group_cache.get(r.group)
            if not emails:
                emails = [e for e, in (
                    UserGroup.query
                        .join(User, UserGroup.users)
                        .filter(UserGroup.name == r.group)
                        .with_entities(User.email)
                )]
            if not emails:
                raise Exception(f'Usergroup {r.group} not found or it has no members')
            self.group_cache[r.group] = [e for e in emails]
            for e in emails:
                l_op = self.latest_op(e)
                if l_op and isinstance(l_op.op, QuitOp):
                    continue
                rig = self.get_right(e)
                change_time(rig, r)
                op_history[e].append(RightLogEntry(r, rig))
            return
        email = r.email
        curr_right = self.get_right(email)
        curr_hist = op_history[email]
        if isinstance(r, ConfirmOp):
            do_confirm(curr_right, r.timestamp)
        elif isinstance(r, UnlockOp):
            curr_right.accessible_from = r.timestamp
            curr_right.accessible_to = curr_right.accessible_from + get_duration_now(curr_right, r.timestamp)
        elif isinstance(r, ChangeTimeOp):
            change_time(curr_right, r)
        elif isinstance(r, QuitOp):
            curr_right.accessible_to = r.timestamp
        elif isinstance(r, UndoConfirmOp):
            curr_right.accessible_from = None
            curr_right.require_confirm = True
        elif isinstance(r, UndoQuitOp):
            if isinstance(curr_hist[-1].op, QuitOp):
                try:
                    last_active = curr_hist[-2].right  # -1 is the QuitOp, so one before that
                except IndexError:
                    last_active = self.initial_right
                curr_right.accessible_to = last_active.accessible_to
        else:
            raise Exception('unknown op')
        curr_hist.append(RightLogEntry(r, curr_right))

    def get_right(self, email: Email) -> Right:
        latest = self.latest_op(email)
        # Make a copy of the right so we keep it immutable.
        return replace(latest.right if latest else self.initial_right)

    def latest_op(self, email: Email) -> Optional[RightLogEntry]:
        try:
            return self.op_history[email][-1]
        except IndexError:
            return None


def change_time(right: Right, op: Union[ChangeTimeOp, ChangeTimeGroupOp]) -> None:
    if right.accessible_to:
        right.accessible_to += timedelta(seconds=op.secs)
    if right.duration and not right.accessible_from:
        right.duration += timedelta(seconds=op.secs)


def get_current_rights(target: str) -> Tuple[RightLog, Path]:
    fp = Path(app.config['FILES_PATH'])
    initial_rights, lines = read_rights(fp / f'{target}.rights.initial', 1)
    rights_log_path = fp / f'{target}.rights.log'
    try:
        logged_rights, _ = read_rights(rights_log_path, 0)
    except FileNotFoundError:
        logged_rights = []
    initial_right: Right = RightSchema.load(lines[0])
    rights = RightLog(initial_right)
    for r in itertools.chain(initial_rights, logged_rights):
        rights.add_op(r)
    return rights, rights_log_path


def read_rights(path: Path, index: int) -> Tuple[List[RightOp], List[Dict]]:
    lines = read_json_lines(path)
    return [RightOpSchema.deserialize(line) for line in lines[index:]], lines


def do_register_right(op: RightOp, target: str) -> RightLog:
    rights, right_log_path = get_current_rights(target)
    if not isinstance(op, ChangeTimeGroupOp):
        latest_op = rights.latest_op(op.email)
        if latest_op and isinstance(latest_op.op, QuitOp) and not isinstance(op, UndoQuitOp):
            raise RouteException('Cannot register a non-UndoQuitOp after QuitOp')
        if isinstance(op, UndoQuitOp) and (not latest_op or not isinstance(latest_op.op, QuitOp)):
            raise RouteException('There is no QuitOp to undo')
    rights.add_op(op)
    with right_log_path.open('a') as f:
        f.write(to_json_str(op) + '\n')
    return rights


def do_dist_rights(op: RightOp, rights: RightLog, target: str) -> List[str]:
    emails = rights.group_cache[op.group] if isinstance(op, ChangeTimeGroupOp) else [op.email]
    session = FuturesSession()
    futures = []
    host_config = app.config['DIST_RIGHTS_HOSTS'][target]
    dist_rights_send_secret = get_secret_or_abort('DIST_RIGHTS_SEND_SECRET')
    hosts = host_config['hosts']
    for m in hosts:
        r = session.put(
            f'{m}/distRights/receive',
            data=to_json_str({
                'rights': [{'email': e, 'right': rights.get_right(e)} for e in emails],
                'secret': dist_rights_send_secret,
                'item_path': host_config['item'],
            }),
            headers={'Content-Type': 'application/json'},
            timeout=5,
        )
        futures.append(r)
    errors: List[str] = []
    for f, h in zip(futures, hosts):
        wait_response_and_collect_error(f, h, errors)
    return errors


@dist_bp.route('/register', methods=['post'])
@csrf.exempt
def register_right(
        op: RightOp,
        target: str,
        secret: str,
) -> Response:
    check_secret(secret, 'DIST_RIGHTS_REGISTER_SECRET')
    target_s = secure_filename(target)
    if not target_s:
        raise RouteException(f'invalid target: {target}')
    with filelock.FileLock(f'/tmp/log_right_{target_s}'):
        rights = do_register_right(op, target_s)
    with filelock.FileLock(f'/tmp/dist_right_{target_s}'):
        errors = do_dist_rights(op, rights, target_s)
    return json_response({'host_errors': errors})


@dataclass
class RightEntry:
    email: Email
    right: Right


@dist_bp.route('/receive', methods=['put'])
@csrf.exempt
def receive_right(
        rights: List[RightEntry],
        item_path: str,
        secret: str,
) -> Response:
    check_secret(secret, 'DIST_RIGHTS_RECEIVE_SECRET')
    uges = (
        UserGroup.query
            .join(User, UserGroup.name == User.name)
            .filter(User.email.in_((re.email for re in rights)))
            .with_entities(UserGroup, User.email)
            .all()
    )
    group_map = {}
    for ug, email in uges:
        group_map[email] = ug
    item: Optional[Item] = Folder.find_by_path(item_path)
    if not item:
        item = DocEntry.find_by_path(item_path)
    if not item:
        raise RouteException(f'Item not found: {item_path}')
    for r in rights:
        ug = group_map[r.email]
        right = r.right
        grant_access(
            ug,
            item,
            AccessType.view,
            accessible_from=right.accessible_from,
            accessible_to=right.accessible_to,
            duration=right.duration,
            duration_from=right.duration_from,
            duration_to=right.duration_to,
            require_confirm=right.require_confirm,
        )
    return ok_response()
