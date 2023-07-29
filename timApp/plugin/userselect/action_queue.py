"""
Fast queue-based action handler for UserSelect.
"""
import datetime
import time
import traceback
from dataclasses import dataclass
from pathlib import Path
from typing import Literal

import filelock
from sqlalchemy import tuple_, select

from timApp.answer.backup import sync_user_group_memberships_if_enabled
from timApp.plugin.userselect.dist_right_util import (
    DistributeRightAction,
    DistributeRightActionSchema,
    apply_dist_right_actions,
)
from timApp.plugin.userselect.utils import group_expired_offset
from timApp.tim_app import app
from timApp.tim_celery import apply_pending_userselect_actions
from timApp.timdb.sqa import db, run_sql
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import membership_current, UserGroupMember
from timApp.util.flask.responsehelper import to_json_str
from timApp.util.logger import log_warning, log_info

"""
General idea:

* Actions are stored in a queue folder.
* Each action is stored as a file
    - File's name is UNIX timestamp in nanoseconds. This allows to remove excessive name collisions.
    - Action file contains new-line (\n) separated values. The values are:
        ACTION_TYPE
        ACTION_METADATA
    - ACTION_TYPE is an identifier for the action.
    - ACTION_METADATA is one or more values specific to the action.
    - For example, GroupAction is serialized as the following file:
        r
        testuser1
        testgroup1
* Queue is processed with Celery
    - Processing is protected by two locks: "processing" and "queue" locks
    - "processing" lock ensures only one worker applies the actions
    - "queue" lock allows another worker to wait for "processing" lock to be released and prevents task spam
"""


def _get_action_queue_path() -> Path:
    """
    Get the path to the action queue folder.

    :return: The path to the action queue folder.
    """
    result = Path(app.config["FILES_PATH"]) / "actions_queue"
    result.mkdir(exist_ok=True)
    return result


@dataclass(slots=True, frozen=True)
class Action:
    """
    Base class for a recorded action
    """

    timestamp: int
    """
    UNIX timestamp of when the action was recorded.
    Used for sorting the actions to apply them in correct order.
    """

    def to_str(self) -> str:
        """
        Return a string representation of the action.
        The string encodes all the information necessary to load the action from string.

        :return: A string representation of the action.
        """
        raise NotImplementedError


@dataclass(slots=True, frozen=True)
class GroupAction(Action):
    """
    Action for adding user to or removing user from a group.
    """

    action: Literal["a", "r"]
    """
    Action identifier. 
    
    a = Add user to group
    r = Remove user from group
    """
    user: str
    """
    Username of the user to apply the action to.
    """
    group: str
    """
    Name of the UserGroup to apply the action to.
    """

    @staticmethod
    def from_str(timestamp: int, action: Literal["a", "r"], text: str) -> "GroupAction":
        """
        Parse a group action from a string.

        :param timestamp: UNIX timestamp of when the action was recorded.
        :param action: Action identifier.
        :param text: Action metadata encoded as a string.

        :return: A GroupAction object
        """
        username, group = text.split("\n", maxsplit=1)
        return GroupAction(
            timestamp=timestamp,
            user=username,
            group=group,
            action=action,
        )

    def to_str(self) -> str:
        return f"{self.action}\n{self.user}\n{self.group}"


@dataclass(slots=True, frozen=True)
class DistRightAction(Action):
    """
    Action for right distribution.
    """

    action: Literal["dr"]
    """
    Action identifier.
    """
    user: str
    """
    User to apply the action to.
    """
    dist_right: DistributeRightAction
    """
    DistributeRightAction object, which encodes the distribution action.
    """

    @staticmethod
    def from_str(timestamp: int, text: str) -> "DistRightAction":
        """
        Parse a DistRightAction from a string.

        :param timestamp: UNIX timestamp of when the action was recorded.
        :param text: Action metadata encoded as a string.
        """
        username, dist_right = text.split("\n", maxsplit=1)
        dr = DistributeRightActionSchema().loads(dist_right)
        return DistRightAction(
            timestamp=timestamp,
            user=username,
            dist_right=dr,
            action="dr",
        )

    def to_str(self) -> str:
        return f"{self.action}\n{self.user}\n{to_json_str(self.dist_right)}"


def _get_pending_actions() -> list[Action]:
    """
    Read pending group actions from disk.

    :return: The pending actions that were fully written to disk.
    """
    actions: list[Action] = []

    for action_file in _get_action_queue_path().iterdir():
        if action_file.name.endswith(".lock"):
            continue
        try:
            timestamp = int(action_file.name)
            action, more = action_file.read_text().split("\n", maxsplit=1)
            match action:
                case "a":
                    actions.append(GroupAction.from_str(timestamp, "a", more))
                case "r":
                    actions.append(GroupAction.from_str(timestamp, "r", more))
                case "dr":
                    actions.append(DistRightAction.from_str(timestamp, more))
        except Exception as e:
            log_warning(f"Error reading action file {action_file}: {e}, skipping")

    return actions


def _purge_actions(actions: list[Action]) -> None:
    """
    Purge specified actions from disk.

    :param actions: The actions to purge.
    """
    queue_path = _get_action_queue_path()
    for action in actions:
        action_path = queue_path / str(action.timestamp)
        action_path.unlink(missing_ok=True)


def _optimize_group_actions(actions: list[GroupAction]) -> list[GroupAction]:
    """
    Optimize the group actions by resolving the effective action for each user.

    :param actions: The group actions to optimize.
    :return: The optimized actions. The list contains only the final effective action for each user.
    """
    latest_actions_by_name: dict[tuple[str, str], GroupAction] = {}
    for a in actions:
        latest_action = latest_actions_by_name.get((a.group, a.user), None)
        if not latest_action:
            latest_actions_by_name[(a.group, a.user)] = a
            continue
        if latest_action.timestamp < a.timestamp:
            latest_actions_by_name[(a.group, a.user)] = a
    return list(latest_actions_by_name.values())


def _group_dist_right_actions(
    actions: list[DistRightAction],
) -> dict[str, list[DistributeRightAction]]:
    """
    Group distribute right actions together by user.

    :param actions: The actions to group. The list will be sorted in place.
    :return: A dictionary mapping group name to a list of distribute right actions.
    """
    result: dict[str, list[DistributeRightAction]] = {}
    actions.sort(key=lambda act: act.timestamp)
    for a in actions:
        if a.user not in result:
            result[a.user] = []
        result[a.user].append(a.dist_right)
    return result


def _get_apply_lock(name: str) -> filelock.BaseFileLock:
    """
    Get a lock for applying actions.

    :param name: The name of the lock.
    :return: A file lock.
    """
    return filelock.FileLock((_get_action_queue_path() / f"{name}.lock").as_posix())


def apply_pending_actions_impl() -> None:
    """
    Applies currently pending actions and flushes them.
    """

    # Two-lock system:
    # 1. Processing lock ensures that only one task processes the pending actions
    # 2. Queue lock allows another task to wait for the processing lock to process new actions
    queue_lock = _get_apply_lock("queue")
    processing_lock = _get_apply_lock("processing")

    # FIXME: This still will not 100% prevent some actions being left unprocessed in time
    # Example: We acquire the lock here...
    queue_lock.acquire()
    # ...but the user registers an action here so no second processing is registered
    processing_lock.acquire()
    # ... and the user registers an action here
    queue_lock.release()

    try:
        now = datetime.datetime.now()
        actions = _get_pending_actions()
        if not actions:
            return

        group_actions = [a for a in actions if isinstance(a, GroupAction)]
        dist_right_actions = [a for a in actions if isinstance(a, DistRightAction)]
        effective_group_actions = _optimize_group_actions(group_actions)
        grouped_dist_right_actions = _group_dist_right_actions(dist_right_actions)

        group_names: set[str] = {a.group for a in effective_group_actions}
        user_names: set[str] = {a.user for a in effective_group_actions} | {
            a.user for a in dist_right_actions
        }
        memberships_to_expire: set[tuple[str, str]] = {
            (a.group, a.user) for a in group_actions if a.action == "r"
        }

        # noinspection PyUnresolvedReferences
        group_cache: dict[str, UserGroup] = {
            ug.name: ug
            for ug in run_sql(
                select(UserGroup).filter(UserGroup.name.in_(group_names))
            ).scalars()
        }
        # noinspection PyUnresolvedReferences
        user_cache: dict[str, User] = {
            u.name: u
            for u in run_sql(select(User).filter(User.name.in_(user_names))).scalars()
        }
        memberships_cache: dict[tuple[int, int], UserGroupMember] = {
            (m.usergroup_id, m.user_id): m
            for m in run_sql(
                select(UserGroupMember)
                .join(UserGroup, UserGroupMember.group)
                .join(User, UserGroupMember.user)
                .filter(
                    tuple_(UserGroup.name, User.name).in_(memberships_to_expire)
                    & membership_current
                )
            ).scalars()
        }

        for a in effective_group_actions:
            ug = group_cache.get(a.group, None)
            if not ug:
                log_warning(
                    f"Group {a.group} not found on '{a.action}' action for user {a.user} (timestamp: {a.timestamp})"
                )
                continue

            u = user_cache.get(a.user, None)
            if not u:
                log_warning(
                    f"User {a.user} not found on '{a.action}' action for group {a.group} (timestamp: {a.timestamp})"
                )
                continue

            # Don't apply actions via mass query to ensure mail lists will get synced
            if a.action == "a":
                u.add_to_group(ug, None)
            elif a.action == "r":
                m = memberships_cache.get((ug.id, u.id), None)
                if m:
                    m.set_expired(time_offset=group_expired_offset)

        db.session.commit()

        for u in user_cache.values():
            sync_user_group_memberships_if_enabled(u)

        for usr_name, drs in grouped_dist_right_actions.items():
            usr = user_cache.get(usr_name, None)
            if not usr:
                log_warning(f"User {usr_name} not found")
                continue
            errors = apply_dist_right_actions(usr, drs)
            if errors:
                errors_str = "\n".join(errors)
                log_warning(
                    f"Error applying dist right actions for user {usr_name}: {errors_str}"
                )

        _purge_actions(actions)

        time_diff = datetime.datetime.now() - now
        log_info(
            f"Applied {len(actions)} actions in {time_diff.total_seconds()} seconds"
        )
    except Exception as e:
        traceback_str = traceback.format_exc()
        log_warning(f"Error applying pending actions: {e}, {traceback_str}")
    finally:
        processing_lock.release()


def _check_lock_acquired(name: str) -> bool:
    """
    Check if the specified lock is already acquired by some task.

    :param name: The name of the lock.
    :return: True if the lock is acquired, False otherwise.
    """
    lock = _get_apply_lock(name)
    try:
        lock.acquire(blocking=False)  # type: ignore
    except filelock.Timeout:
        return True
    lock.release()
    return False


def _register_action(action: Action) -> None:
    """
    Register an action to the action queue.

    :param action: The action to register.
    """
    now = int(time.time() * 1000000)  # ns resolution + space for jumping if file exists
    action_file_path = _get_action_queue_path() / str(now)
    # Increase timestamp and try again
    while action_file_path.exists():
        now += 1
        action_file_path = _get_action_queue_path() / str(now)

    action_file_path.write_text(action.to_str())

    send_to_celery = app.config["USERSELECT_QUEUED_ACTIONS_CELERY"]
    if send_to_celery and not _check_lock_acquired("queue"):
        apply_pending_userselect_actions.delay()


def _register_group_action(user: User, group: str, action: Literal["a", "r"]) -> None:
    """
    Registers a group action.

    :param user: The user to process.
    :param group: The group to process.
    :param action: The action to register.
    """
    _register_action(
        GroupAction(
            timestamp=-1,
            user=user.name,
            group=group,
            action=action,
        )
    )


def register_dist_right_action(user: User, da: DistributeRightAction) -> None:
    """
    Registers a distribute right action.

    :param user: The user to add to the group.
    :param da: The distribute right action to register.
    """
    _register_action(
        DistRightAction(
            timestamp=-1,
            user=user.name,
            dist_right=da,
            action="dr",
        )
    )


def register_group_add_action(user: User, group: str) -> None:
    """
    Registers a group add action.

    :param user: The user to add to the group.
    :param group: The group to add the user to.
    """
    _register_group_action(user, group, "a")


def register_group_remove_action(user: User, group: str) -> None:
    """
    Registers a group remove action.

    :param user: The user to remove from the group.
    :param group: The group to remove the user from.
    """
    _register_group_action(user, group, "r")
