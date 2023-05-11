from dataclasses import dataclass
from datetime import datetime
from typing import Literal, Callable

from timApp.item.distribute_rights import (
    RightOp,
    ConfirmOp,
    QuitOp,
    UndoQuitOp,
    UnlockOp,
    ChangeTimeOp,
    register_right_impl,
    UndoConfirmOp,
)
from timApp.user.user import User
from timApp.util.utils import get_current_time
from tim_common.marshmallow_dataclass import class_schema


@dataclass
class DistributeRightAction:
    operation: Literal[
        "confirm",
        "quit",
        "unlock",
        "changetime",
        "undoquit",
        "undoconfirm",
    ]
    target: str | list[str]
    timestamp: datetime | None = None
    minutes: float = 0.0

    @property
    def timestamp_or_now(self) -> datetime:
        return self.timestamp or get_current_time()


DistributeRightActionSchema = class_schema(DistributeRightAction)


RIGHT_TO_OP: dict[str, Callable[[DistributeRightAction, str], RightOp]] = {
    "confirm": lambda r, usr: ConfirmOp(
        type="confirm",
        email=usr,
        timestamp=r.timestamp_or_now,
    ),
    "quit": lambda r, usr: QuitOp(
        type="quit",
        email=usr,
        timestamp=r.timestamp_or_now,
    ),
    "undoquit": lambda r, usr: UndoQuitOp(
        type="undoquit",
        email=usr,
        timestamp=r.timestamp_or_now,
    ),
    "unlock": lambda r, usr: UnlockOp(
        type="unlock",
        email=usr,
        timestamp=r.timestamp_or_now,
    ),
    "changetime": lambda r, usr: ChangeTimeOp(
        type="changetime",
        email=usr,
        secs=int(r.minutes * 60),
        timestamp=r.timestamp_or_now,
    ),
    "undoconfirm": lambda r, usr: UndoConfirmOp(
        type="undoconfirm",
        email=usr,
        timestamp=r.timestamp_or_now,
    ),
}


def apply_dist_right_actions(
    user_acc: User, dist_right: list[DistributeRightAction]
) -> list[str]:
    errors = []
    for distribute in dist_right:
        convert = RIGHT_TO_OP[distribute.operation]
        right_op = convert(distribute, user_acc.email)
        apply_errors = register_right_impl(right_op, distribute.target)

        if isinstance(right_op, QuitOp):
            # Ignore failing to undo twice. It is an error but it's not strictly an issue for UserSelect
            # However, do this only for QuitOp to prevent other issues like trying to confirm users who has already quit
            # TODO: Don't depend on string matching to filter out the error
            apply_errors = [
                e for e in apply_errors if "Cannot register a non-UndoQuitOp" not in e
            ]

        errors.extend(apply_errors)

    return errors
