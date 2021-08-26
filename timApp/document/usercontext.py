from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Optional

if TYPE_CHECKING:
    from timApp.user.user import User


@dataclass(frozen=True)
class UserContext:
    user: User
    logged_user: User
    answer_nr: Optional[int] = None  # needed if variable tasks, None = not task at all or not variable task
    ask_new: Optional[bool] = None # to send for plugins to force new question
    # TODO: UserContext is not the best place for task dependent values
    # TODO: reason for those is that rndutils has only user_ctx to use

    @staticmethod
    def from_one_user(u: User) -> UserContext:
        return UserContext(user=u, logged_user=u)

    @property
    def is_different(self) -> bool:
        return self.user != self.logged_user
