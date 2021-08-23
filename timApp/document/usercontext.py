from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from timApp.user.user import User


@dataclass(frozen=True)
class UserContext:
    user: User
    logged_user: User
    answer_nr: int = -1  # needed if variable tasks, -1 = not task at all or not variable task
    ask_new: bool = False # to send for plugins to force new question

    @staticmethod
    def from_one_user(u: User) -> UserContext:
        return UserContext(user=u, logged_user=u)

    @property
    def is_different(self) -> bool:
        return self.user != self.logged_user
