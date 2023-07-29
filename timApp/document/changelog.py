from collections import defaultdict
from typing import List, Tuple, Optional, TYPE_CHECKING, Union

from sqlalchemy import select

import timApp
from timApp.document.changelogentry import ChangelogEntry
from timApp.document.docparagraph import DocParagraph
from timApp.timdb.sqa import db

if TYPE_CHECKING:
    from timApp.user.user import User
    from timApp.user.usergroup import UserGroup


def get_author_str(u: Union["User", "UserGroup"], es: list[ChangelogEntry]):
    display_name = u.pretty_full_name
    num_changes = len(es)
    return display_name if num_changes <= 1 else f"{display_name} ({num_changes} edits)"


class AuthorInfo:
    def __init__(
        self,
        user_map: dict[int, Union["User", "UserGroup"]],
        entries: dict[int, list[ChangelogEntry]],
    ) -> None:
        self.authors: dict[Union["User", "UserGroup"], list[ChangelogEntry]] = {}
        for k, v in entries.items():
            self.authors[user_map[k]] = v

    @property
    def display_name(self):
        return "; ".join(get_author_str(u, es) for u, es in self.authors.items())

    @property
    def time(self):
        return max(entries[-1].time for entries in self.authors.values())


class Changelog:
    def __init__(self) -> None:
        self.entries: list[ChangelogEntry] = []

    def append(self, entry: ChangelogEntry):
        self.entries.append(entry)

    def to_json(self):
        return self.entries

    def get_authorinfo(self, pars: list[DocParagraph]) -> dict[str, AuthorInfo]:
        usergroup_ids = set()
        par_ids = {p.get_id() for p in pars}
        par_author_map = {}
        if not par_ids:
            return par_author_map
        par_entry_map: dict[str, dict[int, list[ChangelogEntry]]] = defaultdict(
            lambda: defaultdict(list)
        )
        ug_obj_map = {}
        for e in self.entries:
            if e.par_id in par_ids:
                usergroup_ids.add(e.group_id)
                par_entry_map[e.par_id][e.group_id].append(e)
        User = timApp.user.user.User
        UserGroup = timApp.user.usergroup.UserGroup
        result = db.session.execute(
            select(UserGroup, User)
            .select_from(UserGroup)
            .filter(UserGroup.id.in_(usergroup_ids))
            .outerjoin(User, User.name == UserGroup.name)
        ).all()  # type: List[Tuple[UserGroup, Optional[User]]]
        for ug, u in result:
            ug_obj_map[ug.id] = u or ug
        for i in par_ids:
            entry = par_entry_map.get(i)
            if not entry:
                continue
            par_author_map[i] = AuthorInfo(ug_obj_map, par_entry_map[i])

        return par_author_map
