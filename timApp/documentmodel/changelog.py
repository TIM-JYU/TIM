from collections import defaultdict
from typing import List, Dict, Tuple, Optional

import timApp.timdb.models
from timApp.documentmodel.changelogentry import ChangelogEntry
from timApp.documentmodel.docparagraph import DocParagraph
from timApp.timdb.tim_models import db
from timApp.types import UserOrGroup


def get_author_str(u: UserOrGroup, es: List[ChangelogEntry]):
    display_name = u.pretty_full_name
    num_changes = len(es)
    return display_name if num_changes <= 1 else f'{display_name} ({num_changes} edits)'


class AuthorInfo:
    def __init__(self,
                 user_map: Dict[int, UserOrGroup],
                 entries: Dict[int, List[ChangelogEntry]]) -> None:
        self.authors: Dict[UserOrGroup, List[ChangelogEntry]] = {}
        for k, v in entries.items():
            self.authors[user_map[k]] = v

    @property
    def display_name(self):
        return '; '.join(get_author_str(u, es) for u, es in self.authors.items())

    @property
    def time(self):
        return max(entries[-1].time for entries in self.authors.values())


class Changelog:
    def __init__(self) -> None:
        self.entries: List[ChangelogEntry] = []

    def append(self, entry: ChangelogEntry):
        self.entries.append(entry)

    def to_json(self):
        return self.entries

    def get_authorinfo(self, pars: List[DocParagraph]) -> Dict[str, AuthorInfo]:
        usergroup_ids = set()
        par_ids = set(p.get_id() for p in pars)
        par_author_map = {}
        if not par_ids:
            return par_author_map
        par_entry_map: Dict[str, Dict[int, List[ChangelogEntry]]] = defaultdict(lambda: defaultdict(list))
        ug_obj_map = {}
        for e in self.entries:
            if e.par_id in par_ids:
                usergroup_ids.add(e.group_id)
                par_entry_map[e.par_id][e.group_id].append(e)
        User = timApp.timdb.models.user.User
        UserGroup = timApp.timdb.models.usergroup.UserGroup
        result = db.session.query(UserGroup, User).filter(
            UserGroup.id.in_(usergroup_ids)).outerjoin(User,
                                                       User.name == UserGroup.name).all()  # type: List[Tuple[UserGroup, Optional[User]]]
        for ug, u in result:
            ug_obj_map[ug.id] = u or ug
        for i in par_ids:
            entry = par_entry_map.get(i)
            if not entry:
                continue
            par_author_map[i] = AuthorInfo(ug_obj_map, par_entry_map[i])

        return par_author_map
