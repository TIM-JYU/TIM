from typing import List, Dict

from timdb.models.user import User


class Bookmarks:
    def __init__(self, user: User):
        self.user = user
        f = user.get_personal_folder()
        self.bookmark_document = f.get_document('$Bookmarks',
                                                create_if_not_exist=True,
                                                creator_group_id=user.get_personal_group().id).document

    def add_bookmark(self, groupname: str, name: str, path: str):
        bookmark_data = self.get_bookmarks()
        added = False
        for g in bookmark_data:
            items = g.get(groupname)
            if items is not None:
                added = True
                self._add_item_to_group(items, name, path)
        if not added:
            empty = []
            new_group = {groupname: empty}
            bookmark_data.append(new_group)
            self._add_item_to_group(empty, name, path)
        self._set_bookmarks(bookmark_data)

    def delete_bookmark(self, groupname: str, name: str):
        bookmark_data = self.get_bookmarks()
        try:
            group = filter(lambda x: x.get(groupname) is not None, bookmark_data).__next__()
        except StopIteration:
            return
        filtered = [item for item in group.get(groupname) if item.get(name) is None]
        group[groupname] = filtered
        self._set_bookmarks(bookmark_data)

    def delete_group(self, groupname):
        bookmark_data = self.get_bookmarks()
        filtered = [group for group in bookmark_data if group.get(groupname) is None]
        self._set_bookmarks(filtered)

    @staticmethod
    def _add_item_to_group(groupitems: List[Dict[str, str]], name: str, path: str):
        item_found = False
        for i in groupitems:
            bookmark = i.get(name)
            if bookmark is not None:
                i[name] = path
                item_found = True
        if not item_found:
            groupitems.append({name: path})

    def add_group(self, groupname: str):
        bookmark_data = self.get_bookmarks()
        for g in bookmark_data:
            items = g.get(groupname)
            if items is not None:
                return
        bookmark_data.append({groupname: []})
        self._set_bookmarks(bookmark_data)

    def get_bookmarks(self):
        return self.bookmark_document.get_settings().get_bookmarks()

    def _set_bookmarks(self, bookmark_data):
        new_settings = self.bookmark_document.get_settings()
        new_settings.set_bookmarks(bookmark_data)
        self.bookmark_document.set_settings(new_settings.get_dict())
