from typing import List, Dict

from timdb.models.user import User


class Bookmarks:
    def __init__(self, user: User):
        self.user = user
        f = user.get_personal_folder()
        self.bookmark_document = f.get_document('$Bookmarks',
                                                create_if_not_exist=True,
                                                creator_group_id=user.get_personal_group().id).document
        self.bookmark_data = self.get_bookmarks()

    def add_bookmark(self, groupname: str, name: str, path: str, move_to_top: bool = False) -> 'Bookmarks':
        bookmark_data = self.bookmark_data
        added = False
        for g in bookmark_data:
            items = g.get(groupname)
            if items is not None:
                added = True
                self._add_item_to_group(items, name, path, move_to_top=move_to_top)
        if not added:
            empty = []
            new_group = {groupname: empty}
            bookmark_data.append(new_group)
            self._add_item_to_group(empty, name, path)
        return self

    def delete_bookmark(self, groupname: str, name: str) -> 'Bookmarks':
        bookmark_data = self.bookmark_data
        try:
            group = filter(lambda x: x.get(groupname) is not None, bookmark_data).__next__()
        except StopIteration:
            return
        self._delete_item_from_group(group.get(groupname), name)
        return self

    @staticmethod
    def _delete_item_from_group(groupitems: List[Dict[str, str]], name: str):
        to_remove = [item for item in groupitems if item.get(name) is not None]
        if to_remove:
            groupitems.remove(to_remove[0])

    def delete_group(self, groupname) -> 'Bookmarks':
        bookmark_data = self.bookmark_data
        filtered = [group for group in bookmark_data if group.get(groupname) is None]
        self.bookmark_data = filtered
        return self

    def _add_item_to_group(self, groupitems: List[Dict[str, str]], name: str, path: str, move_to_top: bool = False):
        item_found = False
        for i in groupitems:
            bookmark = i.get(name)
            if bookmark is not None:
                i[name] = path
                item_found = True
        if not item_found and not move_to_top:
            groupitems.append({name: path})
        elif move_to_top:
            self._delete_item_from_group(groupitems, name)
            groupitems.insert(0, {name: path})
        else:
            groupitems.insert(0, {name: path})

    def add_group(self, groupname: str) -> 'Bookmarks':
        bookmark_data = self.bookmark_data
        for g in bookmark_data:
            items = g.get(groupname)
            if items is not None:
                return
        bookmark_data.append({groupname: []})
        return self

    def get_bookmarks(self):
        return self.bookmark_document.get_settings().get_bookmarks()

    def save_bookmarks(self):
        self._set_bookmarks(self.bookmark_data)

    def _set_bookmarks(self, bookmark_data):
        new_settings = self.bookmark_document.get_settings()
        new_settings.set_bookmarks(bookmark_data)
        self.bookmark_document.set_settings(new_settings.get_dict())

    def as_json(self):
        result = []
        for group in self.get_bookmarks():
            group_name = next(group.__iter__())
            items = group[group_name]
            result_items = []
            for i in items:
                item_name = next(i.__iter__())
                result_items.append({'name': item_name, 'path': i[item_name]})
            result.append({'name': group_name, 'items': result_items, 'editable': group_name != 'Last edited'})
        return result
