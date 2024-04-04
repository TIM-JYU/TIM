from typing import Optional, TypedDict

from timApp.document.docsettings import DocSettings
from timApp.user.preferences import BookmarkCollection, BookmarkEntryList
from timApp.user.user import User


class BookmarkDictEntry(TypedDict):
    name: str
    link: str


class BookmarkDictGroup(TypedDict):
    name: str
    items: list[BookmarkDictEntry]
    editable: bool


MY_COURSES_GROUP = "My courses"
HIDDEN_COURSES_GROUP = "Hidden courses"
LAST_READ_GROUP = "Last read"
LAST_EDITED_GROUP = "Last edited"


class Bookmarks:
    """
    Class for managing bookmarks.

    .. note:: Bookmarks are stored as a JSON list in form

        [
          {
            "Bookmark group" : [
              { "Link 1": "https://example.com" },
              { "Link 2": "https://example.com" }
            ]
          },
          {
            "Bookmark group 2" : [
              { "Link 1": "https://example.com" },
              { "Link 2": "https://example.com" }
            ]
          }
        ]

        This class mainly allows to handle bookmarks in a more convenient way.
    """

    def __init__(self, user: User):
        """
        Loads bookmarks from user preferences.

        :param user: User whose bookmarks to manage.
        """
        self.user = user
        self.bookmark_data = self.get_bookmarks()

    def add_bookmark(
        self,
        groupname: str,
        name: str,
        link: str,
        move_to_top: bool = False,
        limit: int | None = None,
    ) -> "Bookmarks":
        """
        Adds a bookmark to the given bookmark group.

        :param groupname: Group name to add.
        :param name: Bookmark name.
        :param link: Bookmark link.
        :param move_to_top: If True, adds bookmark to the top of the group.
        :param limit: If not None, limits the number of bookmarks in the group.
        :return: This object.
        """
        bookmark_data = self.bookmark_data
        added = False
        for g in bookmark_data:
            items = g.get(groupname)
            if items is not None:
                added = True
                self._add_item_to_group(
                    items, name, link, move_to_top=move_to_top, limit=limit
                )
                break
        if not added:
            empty: BookmarkEntryList = []
            new_group = {groupname: empty}
            bookmark_data.append(new_group)
            self._add_item_to_group(empty, name, link)
        return self

    def has_bookmark(self, groupname: str, name: str) -> bool:
        """
        Checks if a bookmark with the given name exists in the given group.

        :param groupname: Group name.
        :param name: Bookmark name.
        :return: True if the bookmark exists, False otherwise.
        """
        bookmark_data = self.bookmark_data
        for folder in bookmark_data:
            items = folder.get(groupname)
            if items is not None:
                for i in items:
                    if i.get(name) is not None:
                        return True
        return False

    def get_bookmarks_in_group(self, groupname: str) -> Optional[BookmarkEntryList]:
        """
        Returns a list of bookmarks in the given group.

        :param groupname: Group name.
        :return: List of bookmarks in the group or None if the group does not exist.
        """
        bookmark_data = self.bookmark_data
        for folder in bookmark_data:
            items = folder.get(groupname)
            if items is not None:
                return items
        return None

    def delete_bookmark(self, groupname: str, name: str) -> "Bookmarks":
        """
        Deletes a bookmark from the given group.

        :param groupname: Group name.
        :param name: Bookmark name.
        :return: This object.
        """
        bookmark_data = self.bookmark_data
        items = next(
            (group[groupname] for group in bookmark_data if groupname in group),
            None,
        )
        if not items:
            return self
        self._delete_item_from_group(items, name)
        return self

    @staticmethod
    def _delete_item_from_group(groupitems: BookmarkEntryList, name: str) -> None:
        to_remove = [item for item in groupitems if item.get(name) is not None]
        if to_remove:
            groupitems.remove(to_remove[0])

    def delete_group(self, groupname: str) -> "Bookmarks":
        """
        Deletes a bookmark group.

        :param groupname: Group name to delete.
        :return: This object.
        """
        bookmark_data = self.bookmark_data
        filtered = [group for group in bookmark_data if group.get(groupname) is None]
        self.bookmark_data = filtered
        return self

    def _add_item_to_group(
        self,
        groupitems: BookmarkEntryList,
        name: str,
        link: str,
        move_to_top: bool = False,
        limit: int | None = None,
    ) -> None:
        item_found = False
        for i in groupitems:
            bookmark = i.get(name)
            if bookmark is not None:
                i[name] = link
                item_found = True
        if not item_found:
            groupitems.insert(0, {name: link})
        elif move_to_top:
            self._delete_item_from_group(groupitems, name)
            groupitems.insert(0, {name: link})
        if limit is not None:
            groupitems[:] = groupitems[:limit]

    def add_group(self, groupname: str) -> "Bookmarks":
        """
        Adds a bookmark group.

        :param groupname: Group name to add.
        :return: This object.
        """
        bookmark_data = self.bookmark_data
        for g in bookmark_data:
            items = g.get(groupname)
            if items is not None:
                return self
        bookmark_data.append({groupname: []})
        return self

    def get_bookmarks(self) -> BookmarkCollection:
        """
        Returns a collection of all bookmarks.

        :return: Collection of all bookmarks.
        """

        p = self.user.get_prefs()
        if p.bookmarks:
            return p.bookmarks
        f = self.user.get_personal_folder()
        doc_info = f.get_document("Bookmarks", create_if_not_exist=False)
        if not doc_info:
            return []
        bookmark_document = doc_info.document
        with bookmark_document.get_lock():
            # Note: get_own_settings is intentional, so not get_settings.
            settings = bookmark_document.get_own_settings()
            return DocSettings(bookmark_document, settings).get_bookmarks()

    def save_bookmarks(self) -> None:
        """
        Saves edited bookmarks to the database.
        """
        self._set_bookmarks(self.bookmark_data)

    def _set_bookmarks(self, bookmark_data: BookmarkCollection) -> None:
        p = self.user.get_prefs()
        p.bookmarks = bookmark_data
        self.user.set_prefs(p)

    def as_dict(self) -> list[BookmarkDictGroup]:
        """
        Returns the bookmark data as a list of dicts.

        :return: A list of all bookmarks as dict objects.
        """
        result: list[BookmarkDictGroup] = []
        for group in self.get_bookmarks():
            group_name = next(group.__iter__())
            items = group[group_name]
            result_items: list[BookmarkDictEntry] = []
            for i in items:
                item_name = next(i.__iter__())
                result_items.append({"name": item_name, "link": i[item_name]})
            result.append(
                {
                    "name": group_name,
                    "items": result_items,
                    "editable": group_name != LAST_EDITED_GROUP,
                }
            )
        return result
