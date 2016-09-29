from timdb.models.docentry import DocEntry
from timdb.models.folder import Folder
from timroutetest import TimRouteTest


class BookmarkTest(TimRouteTest):
    def test_bookmarks(self):
        self.login_test1()
        bookmarks = self.get('/bookmarks/get', expect_status=200, as_json=True)
        f = Folder.find_by_location('users/{}'.format(self.current_user_name()), '')
        self.assertIsNone(f)
        d = DocEntry.query.filter_by(name='users/{}/$Bookmarks'.format(self.current_user_name())).first()
        self.assertIsNotNone(d)
        self.assertListEqual([], bookmarks)
        group_name = 'mygroup'
        group_name2 = 'mygroup2'
        item = 'test item'
        item_path = 'some/path/to/item'
        bookmarks = self.post('/bookmarks/createGroup/{}'.format(group_name), expect_status=200, as_json=True)
        self.assertListEqual([{'name': 'mygroup', 'items': [], 'editable': True}], bookmarks)
        bookmarks = self.json_post('/bookmarks/add', {'group': group_name2, 'name': item, 'link': item_path},
                                   expect_status=200,
                                   as_json=True)
        self.assertListEqual([{'items': [], 'name': group_name, 'editable': True},
                              {'items': [{'name': item, 'path': item_path}],
                               'name': group_name2, 'editable': True}], bookmarks)

        bookmarks = self.json_post('/bookmarks/deleteGroup', {'group': group_name}, expect_status=200,
                                   as_json=True)
        self.assertListEqual([{'items': [{'name': item, 'path': item_path}],
                               'name': group_name2, 'editable': True}], bookmarks)
        bookmarks = self.json_post('/bookmarks/deleteGroup', {'group': group_name}, expect_status=200,
                                   as_json=True)
        self.assertListEqual([{'items': [{'name': item, 'path': item_path}],
                               'name': group_name2, 'editable': True}], bookmarks)

        bookmarks = self.json_post('/bookmarks/delete', {'group': group_name2, 'name': item}, expect_status=200,
                                   as_json=True)
        self.assertListEqual([{'items': [], 'name': group_name2, 'editable': True}], bookmarks)
        bookmarks = self.json_post('/bookmarks/delete', {'group': group_name2, 'name': item}, expect_status=200,
                                   as_json=True)
        self.assertListEqual([{'items': [], 'name': group_name2, 'editable': True}], bookmarks)

        bookmarks = self.json_post('/bookmarks/add', {'group': group_name2, 'name': item, 'link': item_path},
                                   expect_status=200,
                                   as_json=True)
        self.assertListEqual([{'items': [{'name': item, 'path': item_path}],
                               'name': group_name2, 'editable': True}], bookmarks)
        self.logout()
        self.get('/bookmarks/get', expect_status=403)
