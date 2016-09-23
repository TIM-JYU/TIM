from timroutetest import TimRouteTest


class BookmarkTest(TimRouteTest):
    def test_bookmarks(self):
        self.login_test1()
        bookmarks = self.get('/bookmarks/get', expect_status=200, as_json=True)
        self.assertListEqual([], bookmarks)
        group_name = 'mygroup'
        group_name2 = 'mygroup2'
        item = 'test item'
        item_path = 'some/path/to/item'
        bookmarks = self.post('/bookmarks/createGroup/{}'.format(group_name), expect_status=200, as_json=True)
        self.assertListEqual([{'name': 'mygroup', 'items': [], 'editable': True}], bookmarks)
        bookmarks = self.post('/bookmarks/add/{}/{}/{}'.format(group_name2, item, item_path), expect_status=200,
                              as_json=True)
        self.assertListEqual([{'items': [], 'name': group_name, 'editable': True},
                              {'items': [{'name': item, 'path': item_path}],
                               'name': group_name2, 'editable': True}], bookmarks)

        bookmarks = self.post('/bookmarks/deleteGroup/{}'.format(group_name), expect_status=200,
                              as_json=True)
        self.assertListEqual([{'items': [{'name': item, 'path': item_path}],
                               'name': group_name2, 'editable': True}], bookmarks)
        bookmarks = self.post('/bookmarks/deleteGroup/{}'.format(group_name), expect_status=200,
                              as_json=True)
        self.assertListEqual([{'items': [{'name': item, 'path': item_path}],
                               'name': group_name2, 'editable': True}], bookmarks)

        bookmarks = self.post('/bookmarks/delete/{}/{}'.format(group_name2, item), expect_status=200,
                              as_json=True)
        self.assertListEqual([{'items': [], 'name': group_name2, 'editable': True}], bookmarks)
        bookmarks = self.post('/bookmarks/delete/{}/{}'.format(group_name2, item), expect_status=200,
                              as_json=True)
        self.assertListEqual([{'items': [], 'name': group_name2, 'editable': True}], bookmarks)

        bookmarks = self.post('/bookmarks/add/{}/{}/{}'.format(group_name2, item, item_path), expect_status=200,
                              as_json=True)
        self.assertListEqual([{'items': [{'name': item, 'path': item_path}],
                               'name': group_name2, 'editable': True}], bookmarks)
        self.logout()
        self.get('/bookmarks/get', expect_status=403)
