from tests.db.timdbtest import TEST_USER_2_ID
from tests.server.timroutetest import TimRouteTest


class FolderTest(TimRouteTest):
    def get_personal_folder_path(self, path):
        return 'users/{}/{}'.format(self.current_user_name(), path)

    def test_folder_manage(self):
        self.login_test3()
        f = self.create_folder(self.get_personal_folder_path('test_manage'))
        self.get('/manage/{}'.format(f['path']))
        self.login_test2()
        self.get('/manage/{}'.format(f['path']), expect_status=403)
        db = self.get_db()
        db.users.grant_access(db.users.get_personal_usergroup_by_id(TEST_USER_2_ID), f['id'], 'manage')
        self.get('/manage/{}'.format(f['path']))

    def test_folder_delete(self):
        self.login_test1()
        to_delete = self.get_personal_folder_path('delete')
        f = self.create_folder(to_delete)
        timdb = self.get_db()
        timdb.users.grant_view_access(timdb.users.get_anon_group_id(), f['id'])
        self.delete('/folders/{}'.format(f['id']), expect_content=self.ok_resp)

    def test_intermediate_folders(self):
        self.login_test1()
        fname = self.get_personal_folder_path('a/b/c/d')
        self.create_folder(fname)

    def test_folders(self):
        self.login_test1()
        db = self.get_db()
        user_folder = 'users/{}'.format(self.current_user_name())
        fname = self.get_personal_folder_path('testing')

        f = self.create_folder(fname)
        self.create_folder(fname,
                           expect_content={'error': 'Item with a same name already exists.'},
                           expect_status=403)
        new_name = fname + '1'
        f2 = self.json_put('/rename/{}'.format(f['id']), {"new_name": new_name})
        self.assertEqual(new_name, f2['new_name'])
        self.json_put('/rename/{}'.format(f['id']),
                      {"new_name": new_name + '/testing1'}, expect_status=403,
                      expect_content={'error': 'A folder cannot contain itself.'}),

        # Create another folder and give access to anonymous users
        fname2 = self.get_personal_folder_path('testing2')
        f3 = self.create_folder(fname2)
        db.users.grant_access(db.users.get_anon_group_id(), f3['id'], 'view')
        self.maxDiff = None
        folder_loc = 'users/testuser1'
        self.get('/getItems', query_string={'folder': user_folder},
                 expect_content=[{'name': 'testing1',
                                  'title': 'foldertitle',
                                  'id': f['id'],
                                  'isFolder': True,
                                  'modified': 'just now',
                                  'path': new_name,
                                  'location': folder_loc,
                                  'owner': {'id': 7, 'name': 'testuser1'},
                                  'rights': {'browse_own_answers': True,
                                             'can_comment': True,
                                             'can_mark_as_read': True,
                                             'editable': True,
                                             'manage': True,
                                             'owner': True,
                                             'see_answers': True,
                                             'teacher': True},
                                  'unpublished': True,
                                  'public': True},
                                 {'name': 'testing2',
                                  'title': 'foldertitle',
                                  'id': f3['id'],
                                  'isFolder': True,
                                  'modified': 'just now',
                                  'path': fname2,
                                  'location': folder_loc,
                                  'owner': {'id': 7, 'name': 'testuser1'},
                                  'rights': {'browse_own_answers': True,
                                             'can_comment': True,
                                             'can_mark_as_read': True,
                                             'editable': True,
                                             'manage': True,
                                             'owner': True,
                                             'see_answers': True,
                                             'teacher': True},
                                  'unpublished': False,
                                  'public': True}])
        self.logout()
        self.get('/getItems', query_string={'folder': user_folder},
                 expect_content=[{'name': 'testing2',
                                  'title': 'foldertitle',
                                  'id': f3['id'],
                                  'isFolder': True,
                                  'modified': 'just now',
                                  'path': fname2,
                                  'location': folder_loc,
                                  'owner': {'id': 7, 'name': 'testuser1'},
                                  'rights': {'browse_own_answers': False,
                                             'can_comment': False,
                                             'can_mark_as_read': False,
                                             'editable': False,
                                             'manage': False,
                                             'owner': False,
                                             'see_answers': False,
                                             'teacher': False},
                                  'unpublished': False,
                                  'public': True}])

    def test_folders_invalid(self):
        self.login_test1()
        invalid = self.get_personal_folder_path('/test')
        invalid2 = "test"
        invalid3 = "1234"
        self.create_folder(invalid,
                           expect_content={'error': 'The folder name cannot have empty parts.'},
                           expect_status=400)
        self.create_folder(invalid2,
                           expect_content={'error': 'You cannot create folders in this folder. Try users/{} '
                                                    'instead.'.format(self.current_user_name())},
                           expect_status=403)
        self.create_folder(invalid3,
                           expect_content={
                               'error': 'The folder name can not be a number to avoid confusion with document id.'},
                           expect_status=400)
