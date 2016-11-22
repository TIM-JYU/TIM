from routes.common import *
from tests.server.timroutetest import TimRouteTest


class FolderTest(TimRouteTest):
    def get_folder(self, path):
        return 'users/{}/{}'.format(session['user_name'], path)

    def create_folder(self, fname, expect_status=200, **kwargs):
        j = self.json_post('/createItem',
                           {'item_path': fname,
                            'item_type': 'folder'}, expect_status=expect_status, as_json=True, **kwargs)
        if expect_status == 200:
            self.assertEqual(fname, j['name'])
            self.assertIsInstance(j['id'], int)
        return j

    def test_folder_delete(self):
        self.login_test1()
        to_delete = self.get_folder('delete')
        j = self.create_folder(to_delete)
        timdb = self.get_db()
        timdb.users.grant_view_access(timdb.users.get_anon_group_id(), j['id'])
        self.delete('/folders/{}'.format(j['id']), expect_status=200, as_json=True, expect_content=self.ok_resp)

    def test_intermediate_folders(self):
        self.login_test1()
        fname = self.get_folder('a/b/c/d')
        self.create_folder(fname)

    def test_folders(self):
        self.login_test1()
        db = self.get_db()
        user_folder = 'users/{}'.format(session['user_name'])
        fname = self.get_folder('testing')

        j = self.create_folder(fname)
        self.create_folder(fname,
                           expect_content={'error': 'Item with a same name already exists.'},
                           expect_status=403)
        new_name = fname + '1'
        resp = self.json_put('/rename/{}'.format(j['id']), {"new_name": new_name})
        j2 = self.assertResponseStatus(resp, return_json=True)
        self.assertEqual(new_name, j2['new_name'])
        self.assertDictResponse({'error': 'A folder cannot contain itself.'},
                                self.json_put('/rename/{}'.format(j['id']),
                                              {"new_name": new_name + '/testing1'}),
                                expect_status=403)

        # Create another folder and give access to anonymous users
        fname2 = self.get_folder('testing2')
        j3 = self.create_folder(fname2)
        db.users.grant_access(db.users.get_anon_group_id(), j3['id'], 'view')

        folder_loc = 'users/testuser1'
        self.assertListResponse([{'name': 'testing1',
                                  'title': 'testing1',
                                  'id': j['id'],
                                  'isFolder': True,
                                  'modified': None,
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
                                  'unpublished': True},
                                 {'name': 'testing2',
                                  'title': 'testing2',
                                  'id': j3['id'],
                                  'isFolder': True,
                                  'modified': None,
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
                                  'unpublished': False}]
                                ,
                                self.json_req('/getItems', query_string={'folder': user_folder}))
        self.logout()
        self.assertListResponse([{'name': 'testing2',
                                  'title': 'testing2',
                                  'id': j3['id'],
                                  'isFolder': True,
                                  'modified': None,
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
                                  'unpublished': False}],
                                self.json_req('/getItems', query_string={'folder': user_folder}))
        db.close()

    def test_folders_invalid(self):
        self.login_test1()
        invalid = self.get_folder('/test')
        invalid2 = "test"
        invalid3 = "1234"
        self.create_folder(invalid,
                           expect_content={'error': 'The folder name cannot have empty parts.'},
                           expect_status=400)
        self.create_folder(invalid2,
                           expect_content={'error': 'You cannot create folders in this folder. Try users/{} '
                                                    'instead.'.format(session['user_name'])},
                           expect_status=403)
        self.create_folder(invalid3,
                           expect_content={
                               'error': 'The folder name can not be a number to avoid confusion with document id.'},
                           expect_status=400)
