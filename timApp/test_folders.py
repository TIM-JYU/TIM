from routes.common import *
from timroutetest import TimRouteTest


class FolderTest(TimRouteTest):
    def get_folder(self, path):
        return 'users/{}/{}'.format(session['user_name'], path)

    def create_folder(self, fname, expect_status=200, **kwargs):
        j = self.json_post('/createFolder',
                           {"name": fname,
                            "owner": session['user_name']}, expect_status=expect_status, as_json=True, **kwargs)
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

        self.assertListResponse([{'id': j['id'], 'name': 'testing1',
                                  'isOwner': True,
                                  'isFolder': True,
                                  'modified': None,
                                  'canEdit': True,
                                  'fullname': new_name,
                                  'owner': {'id': 7, 'name': 'testuser1'},
                                  'unpublished': True},
                                 {'id': j3['id'], 'name': 'testing2',
                                  'isOwner': True,
                                  'isFolder': True,
                                  'modified': None,
                                  'canEdit': True,
                                  'fullname': fname2,
                                  'owner': {'id': 7, 'name': 'testuser1'},
                                  'unpublished': False}],
                                self.json_req('/getItems', query_string={'folder': user_folder}))
        self.logout()
        self.assertListResponse([{'id': j3['id'], 'name': 'testing2',
                                  'isOwner': False,
                                  'isFolder': True,
                                  'modified': None,
                                  'canEdit': False,
                                  'fullname': fname2,
                                  'owner': {'id': 7, 'name': 'testuser1'},
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
