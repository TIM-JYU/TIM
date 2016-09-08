from routes.common import *
from timroutetest import TimRouteTest


class FolderTest(TimRouteTest):
    def test_folders(self):
        self.login_test1()
        db = self.get_db()
        user_folder = 'users/{}'.format(session['user_name'])
        fname = "{}/testing".format(user_folder)

        personal_group_id = db.users.get_personal_usergroup_by_id(session['user_id'])
        resp = self.json_post('/createFolder',
                              {"name": fname,
                               "owner": session['user_name']})
        j = self.assertResponseStatus(resp, return_json=True)
        self.assertEqual(fname, j['name'])
        self.assertIsInstance(j['id'], int)
        self.assertDictResponse({'error': 'Item with a same name already exists.'},
                                self.json_post('/createFolder',
                                               {"name": fname,
                                                "owner": session['user_name']}),
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
        fname2 = "{}/{}".format(user_folder, 'testing2')
        resp = self.json_post('/createFolder',
                              {"name": fname2,
                               "owner": session['user_name']})
        j3 = self.assertResponseStatus(resp, return_json=True)
        db.users.grant_access(db.users.get_anon_group_id(), j3['id'], 'view')

        self.assertListResponse([{'id': j['id'], 'name': 'testing1',
                                  'isOwner': True,
                                  'fullname': new_name,
                                  'owner': {'id': 7, 'name': 'testuser1'},
                                  'unpublished': True},
                                 {'id': j3['id'], 'name': 'testing2',
                                  'isOwner': True,
                                  'fullname': fname2,
                                  'owner': {'id': 7, 'name': 'testuser1'},
                                  'unpublished': False}],
                                self.json_req('/getFolders', query_string={'root_path': user_folder}))
        self.logout()
        self.assertListResponse([{'id': j3['id'], 'name': 'testing2',
                                  'isOwner': False,
                                  'fullname': fname2,
                                  'owner': {'id': 7, 'name': 'testuser1'},
                                  'unpublished': False}],
                                self.json_req('/getFolders', query_string={'root_path': user_folder}))
        db.close()

    def test_folders_invalid(self):
        self.login_test1()
        db = self.get_db()
        user_folder = 'users/{}'.format(session['user_name'])
        invalid = "{}//test".format(user_folder)
        invalid2 = "test"
        invalid3 = "1234"
        personal_group_id = db.users.get_personal_usergroup_by_id(session['user_id'])
        self.assertDictResponse({'error': 'The folder name cannot have empty parts.'},
                                self.json_post('/createFolder',
                                               {"name": invalid,
                                                "owner": session['user_name']}),
                                expect_status=400)
        self.assertDictResponse({'error': 'You cannot create folders in this folder. Try users/{} '
                                          'instead.'.format(session['user_name'])},
                                self.json_post('/createFolder',
                                               {"name": invalid2,
                                                "owner": session['user_name']}),
                                expect_status=403)
        self.assertDictResponse({'error': 'The folder name can not be a number to avoid confusion with document id.'},
                                self.json_post('/createFolder',
                                               {"name": invalid3,
                                                "owner": session['user_name']}),
                                expect_status=400)
