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
                               "owner": personal_group_id})
        j = self.assertResponseStatus(resp, return_json=True)
        self.assertEqual(fname, j['name'])
        self.assertIsInstance(j['id'], int)
        new_name = fname + '1'
        resp = self.json_put('/rename/{}'.format(j['id']), {"new_name": new_name})
        j2 = self.assertResponseStatus(resp, return_json=True)
        self.assertEqual(new_name, j2['new_name'])

        # Create another folder and give access to anonymous users
        fname2 = "{}/{}".format(user_folder, 'testing2')
        resp = self.json_post('/createFolder',
                              {"name": fname2,
                               "owner": personal_group_id})
        j3 = self.assertResponseStatus(resp, return_json=True)
        db.users.grant_access(db.users.get_anon_group_id(), j3['id'], 'view')

        self.assertListResponse([{'id': j['id'], 'name': 'testing1',
                                  'isOwner': True,
                                  'fullname': new_name,
                                  'owner': {'id': 7, 'name': 'testuser1'}},
                                 {'id': j3['id'], 'name': 'testing2',
                                  'isOwner': True,
                                  'fullname': fname2,
                                  'owner': {'id': 7, 'name': 'testuser1'}}],
                                self.json_req('/getFolders', query_string={'root_path': user_folder}))
        self.logout()
        self.assertListResponse([{'id': j3['id'], 'name': 'testing2',
                                  'isOwner': False,
                                  'fullname': fname2,
                                  'owner': {'id': 7, 'name': 'testuser1'}}],
                                self.json_req('/getFolders', query_string={'root_path': user_folder}))
        db.close()
