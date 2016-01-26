from routes.common import *
from timroutetest import TimRouteTest


class FolderTest(TimRouteTest):
    def test_folders(self):
        self.login_test1()
        db = self.get_db()
        fname = "users/{}/testing".format(session['user_name'])
        resp = self.json_post('/createFolder',
                              {"name": fname,
                               "owner": db.users.get_personal_usergroup_by_id(session['user_id'])})
        j = self.assertResponseStatus(resp, return_json=True)
        self.assertEqual(fname, j['name'])
        self.assertIsInstance(j['id'], int)
        new_name = fname + '1'
        resp = self.json_put('/rename/{}'.format(j['id']), {"new_name": new_name})
        j = self.assertResponseStatus(resp, return_json=True)
        self.assertEqual(new_name, j['new_name'])
        db.close()
