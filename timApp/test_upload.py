import io
from flask import session

from timroutetest import TimRouteTest


class UploadTest(TimRouteTest):
    def test_upload_permissions(self):
        db = self.get_db()
        db.users.create_user_with_group('testuser3', 'Test user 3', 'test3@example.com', password='test3pass',
                                        is_admin=True)
        self.login_test1()
        resp = self.app.post('/upload/', data={'folder': 'users/{}'.format(session['user_name']),
                                               'file': (io.BytesIO(b'test file'),
                                                        'test.md')})
        self.assertResponseStatus(resp)
        fname = 'custom'
        resp = self.app.post('/upload/', data={'folder': fname,
                                               'file': (io.BytesIO(b'test file'),
                                                        'test.md')})
        self.assertDictResponse({'error': 'You cannot create documents in this folder. '
                                          'Try users/{} instead.'.format(session['user_name'])}, resp,
                                expect_status=403)
        test1_group = db.users.get_personal_usergroup_by_id(session['user_id'])
        self.login_test3()

        resp = self.json_post('/createFolder',
                              {'name': fname,
                               'owner': db.users.get_personal_usergroup_by_id(session['user_id'])})
        j = self.assertResponseStatus(resp, return_json=True)
        db.users.grant_access(test1_group, j['id'], 'edit')

        self.login_test1()
        resp = self.app.post('/upload/', data={'folder': fname,
                                               'file': (io.BytesIO(b'test file'),
                                                        'test.md')})
        self.assertResponseStatus(resp)

        db.close()
