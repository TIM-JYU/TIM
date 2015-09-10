"""Unit tests for TIM routes."""

import json
import unittest

from flask.testing import FlaskClient
from documentmodel.document import Document

import tim
from timdbtest import TimDbTest


class TimTest(TimDbTest):
    """:type app: FlaskClient"""
    app = None

    @classmethod
    def setUpClass(cls):
        TimDbTest.setUpClass()
        tim.app.config['DATABASE'] = cls.db_path
        tim.app.config['TESTING'] = True
        cls.app = tim.app.test_client()

    def assertInResponse(self, expected, resp, expect_status=200):
        self.assertEqual(expect_status, resp.status_code)
        self.assertIn(expected, resp.get_data(as_text=True))

    def assertResponse(self, expected, resp, expect_status=200):
        self.assertEqual(expect_status, resp.status_code)
        self.assertEqual(expected, resp.get_data(as_text=True))

    def assertDictResponse(self, expected, resp, expect_status=200):
        self.assertEqual(expect_status, resp.status_code)
        self.assertDictEqual(expected, json.loads(resp.get_data(as_text=True)))

    def json_put(self, app, url, json_data=None):
        return self.json_req(app, url, json_data, 'PUT')

    def json_post(self, app, url, json_data=None):
        return self.json_req(app, url, json_data, 'POST')

    def json_req(self, app, url, json_data=None, method='GET'):
        return app.open(url,
                        data=json.dumps(json_data),
                        content_type='application/json',
                        method=method)

    def test_commenting(self):
        with TimTest.app as a:
            login_resp = a.post('/altlogin',
                                data={'email': 'test1@example.com', 'password': 'test1pass'},
                                follow_redirects=True)
            self.assertInResponse('Logged in as: Test user 1 (testuser1)', login_resp)
            doc_name = 'users/testuser1/testing'
            self.assertDictResponse({'id': 3, 'name': doc_name},
                                    self.json_post(a, '/createDocument', {
                                        'doc_name': doc_name
                                    }))
            self.assertResponse('Success',
                                self.json_put(a, '/addPermission/{}/{}/{}'.format(3, 'Anonymous users', 'view')))
            pars = Document(3).get_paragraphs()
            self.assertEqual(1, len(pars))
            first_id = pars[0].get_id()
            public_comment_text = 'This is a comment.'
            self.assertInResponse(public_comment_text,
                                  self.json_post(a,
                                                 '/postNote', {'text': public_comment_text,
                                                               'access': 'everyone',
                                                               'docId': 3,
                                                               'par': first_id}))

        with TimTest.app as a:
            login_resp = a.post('/altlogin',
                                data={'email': 'test2@example.com', 'password': 'test2pass'},
                                follow_redirects=True)
            self.assertInResponse('Logged in as: Test user 2 (testuser2)', login_resp)
            self.assertInResponse(public_comment_text, a.get('/view/' + doc_name))


if __name__ == '__main__':
    unittest.main()
