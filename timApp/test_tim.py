"""Unit tests for TIM routes."""

import json
import unittest
from flask import session

from flask.testing import FlaskClient
from documentmodel.document import Document
from markdownconverter import md_to_html

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

    def assertResponseStatus(self, resp, expect_status=200):
        self.assertEqual(expect_status, resp.status_code)

    def assertInResponse(self, expected, resp, expect_status=200):
        self.assertResponseStatus(resp, expect_status)
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
        timdb = self.get_db()
        with TimTest.app as a:
            login_resp = a.post('/altlogin',
                                data={'email': 'test1@example.com', 'password': 'test1pass'},
                                follow_redirects=True)
            self.assertInResponse('Logged in as: Test user 1 (testuser1)', login_resp)
            doc_name = 'users/testuser1/testing'
            doc_id = 3
            self.assertDictResponse({'id': doc_id, 'name': doc_name},
                                    self.json_post(a, '/createDocument', {
                                        'doc_name': doc_name
                                    }))
            self.assertResponse('Success',
                                self.json_put(a, '/addPermission/{}/{}/{}'.format(3, 'Anonymous users', 'view')))
            pars = Document(doc_id).get_paragraphs()
            self.assertEqual(1, len(pars))
            first_id = pars[0].get_id()
            comment_of_test1 = 'This is a comment.'
            html_comment_of_test1 = md_to_html(comment_of_test1)
            self.assertInResponse(html_comment_of_test1,
                                  self.json_post(a,
                                                 '/postNote', {'text': comment_of_test1,
                                                               'access': 'everyone',
                                                               'docId': doc_id,
                                                               'par': first_id}))
            self.assertInResponse(html_comment_of_test1, a.get('/view/' + doc_name))
            par_text = 'testing editing now...\nnew line\n'
            par_html = md_to_html(par_text)
            self.assertEqual('<p>testing editing now... new line</p>', par_html)
            self.assertInResponse(par_html, self.json_post(a, '/postParagraph/', {
                "text": par_text,
                "docId": doc_id,
                "par": first_id,
                "par_next": None,
                "area_start": None,
                "area_end": None
            }))
            self.assertDictResponse({'text': par_text}, a.get('/getBlock/{}/{}'.format(doc_id, first_id)))
            self.assertInResponse(par_html, self.json_post(a, '/postParagraph/', {
                "text": par_text,
                "docId": doc_id,
                "par": first_id,
                "par_next": None,
                "area_start": None,
                "area_end": None
            }))

        with TimTest.app as a:
            login_resp = a.post('/altlogin',
                                data={'email': 'test2@example.com', 'password': 'test2pass'},
                                follow_redirects=True)
            self.assertInResponse('Logged in as: Test user 2 (testuser2)', login_resp)
            self.assertInResponse(comment_of_test1, a.get('/view/' + doc_name))
            comment_of_test2 = 'g8t54h954hy95hg54h'
            self.assertInResponse(comment_of_test2,
                                  self.json_post(a,
                                                 '/postNote', {'text': comment_of_test2,
                                                               'access': 'everyone',
                                                               'docId': doc_id,
                                                               'par': first_id}))

            ug = timdb.users.getPersonalUserGroup(session['user_id'])
            notes = timdb.notes.getNotes(ug, Document(doc_id), include_public=False)
            self.assertEqual(1, len(notes))
            test2_note_id = notes[0]['id']

        with TimTest.app as a:
            login_resp = a.post('/altlogin',
                                data={'email': 'test1@example.com', 'password': 'test1pass'},
                                follow_redirects=True)
            self.assertInResponse('Logged in as: Test user 1 (testuser1)', login_resp)
            self.assertInResponse(comment_of_test2,
                                  a.get('/note/{}'.format(test2_note_id)))
            self.assertResponseStatus(self.json_post(a,
                                                     '/deleteNote', {'id': test2_note_id,
                                                                     'docId': doc_id,
                                                                     'par': first_id}))
            ug = timdb.users.getPersonalUserGroup(session['user_id'])
            notes = timdb.notes.getNotes(ug, Document(doc_id), include_public=True)
            self.assertEqual(1, len(notes))


if __name__ == '__main__':
    unittest.main()
