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

    def assertManyInResponse(self, expecteds, resp, expect_status=200):
        for e in expecteds:
            self.assertInResponse(e, resp, expect_status)

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

    def test_activities(self):
        timdb = self.get_db()
        a = TimTest.app

        login_resp = a.post('/altlogin',
                            data={'email': 'test1@example.com', 'password': 'test1pass'},
                            follow_redirects=True)
        self.assertInResponse('Logged in as: Test user 1 (testuser1)', login_resp)
        doc_names = ['users/testuser1/testing',
                     'users/testuser1/testing2',
                     'users/testuser1/testing3',
                     'users/testuser1/testing4',
                     'users/testuser1/testing5']
        doc_name = doc_names[0]
        doc_id = 3
        doc_ids = set()
        for idx, n in enumerate(doc_names):
            self.assertDictResponse({'id': doc_id + idx, 'name': doc_names[idx]},
                                    self.json_post(a, '/createDocument', {
                                        'doc_name': n
                                    }))
            doc_ids.add(doc_id + idx)
        self.assertResponse('Success',
                            self.json_put(a, '/addPermission/{}/{}/{}'.format(3, 'Anonymous users', 'view')))
        self.assertResponse('Success',
                            self.json_put(a, '/addPermission/{}/{}/{}'.format(4, 'Logged-in users', 'view')))
        self.assertResponse('Success',
                            self.json_put(a, '/addPermission/{}/{}/{}'.format(5, 'testuser2', 'view')))
        self.assertResponse('Success',
                            self.json_put(a, '/addPermission/{}/{}/{}'.format(6, 'testuser2', 'edit')))
        Document(doc_id).add_paragraph('Hello')
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
        self.assertInResponse(par_html, self.json_post(a, '/postParagraph/', {
            "text": par_text,
            "docId": doc_id,
            "par": first_id,
            "par_next": None
        }))
        self.assertDictResponse({'text': par_text}, a.get('/getBlock/{}/{}'.format(doc_id, first_id)))
        self.assertInResponse(par_html, self.json_post(a, '/postParagraph/', {
            "text": par_text,
            "docId": doc_id,
            "par": first_id,
            "par_next": None
        }))
        par2_text = 'new par'
        par2_html = md_to_html(par2_text)
        self.assertManyInResponse([par_html, par2_html], self.json_post(a, '/postParagraph/', {
            "text": par_text + '#-\n' + par2_text,
            "docId": doc_id,
            "par": first_id,
            "par_next": None
        }))
        pars = Document(doc_id).get_paragraphs()
        self.assertEqual(2, len(pars))
        second_par_id = pars[1].get_id()
        par2_new_text = '    ' + par2_text
        par2_new_html = md_to_html(par2_new_text)
        self.assertInResponse(par2_new_html, self.json_post(a, '/postParagraph/', {
            "text": par2_new_text,
            "docId": doc_id,
            "par": second_par_id,
            "par_next": None
        }))
        self.assertResponseStatus(a.post('/logout', follow_redirects=True))
        self.assertResponseStatus(a.get('/settings/'), 403)
        for d in doc_ids - {3}:
            self.assertResponseStatus(a.get('/view/' + str(d)), 403)
        self.assertResponseStatus(a.get('/view/' + str(3)))

        a.post('/altlogin',
               data={'email': 'test2@example.com', 'password': 'test2pass'})
        view_resp = a.get('/view/' + doc_name)
        self.assertInResponse('Logged in as: Test user 2 (testuser2)', view_resp)
        self.assertInResponse(comment_of_test1, view_resp)
        not_viewable_docs = {7}
        for view_id in doc_ids - not_viewable_docs:
            self.assertResponseStatus(a.get('/view/' + str(view_id)))

        for view_id in not_viewable_docs:
            self.assertResponseStatus(a.get('/view/' + str(view_id)), 403)
        self.assertResponseStatus(a.get('/view/1'), 404)

        with a as a:
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

        a.post('/altlogin',
               data={'email': 'test1@example.com', 'password': 'test1pass'},
               follow_redirects=True)
        self.assertInResponse(comment_of_test2,
                              a.get('/note/{}'.format(test2_note_id)))
        with a as a:
            self.assertResponseStatus(self.json_post(a,
                                                     '/deleteNote', {'id': test2_note_id,
                                                                     'docId': doc_id,
                                                                     'par': first_id}))
            ug = timdb.users.getPersonalUserGroup(session['user_id'])
            notes = timdb.notes.getNotes(ug, Document(doc_id), include_public=True)
            self.assertEqual(1, len(notes))


if __name__ == '__main__':
    unittest.main()
