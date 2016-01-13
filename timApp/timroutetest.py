"""Defines the TimRouteTest class."""

import json
import unittest

import flask

import tim
from documentmodel.document import Document
from timdbtest import TimDbTest


def load_json(resp):
    return json.loads(resp.get_data(as_text=True))


class TimRouteTest(TimDbTest):
    """
    A base class for running tests for TIM routes.
    """
    doc_num = 1

    @classmethod
    def setUpClass(cls):
        TimDbTest.setUpClass()
        tim.app.config['DATABASE'] = cls.db_path
        tim.app.config['TESTING'] = True
        cls.app = tim.app.test_client()
        cls.app = cls.app.__enter__()

    def assertResponseStatus(self, resp, expect_status=200, return_json=False):
        resp_data = resp.get_data(as_text=True)
        self.assertEqual(expect_status, resp.status_code, msg=resp_data)
        if return_json:
            return json.loads(resp_data)
        else:
            return resp_data

    def assertInResponse(self, expected, resp, expect_status=200, json_key=None):
        resp_text = resp.get_data(as_text=True)
        self.assertResponseStatus(resp, expect_status)
        if json_key is not None:
            resp_text = json.loads(resp_text)[json_key]
        self.assertIn(expected, resp_text,
                      msg="""\n--------THIS TEXT:--------\n{}\n--------WAS NOT FOUND IN:---------\n{}""".format(
                              expected, resp_text))

    def assertManyInResponse(self, expecteds, resp, expect_status=200):
        for e in expecteds:
            self.assertInResponse(e, resp, expect_status)

    def assertResponse(self, expected, resp, expect_status=200):
        self.assertEqual(expect_status, resp.status_code)
        self.assertEqual(expected, resp.get_data(as_text=True))

    def assertDictResponse(self, expected, resp, expect_status=200):
        self.assertEqual(expect_status, resp.status_code)
        self.assertDictEqual(expected, load_json(resp))

    def json_put(self, url, json_data=None):
        return self.json_req(url, json_data, 'PUT')

    def json_post(self, url, json_data=None):
        return self.json_req(url, json_data, 'POST')

    def json_req(self, url, json_data=None, method='GET'):
        return self.app.open(url,
                             data=json.dumps(json_data),
                             content_type='application/json',
                             method=method)

    def post_par(self, doc, text, par_id):
        doc.version = None  # Force version update when calling get_version
        return self.json_post('/postParagraph/', {
            "text": text,
            "docId": doc.doc_id,
            "par": par_id,
            "par_next": None
        })

    def new_par(self, doc, text, next_id=None):
        doc.version = None  # Force version update when calling get_version
        return self.json_post('/newParagraph/', {
            "text": text,
            "docId": doc.doc_id,
            "par_next": next_id
        })

    def login_test1(self, force=False):
        # Make a bogus request; something is wrong with session being cleared after a request if we're in a different
        # test class when running multiple tests at once
        self.app.get('/zzz')
        if not force \
                and self.app.application.got_first_request \
                and flask.session.get('user_name') == 'testuser1'\
                and flask.session.get('user_id') != 0:
            return
        return self.app.post('/altlogin',
                             data={'email': 'test1@example.com', 'password': 'test1pass'},
                             follow_redirects=True)

    def login_test2(self, force=False):
        if not force and self.app.application.got_first_request and flask.session['user_name'] == 'testuser2':
            return
        return self.app.post('/altlogin',
                             data={'email': 'test2@example.com', 'password': 'test2pass'},
                             follow_redirects=True)

    def create_doc(self, docname=None, from_file=None, initial_par=None, settings=None, assert_status=200):
        if docname is None:
            docname = 'users/{}/doc{}'.format(flask.session['user_name'], self.doc_num)
            self.__class__.doc_num += 1
        resp = self.json_post('/createDocument', {
            'doc_name': docname
        })
        self.assertResponseStatus(resp, assert_status)
        resp_data = load_json(resp)
        doc = Document(resp_data['id'])
        if from_file is not None:
            with open(from_file, encoding='utf-8') as f:
                self.new_par(doc, f.read())
        elif initial_par is not None:
            self.new_par(doc, initial_par)
        if settings is not None:
            doc.set_settings(settings)
        return doc


if __name__ == '__main__':
    unittest.main()
