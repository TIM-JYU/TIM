"""Defines the TimRouteTest class."""

import json
import unittest
import socket
from functools import lru_cache

import flask
from flask import session
from lxml import html

from tim_app import db
from timdbtest import TimDbTest

import tim
from documentmodel.document import Document
from flask.testing import FlaskClient


def load_json(resp):
    return json.loads(resp.get_data(as_text=True))


orig_getaddrinfo = socket.getaddrinfo


# noinspection PyIncorrectDocstring
@lru_cache(maxsize=100)
def fast_getaddrinfo(host, port, family=0, addrtype=0, proto=0, flags=0):
    """
    On Windows/Boot2docker, the getaddrinfo function is really slow,
    so we wrap the function and cache the result.
    """
    return orig_getaddrinfo(host, port, family, addrtype, proto, flags)


socket.getaddrinfo = fast_getaddrinfo

testclient = tim.app.test_client()
testclient = testclient.__enter__()  # type: FlaskClient


class TimRouteTest(TimDbTest):
    """
    A base class for running tests for TIM routes.
    """
    doc_num = 1
    ok_resp = {'status': 'ok'}
    permission_error = {'error': "Sorry, you don't have permission to view this resource."}

    @classmethod
    def setUpClass(cls):
        TimDbTest.setUpClass()
        cls.app = testclient

    def assertResponseStatus(self, resp, expect_status=200, return_json=False):
        resp_data = resp.get_data(as_text=True)
        self.assertEqual(expect_status, resp.status_code, msg=resp_data)
        if return_json:
            return json.loads(resp_data)
        else:
            return resp_data

    def assertInResponse(self, expected, resp, expect_status=200, json_key=None, as_tree=False):
        resp_text = resp.get_data(as_text=True)
        self.assertResponseStatus(resp, expect_status)
        if json_key is not None:
            resp_text = json.loads(resp_text)[json_key]
        assert_msg = """\n--------THIS TEXT:--------\n{}\n--------WAS NOT FOUND IN:---------\n{}""".format(
            expected, resp_text)
        if as_tree:
            self.assertLessEqual(1, len(html.fragment_fromstring(resp_text, create_parent=True).findall(expected)),
                                 msg=assert_msg)
        else:
            self.assertIn(expected, resp_text,
                          msg=assert_msg)

    def assertManyInResponse(self, expecteds, resp, expect_status=200):
        for e in expecteds:
            self.assertInResponse(e, resp, expect_status)

    def assertResponse(self, expected, resp, expect_status=200):
        self.assertEqual(expect_status, resp.status_code)
        self.assertEqual(expected, resp.get_data(as_text=True))

    def assertDictResponse(self, expected, resp, expect_status=200):
        self.assertEqual(expect_status, resp.status_code)
        self.assertDictEqual(expected, load_json(resp))

    def assertListResponse(self, expected, resp, expect_status=200):
        self.assertEqual(expect_status, resp.status_code)
        self.assertListEqual(expected, load_json(resp))

    def get(self, url: str, as_tree=False, as_json=False, expect_status=None, **kwargs):
        resp = self.app.get(url, **kwargs)
        if expect_status:
            self.assertResponseStatus(resp, expect_status)
        resp = resp.get_data(as_text=True)
        if as_tree:
            return html.fromstring(resp)
        elif as_json:
            return json.loads(resp)
        else:
            return resp

    def json_put(self, url: str, json_data=None):
        return self.json_req(url, json_data, 'PUT')

    def json_post(self, url: str, json_data=None):
        return self.json_req(url, json_data, 'POST')

    def json_req(self, url: str, json_data=None, method='GET', **kwargs):
        return self.app.open(url,
                             data=json.dumps(json_data),
                             content_type='application/json',
                             method=method,
                             **kwargs)

    def post_par(self, doc: Document, text: str, par_id: str):
        doc.clear_mem_cache()
        return self.json_post('/postParagraph/', {
            "text": text,
            "docId": doc.doc_id,
            "par": par_id,
            "par_next": None
        })

    def new_par(self, doc: Document, text: str, next_id=None):
        doc.clear_mem_cache()
        return self.json_post('/newParagraph/', {
            "text": text,
            "docId": doc.doc_id,
            "par_next": next_id
        })

    def current_user_name(self):
        return session['user_name']

    def login_test1(self, force=False):
        return self.login('testuser1', 'test1@example.com', 'test1pass', force=force)

    def login_test2(self, force=False):
        return self.login('testuser2', 'test2@example.com', 'test2pass', force=force)

    def login_test3(self, force=False):
        return self.login('testuser3', 'test3@example.com', 'test3pass', force=force)

    def logout(self):
        self.assertResponseStatus(self.app.post('/logout'), expect_status=302)

    def login(self, username, email, passw, force=False, clear_last_doc=True):
        if self.app.application.got_first_request:
            if not force:
                if flask.session.get('user_name') == username \
                        and flask.session.get('user_id') != 0:
                    return
            if clear_last_doc:
                with self.app.session_transaction() as s:
                    s.pop('last_doc', None)
                    s.pop('came_from', None)
        return self.app.post('/altlogin',
                             data={'email': email, 'password': passw},
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

    def tearDown(self):
        """While testing, the Flask-SQLAlchemy session needs to be removed manually;
        see https://pythonhosted.org/Flask-Testing/#testing-with-sqlalchemy"""
        db.session.remove()


if __name__ == '__main__':
    unittest.main()
