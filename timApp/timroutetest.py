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

    def get(self, url: str, as_tree=False, as_json=False, as_response=False, expect_status=None, expect_content=None, **kwargs):
        return self.request(url, 'GET', as_tree=as_tree, as_response=as_response, as_json=as_json, expect_status=expect_status, expect_content=expect_content, **kwargs)

    def post(self, url: str, as_tree=False, as_json=False, as_response=False, expect_status=None, expect_content=None, **kwargs):
        return self.request(url, 'POST', as_tree=as_tree, as_response=as_response, as_json=as_json, expect_status=expect_status, expect_content=expect_content, **kwargs)

    def request(self, url: str, method, as_tree=False, as_json=False, as_response=False, expect_status=None, expect_content=None, headers=None, **kwargs):
        if headers is None:
            headers = []
        headers.append(('X-Requested-With', 'XMLHttpRequest'))
        resp = self.app.open(url, method=method, headers=headers, **kwargs)
        if expect_status:
            self.assertResponseStatus(resp, expect_status)
        resp_data = resp.get_data(as_text=True)
        if as_tree:
            return html.fromstring(resp_data)
        elif as_json:
            loaded = json.loads(resp_data)
            if expect_content is not None:
                self.assertDictEqual(expect_content, loaded)
            return loaded
        elif as_response:
            return resp
        else:
            if expect_content is not None:
                self.assertEqual(expect_content, resp_data)
            return resp_data

    def json_put(self, url: str, json_data=None, **kwargs):
        return self.json_req(url, json_data, 'PUT', **kwargs)

    def json_post(self, url: str, json_data=None, **kwargs):
        return self.json_req(url, json_data, 'POST', **kwargs)

    def json_req(self, url: str, json_data=None, method='GET', **kwargs):
        return self.request(url,
                            method=method,
                            data=json.dumps(json_data),
                            content_type='application/json',
                            as_response=True,
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

    def login_test1(self, force=False, add=False):
        return self.login('testuser1', 'test1@example.com', 'test1pass', force=force, add=add)

    def login_test2(self, force=False, add=False):
        return self.login('testuser2', 'test2@example.com', 'test2pass', force=force, add=add)

    def login_test3(self, force=False, add=False):
        return self.login('testuser3', 'test3@example.com', 'test3pass', force=force, add=add)

    def logout(self, user_id=None):
        return self.json_post('/logout', json_data={'user_id': user_id}, expect_status=200, as_json=True)

    def login(self, username, email, passw, force=False, clear_last_doc=True, add=False):
        if self.app.application.got_first_request:
            if not force and not add:
                db = self.get_db()
                u = db.users.get_user_by_name(username)
                # if not flask.has_request_context():
                #     print('creating request context')
                #     tim.app.test_request_context().__enter__()
                with self.app.session_transaction() as s:
                    s['user_name'] = username
                    s['email'] = email
                    s['user_id'] = u.id
                    s['real_name'] = u.real_name
                    s.pop('other_users', None)
                self.app.session_transaction().__enter__()
                return
            if clear_last_doc:
                with self.app.session_transaction() as s:
                    s.pop('last_doc', None)
                    s.pop('came_from', None)
        return self.post('/altlogin',
                         data={'email': email, 'password': passw, 'add_user': add},
                         follow_redirects=True, as_json=True)

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
