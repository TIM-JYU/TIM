"""Defines the TimRouteTest class."""

import json
import unittest
import socket
from functools import lru_cache
from typing import Union, Optional, List, Dict, Tuple

import flask
from flask import Response
from flask import session
from lxml import html

from tim_app import db
from timdbtest import TimDbTest

import tim
from documentmodel.document import Document
from flask.testing import FlaskClient


def load_json(resp: Response):
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

    # The expected content of an AJAX response that does not return any specific information.
    ok_resp = {'status': 'ok'}

    # The expected content of an AJAX response that returns a generic permission error.
    permission_error = {'error': "Sorry, you don't have permission to view this resource."}

    @classmethod
    def setUpClass(cls):
        TimDbTest.setUpClass()
        cls.app = testclient

    def assertResponseStatus(self, resp: Response, expect_status: int = 200, return_json: bool = False) -> object:
        """Asserts that the response has the specified status code and returns the response content either as text or JSON dict.

        :param resp: The response to be checked.
        :param expect_status: The expected status code.
        :param return_json: Whether to return JSON dict (True) or plain text (False).
        :return: The response as JSON dict or plain text.
        """
        resp_data = resp.get_data(as_text=True)
        self.assertEqual(expect_status, resp.status_code, msg=resp_data)
        if return_json:
            return json.loads(resp_data)
        else:
            return resp_data

    def assertInResponse(self, expected: str, resp: Response, expect_status: int = 200, json_key: Optional[str] = None, as_tree: bool = False) -> object:
        """Asserts that the response has the specified status code and contains the specified content.

        :param expected: The expected content.
        :param resp: The response object.
        :param expect_status: The expected status code.
        :param json_key: If specified, the response is interpreted as JSON and only the content of this key's value is
        taken into consideration.
        :param as_tree: Whether to interpret the response content as HTML tree. In this case, the value of parameter
        'expected' is interpreted as an XPATH selector that must be found in the tree.
        """
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

    def assertManyInResponse(self, expecteds: List[str], resp: Response, expect_status: int = 200) -> None:
        """Asserts that the response has the specified status code and contains each of the specified strings.

        :param expecteds: The list of expected values.
        :param resp: The response object.
        :param expect_status: The expected status code.
        """
        for e in expecteds:
            self.assertInResponse(e, resp, expect_status)

    def assertDictResponse(self, expected: Dict, resp: Response, expect_status: int = 200) -> None:
        """Asserts that the JSON response has the specified status code and is equal to the specified dict.

        :param expected: The expected dictionary.
        :param resp: The response object.
        :param expect_status: The expected status code.
        """
        self.assertEqual(expect_status, resp.status_code)
        self.assertDictEqual(expected, load_json(resp))

    def assertListResponse(self, expected, resp, expect_status=200):
        """Asserts that the JSON response has the specified status code and is equal to the specified list of dicts.

        :param expected: The expected list.
        :param resp: The response object.
        :param expect_status: The expected status code.
        """
        self.assertEqual(expect_status, resp.status_code)
        self.assertListEqual(expected, load_json(resp))

    def get(self, url: str, as_tree: bool = False, as_json: bool = False, as_response: bool = False, expect_status: Optional[int] = None,
            expect_content: Union[None, None] = None, **kwargs):
        """Performs a GET request.

        See the 'request' method for parameter explanations.
        """
        return self.request(url, 'GET', as_tree=as_tree, as_response=as_response, as_json=as_json, expect_status=expect_status, expect_content=expect_content, **kwargs)

    def post(self, url: str, as_tree=False, as_json=False, as_response=False, expect_status=None, expect_content=None,
             **kwargs):
        """Performs a GET request.

        See the 'request' method for parameter explanations.
        """
        return self.request(url, 'POST', as_tree=as_tree, as_response=as_response, as_json=as_json,
                            expect_status=expect_status, expect_content=expect_content, **kwargs)

    def request(self, url: str, method: str, as_tree: bool = False, as_json: bool = False, as_response: bool = False,
                expect_status: Optional[int] = None,
                expect_content: Union[None, str, Dict] = None,
                headers: Optional[List[Tuple[str,str]]] = None,
                **kwargs) -> Union[Response, str, Dict]:
        """Performs a request.

        For JSON requests, use the shortcut json_* methods.

        :param url: The request URL.
        :param method: The request method (e.g. GET, POST, PUT, DELETE).
        :param as_tree: Whether to return the response as an HTML tree.
        :param as_json: Whether to return the response as a JSON dict.
        :param as_response: Whether to return the raw response object.
        :param expect_status: The expected status code.
        :param expect_content: The expected response content.
         * If as_json is True, this parameter is interpreted as a dictionary that must match the response content.
         * If all of as_tree, as_json and as_response are False, this parameter is interpreted as a string
           that must match the response content.
        :param headers: Custom headers for the request.
        :param kwargs: Custom parameters to be passed to app.open method. Can be, for example, query_string={'a': 'b'}
           for passing URL arguments.
        :return: If as_tree is True: Returns the response as an HTML tree.
                 If as_json is True: Returns the response as a JSON dict.
                 If as_response is True: Returns the raw response object.
                 Otherwise: Returns the response as a string.
        """
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

    def json_put(self, url: str, json_data: Optional[Dict] = None, **kwargs):
        """Performs a JSON PUT request.

        :param url: The request URL.
        :param json_data: The JSON data to be submitted.
        :param kwargs: Any custom parameters that are accepted by the 'request' method.
        :return: See the 'request' method.
        """
        return self.json_req(url, json_data, 'PUT', **kwargs)

    def json_post(self, url: str, json_data: Optional[Dict]=None, **kwargs):
        """Performs a JSON POST request.

        :param url: The request URL.
        :param json_data: The JSON data to be submitted.
        :param kwargs: Any custom parameters that are accepted by the 'request' method.
        :return: See the 'request' method.
        """
        return self.json_req(url, json_data, 'POST', **kwargs)

    def json_req(self, url: str, json_data: Optional[Dict]=None, method: str='GET', **kwargs):
        """Performs a JSON request.

        :param url: The request URL.
        :param method: The request method.
        :param json_data: The JSON data to be submitted.
        :param kwargs: Any custom parameters that are accepted by the 'request' method.
        :return: See the 'request' method.
        """
        return self.request(url,
                            method=method,
                            data=json.dumps(json_data),
                            content_type='application/json',
                            as_response=True,
                            **kwargs)

    def post_par(self, doc: Document, text: str, par_id: str):
        """Edits a paragraph in a document.

        :param doc: The document to be edited.
        :param text: The new text for the paragraph.
        :param par_id: The id of the paragraph to be edited.
        :return: The response object.
        """
        doc.clear_mem_cache()
        return self.json_post('/postParagraph/', {
            "text": text,
            "docId": doc.doc_id,
            "par": par_id,
            "par_next": None
        })

    def new_par(self, doc: Document, text: str, next_id: Optional[str]=None):
        """Posts a new paragraph in a document.

        :param doc: The document to be edited.
        :param text: The text for the paragraph.
        :param next_id: The id of the paragraph following the new paragraph.
        :return: The response object.
        """
        doc.clear_mem_cache()
        return self.json_post('/newParagraph/', {
            "text": text,
            "docId": doc.doc_id,
            "par_next": next_id
        })

    @staticmethod
    def current_user_name() -> str:
        """Returns the name of the current user.

        :return: The name of the current user.
        """
        return session['user_name']

    def login_test1(self, force: bool = False, add: bool = False):
        """Logs testuser1 in.

        :param force: Whether to force the login route to be called even if the user is already logged in.
        :param add: Whether to add this user to the session group.
        :return: Response as a JSON dict.
        """
        return self.login('testuser1', 'test1@example.com', 'test1pass', force=force, add=add)

    def login_test2(self, force: bool = False, add: bool = False):
        """Logs testuser2 in.

        :param force: Whether to force the login route to be called even if the user is already logged in.
        :param add: Whether to add this user to the session group.
        :return: Response as a JSON dict.
        """
        return self.login('testuser2', 'test2@example.com', 'test2pass', force=force, add=add)

    def login_test3(self, force: bool = False, add: bool = False):
        """Logs testuser3 in.

        :param force: Whether to force the login route to be called even if the user is already logged in.
        :param add: Whether to add this user to the session group.
        :return: Response as a JSON dict.
        """
        return self.login('testuser3', 'test3@example.com', 'test3pass', force=force, add=add)

    def logout(self, user_id: Optional[int] = None):
        """Logs the specified user out.

        :param user_id: The id of the user to log out. If None, everyone in the session gets logged out.
        :return: Response as a JSON dict.
        """
        return self.json_post('/logout', json_data={'user_id': user_id}, expect_status=200, as_json=True)

    def login(self, username: str, email: str, passw: str, force: bool = False, clear_last_doc: bool = True, add: bool = False):
        """Logs a user in.

        :param username: The username of the user.
        :param email: The email of the user.
        :param passw: The password of the user.
        :param clear_last_doc: Whether to clear the last document information from session (TODO: This parameter is
               possibly not needed anymore).
        :param force: Whether to force the login route to be called even if the user is already logged in.
        :param add: Whether to add this user to the session group.
        :return: Response as a JSON dict.
        """
        if self.app.application.got_first_request:
            if not force and not add:
                database = self.get_db()
                u = database.users.get_user_by_name(username)
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

    def create_doc(self, docname: Optional[str] = None, from_file: Optional[str] = None, initial_par: Optional[str] = None,
                   settings: Optional[Dict] = None, assert_status: int = 200):
        """Creates a new document.

        :param docname: The path of the document.
        :param from_file: If specified, loads the document content from the specified file.
        :param initial_par: The content of the initial paragraph.
        :param settings: The settings for the document.
        :param assert_status: The expected status code for the createDocument route.
        :return: The document object.
        """
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
