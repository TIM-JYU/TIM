"""Defines the TimRouteTest class."""
import base64
import io
import json
import re
import socket
import warnings
from base64 import b64encode
from contextlib import contextmanager
from functools import lru_cache
from typing import Union, Optional, List, Dict, Tuple, Any
from urllib.parse import urlparse

import responses
from flask import Response, current_app
from flask import session
from flask.testing import FlaskClient
from lxml import html
from lxml.html import HtmlElement
from requests import PreparedRequest

import timApp.tim
from timApp.answer.answer import Answer
from timApp.auth.login import log_in_as_anonymous
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document
from timApp.document.specialnames import TEMPLATE_FOLDER_NAME, PREAMBLE_FOLDER_NAME
from timApp.document.timjsonencoder import TimJsonEncoder
from timApp.document.translation.translation import Translation
from timApp.item.routes import create_item_direct
from timApp.messaging.messagelist.listinfo import ArchiveType
from timApp.messaging.messagelist.mailman_events import NewMessageEvent, SubscriptionEvent, EVENTS, MailmanMessageList
from timApp.messaging.messagelist.messagelist_models import MessageListModel
from timApp.plugin import containerLink
from timApp.plugin.containerLink import do_request
from timApp.readmark.readparagraphtype import ReadParagraphType
from timApp.tests.db.timdbtest import TimDbTest
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.utils import remove_prefix


def load_json(resp: Response):
    return json.loads(resp.get_data(as_text=True))


def is_redirect(response: Response):
    return response.status_code in (302, 303)


orig_getaddrinfo = socket.getaddrinfo

TEXTUAL_MIMETYPES = {'text/html', 'application/json', 'text/plain'}
LOCALHOST = 'http://localhost/'

BasicAuthParams = Tuple[str, str]


@lru_cache(maxsize=100)
def fast_getaddrinfo(host, port, family=0, addrtype=0, proto=0, flags=0):
    """On Windows/Boot2docker, the getaddrinfo function is really slow, so we wrap the function and cache the result."""
    return orig_getaddrinfo(host, port, family, addrtype, proto, flags)


socket.getaddrinfo = fast_getaddrinfo

testclient: FlaskClient = timApp.tim.app.test_client()
testclient = testclient.__enter__()


def get_content(element: HtmlElement, selector: str = '.parContent') -> List[str]:
    return [r.text_content().strip() for r in element.cssselect(selector)]


def get_cookie_value(resp: Response, key: str) -> Optional[str]:
    """
    Get value of the cookie with given key.
    :param resp: Response.
    :param key: Cookie key.
    :return: Cookie value as string, or None if not found.
    """
    cookies = resp.headers.getlist('Set-Cookie')
    for cookie in cookies:
        match = re.match(f"{key}=(?P<value>\d+);", cookie)
        if match:
            return match.group('value')
    return None


class TimRouteTest(TimDbTest):
    """A base class for running tests for TIM routes."""
    doc_num = 1

    # The expected content of an AJAX response that does not return any specific information.
    ok_resp = {'status': 'ok'}

    # The expected content of an AJAX response that returns a generic permission error.
    permission_error = {'error': "Sorry, you don't have permission to use this resource."}

    @classmethod
    def setUpClass(cls):
        super().setUpClass()
        cls.client = testclient

    def get(self,
            url: str,
            as_tree: bool = False,
            expect_status: Optional[int] = 200,
            expect_content: Union[None, str, Dict, List] = None,
            expect_contains: Union[None, str, List[str], Dict] = None,
            expect_xpath: Optional[str] = None,
            expect_cookie: Optional[Tuple[str, Optional[str]]] = None,
            json_key: Optional[str] = None,
            headers: Optional[List[Tuple[str, str]]] = None,
            auth: Optional[BasicAuthParams] = None,
            **kwargs):
        """Performs a GET request.

        See the 'request' method for parameter explanations.

        """
        return self.request(url,
                            'GET',
                            as_tree=as_tree,
                            expect_status=expect_status,
                            expect_content=expect_content,
                            expect_contains=expect_contains,
                            expect_xpath=expect_xpath,
                            expect_cookie=expect_cookie,
                            json_key=json_key,
                            headers=headers,
                            auth=auth,
                            **kwargs)

    def post(self,
             url: str,
             as_tree: bool = False,
             expect_status: Optional[int] = 200,
             expect_content: Union[None, str, Dict, List] = None,
             expect_contains: Union[None, str, List[str]] = None,
             expect_xpath: Optional[str] = None,
             json_key: Optional[str] = None,
             headers: Optional[List[Tuple[str, str]]] = None,
             **kwargs):
        """Performs a POST request.

        See the 'request' method for parameter explanations.

        """
        return self.request(url,
                            'POST',
                            as_tree=as_tree,
                            expect_status=expect_status,
                            expect_content=expect_content,
                            expect_contains=expect_contains,
                            expect_xpath=expect_xpath,
                            json_key=json_key,
                            headers=headers,
                            **kwargs)

    def delete(self,
               url: str,
               as_tree: bool = False,
               expect_status: Optional[int] = 200,
               expect_content: Union[None, str, Dict, List] = None,
               expect_contains: Union[None, str, List[str]] = None,
               expect_xpath: Optional[str] = None,
               json_key: Optional[str] = None,
               headers: Optional[List[Tuple[str, str]]] = None,
               **kwargs):
        """Performs a DELETE request.

        See the 'request' method for parameter explanations.

        """
        return self.request(url,
                            'DELETE',
                            as_tree=as_tree,
                            expect_status=expect_status,
                            expect_content=expect_content,
                            expect_contains=expect_contains,
                            expect_xpath=expect_xpath,
                            json_key=json_key,
                            headers=headers,
                            **kwargs)

    def request(self,
                url: str,
                method: str,
                as_tree: bool = False,
                as_response: bool = False,
                expect_status: Optional[int] = 200,
                expect_content: Union[None, str, Dict, List] = None,
                expect_contains: Union[None, str, List[str]] = None,
                expect_mimetype: Optional[str] = None,
                expect_xpath: Optional[str] = None,
                expect_cookie: Optional[Tuple[str, Optional[str]]] = None,
                json_key: Optional[str] = None,
                headers: Optional[List[Tuple[str, str]]] = None,
                xhr=True,
                auth: Optional[BasicAuthParams] = None,
                force_return_text=False,
                **kwargs) -> Union[Response, str, Dict]:
        """Performs a request.

        For JSON POST/PUT requests, use the shortcut json_* methods.

        :param url: The request URL.
        :param method: The request method (e.g. GET, POST, PUT, DELETE).
        :param as_tree: Whether to return the response as an HTML tree.
        :param expect_status: The expected status code.
        :param expect_content: The expected response content.
         * If the response is a redirect, this parameter is interpreted as the expected redirect target URL.
         * Otherwise, if as_tree is True, this parameter is not used.
         * Otherwise, if the response mimetype is application/json, this parameter is interpreted as a dictionary or list
         that must match the response content.
         * Otherwise, this parameter is interpreted as a string that must match the response content.
        :param expect_contains: The expected subset(s) of the response content. This can be a string or a list of strings.
        :param expect_xpath: The expected XPath expression that must match at least one element in the response tree.
           This parameter can also be used for JSON responses as long as json_key is provided and the data in that key
           is HTML.
        :param expect_cookie: Cookie key and value as a tuple. The value can be None to check if cookie doesn't exist.
        :param json_key: The expected key that is found in the returned JSON data. Any other data is discarded.
        :param headers: Custom headers for the request.
        :param kwargs: Custom parameters to be passed to test client's 'open' method. Can be, for example,
           query_string={'a': '1', 'b': '2'} for passing URL arguments.
        :return: If as_tree is True: Returns the response as an HTML tree.
                 Otherwise, if the response mimetype is application/json, returns the response as a JSON dict or list.
                 Otherwise, returns the response as a string.

        """
        if headers is None:
            headers = []
        if xhr:
            headers.append(('X-Requested-With', 'XMLHttpRequest'))
        if auth:
            u, p = auth
            up = f'{u}:{p}'.encode()
            headers.append(('Authorization', f'Basic {b64encode(up).decode()}'))
        resp = self.client.open(url, method=method, headers=headers, **kwargs)
        is_textual = resp.mimetype in TEXTUAL_MIMETYPES
        if expect_status is not None:
            self.assertEqual(expect_status, resp.status_code, msg=resp.get_data(as_text=True) if is_textual else None)
        if expect_mimetype is not None:
            self.assertEqual(expect_mimetype, resp.mimetype)
        if is_redirect(resp) and expect_content is not None:
            self.assertEqual(expect_content, remove_prefix(resp.location, LOCALHOST))
        if expect_cookie is not None:
            self.assertEqual(expect_cookie[1], get_cookie_value(resp, expect_cookie[0]))
        resp_data = resp.get_data(as_text=is_textual)
        if not is_textual:
            return resp_data
        if force_return_text:
            return resp_data
        if expect_status >= 400 and json_key is None and (
                isinstance(expect_content, str) or isinstance(expect_contains, str)):
            json_key = 'error'
        if as_response:
            return resp
        if as_tree:
            if json_key is not None:
                resp_data = json.loads(resp_data)[json_key]
            if as_tree is True:
                tree = html.fromstring(resp_data)
                if expect_xpath is not None:
                    self.assertLessEqual(1, len(tree.findall(expect_xpath)))
            elif as_tree == 'fragments':
                tree = html.fragments_fromstring(resp_data)
            else:
                raise Exception(f'Unknown value for as_tree: {as_tree}')
            return tree
        elif resp.mimetype == 'application/json':
            loaded = json.loads(resp_data)
            if json_key is not None:
                loaded = loaded[json_key]
            if expect_content is not None:
                self.assertEqual(expect_content, loaded)
            if expect_contains is not None:
                self.check_contains(expect_contains, loaded)
            if expect_xpath is not None:
                self.assertIsNotNone(json_key)
                self.assertLessEqual(1, len(html.fragment_fromstring(loaded, create_parent=True).findall(expect_xpath)))
            return loaded
        else:
            if expect_content is not None and not is_redirect(resp):
                self.assertEqual(expect_content, resp_data)
            elif expect_contains is not None:
                self.check_contains(expect_contains, resp_data)
            return resp_data if not is_redirect(resp) else resp.location

    def check_contains(self, expect_contains, data):
        if isinstance(expect_contains, str):
            self.assertIn(expect_contains, data)
        elif isinstance(expect_contains, list):
            for s in expect_contains:
                self.assertIn(s, data)
        elif isinstance(expect_contains, dict):
            self.assert_dict_subset(data, expect_contains)
        else:
            self.assertTrue(False, 'Unknown type for expect_contains parameter')

    def json_put(self,
                 url: str,
                 json_data: Optional[Dict] = None,
                 as_tree: bool = False,
                 expect_status: Optional[int] = 200,
                 expect_content: Union[None, str, Dict, List] = None,
                 expect_contains: Union[None, str, List[str]] = None,
                 expect_xpath: Optional[str] = None,
                 json_key: Optional[str] = None,
                 headers: Optional[List[Tuple[str, str]]] = None,
                 **kwargs):
        """Performs a JSON PUT request.

        :param url: The request URL.
        :param json_data: The JSON data to be submitted.
        :param kwargs: Any custom parameters that are accepted by the 'request' method.
        :return: See the 'request' method.

        """
        return self.json_req(url,
                             json_data,
                             'PUT',
                             as_tree=as_tree,
                             expect_status=expect_status,
                             expect_content=expect_content,
                             expect_contains=expect_contains,
                             expect_xpath=expect_xpath,
                             json_key=json_key,
                             headers=headers,
                             **kwargs)

    def json_delete(self,
                    url: str,
                    json_data: Optional[Dict] = None,
                    as_tree: bool = False,
                    expect_status: Optional[int] = 200,
                    expect_content: Union[None, str, Dict, List] = None,
                    expect_contains: Union[None, str, List[str]] = None,
                    expect_xpath: Optional[str] = None,
                    json_key: Optional[str] = None,
                    headers: Optional[List[Tuple[str, str]]] = None,
                    auth: Optional[BasicAuthParams] = None,
                    **kwargs):
        """Performs a JSON DELETE request.

        :param url: The request URL.
        :param json_data: The JSON data to be submitted.
        :param kwargs: Any custom parameters that are accepted by the 'request' method.
        :return: See the 'request' method.

        """
        return self.json_req(url,
                             json_data,
                             'DELETE',
                             as_tree=as_tree,
                             expect_status=expect_status,
                             expect_content=expect_content,
                             expect_contains=expect_contains,
                             expect_xpath=expect_xpath,
                             json_key=json_key,
                             headers=headers,
                             auth=auth,
                             **kwargs)

    def json_post(self,
                  url: str,
                  json_data: Optional[Union[Dict, List]] = None,
                  as_tree: bool = False,
                  expect_status: Optional[int] = 200,
                  expect_content: Union[None, str, Dict, List] = None,
                  expect_contains: Union[None, str, List[str], Dict] = None,
                  expect_xpath: Optional[str] = None,
                  expect_cookie: Optional[Tuple[str, Optional[str]]] = None,
                  json_key: Optional[str] = None,
                  headers: Optional[List[Tuple[str, str]]] = None,
                  auth: Optional[BasicAuthParams] = None,
                  **kwargs):
        """Performs a JSON POST request.

        :param url: The request URL.
        :param json_data: The JSON data to be submitted.
        :param kwargs: Any custom parameters that are accepted by the 'request' method.
        :return: See the 'request' method.

        """
        return self.json_req(url,
                             json_data,
                             'POST',
                             as_tree=as_tree,
                             expect_status=expect_status,
                             expect_content=expect_content,
                             expect_contains=expect_contains,
                             expect_xpath=expect_xpath,
                             expect_cookie=expect_cookie,
                             json_key=json_key,
                             headers=headers,
                             auth=auth,
                             **kwargs)

    def json_req(self,
                 url: str,
                 json_data: Optional[Dict] = None,
                 method: str = 'GET',
                 as_tree: bool = False,
                 expect_status: Optional[int] = 200,
                 expect_content: Union[None, str, Dict, List] = None,
                 expect_contains: Union[None, str, List[str]] = None,
                 expect_xpath: Optional[str] = None,
                 expect_cookie: Optional[Tuple[str, Optional[str]]] = None,
                 json_key: Optional[str] = None,
                 headers: Optional[List[Tuple[str, str]]] = None,
                 auth: Optional[BasicAuthParams] = None,
                 content_type: Optional[str] = None,
                 **kwargs):
        """Performs a JSON request.

        :param url: The request URL.
        :param method: The request method.
        :param json_data: The JSON data to be submitted.
        :param kwargs: Any custom parameters that are accepted by the 'request' method.
        :return: See the 'request' method.

        """
        return self.request(url,
                            method=method,
                            data=json.dumps(json_data, cls=TimJsonEncoder),
                            content_type=content_type or 'application/json',
                            as_tree=as_tree,
                            expect_status=expect_status,
                            expect_content=expect_content,
                            expect_contains=expect_contains,
                            expect_xpath=expect_xpath,
                            expect_cookie=expect_cookie,
                            json_key=json_key,
                            headers=headers,
                            auth=auth,
                            **kwargs)

    def post_par(self, doc: Document, text: str, par_id: str, extra_data=None, **kwargs):
        """Edits a paragraph in a document.

        :param doc: The document to be edited.
        :param text: The new text for the paragraph.
        :param par_id: The id of the paragraph to be edited.
        :return: The response object.

        """
        doc.clear_mem_cache()
        if extra_data is None:
            extra_data = {}
        return self.json_post('/postParagraph/', {
            "text": text,
            "docId": doc.doc_id,
            "par": par_id,
            "par_next": None,
            **extra_data,
        }, **kwargs)

    def post_area(self, doc: DocInfo, text: str, area_start: str, area_end: str, **kwargs):
        """Edits an area in a document.

        :param doc: The document to be edited.
        :param text: The new text for the paragraph.
        :return: The response object.

        """
        doc.document.clear_mem_cache()
        return self.json_post('/postParagraph/', {
            "text": text,
            "docId": doc.id,
            "area_start": area_start,
            "area_end": area_end,
            "par": None,
            "par_next": None,
        }, **kwargs)

    def new_par(self, doc: Document, text: str, next_id: Optional[str] = None, additional_data=None, **kwargs):
        """Posts a new paragraph in a document.

        :param additional_data: Additional data to pass in newParagraph route.
        :param doc: The document to be edited.
        :param text: The text for the paragraph.
        :param next_id: The id of the paragraph following the new paragraph.
        :return: The response object.

        """
        if not additional_data:
            additional_data = {}
        doc.clear_mem_cache()
        return self.json_post('/newParagraph/', {
            "text": text,
            "docId": doc.doc_id,
            "par_next": next_id,
            **additional_data
        }, **kwargs)

    def delete_par(self, doc: DocInfo, par_id: str, **kwargs):
        doc.document.clear_mem_cache()
        return self.json_post(f'/deleteParagraph/{doc.id}', {
            "par": par_id,
        }, **kwargs)

    def delete_area(self, doc: DocInfo, area_start: str, area_end: str, **kwargs):
        doc.document.clear_mem_cache()
        return self.json_post(f'/deleteParagraph/{doc.id}', {
            "area_start": area_start,
            "area_end": area_end,
        }, **kwargs)

    def update_whole_doc(self, doc: DocInfo, text: str, **kwargs):
        doc.document.clear_mem_cache()
        return self.json_post(f'/update/{doc.id}', {'fulltext': text, 'original': doc.document.export_markdown()},
                              **kwargs)

    def post_answer(self, plugin_type, task_id: str, user_input,
                    save_teacher=False, teacher=False, user_id=None, answer_id=None, ref_from=None,
                    expect_content=None, expect_status=200,
                    **kwargs):
        return self.json_put(
            f'/{plugin_type}/{task_id}/answer',
            {"input": user_input,
             "ref_from": {'docId': ref_from[0], 'par': ref_from[1]} if ref_from else None,
             "abData": {"saveTeacher": save_teacher,
                        "teacher": teacher,
                        "userId": user_id,
                        "answer_id": answer_id,
                        "saveAnswer": True}},
            expect_content=expect_content,
            expect_status=expect_status,
            **kwargs,
        )

    def post_answer_no_abdata(self, plugin_type, task_id, user_input, ref_from=None, **kwargs):
        return self.json_put(f'/{plugin_type}/{task_id}/answer',
                             {"input": user_input,
                              "ref_from": {'docId': ref_from[0], 'par': ref_from[1]} if ref_from else None,
                              }, **kwargs)

    def get_task_answers(self, task_id, user: Optional[User] = None):
        answer_list = self.get(f'/getAnswers/{task_id}/{user.id if user else self.current_user_id()}')
        return answer_list

    @staticmethod
    def current_user_id() -> Optional[int]:
        """Returns the name of the current user.

        :return: The name of the current user.

        """
        return session.get('user_id')

    @property
    def is_logged_in(self):
        return self.current_user_id() is not None

    @property
    def current_user(self) -> Optional[User]:
        curr_id = self.current_user_id()
        return User.get_by_id(curr_id) if curr_id is not None else None

    def current_group(self) -> UserGroup:
        return self.current_user.get_personal_group()

    def login_anonymous(self):
        with self.client.session_transaction() as s:
            log_in_as_anonymous(s)
            db.session.commit()
        self.client.session_transaction().__enter__()

    def login_test1(self, force: bool = False, add: bool = False, **kwargs):
        """Logs testuser1 in.

        :param force: Whether to force the login route to be called even if the user is already logged in.
        :param add: Whether to add this user to the session group.
        :return: Response as a JSON dict.

        """
        return self.login('test1@example.com', 'test1pass', 'testuser1', force=force, add=add, **kwargs)

    def login_test2(self, force: bool = False, add: bool = False, **kwargs):
        """Logs testuser2 in.

        :param force: Whether to force the login route to be called even if the user is already logged in.
        :param add: Whether to add this user to the session group.
        :return: Response as a JSON dict.

        """
        return self.login('test2@example.com', 'test2pass', 'testuser2', force=force, add=add, **kwargs)

    def login_test3(self, force: bool = False, add: bool = False, **kwargs):
        """Logs testuser3 in.

        :param force: Whether to force the login route to be called even if the user is already logged in.
        :param add: Whether to add this user to the session group.
        :return: Response as a JSON dict.

        """
        return self.login('test3@example.com', 'test3pass', 'testuser3', force=force, add=add, **kwargs)

    def logout(self, user_id: Optional[int] = None):
        """Logs the specified user out.

        :param user_id: The id of the user to log out. If None, everyone in the session gets logged out.
        :return: Response as a JSON dict.

        """
        return self.json_post('/logout', json_data={'user_id': user_id})

    def login(
            self,
            email: Optional[str] = None,
            passw: Optional[str] = None,
            username: Optional[str] = None,
            force: bool = False,
            clear_last_doc: bool = True,
            add: bool = False,
            **kwargs,
    ):
        """Logs a user in.

        :param username: The username of the user.
        :param email: The email of the user.
        :param passw: The password of the user.
        :param force: Whether to force the login route to be called even if the user is already logged in.
        :param add: Whether to add this user to the session group.
        :return: Response as a JSON dict.

        """
        if self.client.application.got_first_request:
            if not force and not add:
                u = User.get_by_name(username)
                # if not flask.has_request_context():
                #     print('creating request context')
                #     tim.app.test_request_context().__enter__()
                if not u:
                    raise Exception(f"User not found: {username}")
                with self.client.session_transaction() as s:
                    s['user_id'] = u.id
                    s.pop('other_users', None)
                self.client.session_transaction().__enter__()
                return
            with self.client.session_transaction() as s:
                s.pop('last_doc', None)
                s.pop('came_from', None)
        return self.post('/emailLogin',
                         data={'email': email, 'password': passw, 'add_user': add},
                         follow_redirects=True, **kwargs)

    def create_doc(self, path: Optional[str] = None,
                   from_file: Optional[str] = None,
                   initial_par: Optional[Union[str, List[str]]] = None,
                   settings: Optional[Dict] = None,
                   copy_from: Optional[int] = None,
                   cite: Optional[int] = None,
                   template: Optional[str] = None,
                   title: Optional[str] = None,
                   expect_status=200,
                   **kwargs
                   ) -> Optional[DocEntry]:
        """Creates a new document.

        :param copy_from: The id of an existing document if creating a copy.
        :param cite: The id of an existing document if citing another document.
        :param path: The path of the document.
        :param from_file: If specified, loads the document content from the specified file.
        :param initial_par: The content of the initial paragraph.
        :param settings: The settings for the document.
        :return: The DocEntry object.

        """
        if path is None:
            path = f'{self.current_user.get_personal_folder().path}/doc{self.doc_num}'
            self.__class__.doc_num += 1
        if title is None:
            title = 'document ' + str(self.doc_num)

        # Optimization: during server tests, creating a document is a very frequent operation, so we just call
        # the route function directly if we're not testing anything.
        if expect_status != 200 or 'expect_content' in kwargs:
            data = {
                'item_path': path,
                'item_type': 'document',
                'item_title': title,
                **({'copy': copy_from} if copy_from else {}),
                **({'template': template} if template else {}),
                **({'cite': cite} if cite else {})
            }
            resp = self.json_post(
                '/createItem',
                data,
                expect_status=expect_status,
                **kwargs,
            )
            if expect_status != 200:
                return None
            self.assertIsInstance(resp['id'], int)
            self.assertEqual(path, resp['path'])
            de = DocEntry.find_by_path(path)
        else:
            de = create_item_direct(
                item_path=path,
                item_type='document',
                item_title=title,
                copy=copy_from,
                template=template,
                cite=cite,
            )
            # TODO this isn't really correct but gives equivalent behavior compared to the True branch.
            #  The modifier should be corrected to be the current user in the True branch after
            #  calling DocEntry.find_by_path. After that, some tests need to be corrected.
            de.document.modifier_group_id = 0
        doc = de.document
        self.init_doc(doc, from_file, initial_par, settings)
        return de

    def create_folder(self, path: str, title: str = 'foldertitle', expect_status=200, **kwargs):
        f = self.json_post('/createItem',
                           {'item_path': path,
                            'item_type': 'folder',
                            'item_title': title}, expect_status=expect_status, **kwargs)

        if expect_status == 200:
            self.assertEqual(path, f['path'])
            self.assertIsInstance(f['id'], int)
        return f

    def assert_js_variable(self, element: HtmlElement, variable_name: str, expect_content: Any):
        """
        Check a JavaScript variable from view_html.jinja2.
        :param element: HTML-tree.
        :param variable_name: Variable name as it's in the <script>.
        :param expect_content: Expected content.
        :return: None; raises error if variable was not found or content didn't match.
        """
        var = self.get_js_variable(element, variable_name)
        self.assertEqual(expect_content, var)

    def get_js_variable(self, element, variable_name):
        scripts = element.cssselect('script[class="global-vars"]')
        for s in scripts:
            variables = s.text
            # '\s*' are zero or more whitespaces, '(.*)' is variable content between '=' and ';'.
            matches = re.findall(f"{variable_name}\s*=\s*(.*);", variables)
            if matches:
                var = json.loads(matches[0])
                return var
        raise AssertionError(f"'{variable_name}' not found")

    def assert_elements_equal(self, e1, e2):
        try:
            self.assertEqual(e1.tag, e2.tag)
            self.assertEqual((e1.text or '').strip(), (e2.text or '').strip())
            self.assertEqual((e1.tail or '').strip(), (e2.tail or '').strip())
            self.assertEqual(e1.attrib, e2.attrib)
            self.assertEqual(len(e1), len(e2))
        except AssertionError:
            print(html.tostring(e1, pretty_print=True).decode('utf8'))
            print('--------------------------------------')
            print(html.tostring(e2, pretty_print=True).decode('utf8'))
            raise

        for c1, c2 in zip(e1, e2):
            self.assert_elements_equal(c1, c2)

    def create_translation(self, doc: DocEntry,
                           doc_title: str = 'title',
                           lang: str = 'en',
                           expect_contains=None,
                           expect_content=None,
                           expect_status=200,
                           **kwargs) -> Optional[Translation]:
        if expect_contains is None and expect_content is None:
            expect_contains = {'title': doc_title, 'path': doc.name + '/' + lang, 'name': doc.short_name}
        j = self.json_post(f'/translate/{doc.id}/{lang}',
                           {'doc_title': doc_title},
                           expect_contains=expect_contains, expect_content=expect_content, expect_status=expect_status,
                           **kwargs)
        return Translation.query.get(j['id']) if expect_status == 200 else None

    def assert_content(self, element: HtmlElement, expected: List[str]):
        pars = get_content(element)
        self.assertEqual(expected, pars)

    def get_updated_pars(self, d: DocInfo, **kwargs):
        return self.get(f'/getUpdatedPars/{d.id}', **kwargs)

    def get_personal_item_path(self, path):
        return f'{self.current_user.get_personal_folder().path}/{path}'

    def copy(self,
             doc: DocInfo,
             par_start: DocParagraph,
             par_end: DocParagraph, **kwargs):
        self.json_post(f'/clipboard/copy/{doc.id}/{par_start.get_id()}/{par_end.get_id()}', **kwargs)

    def cut(self,
            doc: DocInfo,
            par_start: DocParagraph,
            par_end: DocParagraph, **kwargs):
        self.json_post(f'/clipboard/cut/{doc.id}/{par_start.get_id()}/{par_end.get_id()}', **kwargs)

    def paste(self,
              doc: DocInfo,
              par_before: Optional[DocParagraph] = None,
              par_after: Optional[DocParagraph] = None,
              as_ref: bool = False, **kwargs):
        self.json_post(f'/clipboard/paste/{doc.id}',
                       {'par_before': par_before.get_id() if par_before else None,
                        'par_after': par_after.get_id() if par_after else None,
                        'as_ref': as_ref}, **kwargs)

    def show(self, doc: DocInfo):
        self.get('/clipboard', query_string={'doc_id': doc.id})

    def create_preamble_for(self, d: DocInfo, preamble_name='preamble', **kwargs):
        folder = d.location
        p = self.create_doc(f'{folder}/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/{preamble_name}', **kwargs)
        return p

    def assert_same_html(self, elem, expected_html: str):
        self.assert_elements_equal(html.fromstring(expected_html), elem)

    def get_no_warn(self, url: str, **kwargs):
        with warnings.catch_warnings():
            warnings.simplefilter('ignore', ResourceWarning)
            result = self.get(url, **kwargs)
        return result

    def make_admin(self, u: User):
        gr = UserGroup.get_admin_group()
        u.add_to_group(gr, added_by=None)
        db.session.commit()

    def post_comment(self, par: DocParagraph, public: bool, text: str, orig: Optional[DocParagraph] = None, **kwargs):
        glob_id = dict(doc_id=par.doc.doc_id, par_id=par.get_id())
        orig_glob_id = glob_id if not orig else dict(doc_id=orig.doc.doc_id, par_id=orig.get_id())
        return self.json_post('/postNote', {'text': text,
                                            'access': 'everyone' if public else 'justme',
                                            'ctx': {
                                                'curr': glob_id,
                                                'orig': orig_glob_id,
                                            }}, **kwargs)

    def edit_comment(self, note_id, par, public, text, **kwargs):
        glob_id = dict(doc_id=par.doc.doc_id, par_id=par.get_id())
        return self.json_post('/editNote',
                              {'text': text,
                               'id': note_id,
                               'access': 'everyone' if public else 'justme',
                               'ctx': {
                                   'curr': glob_id,
                                   'orig': glob_id,
                               },
                               }, **kwargs)

    def post_preview(self, d: DocInfo, text: str, spellcheck=False, par_next=None, par=None, **kwargs):
        data = {'text': text}
        if par_next:
            data['par_next'] = par_next
        if par:
            data['par'] = par
        if spellcheck:
            data['proofread'] = True
        return self.json_post(f'/preview/{d.id}', data, **kwargs)

    def upload_file(self, d: DocInfo, content: bytes, filename: str, **extra_data):
        return self.post('/upload/', data={'doc_id': str(d.id), 'file': (io.BytesIO(content), filename), **extra_data})

    def mark_as_unread(self, doc: DocInfo, par_id, expect_status=200):
        self.json_put(f'/unread/{doc.id}/{par_id}', json_data={}, expect_status=expect_status)

    def mark_as_read(self, doc: DocInfo, par_id: str, read_type=ReadParagraphType.click_red, **kwargs):
        self.json_put(f'/read/{doc.id}/{par_id}/{read_type.value}', **kwargs, json_data={})

    def print_html(self, e: HtmlElement):
        print(html.tostring(e, pretty_print=True).decode())

    def create_plugin_json(self, d: DocInfo,
                           task_name: str,
                           par_id: Optional[str] = None,
                           markup=None,
                           state=None,
                           toplevel=None,
                           info=None):
        if not toplevel:
            toplevel = {}
        if not markup:
            markup = {}
        basic_task_id = f"{d.id}.{task_name}"
        expected_json = {
            **toplevel,
            "info": info,
            "markup": {
                **markup,
            },
            "state": state,
            "taskID": basic_task_id,
            "anonymous": False,
            "doLazy": False,
            "preview": False,
            "review": False,
            "targetFormat": "latex",
            "taskIDExt": f"{d.id}.{task_name}.{par_id}" if par_id else basic_task_id,
            "user_id": self.current_user.name,
            'current_user_id': self.current_user.name,
            "userPrint": False,
            "viewmode": True,
        }
        return expected_json

    def make_base64(self, d: dict):
        """Converts the given dict to a base64-encoded JSON string."""
        return base64.b64encode(json.dumps(d, sort_keys=True).encode()).decode()

    def get_plugin_json(self, e: HtmlElement):
        b64str = e.attrib['json']
        return json.loads(base64.b64decode(b64str))

    def assert_plugin_json(self, e: HtmlElement, content: Dict[str, Any]):
        json_str = self.get_plugin_json(e)
        self.assertEqual(content, json_str)

    def get_state(self, aid: int, **kwargs):
        self.get('/getState',
                 query_string={
                     'user_id': self.current_user_id(),
                     'answer_id': aid,
                 },
                 **kwargs,
                 )

    def verify_answer_content(self, task: str, content_field: Optional[str], content, u: User, expected_count=1):
        anss: List[Answer] = u.answers.filter_by(task_id=task).order_by(Answer.answered_on.desc()).all()
        self.assertEqual(expected_count, len(anss))
        if expected_count == 0:
            return None
        first = anss[0]
        if content_field:
            self.assertEqual(content, first.content_as_json[content_field])
        else:
            self.assertEqual(content, first.content_as_json)
        return first

    def add_answer(
            self,
            d: DocInfo,
            task_name: str,
            content: Any = '',
            points: Union[None, int, float] = None,
            valid: bool = True,
            content_key: Optional[str] = 'c',
            user: Optional[User] = None,
    ):
        if user is None:
            user = self.current_user
        a = Answer(
            users_all=[user],
            task_id=f'{d.id}.{task_name}',
            content=json.dumps({content_key: content}) if content_key is not None else json.dumps(content),
            points=points,
            valid=valid,
        )
        db.session.add(a)
        return a

    def refresh_client(self):
        """Refreshes the Flask TestClient instance by emulating "with" statement exit and entrance.
        This method shouldn't have to be called very often, but it appears to be necessary in
        rare cases (see test_caching).
        """
        self.client.__exit__(None, None, None)
        self.client.__enter__()

    @contextmanager
    def internal_container_ctx(self):
        """Redirects internal container requests to go through Flask test client.
         Otherwise such requests would fail during test, unless BrowserTest class is used.

        TODO: Using BrowserTest in cases where it's not actually a browser test should be fixed to
         use this method instead.
        """
        with responses.RequestsMock(assert_all_requests_are_fired=False) as m:
            def rq_cb(request: PreparedRequest, fn):
                r: Response = fn(
                    request.path_url,
                    json_data=json.loads(request.body),
                    as_response=True,
                    content_type=request.headers.get('content-type', 'application/octet-stream'),
                )
                return r.status_code, {}, r.data

            def rq_cb_put(request: PreparedRequest):
                return rq_cb(request, self.json_put)

            def rq_cb_post(request: PreparedRequest):
                return rq_cb(request, self.json_post)

            host = current_app.config['INTERNAL_PLUGIN_DOMAIN']
            m.add_callback('PUT', re.compile(f'http://{host}:5001/'), callback=rq_cb_put)
            m.add_callback('POST', re.compile(f'http://{host}:5001/'), callback=rq_cb_post)
            m.add_passthru('http://csplugin:5000')
            m.add_passthru('http://jsrunner:5000')
            m.add_passthru('http://fields:5000')
            yield

    @contextmanager
    def importdata_ctx(self, aalto_return=None):
        with responses.RequestsMock() as m:
            if aalto_return:
                m.add(
                    'GET',
                    'https://plus.cs.aalto.fi/api/v2/courses/1234/aggregatedata/?format=json',
                    body=json.dumps(aalto_return),
                    status=200,
                )

            def rq_cb(request: PreparedRequest):
                r = self.json_put(request.path_url, json_data=json.loads(request.body))
                return 200, {}, json.dumps(r)

            host = current_app.config['INTERNAL_PLUGIN_DOMAIN']
            m.add_callback('PUT', f'http://{host}:5001/importData/answer', callback=rq_cb)
            m.add_passthru('http://jsrunner:5000')
            yield

    @contextmanager
    def temp_config(self, settings: Dict[str, Any]):
        old_settings = {k: current_app.config[k] for k in settings.keys()}
        for k, v in settings.items():
            current_app.config[k] = v
        try:
            yield
        finally:
            for k, v in old_settings.items():
                current_app.config[k] = v


class TimPluginFix(TimRouteTest):
    """Unused class. This was a test whether local plugins could be made to work without BrowserTest class.
    """

    def setUp(self):
        super().setUp()

        # Some plugins live in TIM container, which means we cannot use the requests library to call those plugins
        # because there is no real server running (it is just the test client).
        # Here we replace the request method in containerLink so that all such requests are redirected
        # to the test client.
        def test_do_request(method: str, url: str, data, params, headers, read_timeout):
            parsed = urlparse(url)
            if parsed.hostname != 'localhost':
                return do_request(method, url, data, params, headers, read_timeout)
            r = self.request(
                url=parsed.path,
                method=method,
                headers=[(k, v) for k, v in headers.items()] if headers else None,
                data=data,
                query_string=params,
                force_return_text=True,
                follow_redirects=True,
            )
            testclient.__exit__(None, None, None)
            return r

        containerLink.plugin_request_fn = test_do_request

    def tearDown(self):
        super().tearDown()
        containerLink.plugin_request_fn = do_request


class TimMessageListTest(TimRouteTest):
    MessageEventType = Union[NewMessageEvent, SubscriptionEvent]

    @classmethod
    def setUpClass(cls):
        import mailmanclient as mc
        super().setUpClass()

        # Sanity check to ensure we're not operating on real mailman instance
        assert 'mailman-test' in app.config['MAILMAN_URL']

        mailman_client = mc.Client(app.config['MAILMAN_URL'],
                                   app.config['MAILMAN_USER'],
                                   app.config['MAILMAN_PASS'])
        cls.mailman_client = mailman_client

        lists: List[mc.MailingList] = list(mailman_client.lists)
        users: List[mc.User] = list(mailman_client.users)

        # Delete previous lists and users before testing
        for ml in lists:
            ml.delete()
        for mu in users:
            mu.delete()

    def create_list(self, name: str, archive: ArchiveType) -> Tuple[Dict[str, Any], MessageListModel]:
        manage_doc = self.json_post('/messagelist/createlist', {
            'options': {
                'name': name,
                'archive': archive.value,
                'domain': 'example.com'
            }
        })
        message_list: MessageListModel = MessageListModel.query.filter_by(name=name).one()
        return manage_doc, message_list

    def trigger_mailman_event(self, event: MessageEventType) -> None:
        auth = (app.config.get('MAILMAN_EVENT_API_USER'), app.config.get('MAILMAN_EVENT_API_KEY'))
        self.json_post('/mailman/event', EVENTS[event.event].dump(event), auth=auth)

    def trigger_message_send(self, message_list: MessageListModel, user: User, subject='Subject', body='Body'):
        self.trigger_mailman_event(NewMessageEvent(
            event='new_message',
            mlist=MailmanMessageList(
                id=str(message_list.id),
                name=message_list.name,
                host=message_list.email_list_domain,
            ),
            message={
                'to': [[message_list.name, message_list.email_address]],
                'from': [[user.real_name, user.email]],
                'subject': subject,
                'body': body,
                'date': '2020-01-01T12:00:00Z'
            }
        ))


def get_note_id_from_json(json):
    note_id = int(re.search(r'note-id="(\d+)"', json['texts']).groups()[0])
    return note_id
