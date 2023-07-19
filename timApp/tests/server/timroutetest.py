"""Defines the TimRouteTest class."""
import base64
import io
import json
import re
import socket
import warnings
from base64 import b64encode
from contextlib import contextmanager
from datetime import datetime
from functools import lru_cache
from typing import Union, Any, Generator

import responses
from flask import (
    Response,
    current_app,
    session,
    has_app_context,
    has_request_context,
    g,
)
from flask.sessions import SessionMixin
from flask.testing import FlaskClient
from lxml import html
from lxml.html import HtmlElement
from requests import PreparedRequest
from sqlalchemy import select
from sqlalchemy.orm import close_all_sessions, joinedload

import timApp.tim
from timApp.answer.answer import Answer
from timApp.auth.login import log_in_as_anonymous
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document
from timApp.document.specialnames import (
    TEMPLATE_FOLDER_NAME,
    PREAMBLE_FOLDER_NAME,
    DEFAULT_PREAMBLE_DOC,
)
from timApp.document.translation.language import Language
from timApp.document.translation.translation import Translation
from timApp.item.item import Item
from timApp.item.routes import create_item_direct
from timApp.messaging.messagelist.listinfo import ArchiveType
from timApp.messaging.messagelist.mailman_events import (
    NewMessageEvent,
    SubscriptionEvent,
    EVENTS,
    MailmanMessageList,
)
from timApp.messaging.messagelist.messagelist_models import MessageListModel
from timApp.readmark.readparagraphtype import ReadParagraphType
from timApp.tests.db.timdbtest import TimDbTest
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.utils import remove_prefix
from tim_common.timjsonencoder import TimJsonEncoder


def load_json(resp: Response):
    return json.loads(resp.get_data(as_text=True))


def is_redirect(response: Response):
    return response.status_code in (302, 303)


orig_getaddrinfo = socket.getaddrinfo

TEXTUAL_MIMETYPES = {"text/html", "application/json", "text/plain"}
LOCALHOST = "http://localhost/"

BasicAuthParams = tuple[str, str]


@lru_cache(maxsize=100)
def fast_getaddrinfo(host, port, family=0, addrtype=0, proto=0, flags=0):
    """On Windows/Boot2docker, the getaddrinfo function is really slow, so we wrap the function and cache the result."""
    return orig_getaddrinfo(host, port, family, addrtype, proto, flags)


socket.getaddrinfo = fast_getaddrinfo


def get_content(element: HtmlElement, selector: str = ".parContent") -> list[str]:
    return [r.text_content().strip() for r in element.cssselect(selector)]


def get_cookie_value(resp: Response, key: str) -> str | None:
    """
    Get value of the cookie with given key.

    :param resp: Response.
    :param key: Cookie key.
    :return: Cookie value as string, or None if not found.
    """
    cookies = resp.headers.getlist("Set-Cookie")
    for cookie in cookies:
        match = re.match(rf"{key}=(?P<value>\d+);", cookie)
        if match:
            return match.group("value")
    return None


class TimRouteTestBase(TimDbTest):
    """A base class for running tests for TIM routes."""

    doc_num = 1

    # The expected content of an AJAX response that does not return any specific information.
    ok_resp = {"status": "ok"}

    # The expected content of an AJAX response that returns a generic permission error.
    permission_error = {
        "error": "Sorry, you don't have permission to use this resource."
    }

    @classmethod
    def setUpClass(cls):
        super().setUpClass()
        with app.app_context():
            # Default language on create_translation NOTE not same as british or
            # american english.
            cls.add_language("english")
            db.session.commit()
            db.session.expire_all()

    def tearDown(self):
        self.client.__exit__(None, None, None)
        close_all_sessions()

    def _init_client(self) -> FlaskClient:
        # Must be implemented by subclasses
        raise NotImplementedError

    def setUp(self):
        self.check_skip_tests()
        # Create a default Flask client that holds the app context
        self.client = self._init_client()

        # FIXME: It is a VERY bad idea to enter a client context for the duration of the entire test
        #   because the client is not multithreaded. See https://github.com/pallets/flask/issues/4734
        #   Instead, the client contex should be entered only in specific tests and explicitly
        self.client = self.client.__enter__()
        self.client.open("/")

    @classmethod
    def add_language(cls, lang_name: str) -> Language:
        """
        Add a Language to the database.

        :param lang_name: Name of the language that langcodes could recognize.
        :return: The newly added Language.
        """
        lang = Language.create_from_name(lang_name)
        db.session.add(lang)
        return lang

    def get(
        self,
        url: str,
        as_tree: bool = False,
        expect_status: int | None = 200,
        expect_content: None | str | dict | list = None,
        expect_contains: None | str | list[str] | dict = None,
        expect_xpath: str | None = None,
        expect_cookie: tuple[str, str | None] | None = None,
        json_key: str | None = None,
        headers: list[tuple[str, str]] | None = None,
        auth: BasicAuthParams | None = None,
        **kwargs,
    ):
        """Performs a GET request.

        See the 'request' method for parameter explanations.

        """
        return self.request(
            url,
            "GET",
            as_tree=as_tree,
            expect_status=expect_status,
            expect_content=expect_content,
            expect_contains=expect_contains,
            expect_xpath=expect_xpath,
            expect_cookie=expect_cookie,
            json_key=json_key,
            headers=headers,
            auth=auth,
            **kwargs,
        )

    def post(
        self,
        url: str,
        as_tree: bool = False,
        expect_status: int | None = 200,
        expect_content: None | str | dict | list = None,
        expect_contains: None | str | list[str] = None,
        expect_xpath: str | None = None,
        json_key: str | None = None,
        headers: list[tuple[str, str]] | None = None,
        **kwargs,
    ):
        """Performs a POST request.

        See the 'request' method for parameter explanations.

        """
        return self.request(
            url,
            "POST",
            as_tree=as_tree,
            expect_status=expect_status,
            expect_content=expect_content,
            expect_contains=expect_contains,
            expect_xpath=expect_xpath,
            json_key=json_key,
            headers=headers,
            **kwargs,
        )

    def delete(
        self,
        url: str,
        as_tree: bool = False,
        expect_status: int | None = 200,
        expect_content: None | str | dict | list = None,
        expect_contains: None | str | list[str] = None,
        expect_xpath: str | None = None,
        json_key: str | None = None,
        headers: list[tuple[str, str]] | None = None,
        **kwargs,
    ):
        """Performs a DELETE request.

        See the 'request' method for parameter explanations.

        """
        return self.request(
            url,
            "DELETE",
            as_tree=as_tree,
            expect_status=expect_status,
            expect_content=expect_content,
            expect_contains=expect_contains,
            expect_xpath=expect_xpath,
            json_key=json_key,
            headers=headers,
            **kwargs,
        )

    def request(
        self,
        url: str,
        method: str,
        as_tree: bool = False,
        as_response: bool = False,
        expect_status: int | None = 200,
        expect_content: None | str | dict | list = None,
        expect_contains: None | str | list[str] = None,
        expect_mimetype: str | None = None,
        expect_xpath: str | None = None,
        expect_cookie: tuple[str, str | None] | None = None,
        json_key: str | None = None,
        headers: list[tuple[str, str]] | None = None,
        xhr=True,
        auth: BasicAuthParams | None = None,
        force_return_text: bool = False,
        expire_session_after_request: bool = True,
        client: FlaskClient | None = None,
        **kwargs,
    ) -> Response | str | dict:
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
        :param xhr: Whether to set the X-Requested-With header to XMLHttpRequest.
        :param auth: Basic auth username and password as a tuple.
        :param force_return_text: Whether to force returning the response as text.
        :param expire_session_after_request: Whether to expire all session objects after the request.
        :param client: The test client to use. If not provided, the default test client is used.
        :return: If as_tree is True: Returns the response as an HTML tree.
                 Otherwise, if the response mimetype is application/json, returns the response as a JSON dict or list.
                 Otherwise, returns the response as a string.

        """

        @contextmanager
        def clean_db_after_request():
            try:
                yield
            finally:
                if expire_session_after_request and has_app_context():
                    db.session.remove()
                    # Reattach the user object to the session so that it can be tracked for changes
                    g.pop("user", None)

        with clean_db_after_request():
            if headers is None:
                headers = []
            if xhr:
                headers.append(("X-Requested-With", "XMLHttpRequest"))
            if auth:
                u, p = auth
                up = f"{u}:{p}".encode()
                headers.append(("Authorization", f"Basic {b64encode(up).decode()}"))
            c = client or self.client
            resp = c.open(url, method=method, headers=headers, **kwargs)

            is_textual = resp.mimetype in TEXTUAL_MIMETYPES
            if expect_status is not None:
                self.assertEqual(
                    expect_status,
                    resp.status_code,
                    msg=resp.get_data(as_text=True) if is_textual else None,
                )
            if expect_mimetype is not None:
                self.assertEqual(expect_mimetype, resp.mimetype)
            if is_redirect(resp) and expect_content is not None:
                self.assertEqual(
                    expect_content, remove_prefix(resp.location, LOCALHOST)
                )
            if expect_cookie is not None:
                self.assertEqual(
                    expect_cookie[1], get_cookie_value(resp, expect_cookie[0])
                )
            resp_data = resp.get_data(as_text=is_textual)
            if not is_textual:
                return resp_data
            if force_return_text:
                return resp_data
            if (
                expect_status >= 400
                and json_key is None
                and (
                    isinstance(expect_content, str) or isinstance(expect_contains, str)
                )
            ):
                json_key = "error"
            if as_response:
                return resp
            if as_tree:
                if json_key is not None:
                    resp_data = json.loads(resp_data)[json_key]
                if as_tree is True:
                    tree = html.fromstring(resp_data)
                    if expect_xpath is not None:
                        self.assertLessEqual(1, len(tree.findall(expect_xpath)))
                elif as_tree == "fragments":
                    tree = html.fragments_fromstring(resp_data)
                else:
                    raise Exception(f"Unknown value for as_tree: {as_tree}")
                return tree
            elif resp.mimetype == "application/json":
                loaded = json.loads(resp_data)
                if json_key is not None:
                    loaded = loaded[json_key]
                if expect_content is not None:
                    self.assertEqual(expect_content, loaded)
                if expect_contains is not None:
                    self.check_contains(expect_contains, loaded)
                if expect_xpath is not None:
                    self.assertIsNotNone(json_key)
                    self.assertLessEqual(
                        1,
                        len(
                            html.fragment_fromstring(
                                loaded, create_parent=True
                            ).findall(expect_xpath)
                        ),
                    )
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
            self.assertTrue(False, "Unknown type for expect_contains parameter")

    def json_put(
        self,
        url: str,
        json_data: dict | None = None,
        as_tree: bool = False,
        expect_status: int | None = 200,
        expect_content: None | str | dict | list = None,
        expect_contains: None | str | list[str] = None,
        expect_xpath: str | None = None,
        json_key: str | None = None,
        headers: list[tuple[str, str]] | None = None,
        **kwargs,
    ):
        """Performs a JSON PUT request.

        :param url: The request URL.
        :param json_data: The JSON data to be submitted.
        :param kwargs: Any custom parameters that are accepted by the 'request' method.
        :return: See the 'request' method.

        """
        return self.json_req(
            url,
            json_data,
            "PUT",
            as_tree=as_tree,
            expect_status=expect_status,
            expect_content=expect_content,
            expect_contains=expect_contains,
            expect_xpath=expect_xpath,
            json_key=json_key,
            headers=headers,
            **kwargs,
        )

    def json_delete(
        self,
        url: str,
        json_data: dict | None = None,
        as_tree: bool = False,
        expect_status: int | None = 200,
        expect_content: None | str | dict | list = None,
        expect_contains: None | str | list[str] = None,
        expect_xpath: str | None = None,
        json_key: str | None = None,
        headers: list[tuple[str, str]] | None = None,
        auth: BasicAuthParams | None = None,
        **kwargs,
    ):
        """Performs a JSON DELETE request.

        :param url: The request URL.
        :param json_data: The JSON data to be submitted.
        :param kwargs: Any custom parameters that are accepted by the 'request' method.
        :return: See the 'request' method.

        """
        return self.json_req(
            url,
            json_data,
            "DELETE",
            as_tree=as_tree,
            expect_status=expect_status,
            expect_content=expect_content,
            expect_contains=expect_contains,
            expect_xpath=expect_xpath,
            json_key=json_key,
            headers=headers,
            auth=auth,
            **kwargs,
        )

    def json_post(
        self,
        url: str,
        json_data: dict | list | None = None,
        as_tree: bool = False,
        expect_status: int | None = 200,
        expect_content: None | str | dict | list = None,
        expect_contains: None | str | list[str] | dict = None,
        expect_xpath: str | None = None,
        expect_cookie: tuple[str, str | None] | None = None,
        json_key: str | None = None,
        headers: list[tuple[str, str]] | None = None,
        auth: BasicAuthParams | None = None,
        **kwargs,
    ):
        """Performs a JSON POST request.

        :param url: The request URL.
        :param json_data: The JSON data to be submitted.
        :param kwargs: Any custom parameters that are accepted by the 'request' method.
        :return: See the 'request' method.

        """
        return self.json_req(
            url,
            json_data,
            "POST",
            as_tree=as_tree,
            expect_status=expect_status,
            expect_content=expect_content,
            expect_contains=expect_contains,
            expect_xpath=expect_xpath,
            expect_cookie=expect_cookie,
            json_key=json_key,
            headers=headers,
            auth=auth,
            **kwargs,
        )

    def json_req(
        self,
        url: str,
        json_data: dict | None = None,
        method: str = "GET",
        as_tree: bool = False,
        expect_status: int | None = 200,
        expect_content: None | str | dict | list = None,
        expect_contains: None | str | list[str] = None,
        expect_xpath: str | None = None,
        expect_cookie: tuple[str, str | None] | None = None,
        json_key: str | None = None,
        headers: list[tuple[str, str]] | None = None,
        auth: BasicAuthParams | None = None,
        content_type: str | None = None,
        **kwargs,
    ):
        """Performs a JSON request.

        :param url: The request URL.
        :param method: The request method.
        :param json_data: The JSON data to be submitted.
        :param kwargs: Any custom parameters that are accepted by the 'request' method.
        :return: See the 'request' method.

        """
        return self.request(
            url,
            method=method,
            data=json.dumps(json_data, cls=TimJsonEncoder),
            content_type=content_type or "application/json",
            as_tree=as_tree,
            expect_status=expect_status,
            expect_content=expect_content,
            expect_contains=expect_contains,
            expect_xpath=expect_xpath,
            expect_cookie=expect_cookie,
            json_key=json_key,
            headers=headers,
            auth=auth,
            **kwargs,
        )

    def post_par(
        self, doc: Document, text: str, par_id: str, extra_data=None, **kwargs
    ):
        """Edits a paragraph in a document.

        :param doc: The document to be edited.
        :param text: The new text for the paragraph.
        :param par_id: The id of the paragraph to be edited.
        :return: The response object.

        """
        doc.clear_mem_cache()
        if extra_data is None:
            extra_data = {}
        return self.json_post(
            "/postParagraph/",
            {
                "text": text,
                "docId": doc.doc_id,
                "par": par_id,
                "par_next": None,
                **extra_data,
            },
            **kwargs,
        )

    def post_area(
        self, doc: DocInfo, text: str, area_start: str, area_end: str, **kwargs
    ):
        """Edits an area in a document.

        :param doc: The document to be edited.
        :param text: The new text for the paragraph.
        :return: The response object.

        """
        doc.document.clear_mem_cache()
        return self.json_post(
            "/postParagraph/",
            {
                "text": text,
                "docId": doc.id,
                "area_start": area_start,
                "area_end": area_end,
                "par": None,
                "par_next": None,
            },
            **kwargs,
        )

    def new_par(
        self,
        doc: Document,
        text: str,
        next_id: str | None = None,
        additional_data=None,
        **kwargs,
    ):
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
        return self.json_post(
            "/newParagraph/",
            {"text": text, "docId": doc.doc_id, "par_next": next_id, **additional_data},
            **kwargs,
        )

    def delete_par(self, doc: DocInfo, par_id: str, **kwargs):
        doc.document.clear_mem_cache()
        return self.json_post(
            f"/deleteParagraph/{doc.id}",
            {
                "par": par_id,
            },
            **kwargs,
        )

    def delete_area(self, doc: DocInfo, area_start: str, area_end: str, **kwargs):
        doc.document.clear_mem_cache()
        return self.json_post(
            f"/deleteParagraph/{doc.id}",
            {
                "area_start": area_start,
                "area_end": area_end,
            },
            **kwargs,
        )

    def update_whole_doc(self, doc: DocInfo, text: str, **kwargs):
        doc.document.clear_mem_cache()
        return self.json_post(
            f"/update/{doc.id}",
            {"fulltext": text, "original": doc.document.export_markdown()},
            **kwargs,
        )

    def post_answer(
        self,
        plugin_type,
        task_id: str,
        user_input,
        save_teacher=False,
        teacher=False,
        user_id=None,
        answer_id=None,
        ref_from=None,
        expect_content=None,
        expect_status=200,
        init_mock=None,
        **kwargs,
    ):
        with self.internal_container_ctx() as m:
            if init_mock:
                init_mock(m)
            return self.json_put(
                f"/{plugin_type}/{task_id}/answer",
                {
                    "input": user_input,
                    "ref_from": {"docId": ref_from[0], "par": ref_from[1]}
                    if ref_from
                    else None,
                    "abData": {
                        "saveTeacher": save_teacher,
                        "teacher": teacher,
                        "userId": user_id,
                        "answer_id": answer_id,
                        "saveAnswer": True,
                    },
                },
                expect_content=expect_content,
                expect_status=expect_status,
                **kwargs,
            )

    def post_answer_no_abdata(
        self, plugin_type, task_id, user_input, ref_from=None, **kwargs
    ):
        return self.json_put(
            f"/{plugin_type}/{task_id}/answer",
            {
                "input": user_input,
                "ref_from": {"docId": ref_from[0], "par": ref_from[1]}
                if ref_from
                else None,
            },
            **kwargs,
        )

    def get_task_answers(self, task_id, user: User | None = None):
        answer_list = self.get(
            f"/getAnswers/{task_id}/{user.id if user else self.current_user_id()}"
        )
        return answer_list

    @staticmethod
    def current_user_id() -> int | None:
        """Returns the name of the current user.

        :return: The name of the current user.

        """
        return session.get("user_id")

    @property
    def is_logged_in(self):
        return self.current_user_id() is not None

    @property
    def current_user(self) -> User | None:
        curr_id = self.current_user_id()
        return User.get_by_id(curr_id) if curr_id is not None else None

    def current_group(self) -> UserGroup:
        return self.current_user.get_personal_group()

    def login_anonymous(self):
        with self.refreshing_session_transaction() as s:
            log_in_as_anonymous(s)
            self.commit_db()

    def login_test1(self, force: bool = False, add: bool = False, **kwargs):
        """Logs testuser1 in.

        :param force: Whether to force the login route to be called even if the user is already logged in.
        :param add: Whether to add this user to the session group.
        :return: Response as a JSON dict.

        """
        return self.login(
            "test1@example.com",
            "test1pass",
            "testuser1",
            force=force,
            add=add,
            **kwargs,
        )

    def login_test2(self, force: bool = False, add: bool = False, **kwargs):
        """Logs testuser2 in.

        :param force: Whether to force the login route to be called even if the user is already logged in.
        :param add: Whether to add this user to the session group.
        :return: Response as a JSON dict.

        """
        return self.login(
            "test2@example.com",
            "test2pass",
            "testuser2",
            force=force,
            add=add,
            **kwargs,
        )

    def login_test3(self, force: bool = False, add: bool = False, **kwargs):
        """Logs testuser3 in.

        :param force: Whether to force the login route to be called even if the user is already logged in.
        :param add: Whether to add this user to the session group.
        :return: Response as a JSON dict.

        """
        return self.login(
            "test3@example.com",
            "test3pass",
            "testuser3",
            force=force,
            add=add,
            **kwargs,
        )

    def logout(self, user_id: int | None = None):
        """Logs the specified user out.

        :param user_id: The id of the user to log out. If None, everyone in the session gets logged out.
        :return: Response as a JSON dict.

        """
        return self.json_post("/logout", json_data={"user_id": user_id})

    def login(
        self,
        email: str | None = None,
        passw: str | None = None,
        username: str | None = None,
        force: bool = False,
        clear_last_doc: bool = True,
        manual: bool = False,
        add: bool = False,
        **kwargs,
    ):
        """Logs a user in.

        :param username: The username of the user.
        :param email: The email of the user.
        :param passw: The password of the user.
        :param force: Whether to force the login route to be called even if the user is already logged in.
        :param add: Whether to add this user to the session group.
        :param manual: If true, always executes the login manually (without quick user context creation).
        :return: Response as a JSON dict.

        """
        if not manual:
            if not force and not add:
                u = User.get_by_name(username)
                if not u:
                    raise Exception(f"User not found: {username}")
                with self.refreshing_session_transaction() as s:
                    s["user_id"] = u.id
                    s.pop("other_users", None)
                if has_request_context():
                    # Force user object to refresh for the current request
                    g.pop("user", None)
                return
            with self.refreshing_session_transaction() as s:
                s.pop("last_doc", None)
                s.pop("came_from", None)
        return self.post(
            "/emailLogin",
            data={"email": email, "password": passw, "add_user": add},
            follow_redirects=True,
            **kwargs,
        )

    @contextmanager
    def refreshing_session_transaction(self) -> Generator[SessionMixin, None, None]:
        """A context manager that refreshes the active session context after the block is executed."""
        with self.client.session_transaction() as s:
            yield s
            if has_request_context():
                # If we are already in a request (or we had already one request), sync the session with the transaction
                # This way any further calls to TIM API will reference the correct user
                session_keys = set(session.keys())
                for k in session_keys:
                    if k not in session:
                        session.pop(k, None)
                session.update(s)

    def create_doc(
        self,
        path: str | None = None,
        from_file: str | None = None,
        initial_par: str | list[str] | None = None,
        settings: dict | None = None,
        copy_from: int | None = None,
        cite: int | None = None,
        template: str | None = None,
        title: str | None = None,
        expect_status=200,
        **kwargs,
    ) -> DocEntry | None:
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
            path = f"{self.current_user.get_personal_folder().path}/doc{self.doc_num}"
            self.__class__.doc_num += 1
        if title is None:
            title = "document " + str(self.doc_num)

        # Optimization: during server tests, creating a document is a very frequent operation, so we just call
        # the route function directly if we're not testing anything.
        if expect_status != 200 or "expect_content" in kwargs:
            data = {
                "item_path": path,
                "item_type": "document",
                "item_title": title,
                **({"copy": copy_from} if copy_from else {}),
                **({"template": template} if template else {}),
                **({"cite": cite} if cite else {}),
            }
            resp = self.json_post(
                "/createItem",
                data,
                expect_status=expect_status,
                **kwargs,
            )
            if expect_status != 200:
                return None
            self.assertIsInstance(resp["id"], int)
            self.assertEqual(path, resp["path"])
            de = DocEntry.find_by_path(path)

            # After finding the document, modify the modifier group in case paragraphs will be added
            # This will keep the edit log consistent
            current_user_group_id = self.current_user.get_personal_group().id
            de.document.modifier_group_id = current_user_group_id
        else:
            de = create_item_direct(
                item_path=path,
                item_type="document",
                item_title=title,
                copy=copy_from,
                template=template,
                cite=cite,
            )
        doc = de.document
        self.init_doc(doc, from_file, initial_par, settings)
        return de

    def create_folder(
        self, path: str, title: str = "foldertitle", expect_status=200, **kwargs
    ):
        f = self.json_post(
            "/createItem",
            {"item_path": path, "item_type": "folder", "item_title": title},
            expect_status=expect_status,
            **kwargs,
        )

        if expect_status == 200:
            self.assertEqual(path, f["path"])
            self.assertIsInstance(f["id"], int)
        return f

    def assert_js_variable(
        self, element: HtmlElement, variable_name: str, expect_content: Any
    ):
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
            matches = re.findall(rf"{variable_name}\s*=\s*(.*);", variables)
            if matches:
                var = json.loads(matches[0])
                return var
        raise AssertionError(f"'{variable_name}' not found")

    def assert_elements_equal(self, e1, e2):
        try:
            self.assertEqual(e1.tag, e2.tag)
            self.assertEqual((e1.text or "").strip(), (e2.text or "").strip())
            self.assertEqual((e1.tail or "").strip(), (e2.tail or "").strip())
            self.assertEqual(e1.attrib, e2.attrib)
            self.assertEqual(len(e1), len(e2))
        except AssertionError:
            print(html.tostring(e1, pretty_print=True).decode("utf8"))
            print("--------------------------------------")
            print(html.tostring(e2, pretty_print=True).decode("utf8"))
            raise

        for c1, c2 in zip(e1, e2):
            self.assert_elements_equal(c1, c2)

    def create_translation(
        self,
        doc: DocEntry,
        doc_title: str = "title",
        lang: str = "en",
        expect_contains=None,
        expect_content=None,
        expect_status=200,
        **kwargs,
    ) -> Translation | None:
        if expect_contains is None and expect_content is None:
            expect_contains = {
                "title": doc_title,
                "path": doc.name + "/" + lang,
                "name": doc.short_name,
            }
        j = self.json_post(
            f"/translate/{doc.id}/{lang}/Manual",
            {"doc_title": doc_title},
            expect_contains=expect_contains,
            expect_content=expect_content,
            expect_status=expect_status,
            **kwargs,
        )
        return db.session.get(Translation, j["id"], options=[joinedload(Translation.docentry)]) if expect_status == 200 else None

    def assert_content(self, element: HtmlElement, expected: list[str]):
        pars = get_content(element)
        self.assertEqual(expected, pars)

    def get_updated_pars(self, d: DocInfo, **kwargs):
        return self.get(f"/getUpdatedPars/{d.id}", **kwargs)

    def get_personal_item_path(self, path):
        return f"{self.current_user.get_personal_folder().path}/{path}"

    def copy(
        self,
        doc: DocInfo,
        par_start: DocParagraph,
        par_end: DocParagraph,
        area_name: str | None = None,
        **kwargs,
    ):
        self.json_post(
            f"/clipboard/copy/{doc.id}/{par_start.get_id()}/{par_end.get_id()}",
            {
                "area_name": area_name,
            },
            **kwargs,
        )

    def cut(
        self, doc: DocInfo, par_start: DocParagraph, par_end: DocParagraph, **kwargs
    ):
        self.json_post(
            f"/clipboard/cut/{doc.id}/{par_start.get_id()}/{par_end.get_id()}", **kwargs
        )

    def paste(
        self,
        doc: DocInfo,
        par_before: DocParagraph | None = None,
        par_after: DocParagraph | None = None,
        as_ref: bool = False,
        **kwargs,
    ):
        self.json_post(
            f"/clipboard/paste/{doc.id}",
            {
                "par_before": par_before.get_id() if par_before else None,
                "par_after": par_after.get_id() if par_after else None,
                "as_ref": as_ref,
            },
            **kwargs,
        )

    def show(self, doc: DocInfo):
        self.get("/clipboard", query_string={"doc_id": doc.id})

    def create_preamble_for(
        self, d: Item, preamble_name: str = DEFAULT_PREAMBLE_DOC, **kwargs
    ) -> DocEntry | None:
        folder = d.location
        p = self.create_doc(
            f"{folder}/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/{preamble_name}",
            **kwargs,
        )
        return p

    def assert_same_html(self, elem, expected_html: str):
        self.assert_elements_equal(html.fromstring(expected_html), elem)

    def get_no_warn(self, url: str, **kwargs):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", ResourceWarning)
            result = self.get(url, **kwargs)
        return result

    def make_admin(self, u: User):
        gr = UserGroup.get_admin_group()
        u.add_to_group(gr, added_by=None)
        self.commit_db()

    def post_comment(
        self,
        par: DocParagraph,
        public: bool,
        text: str,
        orig: DocParagraph | None = None,
        **kwargs,
    ):
        glob_id = dict(doc_id=par.doc.doc_id, par_id=par.get_id())
        orig_glob_id = (
            glob_id if not orig else dict(doc_id=orig.doc.doc_id, par_id=orig.get_id())
        )
        return self.json_post(
            "/postNote",
            {
                "text": text,
                "access": "everyone" if public else "justme",
                "ctx": {
                    "curr": glob_id,
                    "orig": orig_glob_id,
                },
            },
            **kwargs,
        )

    def edit_comment(self, note_id, par, public, text, **kwargs):
        glob_id = dict(doc_id=par.doc.doc_id, par_id=par.get_id())
        return self.json_post(
            "/editNote",
            {
                "text": text,
                "id": note_id,
                "access": "everyone" if public else "justme",
                "ctx": {
                    "curr": glob_id,
                    "orig": glob_id,
                },
            },
            **kwargs,
        )

    def post_preview(
        self, d: DocInfo, text: str, spellcheck=False, par_next=None, par=None, **kwargs
    ):
        data = {"text": text}
        if par_next:
            data["par_next"] = par_next
        if par:
            data["par"] = par
        if spellcheck:
            data["proofread"] = True
        return self.json_post(f"/preview/{d.id}", data, **kwargs)

    def upload_file(self, d: DocInfo, content: bytes, filename: str, **extra_data):
        return self.post(
            "/upload/",
            data={
                "doc_id": str(d.id),
                "file": (io.BytesIO(content), filename),
                **extra_data,
            },
        )

    def mark_as_unread(self, doc: DocInfo, par_id, expect_status=200):
        self.json_put(
            f"/unread/{doc.id}/{par_id}", json_data={}, expect_status=expect_status
        )

    def mark_as_read(
        self, doc: DocInfo, par_id: str, read_type=ReadParagraphType.click_red, **kwargs
    ):
        self.json_put(
            f"/read/{doc.id}/{par_id}/{read_type.value}", **kwargs, json_data={}
        )

    def print_html(self, e: HtmlElement):
        print(html.tostring(e, pretty_print=True).decode())

    def create_plugin_json(
        self,
        d: DocInfo,
        task_name: str,
        par_id: str | None = None,
        markup=None,
        state=None,
        toplevel=None,
        info=None,
    ):
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
            "current_user_id": self.current_user.name,
            "userPrint": False,
            "viewmode": True,
        }
        return expected_json

    def make_base64(self, d: dict):
        """Converts the given dict to a base64-encoded JSON string."""
        return base64.b64encode(json.dumps(d, sort_keys=True).encode()).decode()

    def get_plugin_json(self, e: HtmlElement):
        b64str = e.attrib["json"]
        return json.loads(base64.b64decode(b64str))

    def assert_plugin_json(self, e: HtmlElement, content: dict[str, Any]):
        json_str = self.get_plugin_json(e)
        self.assertEqual(content, json_str)

    def get_state(self, aid: int, **kwargs):
        self.get(
            "/getState",
            query_string={
                "user_id": self.current_user_id(),
                "answer_id": aid,
            },
            **kwargs,
        )

    def verify_answer_content(
        self,
        task: str,
        content_field: str | None,
        content,
        u: User,
        expected_count=1,
    ):
        anss: list[Answer] = (
            u.answers.filter_by(task_id=task).order_by(Answer.answered_on.desc()).all()
        )
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
        content: Any = "",
        points: None | int | float = None,
        valid: bool = True,
        content_key: str | None = "c",
        user: User | None = None,
        last_points_modifier: int | None = None,
        answered_on: datetime | None = None,
    ):
        if user is None:
            user = self.current_user
        a = Answer(
            users_all=[user],
            task_id=f"{d.id}.{task_name}",
            content=json.dumps({content_key: content})
            if content_key is not None
            else json.dumps(content),
            points=points,
            valid=valid,
            last_points_modifier=last_points_modifier,
        )
        if answered_on:
            a.answered_on = answered_on
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
    def internal_container_ctx(self) -> Generator[responses.RequestsMock, None, None]:
        """Redirects internal container requests to go through Flask test client.
         Otherwise such requests would fail during test, unless BrowserTest class is used.

        TODO: Using BrowserTest in cases where it's not actually a browser test should be fixed to
         use this method instead.
        """
        with responses.RequestsMock(assert_all_requests_are_fired=False) as m:

            def rq_cb(request: PreparedRequest, fn, body_as_json: bool = True):
                kwargs = {}
                if body_as_json:
                    kwargs["json_data"] = json.loads(request.body)
                with app.test_client() as c:
                    r: Response = fn(
                        request.path_url,
                        as_response=True,
                        content_type=request.headers.get(
                            "content-type", "application/octet-stream"
                        ),
                        client=c,
                        # Do not expire any sessions because there is likely an active session ongoing.
                        # This mock will be invoked likely by TIM calling internal plugin routes
                        # inside another routes.
                        # Because of the way Flask test client works, DB session is shared between
                        # the main route and the internal plugin routes.
                        # Closing a session inside internal plugin route may
                        # invalidate the objects in the main plugin route.
                        expire_session_after_request=False,
                        **kwargs,
                    )
                return r.status_code, {}, r.data

            def rq_cb_get(request: PreparedRequest):
                return rq_cb(request, self.get, body_as_json=False)

            def rq_cb_put(request: PreparedRequest):
                return rq_cb(request, self.json_put)

            def rq_cb_post(request: PreparedRequest):
                return rq_cb(request, self.json_post)

            host = current_app.config["INTERNAL_PLUGIN_DOMAIN"]
            m.add_callback(
                "GET", re.compile(f"http://{host}:5001/"), callback=rq_cb_get
            )
            m.add_callback(
                "PUT", re.compile(f"http://{host}:5001/"), callback=rq_cb_put
            )
            m.add_callback(
                "POST", re.compile(f"http://{host}:5001/"), callback=rq_cb_post
            )
            m.add_passthru("http://csplugin:5000")
            m.add_passthru("http://jsrunner:5000")
            m.add_passthru("http://fields:5000")
            m.add_passthru("http://dumbo:5000")
            m.add_passthru("http://pali:5000")
            m.add_passthru("http://feedback:5000")
            m.add_passthru("http://haskellplugins:5002")
            m.add_passthru("http://showfile:5000")
            m.add_passthru("http://mailman-test:8001")
            yield m

    @contextmanager
    def temp_config(self, settings: dict[str, Any]):
        old_settings = {k: app.config[k] for k in settings.keys()}
        for k, v in settings.items():
            app.config[k] = v
        try:
            yield
        finally:
            for k, v in old_settings.items():
                app.config[k] = v


class TimRouteTest(TimRouteTestBase):

    def _init_client(self) -> FlaskClient:
        return timApp.tim.app.test_client()


class TimMessageListTest(TimRouteTest):
    MessageEventType = Union[NewMessageEvent, SubscriptionEvent]

    @classmethod
    def setUpClass(cls):
        import mailmanclient as mc

        super().setUpClass()

        # Sanity check to ensure we're not operating on real mailman instance
        assert "mailman-test" in app.config["MAILMAN_URL"]

        mailman_client = mc.Client(
            app.config["MAILMAN_URL"],
            app.config["MAILMAN_USER"],
            app.config["MAILMAN_PASS"],
        )
        cls.mailman_client = mailman_client

        lists: list[mc.MailingList] = list(mailman_client.lists)
        users: list[mc.User] = list(mailman_client.users)

        # Delete previous lists and users before testing
        for ml in lists:
            ml.delete()
        for mu in users:
            mu.delete()

    def add_list_member(self, list_name: str, candidates: list[str]) -> None:
        self.json_post(
            "/messagelist/addmember",
            {
                "member_candidates": candidates,
                "msg_list": list_name,
                "send_right": True,
                "delivery_right": True,
            },
        )

    def create_list(
        self, name: str, archive: ArchiveType
    ) -> tuple[dict[str, Any], MessageListModel]:
        manage_doc = self.json_post(
            "/messagelist/createlist",
            {
                "options": {
                    "name": name,
                    "archive": archive.value,
                    "domain": "example.com",
                }
            },
        )
        message_list: MessageListModel = (
            db.session.execute(select(MessageListModel).filter_by(name=name))
            .scalars()
            .one()
        )
        return manage_doc, message_list

    def trigger_mailman_event(self, event: MessageEventType) -> None:
        auth = (
            app.config.get("MAILMAN_EVENT_API_USER"),
            app.config.get("MAILMAN_EVENT_API_KEY"),
        )
        self.json_post("/mailman/event", EVENTS[event.event].dump(event), auth=auth)

    def trigger_message_send(
        self,
        message_list: MessageListModel,
        user: User,
        subject: str | None = None,
        body: str | None = None,
    ):
        message = {
            "to": [[message_list.name, message_list.email_address]],
            "from": [[user.real_name, user.email]],
            "date": "2020-01-01T12:00:00Z",
        }
        if subject:
            message["subject"] = subject
        if body:
            message["body"] = body
        self.trigger_mailman_event(
            NewMessageEvent(
                event="new_message",
                mlist=MailmanMessageList(
                    id=str(message_list.id),
                    name=message_list.name,
                    host=message_list.email_list_domain,
                ),
                message=message,
            )
        )


def get_note_id_from_json(json):
    note_id = int(re.search(r'note-id="(\d+)"', json["texts"]).groups()[0])
    return note_id
