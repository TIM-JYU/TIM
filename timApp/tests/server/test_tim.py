"""Unit tests for TIM routes."""

from flask import current_app
from lxml.cssselect import CSSSelector

from timApp.auth.accesstype import AccessType
from timApp.document.document import Document
from timApp.document.viewcontext import default_view_ctx
from timApp.markdown.markdownconverter import md_to_html
from timApp.note.notes import get_notes
from timApp.tests.server.timroutetest import TimRouteTest, get_note_id_from_json
from timApp.timdb.sqa import db
from timApp.user.user import Consent, User

link_selector = CSSSelector("a")


class TimTest(TimRouteTest):
    def test_activities(self):
        self.login_test1(force=True)

        # Make sure user's personal folder exists
        personal_folder = self.current_user.get_personal_folder().path
        self.get(f"/view/{personal_folder}")

        doc_names = [
            personal_folder + "/testing",
            personal_folder + "/testing2",
            personal_folder + "/testing3",
            personal_folder + "/testing4",
            personal_folder + "/testing5",
        ]
        doc_name = doc_names[0]
        doc_id_list = [3, 4, 5, 6, 7]
        doc_id = doc_id_list[0]
        doc_ids = set()
        for idx, n in enumerate(doc_names):
            self.json_post(
                "/createItem",
                {
                    "item_path": n,
                    "item_type": "document",
                    "item_title": "document " + n,
                },
                expect_contains={"id": doc_id_list[idx], "path": doc_names[idx]},
            )
            doc_ids.add(doc_id_list[idx])
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "type": "always",
                },
                "confirm": False,
                "groups": ["Anonymous users"],
                "type": AccessType.view.value,
                "id": doc_id,
            },
        )
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "type": "always",
                },
                "confirm": False,
                "groups": ["Logged-in users"],
                "type": AccessType.view.value,
                "id": doc_id_list[1],
            },
        )
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "type": "always",
                },
                "confirm": False,
                "groups": ["testuser2"],
                "type": AccessType.view.value,
                "id": doc_id_list[2],
            },
        )
        self.json_put(
            f"/permissions/add",
            {
                "time": {
                    "type": "always",
                },
                "confirm": False,
                "groups": ["testuser2"],
                "type": AccessType.edit.value,
                "id": doc_id_list[3],
            },
        )
        doc = Document(doc_id)
        doc.add_paragraph("Hello")
        pars = doc.get_paragraphs()
        self.assertEqual(1, len(pars))
        first_id = pars[0].get_id()
        comment_of_test1 = "This is a comment."
        comment_of_test1_html = md_to_html(comment_of_test1)
        json = self.post_comment(pars[0], public=True, text=comment_of_test1)
        note_id = get_note_id_from_json(json)

        self.assertTrue(comment_of_test1_html in json["texts"])
        self.get(f"/view/{doc_name}", expect_contains=comment_of_test1_html)
        edited_comment = "was edited!"
        edited_comment_html = md_to_html(edited_comment)
        json = self.edit_comment(note_id, pars[0], True, edited_comment)
        self.assertTrue(edited_comment_html in json["texts"])
        self.assertFalse(comment_of_test1_html in json["texts"])

        self.get("/teacher/" + doc_name)
        self.get("/answers/" + doc_name)
        edit_text = "testing editing now...\nnew line\n"
        par_html = md_to_html(edit_text)
        self.post_par(
            doc, edit_text, first_id, expect_contains=par_html, json_key="texts"
        )
        self.get(f"/getBlock/{doc_id}/{first_id}", expect_content={"text": edit_text})
        self.post_par(
            doc, edit_text, first_id, expect_contains=par_html, json_key="texts"
        )
        par2_text = "new par"
        par2_html = md_to_html(par2_text)
        self.post_par(
            doc,
            edit_text + "#-\n" + par2_text,
            first_id,
            expect_contains=[par_html, par2_html],
            json_key="texts",
        )
        pars = Document(doc_id).get_paragraphs()
        self.assertEqual(2, len(pars))
        second_par_id = pars[1].get_id()
        par2_new_text = "    " + par2_text
        par2_new_html = md_to_html(par2_new_text)
        self.post_par(
            doc,
            par2_new_text,
            second_par_id,
            expect_contains=par2_new_html,
            json_key="texts",
        )
        self.post("/logout", follow_redirects=True)
        self.get("/settings", expect_status=403)
        for d in doc_ids - {doc_id}:
            self.get(f"/view/{d}", expect_status=403)
        self.get(f"/view/{doc_id}")
        self.get(f"/view/{doc_id}", query_string={"login": True}, expect_status=403)

        # Login as another user
        self.login_test2()
        self.get(
            f"/view/{doc_name}", expect_contains=["Test user 2", edited_comment_html]
        )
        not_viewable_docs = {doc_id_list[4]}
        viewable_docs = doc_ids - not_viewable_docs
        for view_id in viewable_docs:
            self.get(f"/view/{view_id}")
            self.get(f"/teacher/{view_id}", expect_status=302)

        for view_id in not_viewable_docs:
            self.get(f"/view/{view_id}", expect_status=403)
            self.get(f"/teacher/{view_id}", expect_status=403)
        self.get("/view/not_exist", expect_status=404)

        comment_of_test2 = "g8t54h954hy95hg54h"
        self.post_comment(
            pars[0],
            public=True,
            text=comment_of_test2,
            expect_contains=comment_of_test2,
            json_key="texts",
        )

        ug = self.current_group().id
        notes = get_notes(ug, Document(doc_id), include_public=False)
        self.assertEqual(1, len(notes))
        test2_note_id = notes[0][0].id

        self.login_test1()
        self.get(
            f"/note/{test2_note_id}", expect_contains=comment_of_test2, json_key="text"
        )
        teacher_right_docs = {doc_id_list[3]}
        for i in teacher_right_docs:
            self.json_put(
                f"/permissions/add",
                {
                    "time": {
                        "type": "always",
                    },
                    "confirm": False,
                    "groups": ["testuser2"],
                    "type": AccessType.teacher.value,
                    "id": i,
                },
            )
        glob_id = dict(doc_id=doc_id, par_id=first_id)
        self.json_post(
            "/deleteNote",
            {
                "id": test2_note_id,
                "ctx": {
                    "orig": glob_id,
                    "curr": glob_id,
                },
            },
        )
        ug = self.current_group().id
        notes = get_notes(ug, Document(doc_id), include_public=True)
        self.assertEqual(1, len(notes))

        self.get(f"/getBlock/{doc_id}/{first_id}", expect_content={"text": edit_text})

        self.get(
            f"/getBlock/{doc_id}/{first_id}",
            query_string={
                "docId": doc_id,
                "area_start": first_id,
                "area_end": second_par_id,
            },
            expect_content={
                "text": Document(doc_id).export_section(first_id, second_par_id)
            },
        )

        self.login_test2()
        for view_id in viewable_docs - teacher_right_docs:
            self.get(f"/view/{view_id}")
            self.get("/teacher/" + str(view_id), expect_status=302)
            self.json_put(
                f"/permissions/add",
                {
                    "time": {
                        "type": "always",
                    },
                    "confirm": False,
                    "groups": ["testuser2"],
                    "type": AccessType.teacher.value,
                    "id": view_id,
                },
                expect_status=403,
            )
        for view_id in teacher_right_docs:
            self.get(f"/view/{view_id}")
            self.get(f"/teacher/{view_id}")
            self.json_put(
                f"/permissions/add",
                {
                    "time": {
                        "type": "always",
                    },
                    "confirm": False,
                    "groups": ["testuser2"],
                    "type": AccessType.teacher.value,
                    "id": view_id,
                },
                expect_status=403,
            )

    def test_same_heading_as_par(self):
        self.login_test1()
        doc = self.create_doc(initial_par=["# Hello", "Hello"]).document
        self.get(f"/view/{doc.doc_id}")

    def test_broken_comment(self):
        self.login_test1()
        doc = self.create_doc(
            settings={"macros": {}, "macro_delimiter": "%%"},
            initial_par="""```{atom=true}\nTest {!!! }\n```""",
        ).document
        tree = self.get(f"/view/{doc.doc_id}", as_tree=True)
        e = tree.cssselect(".parContent > p > span.error")[0]
        self.assertIn("Syntax error in macro template:", e.text)

    def test_windows_eol(self):
        """Windows-style EOLs should work with Dumbo."""
        self.login_test1()
        md_table = """---\r\n|a|\r\n|-|"""
        doc = self.create_doc(initial_par=md_table)
        tree = self.get(f"/view/{doc.id}", as_tree=True)
        table_xpath = r'.//div[@class="par"]/div[@class="parContent"]/table'
        tables = tree.findall(table_xpath)
        self.assertEqual(1, len(tables))

        self.post_preview(
            doc, text=md_table, json_key="texts", expect_xpath=table_xpath
        )

    def test_clear_cache(self):
        self.login_test1()
        doc = self.create_doc(initial_par="Test").document
        self.get(f"/view/{doc.doc_id}")
        self.get(f"/view/{doc.doc_id}", query_string={"nocache": "true"})
        doc.get_index(default_view_ctx)

    def test_document_intermediate_folders(self):
        self.login_test1()
        self.create_doc("users/test-user-1/a/b/c")

    def test_hide_links(self):
        self.login_test1()
        doc = self.create_doc()
        hide = "var hideLinks = true;"

        User.get_anon().grant_access(doc, AccessType.view)
        db.session.commit()
        self.assertFalse(hide in self.get(f"/view/{doc.id}"))

        self.logout()
        self.assertFalse(hide in self.get(f"/view/{doc.id}"))

        doc.document.add_setting("hide_links", "view")
        self.assertTrue(hide in self.get(f"/view/{doc.id}"))

        doc.document.add_paragraph(text="# 1\n\n# 2")
        # Index is visible always
        self.assertTrue(hide in self.get(f"/view/{doc.id}"))

        self.login_test1()
        self.assertFalse(hide in self.get(f"/view/{doc.id}"))

    def test_hide_top_buttons(self):
        self.login_test1()
        doc = self.create_doc()
        hide = "var hideTopButtons = true;"

        User.get_anon().grant_access(doc, AccessType.view)
        db.session.commit()
        self.assertFalse(hide in self.get(f"/view/{doc.id}"))

        self.logout()
        self.assertFalse(hide in self.get(f"/view/{doc.id}"))

        doc.document.add_setting("hide_top_buttons", "view")
        self.assertTrue(hide in self.get(f"/view/{doc.id}"))

        doc.document.add_paragraph(text="# 1\n\n# 2")
        # Index is visible always
        self.assertTrue(hide in self.get(f"/view/{doc.id}"))

        self.login_test1()
        self.assertFalse(hide in self.get(f"/view/{doc.id}"))

    def test_answers_no_crash(self):
        """Don't crash if anonymous user opens answers view."""
        self.login_test1()
        d = self.create_doc(initial_par="""#- {#t plugin=textfield}""")
        self.post_answer("textfield", f"{d.id}.t", user_input={"c": "x"})
        User.get_anon().grant_access(d, AccessType.see_answers)
        db.session.commit()
        self.logout()
        self.get(f"/answers/{d.path}")

    def test_ping(self):
        self.get("/ping")

    def test_getproxy(self):
        self.login_test1()
        self.get("/getproxy", expect_status=422)
        self.get(
            "/getproxy",
            query_string={"url": "hxxp"},
            expect_status=400,
            expect_content="Unknown URL scheme",
        )
        self.get(
            "/getproxy",
            query_string={"url": "http://"},
            expect_status=400,
            expect_content="URL domain not whitelisted: ",
        )
        self.get(
            "/getproxy",
            query_string={"url": "http://x"},
            expect_status=400,
            expect_content="URL domain not whitelisted: x",
        )
        self.get(
            "/getproxy",
            query_string={"url": "https://x"},
            expect_status=400,
            expect_content="URL domain not whitelisted: x",
        )
        self.get(
            "/getproxy",
            query_string={"url": "https://jyu.fi"},
            expect_status=400,
            expect_content="URL domain not whitelisted: jyu.fi",
        )
        self.get(
            "/getproxy",
            query_string={"url": "http://users.jyu.fi"},
            expect_status=400,
            expect_content="URL domain not whitelisted: users.jyu.fi",
        )
        self.get("/getproxy", query_string={"url": "https://korppi.jyu.fi"})
        self.logout()
        self.get(
            "/getproxy",
            query_string={"url": "https://korppi.jyu.fi"},
            expect_status=403,
        )

    def test_par_info(self):
        self.login_test1()
        d = self.create_doc(initial_par="testing")
        r = self.get(f"/par_info/{d.id}/{d.document.get_paragraphs()[0].get_id()}")
        self.assertEqual(d.title, r["item"]["title"])
        self.assertEqual(None, r["par_name"])

    def test_csrf(self):
        try:
            self.login_test1()
            current_app.config["WTF_CSRF_METHODS"] = ["POST", "PUT", "PATCH", "DELETE"]
            self.create_doc(
                expect_status=400,
                expect_content="The CSRF token is missing.",
            )
            self.json_post("/tape/multihtml", [])
            self.json_post("/qst/multihtml", [])
            self.json_post("/timTable/multihtml", [])
            self.json_post("/qst/multimd", [])
            self.json_post("/timTable/multimd", [])
            self.json_put(
                "/qst/answer",
                {
                    "info": {
                        "earlier_answers": 0,
                        "look_answer": False,
                        "user_id": "xxx",
                        "max_answers": None,
                        "valid": True,
                    },
                    "input": {"answers": []},
                    "taskID": "1.xx",
                    "state": None,
                    "markup": {},
                },
            )
        finally:
            current_app.config["WTF_CSRF_METHODS"] = []

    def test_consent(self):
        self.login_test1()
        self.assertEqual(None, self.current_user.consent)
        self.assertEqual(0, len(self.current_user.consents))
        self.json_post(
            f"/settings/updateConsent", {"consent": Consent.CookieAndData.value}
        )
        self.assertEqual(Consent.CookieAndData, self.current_user.consent)
        self.json_post(
            f"/settings/updateConsent", {"consent": Consent.CookieOnly.value}
        )
        self.assertEqual(Consent.CookieOnly, self.current_user.consent)
        self.json_post(
            f"/settings/updateConsent", {"consent": Consent.CookieOnly.value}
        )

        self.assertEqual(2, len(self.current_user.consents))

        self.json_post(f"/settings/updateConsent", {"consent": 9999}, expect_status=422)
        self.json_post(f"/settings/updateConsent", {"consent": "x"}, expect_status=422)
