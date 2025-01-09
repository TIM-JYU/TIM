import json

from timApp.document.documents import import_document_from_file
from timApp.tests.db.timdbtest import TEST_USER_2_ID, TEST_USER_1_ID
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.settings.style_utils import OFFICIAL_STYLES_PATH
from timApp.user.usergroup import UserGroup
from timApp.util.utils import static_tim_doc


class SettingsTest(TimRouteTest):
    def test_info(self):
        self.login_test1()
        d = self.create_doc()
        t1id = self.get_test_user_1_group_id()
        self.get(
            "/settings/info",
            expect_content={
                "annotations": [],
                "answer_uploads": [],
                "answers": [],
                "groups": [{"id": t1id, "name": "testuser1"}],
                "lectureanswers": [],
                "notes": [],
                "owned_documents": [
                    {
                        "id": d.id,
                        "isFolder": False,
                        "location": "users/test-user-1",
                        "modified": "just now",
                        "name": "doc1",
                        "owners": [{"id": t1id, "name": "testuser1"}],
                        "path": "users/test-user-1/doc1",
                        "public": True,
                        "rights": {
                            "browse_own_answers": True,
                            "can_comment": True,
                            "can_mark_as_read": True,
                            "copy": True,
                            "editable": True,
                            "manage": True,
                            "owner": True,
                            "see_answers": True,
                            "teacher": True,
                        },
                        "title": "document 2",
                        "unpublished": True,
                        "visibility": 5,
                    }
                ],
                "owned_folders": [
                    {
                        "id": 2,
                        "isFolder": True,
                        "location": "users",
                        "modified": "just now",
                        "name": "test-user-1",
                        "owners": [{"id": t1id, "name": "testuser1"}],
                        "path": "users/test-user-1",
                        "public": True,
                        "rights": {
                            "browse_own_answers": True,
                            "can_comment": True,
                            "can_mark_as_read": True,
                            "copy": True,
                            "editable": True,
                            "manage": True,
                            "owner": True,
                            "see_answers": True,
                            "teacher": True,
                        },
                        "title": "Test user 1",
                        "unpublished": True,
                        "visibility": 5,
                    }
                ],
                "owned_lectures": [],
                "readparagraphs": [],
                "uploaded_files": [],
                "uploaded_images": [],
                "user": {
                    "consent": None,
                    "contacts": [
                        {
                            "channel": "email",
                            "contact": "test1@example.com",
                            "origin": 1,
                            "primary": True,
                            "verified": True,
                        }
                    ],
                    "created": self.test_user_1.created.isoformat(),
                    "email": "test1@example.com",
                    "given_name": None,
                    "id": TEST_USER_1_ID,
                    "last_name": None,
                    "modified": self.test_user_1.modified.isoformat(),
                    "name": "testuser1",
                    "origin": None,
                    "prefs": '{"custom_css": "", "use_document_word_list": false, '
                    '"disable_menu_hover": false, '
                    '"remember_last_sidebar_menu_tab": false, '
                    '"remember_last_sidebar_menu_state": false, "word_list": '
                    '"", "email_exclude": "", "language": null, '
                    '"style_doc_ids": [], "quick_select_style_doc_ids": [], '
                    '"last_answer_fetch": {}, "auto_mark_all_read": false, '
                    '"bookmarks": [{"Last edited": [{"document 2": '
                    '"/view/users/test-user-1/doc1"}]}], '
                    '"max_uncollapsed_toc_items": null, "parmenu_position": 1, "always_show_header_menu": false, "display_dir_list_badges": false, "display_dir_list_tags": false}',
                    "real_name": "Test user 1",
                },
                "velps": [],
            },
        )
        self.get("/settings/info/testuser2", expect_status=403)
        u = self.test_user_1
        u.groups.append(UserGroup.get_admin_group())
        db.session.commit()
        self.get(
            "/settings/info/testuser2",
            expect_content={
                "annotations": [],
                "answer_uploads": [],
                "answers": [],
                "groups": [
                    {"id": self.get_test_user_2_group_id(), "name": "testuser2"}
                ],
                "lectureanswers": [],
                "notes": [],
                "owned_documents": [],
                "owned_folders": [],
                "owned_lectures": [],
                "readparagraphs": [],
                "uploaded_files": [],
                "uploaded_images": [],
                "user": {
                    "consent": None,
                    "contacts": [
                        {
                            "channel": "email",
                            "contact": "test2@example.com",
                            "origin": 1,
                            "primary": True,
                            "verified": True,
                        }
                    ],
                    "created": self.test_user_2.created.isoformat(),
                    "email": "test2@example.com",
                    "given_name": None,
                    "id": TEST_USER_2_ID,
                    "last_name": None,
                    "modified": self.test_user_2.modified.isoformat(),
                    "name": "testuser2",
                    "origin": None,
                    "prefs": None,
                    "real_name": "Test user 2",
                },
                "velps": [],
            },
        )

    def test_info_no_points(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {plugin=csPlugin #t}
type: python
-pointsRule:
  run: 1
        """
        )
        self.post_answer("csPlugin", f"{d.id}.t", {"usercode": 'print("hi")'})
        self.add_answer(
            d,
            "t1",
            {"usercode": 'print("hi")'},
            points=5,
            content_key=None,
            user=self.test_user_1,
            last_points_modifier=self.test_user_2.id,
        )
        db.session.commit()
        answs = self.get("/settings/info")["answers"]
        for a in answs:
            self.assertIsNone(a["points"])
            self.assertNotIn("points", json.loads(a["content"]))
            self.assertIsNone(a["last_points_modifier"])

    def test_settings_save(self):
        # Create dummy doc for test
        d = import_document_from_file(
            static_tim_doc("style_docs/lighttheme.md"),
            f"{OFFICIAL_STYLES_PATH}/lighttheme",
            UserGroup.get_anonymous_group(),
            title="lighttheme",
        )
        db.session.commit()

        lighttheme_id = d.id

        self.login_test3()

        self.json_post(f"/settings/save", {"invalid": "yes"}, expect_status=400)
        self.get(
            f"/settings/get",
            expect_content={
                "auto_mark_all_read": False,
                "bookmarks": None,
                "custom_css": "",
                "disable_menu_hover": False,
                "email_exclude": "",
                "language": None,
                "last_answer_fetch": {},
                "max_uncollapsed_toc_items": None,
                "remember_last_sidebar_menu_state": False,
                "remember_last_sidebar_menu_tab": False,
                "style_doc_ids": [],
                "use_document_word_list": False,
                "word_list": "",
                "parmenu_position": 1,
                "quick_select_style_doc_ids": [],
                "always_show_header_menu": False,
                "display_dir_list_badges": False,
                "display_dir_list_tags": False,
            },
        )
        self.json_post(
            f"/settings/save",
            {
                "style_doc_ids": [lighttheme_id],  # Technically allow any value
                "email_exclude": "users/something\nusers/another",
                "last_answer_fetch": {},
                "use_document_word_list": True,
                "word_list": "cat\ndog",
                "language": None,
                "custom_css": "somecss",
                "disable_menu_hover": True,
                "remember_last_sidebar_menu_state": True,
                "remember_last_sidebar_menu_tab": True,
                "auto_mark_all_read": True,
                "parmenu_position": 0,
                "quick_select_style_doc_ids": [],
                "always_show_header_menu": True,
                "display_dir_list_badges": True,
                "display_dir_list_tags": True,
            },
        )
        self.get(
            f"/settings/get",
            expect_content={
                "style_doc_ids": [lighttheme_id],
                "email_exclude": "users/something\nusers/another",
                "last_answer_fetch": {},
                "use_document_word_list": True,
                "word_list": "cat\ndog",
                "language": None,
                "max_uncollapsed_toc_items": None,
                "custom_css": "somecss",
                "disable_menu_hover": True,
                "remember_last_sidebar_menu_state": True,
                "remember_last_sidebar_menu_tab": True,
                "auto_mark_all_read": True,
                "bookmarks": None,
                "parmenu_position": 0,
                "quick_select_style_doc_ids": [],
                "always_show_header_menu": True,
                "display_dir_list_badges": True,
                "display_dir_list_tags": True,
            },
        )
        self.json_post(
            f"/settings/save",
            {
                "style_doc_ids": [],
                "email_exclude": "users/something\nusers/another",
                "last_answer_fetch": {},
                "use_document_word_list": True,
                "word_list": "cat\ndog",
                "custom_css": "somecss",
                "disable_menu_hover": True,
                "remember_last_sidebar_menu_state": False,
                "remember_last_sidebar_menu_tab": True,
                "auto_mark_all_read": False,
                "parmenu_position": 1,
                "quick_select_style_doc_ids": [],
                "always_show_header_menu": False,
                "display_dir_list_badges": False,
                "display_dir_list_tags": False,
            },
        )
        self.get(
            f"/settings/get",
            expect_content={
                "style_doc_ids": [],
                "email_exclude": "users/something\nusers/another",
                "last_answer_fetch": {},
                "use_document_word_list": True,
                "word_list": "cat\ndog",
                "language": None,
                "custom_css": "somecss",
                "disable_menu_hover": True,
                "remember_last_sidebar_menu_state": False,
                "remember_last_sidebar_menu_tab": True,
                "auto_mark_all_read": False,
                "max_uncollapsed_toc_items": None,
                "bookmarks": None,
                "parmenu_position": 1,
                "quick_select_style_doc_ids": [],
                "always_show_header_menu": False,
                "display_dir_list_badges": False,
                "display_dir_list_tags": False,
            },
        )

    def test_settings_get_single(self):
        self.login_test1()
        self.get(
            f"/settings/get/last_answer_fetch",
            expect_content={
                "last_answer_fetch": {},
            },
        )
        self.get(
            f"/settings/get/nonexistent",
            expect_content={
                "nonexistent": None,
            },
        )

    def test_settings_no_xss(self):
        self.login_test1()
        scr = '<script>alert("hi")</script>'
        d = self.create_doc(settings={"x": scr})
        r = self.get(d.url)
        self.assertNotIn(scr, r)
