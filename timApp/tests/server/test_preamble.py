"""Server tests for preamble."""
from unittest.mock import patch, Mock

from lxml import html

from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.specialnames import (
    TEMPLATE_FOLDER_NAME,
    PREAMBLE_FOLDER_NAME,
    DEFAULT_PREAMBLE_DOC,
)
from timApp.document.yamlblock import YamlBlock
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup


class PreambleTestBase(TimRouteTest):
    def create_doc_and_preamble(self, folder: str):
        d = self.create_doc(f"{folder}/a/b/test1")
        preamble_name = DEFAULT_PREAMBLE_DOC
        p1, p2, p3 = self.create_preambles(folder, preamble_name)
        p1.document.set_settings({"macros": {"a": "cat", "b": "dog", "d": "sheep"}})
        p2.document.set_settings({"macros": {"b": "mouse", "c": "giraffe"}})
        p3.document.set_settings({"macros": {"c": "elephant", "d": "fly"}})
        return d, p1, p2, p3

    def create_preambles(self, folder: str, preamble_name: str):
        p1 = self.create_doc(
            f"{folder}/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/{preamble_name}"
        )
        p2 = self.create_doc(
            f"{folder}/a/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/{preamble_name}"
        )
        p3 = self.create_doc(
            f"{folder}/a/b/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/{preamble_name}"
        )
        return p1, p2, p3


class PreambleTest(PreambleTestBase):
    def test_preamble_settings(self):
        self.login_test1()
        folder = self.current_user.get_personal_folder().path
        d, p1, p2, p3 = self.create_doc_and_preamble(folder)
        d.document.set_settings({"macros": {"e": "fox"}})

        d_no_preamble = self.create_doc(f"{folder}/a/b/nopreamble")
        self.assertEqual(
            [p.path_without_lang for p in d.get_preamble_docs()],
            [
                f"users/test-user-1/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/{DEFAULT_PREAMBLE_DOC}",
                f"users/test-user-1/a/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/{DEFAULT_PREAMBLE_DOC}",
                f"users/test-user-1/a/b/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/{DEFAULT_PREAMBLE_DOC}",
            ],
        )
        self.assertEqual(
            d.document.get_settings().get_dict()["macros"],
            {"a": "cat", "b": "mouse", "c": "elephant", "d": "fly", "e": "fox"},
        )

        d_no_preamble.document.set_settings({"preamble": None})
        self.assertEqual(
            d_no_preamble.document.get_settings().get_dict(), {"preamble": None}
        )

        self.assertEqual(
            p1.document.get_settings().get_dict()["macros"],
            {"a": "cat", "b": "dog", "d": "sheep"},
        )
        self.assertEqual(
            p2.document.get_settings().get_dict()["macros"],
            {"b": "mouse", "c": "giraffe"},
        )
        self.assertEqual(
            p3.document.get_settings().get_dict()["macros"],
            {"c": "elephant", "d": "fly"},
        )

    def test_preamble_content(self):
        self.login_test2()
        folder = self.current_user.get_personal_folder().path
        d, p1, p2, p3 = self.create_doc_and_preamble(folder)
        d.document.set_settings({"macros": {"e": "fox"}})
        p1.document.add_text("p1")
        p2.document.add_text("p2")
        p3.document.add_text("p3 %%e%% s")
        d.document.add_text("own")
        elem = self.get(d.url, as_tree=True)
        self.assert_content(elem, ["p1", "p2", "p3 fox s", "", "own"])

    def test_preamble_self(self):
        self.login_test3()
        folder = self.current_user.get_personal_folder().path
        d, p1, p2, p3 = self.create_doc_and_preamble(folder)
        p1.document.add_text("p1")
        p2.document.add_text("p2")
        p3.document.add_text("p3")
        self.get(p1.url)

    def test_preamble_perf(self):
        self.login_test1()
        d = self.create_doc()
        with patch.object(DocInfo, "_get_preamble_docs_impl") as m:  # type: Mock
            self.get(d.url)
        m.assert_called_once()


class PreambleTest2(PreambleTestBase):
    def test_preamble_unique_ids(self):
        self.login_test1()
        folder = self.current_user.get_personal_folder().path
        d, p1, p2, p3 = self.create_doc_and_preamble(folder)
        par = p1.document.add_text("test")[0]
        d.document.add_paragraph("test", par_id=par.get_id())
        self.get(
            d.url,
            expect_contains="The paragraphs in the main document must "
            "have distinct ids from the preamble documents. Conflicting ids:",
        )

        p2.document.add_paragraph("test", par_id=par.get_id())
        self.get(
            d.url,
            expect_contains="The paragraphs in preamble documents must have distinct ids among themselves.",
        )

    def test_preamble_area_settings(self):
        self.login_test2()
        folder = self.current_user.get_personal_folder().path
        d, p1, p2, p3 = self.create_doc_and_preamble(folder)
        p1.document.add_text(
            """
#- {area=a settings=""}
a: b

#- {area_end=a}
"""
        )
        resp = self.get(d.url, as_tree=True)

        # There should not be warnings about missing areas.
        alert = resp.cssselect(".alert.alert-info")
        self.assertFalse(alert)

    def test_preamble_heading_numbering(self):
        self.login_test3()
        folder = self.current_user.get_personal_folder().path
        d, p1, p2, p3 = self.create_doc_and_preamble(folder)
        p1.document.add_text("# a")
        p2.document.add_text("# b")
        p3.document.add_text("# c")
        p1.document.set_settings({"auto_number_headings": 1})
        d.document.add_text("# d")
        doc = self.get(d.url, as_tree=True)
        self.assert_content(doc, ["1. a", "2. b", "3. c", "4. d"])
        first_par = d.document.get_paragraphs()[0]
        e = self.post_preview(
            d, text="# d", par=first_par.get_id(), json_key="texts", as_tree=True
        )
        self.assert_content(e, ["4. d"])
        e = self.post_preview(
            d, text="# d", par_next=first_par.get_id(), json_key="texts", as_tree=True
        )
        self.assert_content(e, ["4. d"])

        e = self.post_par(
            d.document, "# x", first_par.get_id(), json_key="texts", as_tree=True
        )
        self.assert_content(e, ["4. x"])
        e = self.get_updated_pars(d)
        changed = e["changed_pars"]
        self.assert_content(html.fromstring(changed[first_par.get_id()]), ["4. x"])

        p1.document.set_settings({})
        d.document.set_settings({"auto_number_headings": 1})
        self.get(d.url)
        e = self.post_par(
            d.document, "# z", first_par.get_id(), json_key="texts", as_tree=True
        )
        self.assert_content(e, ["4. z"])
        e = self.get_updated_pars(d)
        changed = e["changed_pars"]
        self.assert_content(html.fromstring(changed[first_par.get_id()]), ["4. z"])


class PreambleTest3(PreambleTestBase):
    def test_preamble_translation(self):
        self.login_test1()
        folder = self.current_user.get_personal_folder().path
        d, p1, p2, p3 = self.create_doc_and_preamble(folder)
        p1.document.add_text("macro a is %%a%%")
        p2.document.add_text("macro b is %%b%%")
        p3.document.add_text("macro c is %%c%%")
        d.document.add_text("macro d is %%d%%")
        d.document.add_text("macro e is %%e%%")
        dt = self.create_translation(d, "test", "fi")
        p1t = self.create_translation(p1, "test", "fi")
        p2t = self.create_translation(p2, "test", "fi")
        p1tpars = p1t.document.get_paragraphs()
        p2tpars = p2t.document.get_paragraphs()
        p1tpars[0].set_markdown(
            YamlBlock(
                values={"macros": {"a": "kissa", "b": "koira", "d": "lammas"}}
            ).to_markdown()
        )
        p1tpars[0].save()
        e = self.get(dt.url, as_tree=True)
        self.assert_content(
            e,
            [
                "macro a is kissa",
                "macro b is mouse",
                "macro c is elephant",
                "macro d is fly",
                "macro e is",
            ],
        )

        p1tpars[1].set_markdown("makro a on %%a%%")
        p1tpars[1].save()
        e = self.get(dt.url, as_tree=True)
        self.assert_content(
            e,
            [
                "makro a on kissa",
                "macro b is mouse",
                "macro c is elephant",
                "macro d is fly",
                "macro e is",
            ],
        )
        dt.document.set_settings({"macros": {"e": "jänis", "d": "kärpänen"}})
        e = self.get(dt.url, as_tree=True)
        self.assert_content(
            e,
            [
                "makro a on kissa",
                "macro b is mouse",
                "macro c is elephant",
                "",
                "macro d is kärpänen",
                "macro e is jänis",
            ],
        )
        p2tpars[1].set_markdown("makro b on %%b%%")
        p2tpars[1].save()
        with patch.object(
            DocParagraph, "get", wraps=DocParagraph.get
        ) as m:  # type: Mock
            e = self.get(dt.url, as_tree=True)

        # + 1 because of bookmark document
        expected_count = (
            sum(
                map(
                    len,
                    (x.document.get_par_ids() for x in (p1, p2, p3, p1t, p2t, d, dt)),
                )
            )
            + 1
        )

        # TODO: Paragraphs of p1 and p2 are read twice. Delta should be 0 ideally.
        self.assertAlmostEqual(expected_count, m.call_count, delta=4)

        self.assert_content(
            e,
            [
                "makro a on kissa",
                "makro b on mouse",
                "macro c is elephant",
                "",
                "macro d is kärpänen",
                "macro e is jänis",
            ],
        )

        self.assertEqual(len(e.cssselect(".preamble")), 3)
        self.assertEqual(len(e.cssselect("tim-par-ref")), 0)

    def test_multiple_preamble(self):
        self.login_test2()
        folder = self.current_user.get_personal_folder().path
        d, p1, p2, p3 = self.create_doc_and_preamble(folder)
        p1c, p2c, p3c = self.create_preambles(folder, "chat")
        d.document.set_settings({"preamble": "preamble, chat"})
        p1.document.add_text("p1")
        p2.document.add_text("p2")
        p3.document.add_text("p3")
        p1c.document.add_text("p1c")
        p2c.document.add_text("p2c")
        p3c.document.add_text("p3c")
        e = self.get(d.url, as_tree=True)
        self.assert_content(e, ["p1", "p1c", "p2", "p2c", "p3", "p3c", ""])
        d.document.set_settings({"preamble": "chat, preamble"})
        e = self.get(d.url, as_tree=True)
        self.assert_content(e, ["p1c", "p1", "p2c", "p2", "p3c", "p3", ""])
        dt = self.create_translation(d)
        p2ctr = self.create_translation(p2c)
        p = p2ctr.document.get_paragraphs()[0]
        p.set_markdown("p2ctr")
        p.save()
        db.session.add(dt)
        e = self.get(dt.url, as_tree=True)
        self.assert_content(e, ["p1c", "p1", "p2ctr", "p2", "p3c", "p3", ""])
        d.document.set_settings({"preamble": "chat, preamblez"})
        e = self.get(d.url, as_tree=True)
        self.assert_content(e, ["p1c", "p2c", "p3c", ""])

    def test_preamble_ref(self):
        self.login_test3()
        d = self.create_doc()
        d2 = self.create_doc()
        par = d2.document.add_text("from preamble")[0]
        p = self.create_preamble_for(d)
        p.document.add_text("""test""")
        p.document.add_paragraph_obj(par.create_reference(p.document))
        t = self.get(d.url, as_tree=True)
        pars = t.cssselect(".par.preamble")
        for pr in pars:
            self.assertEqual(p.path, pr.attrib["data-from-preamble"])


class PreambleTest4(TimRouteTest):
    def test_absolute_preamble(self):
        """User may refer to preambles located in an absolute path"""
        self.test_user_1.make_admin()
        db.session.commit()
        self.login_test1()

        folder = self.current_user.get_personal_folder().path
        preamble = self.create_doc(f"/{folder}/{PREAMBLE_FOLDER_NAME}/test")
        preamble.document.add_text("p1")
        document = self.create_doc("/kurssit/tie/document")
        document.document.add_text("p2")
        document.document.set_settings(
            {"preamble": f"/{folder}/{PREAMBLE_FOLDER_NAME}/test"}
        )
        document_text = self.get(document.url, as_tree=True)
        self.assert_content(document_text, ["p1", "", "p2"])

    def test_absolute_preamble_order(self):
        """Preambles located in an absolute path will be treated last"""
        self.test_user_2.make_admin()
        db.session.commit()
        self.login_test2()

        folder = self.current_user.get_personal_folder().path
        pt = self.create_doc(
            f"{folder}/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/{DEFAULT_PREAMBLE_DOC}"
        )
        pt.document.add_text("templates")
        pa = self.create_doc(
            f"/kurssit/tie/{PREAMBLE_FOLDER_NAME}/{DEFAULT_PREAMBLE_DOC}"
        )
        pa.document.add_text("absolute")
        document = self.create_doc(f"{folder}/document")
        document.document.add_text("document")
        path = f"{pt.short_name}, /{pa.path}"
        document.document.set_settings({"preamble": path})
        document_text = self.get(document.url, as_tree=True)
        self.assert_content(document_text, ["templates", "absolute", "", "document"])

    def test_preambles_self_reference(self):
        """Preamble shall not refer to itself"""
        self.test_user_3.make_admin()
        db.session.commit()
        self.login_test3()

        folder = self.current_user.get_personal_folder().path
        preamble = self.create_doc(
            f"{folder}/{PREAMBLE_FOLDER_NAME}/{DEFAULT_PREAMBLE_DOC}"
        )
        preamble.document.add_text("test")[0]
        self.assertNotIn(
            "The paragraphs in the main document must "
            "have distinct ids from the preamble documents. "
            "Conflicting ids:",
            self.get(preamble.url),
        )


class PreambleTest5(TimRouteTest):
    def test_extra_group_preambles(self):
        self.test_user_1.make_admin()
        db.session.commit()
        self.login_test1()

        ug1 = UserGroup.create("testgroup1")
        ug2 = UserGroup.create("testgroup2")
        ug3 = UserGroup.create("testgroup3")

        self.test_user_2.add_to_group(ug1, None)
        self.test_user_2.add_to_group(ug2, None)
        self.test_user_3.add_to_group(ug2, None)
        self.test_user_3.add_to_group(ug3, None)
        db.session.commit()

        folder = self.current_user.get_personal_folder().path
        group_preamble_path = f"{folder}/group_preambles"
        self.create_folder(group_preamble_path)

        p1 = self.create_doc(f"{group_preamble_path}/preamble-testgroup1-1")
        p1.document.set_settings({"macros": {"is_testgroup1_1": True}})
        p2 = self.create_doc(f"{group_preamble_path}/preamble-testgroup2-2")
        p2.document.set_settings({"macros": {"is_testgroup2_2": True}})
        p3 = self.create_doc(f"{group_preamble_path}/preamble-testgroup3-2")
        p3.document.set_settings({"macros": {"is_testgroup3_2": True}})

        d = self.create_doc(f"{folder}/testdoc")
        self.test_user_2.grant_access(d, AccessType.view)
        self.test_user_3.grant_access(d, AccessType.view)
        db.session.commit()

        p = self.create_preamble_for(d)
        p.document.set_settings({"extraGroupPreambleFolder": group_preamble_path})

        self.login_test2()
        d = DocEntry.find_by_path(f"{folder}/testdoc")
        d._preamble_docs = None  # Clear cached preamble docs
        d.document.settings_cache = None  # Clear cached settings
        macros_dict = d.document.get_settings().get_dict().get("macros", {})

        self.assertFalse(
            macros_dict.get("is_testgroup1_1", False),
            "testgroup1-1 preamble should not load for testuser2 (priority 1)",
        )
        self.assertTrue(
            macros_dict.get("is_testgroup2_2", False),
            "testgroup2-2 preamble should load for testuser2 (priority 2)",
        )
        self.assertFalse(
            macros_dict.get("is_testgroup3_2", False),
            "testgroup3-2 preamble should not load for testuser2 (not in group)",
        )

        self.login_test3()
        d = DocEntry.find_by_path(f"{folder}/testdoc")
        d._preamble_docs = None  # Clear cached preamble docs
        d.document.settings_cache = None  # Clear cached settings
        macros_dict = d.document.get_settings().get_dict().get("macros", {})

        self.assertFalse(
            macros_dict.get("is_testgroup1_1", False),
            "testgroup1-1 preamble should not load for testuser3 (not in group)",
        )
        self.assertTrue(
            macros_dict.get("is_testgroup2_2", False),
            "testgroup2-2 preamble should load for testuser3 (priority 2)",
        )
        self.assertTrue(
            macros_dict.get("is_testgroup3_2", False),
            "testgroup3-2 preamble should load for testuser3 (priority 2)",
        )
