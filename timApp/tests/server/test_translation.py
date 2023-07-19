from typing import Type
from unittest.mock import patch, Mock

from sqlalchemy import select

from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.docsettings import DocSettings
from timApp.document.document import Document
from timApp.document.translation.deepl import DeeplTranslationService
from timApp.document.translation.language import Language
from timApp.document.translation.reversingtranslator import (
    ReversingTranslationService,
    REVERSE_LANG,
)
from timApp.document.translation.translation import Translation
from timApp.document.translation.translator import Usage, TranslateBlock
from timApp.document.yamlblock import YamlBlock
from timApp.tests.db.timdbtest import TimDbTest
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.util.utils import static_tim_doc

MAX_TEST_CHAR_QUOTA = 50
"""Amount of characters to initialize the quota-limited test translator
with.
"""


def setup_translation_test(cls: Type) -> None:
    with app.app_context():
        db.session.add(ReversingTranslationService())
        db.session.add(QuotaLimitedTestTranslator())
        cls.reverselang = Language(**REVERSE_LANG)
        db.session.add(cls.reverselang)
        db.session.commit()


class TimTranslationTest(TimDbTest):
    """Test class containing the reversing translation service and its
    preferred target language.
    """

    @classmethod
    def setUpClass(cls):
        super().setUpClass()
        setup_translation_test(cls)

    @property
    def reverselang(self) -> Language:
        return db.session.get(Language, REVERSE_LANG["lang_code"])


class TimTranslationRouteTest(TimRouteTest):
    @classmethod
    def setUpClass(cls):
        super().setUpClass()
        setup_translation_test(cls)

    @property
    def reverselang(self) -> Language:
        return db.session.get(Language, REVERSE_LANG["lang_code"])


class QuotaLimitedTestTranslator(ReversingTranslationService):
    """A test translator that can run out of quota mid-translation. Used for
    simulating a translation service, that returns an error when
    translation-quota is exceeded by the caller (such as the case with DeepL
    Free).
    """

    _character_limit = MAX_TEST_CHAR_QUOTA
    """Amount of characters allowed to translate."""

    _character_count = 0
    """Holds the state of characters translated with the translator."""

    def translate(
        self,
        texts: list[TranslateBlock],
        source_lang: Language,
        target_lang: Language,
        *,
        tag_handling: str = "",
    ) -> list[str]:
        # Translate everything at first, because it's easier to implement here.
        result = super().translate(
            texts, source_lang, target_lang, tag_handling=tag_handling
        )
        # Apply changes to the quota (based on the translated text, as it does
        # not matter with ReversingTranslationService).
        for text in result:
            self._character_count += len(text)
            if self._character_count > self._character_limit:
                raise Exception(f"Translation quota ({self._character_limit}) exceeded")

        return result

    def usage(self) -> Usage:
        return Usage(
            character_limit=self._character_limit, character_count=self._character_count
        )

    __mapper_args__ = {"polymorphic_identity": "QuotaLimited"}


class TranslationTest(TimTranslationRouteTest):
    def get_deepl_service(self) -> DeeplTranslationService:
        return (
            db.session.execute(select(DeeplTranslationService).limit(1))
            .scalars()
            .first()
        )

    def test_translation_create(self):
        self.login_test1()
        doc = self.create_doc()
        lang = "en"
        doc_title = "test"
        t = self.create_translation(doc, doc_title, lang)
        self.create_translation(
            doc,
            doc_title,
            lang,
            expect_status=403,
            expect_content={"error": "Translation for this language already exists"},
        )
        self.get(t.url)
        self.logout()
        self.json_post(
            f"/translate/{doc.id}/{lang}/Manual",
            {"doc_title": doc_title},
            expect_status=403,
        )

    def test_translation_create_with_settings(self):
        self.login_test1()
        doc = self.create_doc()
        doc.document.set_settings({"a": "b"})
        lang = "en"
        doc_title = "test"
        t = self.create_translation(doc, doc_title, lang)
        d = t.document
        self.assertEqual("b", d.get_settings().get_dict()["a"])
        self.get(t.url)

    def test_translation_content(self):
        self.login_test1()
        doc = self.create_doc(from_file=static_tim_doc("multiple_mmcqs.md"))
        j = self.create_translation(doc, "MMCQ fi", "fi")
        tr_doc = j.document
        pars = doc.document.get_paragraphs()
        par_ids = {p.get_id() for p in pars}
        tr_pars = tr_doc.get_paragraphs()
        plugin_tr_par = tr_pars[1]
        old_md = self.get(f"/getBlock/{tr_doc.doc_id}/{plugin_tr_par.get_id()}")
        self.assertEqual({}, tr_doc.get_settings().get_dict())

        self.assertFalse(tr_pars[0].is_setting())
        for p in tr_pars:  # type DocParagraph
            self.assertTrue(p.is_translation())
            self.assertTrue(p.get_attr("rp") is not None)
            self.assertIn(p.get_attr("rp"), par_ids)
        finnish = "Vastaa kyllä tai ei seuraaviin"
        english = "Answer yes or no to the following questions"
        new_md = old_md["text"].replace(english, finnish).replace("true", "false")
        self.post_par(
            tr_doc,
            new_md,
            plugin_tr_par.get_id(),
            expect_contains=finnish,
            json_key="texts",
            expect_xpath=".//mmcq",
        )

        md = self.get(f"/getBlock/{tr_doc.doc_id}/{plugin_tr_par.get_id()}")
        self.assertEqual(new_md, md["text"])
        pars = doc.document.get_paragraphs()
        self.assertIn(english, pars[1].get_markdown())

        # make sure that the translated markdown is applied and not the original when answering from Finnish document
        task_id = f'{doc.id}.{pars[1].get_attr("taskId")}'
        task_id_ext = f"{task_id}.{pars[1].get_id()}"
        self.post_answer(
            "mmcq",
            task_id_ext,
            [False, False, False],
            ref_from=(tr_doc.doc_id, plugin_tr_par.get_id()),
        )
        data = self.get_task_answers(task_id)
        self.assertEqual(3.0, data[0]["points"])

        # make sure the English version is not affected
        self.post_answer("mmcq", task_id_ext, [False, True, True])
        data = self.get_task_answers(task_id)
        self.assertEqual(1.0, data[0]["points"])
        self.assertEqual(3.0, data[1]["points"])

    def assert_translation_synced(self, tr_doc: Document, doc: DocInfo):
        tr_doc.clear_mem_cache()
        doc.document.clear_mem_cache()
        self.assertEqual(
            [p.get_id() for p in doc.document.get_paragraphs()],
            [p.get_attr("rp") for p in tr_doc.get_paragraphs()],
        )

    def test_translation_sync(self):
        """Translations are kept in sync."""
        self.login_test1()
        doc = self.create_doc()
        tr = self.create_translation(doc)
        tr_doc = tr.document
        self.assert_translation_synced(tr_doc, doc)

        self.new_par(doc.document, "1")
        self.assert_translation_synced(tr_doc, doc)

        self.new_par(doc.document, "2")
        self.assert_translation_synced(tr_doc, doc)

        self.new_par(doc.document, "3", doc.document.get_paragraphs()[0].get_id())
        self.assert_translation_synced(tr_doc, doc)

        self.new_par(doc.document, "4", doc.document.get_paragraphs()[1].get_id())
        self.assert_translation_synced(tr_doc, doc)

        self.post_par(
            doc.document, "5\n#-\n6", doc.document.get_paragraphs()[-1].get_id()
        )
        self.assert_translation_synced(tr_doc, doc)

        self.delete_par(doc, doc.document.get_paragraphs()[-1].get_id())
        self.assert_translation_synced(tr_doc, doc)

        self.delete_par(doc, doc.document.get_paragraphs()[0].get_id())
        self.assert_translation_synced(tr_doc, doc)

        self.update_whole_doc(doc, "replaced all with this")
        self.assert_translation_synced(tr_doc, doc)

        self.update_whole_doc(doc, "")
        self.assert_translation_synced(tr_doc, doc)

    def test_translation_extraneous_pars(self):
        """Any extraneous blocks (those without "rp" attribute) in translation documents are retained after syncing."""
        self.login_test1()
        doc = self.create_doc(initial_par=["1", "2", "3", "4"])
        tr = self.create_translation(doc)
        tr_doc = tr.document
        self.assert_translation_synced(tr_doc, doc)
        tr_doc.insert_paragraph(
            "new", insert_before_id=tr_doc.get_paragraphs()[1].get_id()
        )
        tr_doc.add_paragraph("new2")

        self.delete_par(doc, doc.document.get_paragraphs()[0].get_id())
        tr_doc.clear_mem_cache()
        tr_pars = tr_doc.get_paragraphs()
        self.assertEqual("new", tr_pars[0].get_markdown())
        self.assertEqual("new2", tr_pars[-1].get_markdown())
        self.assertEqual(
            doc.document.get_paragraphs()[0].get_id(), tr_pars[1].get_attr("rp")
        )

        self.update_whole_doc(doc, "whole new text")
        doc.document.clear_mem_cache()
        tr_doc.clear_mem_cache()
        first_id = doc.document.get_paragraphs()[0].get_id()
        tr_pars = tr_doc.get_paragraphs()
        self.assertEqual("new", tr_pars[0].get_markdown())
        self.assertEqual("new2", tr_pars[1].get_markdown())
        self.assertEqual(first_id, tr_pars[2].get_attr("rp"))

        self.new_par(doc.document, "new first", first_id)
        tr_doc.clear_mem_cache()
        tr_pars = tr_doc.get_paragraphs()
        self.assertEqual("new", tr_pars[0].get_markdown())
        self.assertEqual("new2", tr_pars[1].get_markdown())
        pars = doc.document.get_paragraphs()
        self.assertEqual(pars[0].get_id(), tr_pars[2].get_attr("rp"))
        self.assertEqual(pars[1].get_id(), tr_pars[3].get_attr("rp"))

    def test_translation_perf(self):
        self.login_test1()
        doc = self.create_doc(initial_par="hello")
        lang = "en"
        doc_title = "test"
        tr = self.create_translation(doc, doc_title, lang)
        self.get(tr.url)

        # Cache should be fresh at this point, so __write should not be called.
        with patch.object(DocParagraph, "_DocParagraph__write") as m:  # type: Mock
            self.get(tr.url)
        m.assert_not_called()

    def test_translation_settings(self):
        self.login_test1()
        d = self.create_doc(initial_par="hello")
        t = self.create_translation(d)
        d.document.set_settings({"a": "b"})
        self.new_par(d.document, "test")
        tr_pars = t.document.get_paragraphs()
        orig_pars = d.document.get_paragraphs()
        settings_id = orig_pars[0].get_id()
        self.assertEqual(tr_pars[0].get_attr("rp"), settings_id)
        tr_settings = DocSettings.from_paragraph(tr_pars[0])
        self.assertEqual(tr_settings.get_dict(), {})
        self.assertEqual(t.document.get_settings().get_dict(), {"a": "b"})

        d.document.set_settings({"a": "b", "c": "d"})
        tr_pars[0].set_markdown(YamlBlock(values={"c": "x"}).to_markdown())
        tr_pars[0].save()
        self.assertEqual(t.document.get_settings().get_dict(), {"a": "b", "c": "x"})

    def test_translation_ignored_src_doc(self):
        self.login_test1()
        d = self.create_doc(initial_par="test")
        t = self.create_translation(d)
        fake_id = 9999
        t.document.set_settings({"source_document": fake_id})
        self.assertEqual(t.document.get_source_document().doc_id, d.id)
        self.assertNotEqual(d.id, fake_id)

    def test_translation_invalid(self):
        """Missing rp attribute will not crash the document."""
        self.login_test1()
        d = self.create_doc()
        t = self.create_translation(d)
        t.document.add_text("#- {r=tr}")
        self.get(t.url)

    def test_translation_outofdate(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
a
#-
b
        """
        )
        t = self.create_translation(d)
        self.check_outofdate_count(t, 2)
        par = d.document.get_paragraphs()[0]
        tr_par = t.document.get_paragraphs()[0]

        self.check_preview_diff(t, tr_par, "", "a\n")
        self.post_par(
            t.document,
            tr_par.get_exported_markdown() + " tr",
            tr_par.get_id(),
            extra_data={"tags": {"marktranslated": True}},
        )
        tr_par = t.document.get_paragraphs()[0]
        self.check_preview_diff(t, tr_par, "a\n", "a\n")
        self.check_outofdate_count(t, 1)
        self.post_par(d.document, par.get_exported_markdown() + " edit", par.get_id())
        self.check_preview_diff(t, tr_par, "a\n", "a\n edit\n")
        self.check_outofdate_count(t, 2)
        self.post_par(
            t.document,
            tr_par.get_exported_markdown() + " tr2",
            tr_par.get_id(),
            extra_data={"tags": {"marktranslated": True}},
        )
        self.check_outofdate_count(t, 1)
        self.post_par(
            t.document,
            tr_par.get_exported_markdown() + " tr",
            tr_par.get_id(),
            extra_data={"tags": {"marktranslated": False}},
        )
        self.check_outofdate_count(t, 2)

        # Make sure merely toggling translation state (without changing markdown) will update the paragraph.
        self.post_par(
            t.document,
            tr_par.get_exported_markdown() + " tr",
            tr_par.get_id(),
            extra_data={"tags": {"marktranslated": True}},
        )
        self.check_outofdate_count(t, 1)

        t = Translation.find_by_id(t.id)
        self.test_user_2.grant_access(t, AccessType.view)
        db.session.commit()

        # only editors should see the outofdate messages
        self.login_test2()
        t = Translation.find_by_id(t.id)
        self.check_outofdate_count(t, 0)

    def check_preview_diff(self, t, tr_par, old, new):
        r = self.post_preview(
            t, tr_par.get_exported_markdown(), par=tr_par.get_id(), json_key="trdiff"
        )
        self.assertEqual(old, r["old"])
        self.assertEqual(new, r["new"])

    def check_outofdate_count(self, t, count):
        e = self.get(t.url, as_tree=True)
        outofdates = e.cssselect(".troutofdate")
        self.assertEqual(count, len(outofdates))

    def test_mark_all_translated(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
a
#-
b
#-
c
        """
        )
        t = self.create_translation(d)
        self.json_post(f"/markTranslated/{t.id}")
        self.check_outofdate_count(t, 0)
        self.assertEqual(6, len(t.document.get_changelog().entries))
        self.json_post(f"/markTranslated/{t.id}")
        t.document.clear_mem_cache()
        self.assertEqual(6, len(t.document.get_changelog().entries))

    def test_translation_alias_delete(self):
        self.login_test1()
        d = self.create_doc()
        self.create_translation(d)
        self.json_put(f"/alias/{d.id}/users%2Ftest-user-1%2Falias")
        self.json_delete(f"/alias/users%2Ftest-user-1%2Falias")

    def test_translation_delete(self):
        self.login_test1()
        d = self.create_doc()
        tr = self.create_translation(d)
        self.assertIsNotNone(tr)
        self.delete(f"/documents/{tr.id}")

        d = DocEntry.find_by_path(f"roskis/tl_{tr.id}_{d.id}_{tr.lang_id}_deleted")
        self.assertIsNotNone(d)
        self.assertEqual(len(d.translations), 2)

    def test_invalid_language(self):
        lang = Language(lang_code="baz", lang_name="foo", autonym="foo")
        self.login_test1()
        d = self.create_doc(
            initial_par="""
Foo
#-
Bar
#-
Baz
"""
        )
        d.lang_id = "orig"
        # TODO Would rather use ReversingTranslationService.service_name but
        #  is not str
        data = "Reversing"
        self.json_post(
            f"/translate/{d.id}/{lang.lang_code}/{data}",
            {"doc_title": "title"},
            expect_status=404,
        )

    def test_document_machine_translation_route(self):
        lang = self.reverselang
        self.login_test1()
        d = self.create_doc(
            initial_par="""
Foo
#-
Bar
#-
Baz
"""
        )
        d.lang_id = "orig"
        # TODO Would rather use ReversingTranslationService.service_name but
        #  is not str
        data = "Reversing"
        tr_json = self.json_post(
            f"/translate/{d.id}/{lang.lang_code}/{data}", {"doc_title": "title"}
        )
        tr_doc = db.session.get(Translation, tr_json["id"]).document
        tr_doc.clear_mem_cache()
        mds = map(lambda x: x.md, tr_doc.get_paragraphs())
        self.assertEqual("ooF", next(mds))
        self.assertEqual("raB", next(mds))
        self.assertEqual("zaB", next(mds))
        self.assertEqual(None, next(mds, None))

    def test_document_machine_translation_route_no_api_key(self):
        lang = self.reverselang
        self.login_test1()
        d = self.create_doc(
            initial_par="""
Foo
#-
Bar
#-
Baz
"""
        )
        d.lang_id = "orig"
        data = self.get_deepl_service().service_name
        self.json_post(
            f"/translate/{d.id}/{lang.lang_code}/{data}",
            {"doc_title": "title"},
            expect_status=404,
        )

    def test_document_machine_translation_route_forbidden(self):
        lang = self.reverselang
        self.login_test1()
        d = self.create_doc(
            initial_par="""
Foo
#-
Bar
#-
Baz
"""
        )

        d.lang_id = "orig"
        self.logout()
        self.login_test2()
        data = self.get_deepl_service().service_name
        self.json_post(
            f"/translate/{d.id}/{lang.lang_code}/{data}",
            {"doc_title": "title"},
            expect_status=403,
        )

    def test_paragraph_machine_translation_route(self):
        lang = self.reverselang
        self.login_test1()
        d = self.create_doc(
            initial_par="""
[Foo]{.notranslate}
#-
Bar
#-
Baz
"""
        )
        tr = self.create_translation(d)
        tr_doc = tr.document
        id1, id2, id3, *_ = [x.id for x in tr_doc.get_paragraphs()]
        # TODO Would rather use ReversingTranslationService.service_name but
        #  is not str
        data = "Reversing"
        r = self.json_post(
            f"/translate/paragraph/{tr.id}/{id1}/{lang.lang_code}/{data}"
        )
        tr_doc.clear_mem_cache()
        self.assertEqual(r["status"], "ok")
        # TIM doesn't (seem) to strip the paragraph contents on its own, so
        # because of it the newlines are kept at this point.
        self.assertEqual("\n[Foo]{.notranslate}\n", tr_doc.get_paragraph(id1).md)
        self.assertEqual("", tr_doc.get_paragraph(id2).md)
        self.assertEqual("", tr_doc.get_paragraph(id3).md)

        self.json_post(f"/translate/paragraph/{tr.id}/{id2}/{lang.lang_code}/{data}")
        tr_doc.clear_mem_cache()
        self.assertEqual("\nraB\n", tr_doc.get_paragraph(id2).md)

        self.json_post(f"/translate/paragraph/{tr.id}/{id3}/{lang.lang_code}/{data}")
        tr_doc.clear_mem_cache()
        self.assertEqual("\nzaB\n", tr_doc.get_paragraph(id3).md)

        self.json_post(f"/translate/paragraph/{tr.id}/{id3}/{lang.lang_code}/{data}")
        tr_doc.clear_mem_cache()
        # Applying translation again uses the SOURCE paragraph, so the result
        # is the same.
        self.assertEqual("\nzaB\n", tr_doc.get_paragraph(id3).md)

    def test_paragraph_machine_translation_route_no_api_key(self):
        lang = self.reverselang
        self.login_test1()
        d = self.create_doc(
            initial_par="""
Foo
#-
Bar
#-
Baz
"""
        )
        tr = self.create_translation(d)
        tr_doc = tr.document
        id1, id2, id3, *_ = [x.id for x in tr_doc.get_paragraphs()]
        data = self.get_deepl_service().service_name
        self.json_post(
            f"/translate/paragraph/{tr.id}/{id1}/{lang.lang_code}/{data}",
            expect_status=404,
        )

    def test_paragraph_machine_translation_route_forbidden(self):
        lang = self.reverselang
        self.login_test1()
        d = self.create_doc(
            initial_par="""
Foo
#-
Bar
#-
Baz
"""
        )
        tr = self.create_translation(d)
        self.logout()
        self.login_test2()
        tr_doc = tr.document
        id1, id2, id3, *_ = [x.id for x in tr_doc.get_paragraphs()]
        data = self.get_deepl_service().service_name
        self.json_post(
            f"/translate/paragraph/{tr.id}/{id1}/{lang.lang_code}/{data}",
            expect_status=403,
        )

    def test_paragraph_machine_translation_route_plugin(self):
        lang = self.reverselang
        self.login_test1()
        d = self.create_doc(
            initial_par="""
Foo
``` {plugin="csPlugin" #btn-tex2 .miniSnippets}
header: Harjoittele matemaattisen vastauksen kirjoittamista.
notexistingkey: Bar baz
```
#-
Qux
"""
        )
        tr = self.create_translation(d)
        tr_doc = tr.document
        id1, id2, id3, *_ = [x.id for x in tr_doc.get_paragraphs()]
        data = "Reversing"

        self.json_post(f"/translate/paragraph/{tr.id}/{id1}/{lang.lang_code}/{data}")
        tr_doc.clear_mem_cache()
        self.assertEqual("\nooF\n", tr_doc.get_paragraph(id1).md)

        self.json_post(f"/translate/paragraph/{tr.id}/{id2}/{lang.lang_code}/{data}")
        tr_doc.clear_mem_cache()
        tr_par2 = tr_doc.get_paragraph(id2)
        orig_par2 = d.document.get_paragraph(tr_par2.get_attr("rp"))
        self.assertEqual("btn-tex2", orig_par2.get_attr("taskId"))
        self.assertEqual(["miniSnippets"], orig_par2.get_attr("classes"))
        self.assertEqual("csPlugin", orig_par2.get_attr("plugin"))
        self.assertEqual(
            """
```
header: .atsimattiojrik neskuatsav nesittaametam elettiojraH
notexistingkey: Bar baz
```""",
            tr_par2.md,
        )

        self.json_post(f"/translate/paragraph/{tr.id}/{id3}/{lang.lang_code}/{data}")
        tr_doc.clear_mem_cache()
        self.assertEqual("\nxuQ\n", tr_doc.get_paragraph(id3).md)

    def test_text_machine_translation_route(self):
        lang = self.reverselang
        self.login_test1()
        d = self.create_doc()
        tr = self.create_translation(d)

        md = r"""
# Foo
[Bar]{.notranslate}\

Baz qux [qux](www.example.com)
  """

        data = {
            "originaltext": md,
        }
        resp = self.json_post(
            f"/translate/{tr.id}/{lang.lang_code}/translate_block/Reversing", data
        )
        # NOTE Apparently Pandoc likes to add to headers their text-content as identifier,
        # which does not seem to be a TIM-convention (which could be a problem?).
        self.assertEqual(
            r"""
# ooF{#foo}

[Bar]{.notranslate}\
 xuq zaB

[xuq](www.example.com)
  """,
            resp,
        )

    def test_text_machine_translation_route_no_api_key(self):
        lang = self.reverselang
        self.login_test1()
        d = self.create_doc()
        tr = self.create_translation(d)

        md = r"""
# Foo
[Bar]{.notranslate}\

Baz qux [qux](www.example.com)
"""

        transl = self.get_deepl_service().service_name
        data = {
            "originaltext": md,
        }
        self.json_post(
            f"/translate/{tr.id}/{lang.lang_code}/translate_block/{transl}",
            data,
            expect_status=404,
        )

    def test_text_machine_translation_route_forbidden(self):
        lang = self.reverselang
        self.login_test1()
        d = self.create_doc()
        tr = self.create_translation(d)
        self.logout()
        self.login_test2()

        md = r"""
# Foo
[Bar]{.notranslate}\

Baz qux [qux](www.example.com)
"""

        transl = self.get_deepl_service().service_name
        data = {
            "originaltext": md,
        }
        self.json_post(
            f"/translate/{tr.id}/{lang.lang_code}/translate_block/{transl}",
            data,
            expect_status=403,
        )

    def test_quota_exceeded_error(self):
        """Test the case where quota runs out during translation."""
        lang = self.reverselang
        translator = QuotaLimitedTestTranslator.__mapper_args__["polymorphic_identity"]
        self.login_test1()

        # Create some paragraphs with length equal to the translator's max quota.
        # TODO/NOTE This data is made for the reason, that partially applying
        #  the translation before failure might be implemented in the _future_.
        #  Without that, this is kind of overkill...
        text = "ab" * (MAX_TEST_CHAR_QUOTA // 2) + "a" * (MAX_TEST_CHAR_QUOTA % 2)
        pars_count = 3
        par_len = len(text) // pars_count
        # Split text between paragraphs into equal parts.
        pars_content = [
            text[i : i + par_len] for i in range(par_len, MAX_TEST_CHAR_QUOTA, par_len)
        ]
        pars_content[-1] += text[:par_len]

        # Sanity check that the data is of required length.
        chars_in_content = sum(len(par) for par in pars_content)
        self.assertEqual(pars_count, len(pars_content))
        self.assertEqual(MAX_TEST_CHAR_QUOTA, chars_in_content)

        d = self.create_doc(initial_par="\n#-\n".join(pars_content))
        d.lang_id = "orig"

        # Failing to finish automatic translation (exceeding quota)
        err_resp = self.json_post(
            f"/translate/{d.id}/{lang.lang_code}/{translator}",
            {"doc_title": "title"},
            expect_status=400,  # The code of RouteException.
        )
        # See that the translator's error message was returned.
        self.assertTrue(
            f"Translation quota ({len(text)}) exceeded" in err_resp["error"]
        )

        # Refresh the document.
        d = DocEntry.find_by_id(d.id)

        # See that the failed translation created the document with correct
        # references and contents.

        self.assertTrue(d.has_translation(lang.lang_code))
        self.assertEqual(2, len(d.translations))

        # Get the newly created translation doc.
        tr_doc = d.translations[1].document

        src_paras = d.document.get_paragraphs()
        tr_paras = tr_doc.get_paragraphs()
        self.assertEqual(len(src_paras), len(tr_paras))

        for src_par, tr_par in zip(src_paras, tr_paras):
            self.assertEqual("", tr_par.md)
            self.assertEqual(src_par.id, tr_par.get_attr("rp"))


# TODO Add cases for all 3 translation routes for the special cases (basically
#  just for plugins and tables)
