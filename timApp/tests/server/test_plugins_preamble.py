from lxml import html

from timApp.answer.answer import Answer
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.viewcontext import default_view_ctx
from timApp.plugin.plugin import Plugin
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db


class PluginPreambleTest(TimRouteTest):
    def test_plugin_in_preamble(self):
        self.run_plugin_in_preamble("a/a", create_preamble_translation=True)
        self.run_plugin_in_preamble("b/b", create_preamble_translation=False)

    def run_plugin_in_preamble(self, doc_path: str, create_preamble_translation=True):
        self.login_test1()
        d = self.create_doc(path=self.get_personal_item_path(doc_path))
        p = self.create_preamble_for(d)
        p.document.add_text(
            """
``` {#t plugin="mmcq"}
stem: ""
choices:
  -
    correct: true
    reason: ""
    text: ""
```
            """
        )
        d.document.insert_preamble_pars()
        par = d.document.get_paragraphs()[0]
        plug = Plugin.from_paragraph(par, default_view_ctx)
        self.assertEqual(f"{d.id}.t", plug.task_id.doc_task)
        resp = self.post_answer(plug.type, plug.task_id.extended, [True])
        a: Answer = db.session.get(Answer, resp["savedNew"])
        self.assertEqual(1, a.points)
        self.assertEqual(f"{d.id}.t", a.task_id)
        self.get_state(a.id)

        if create_preamble_translation:
            tr_p = self.create_translation(p)
            tr_par = tr_p.document.get_paragraphs()[0]
            tr_par.set_markdown(par.get_markdown().replace("true", "false"))
            tr_par.save()
        else:
            tr_p = p

        tr = self.create_translation(d)
        tr.document.insert_preamble_pars()

        resp = self.post_answer(
            plug.type,
            plug.task_id.extended,
            [False],
            ref_from=(tr.id, tr.document.get_paragraphs()[0].get_id()),
        )
        a: Answer = db.session.get(Answer, resp["savedNew"])
        self.assertEqual(1 if create_preamble_translation else 0, a.points)
        self.assertEqual(f"{d.id}.t", a.task_id)
        # Reattach the documents to the session
        db.session.add(tr)
        d = DocEntry.find_by_id(d.id)
        self.check_plugin_ref_correct(
            tr, d, p.document.get_paragraphs()[0], preamble_doc=tr_p
        )

    def test_referenced_plugin_in_preamble(self):
        # TODO: re-enable this test case when the issue with cited plugins in translated preambles
        #       has been fixed (see https://github.com/TIM-JYU/TIM/pull/3464)
        # self.run_referenced_plugin_in_preamble("c/c", create_preamble_translation=True)
        self.run_referenced_plugin_in_preamble("d/d", create_preamble_translation=False)

    def run_referenced_plugin_in_preamble(
        self, doc_path: str, create_preamble_translation=True
    ):
        self.login_test1()
        d = self.create_doc(path=self.get_personal_item_path(doc_path))
        plugin_doc = self.create_doc(
            initial_par="""
``` {#t plugin="mmcq"}
stem: ""
choices:
  -
    correct: true
    reason: ""
    text: ""
            """
        )
        p = self.create_preamble_for(d)
        p.document.add_paragraph_obj(
            plugin_doc.document.get_paragraphs()[0].create_reference(p.document)
        )
        plugin_par = plugin_doc.document.get_paragraphs()[0]
        plug = Plugin.from_paragraph(plugin_par, default_view_ctx)
        d.document.insert_preamble_pars()
        # The plugin is a reference, so it exists only in the original document.
        self.post_answer(
            plug.type,
            f"{d.id}.t",
            [True],
            expect_status=400,
            expect_content=f"Task not found in the document: {plug.task_id.task_name}",
        )

        resp = self.post_answer(
            plug.type,
            plug.task_id.extended,
            [True],
            ref_from=(d.id, d.document.get_paragraphs()[0].get_id()),
        )
        a: Answer = db.session.get(Answer, resp["savedNew"])
        self.assertEqual(1, a.points)
        self.assertEqual(plug.task_id.doc_task, a.task_id)

        if create_preamble_translation:
            tr_p = self.create_translation(p)
        else:
            tr_p = p

        tr = self.create_translation(d)
        tr.document.insert_preamble_pars()

        resp = self.post_answer(
            plug.type,
            plug.task_id.extended,
            [False],
            ref_from=(tr.id, tr.document.get_paragraphs()[0].get_id()),
        )
        a: Answer = db.session.get(Answer, resp["savedNew"])
        self.assertEqual(0, a.points)
        self.assertEqual(plug.task_id.doc_task, a.task_id)
        self.check_plugin_ref_correct(tr, plugin_doc, plugin_par, preamble_doc=tr_p)

    def test_reference_to_preamble(self):
        self.login_test1()
        d = self.create_doc(path=self.get_personal_item_path("e/e"))
        p = self.create_preamble_for(d)
        p.document.add_text(
            """
``` {#t plugin="mmcq"}
stem: ""
choices:
  -
    correct: true
    reason: ""
    text: ""
```
                    """
        )
        tr_p = self.create_translation(p)
        tr_p.document.set_settings(
            {"global_plugin_attrs": {"all": {"buttonText": "This is in English"}}}
        )

        self.check_plugin_ref_correct(tr_p, p, p.document.get_paragraphs()[0])
        n = self.create_doc()
        n.document.add_paragraph_obj(
            tr_p.document.get_paragraphs()[1].create_reference(n.document)
        )
        # print(f'd={d.id} p={p.id} tr_p={tr_p.id} n={n.id}')
        self.check_plugin_ref_correct(
            n, p, p.document.get_paragraphs()[0], text_to_check="This is in English"
        )

    def check_plugin_ref_correct(
        self,
        doc_to_check: DocInfo,
        expected_doc: DocInfo,
        expected_par: DocParagraph,
        text_to_check="",
        preamble_doc: DocInfo = None,
    ):
        par = (
            self.get(doc_to_check.url, as_tree=True)
            .cssselect("mmcq")[0]
            .getparent()
            .getparent()
            .getparent()
        )
        doc_to_check = DocEntry.find_by_id(doc_to_check.id)
        expected_doc = DocEntry.find_by_id(expected_doc.id)
        # print(html.tostring(par, pretty_print=True).decode())
        if preamble_doc:
            preamble_doc = DocEntry.find_by_id(preamble_doc.id)
            self.assertEqual(preamble_doc.path, par.attrib["data-from-preamble"])
        else:
            self.assertIsNone(par.attrib.get("data-from-preamble"))
        if text_to_check:
            self.assertIn(text_to_check, html.tostring(par).decode())
        # print(f'{expected_doc.id}.t.{expected_par.get_id()}')
        self.assertEqual(expected_par.get_id(), par.attrib["ref-id"])
        self.assertEqual(str(expected_doc.id), par.attrib["ref-doc-id"])
        self.assertTrue(
            par.cssselect(rf"#{expected_doc.id}\.t\.{expected_par.get_id()}")
        )
