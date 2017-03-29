from typing import Optional

from documentmodel.docparagraph import DocParagraph
from tests.server.timroutetest import TimRouteTest
from timdb.models.docentry import DocEntry
from timdb.readparagraphtype import ReadParagraphType


class ClipboardTest(TimRouteTest):

    def copy(self, doc: DocEntry, par_start: DocParagraph, par_end: DocParagraph):
        self.json_post('/clipboard/copy/{}/{}/{}'.format(doc.id, par_start.get_id(), par_end.get_id()))

    def paste(self, doc: DocEntry, par_before: Optional[DocParagraph] = None, par_after: Optional[DocParagraph] = None,
              as_ref: bool = False):
        self.json_post('/clipboard/paste/{}'.format(doc.id),
                       {'par_before': par_before.get_id() if par_before else None,
                        'par_after': par_after.get_id() if par_after else None,
                        'as_ref': as_ref})

    def test_copy_one(self):
        self.login_test1()
        test_pars = ['test1', 'test2', 'test3']
        d = self.create_doc(initial_par=test_pars)
        pars = d.document.get_paragraphs()
        self.copy(d, pars[0], pars[0])
        self.paste(d, par_after=pars[2])
        d.document.clear_mem_cache()
        pars_new = d.document.get_paragraphs()
        self.assertListEqual(test_pars, [p.get_markdown() for p in pars_new[0:3]])
        self.assertEqual(pars_new[0], pars_new[3])

    def test_copy_many(self):
        self.login_test1()
        test_pars = ['test1', 'test2', 'test3', 'test4']
        d = self.create_doc(initial_par=test_pars)
        pars = d.document.get_paragraphs()
        timdb = self.get_db()
        timdb.readings.mark_read(usergroup_id=self.current_group().id,
                                 doc=d.document,
                                 par=pars[0],
                                 read_type=ReadParagraphType.click_red)
        timdb.readings.mark_read(usergroup_id=self.current_group().id,
                                 doc=d.document,
                                 par=pars[0],
                                 read_type=ReadParagraphType.hover_par)
        timdb.readings.mark_read(usergroup_id=self.current_group().id,
                                 doc=d.document,
                                 par=pars[1],
                                 read_type=ReadParagraphType.hover_par)
        self.copy(d, pars[0], pars[2])
        self.paste(d, par_after=pars[2])
        d.document.clear_mem_cache()
        pars_new = d.document.get_paragraphs()
        new_readings = timdb.readings.get_readings(self.current_group().id, d.document)
        self.assertEqual(6, len(new_readings))
        self.assertEqual(2, len(list(filter(lambda r: r.type == ReadParagraphType.click_red, new_readings))))
        self.assertEqual(4, len(list(filter(lambda r: r.type == ReadParagraphType.hover_par, new_readings))))
        self.assertListEqual(['test1', 'test2', 'test3', 'test1', 'test2', 'test3', 'test4'],
                             [p.get_markdown() for p in pars_new])

    def test_copy_to_empty(self):
        self.login_test1()
        d = self.create_doc(initial_par=['test1', 'test2'])
        pars = d.document.get_paragraphs()

        d2 = self.create_doc()
        self.copy(d, pars[0], pars[1])
        self.paste(d2, par_after=DocParagraph.help_par())
        d2_pars = d2.document.get_paragraphs()
        self.assertEqual(pars, d2_pars)

        d3 = self.create_doc()
        self.copy(d, pars[0], pars[1])
        self.paste(d3, par_before=DocParagraph.help_par())
        d3_pars = d3.document.get_paragraphs()
        self.assertEqual(pars, d3_pars)
