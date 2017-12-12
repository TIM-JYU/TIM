from datetime import timedelta

from lxml.cssselect import CSSSelector

from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.readings import get_readings, get_read_expiry_condition
from timApp.timdb.readparagraphtype import ReadParagraphType
from timApp.timdb.tim_models import ReadParagraph

readline_selector = CSSSelector('div.readline')

UNREAD = ''
READ = ReadParagraphType.click_red.class_str()
MODIFIED = READ + '-modified'
PAR_CLICK = ReadParagraphType.click_par.class_str()
PAR_CLICK_MODIFIED = PAR_CLICK + '-modified'


class ReadingsTest(TimRouteTest):

    def test_readings_normal(self):
        self.login_test1()
        doc = self.create_doc(initial_par=['test', 'test2', 'test3']).document
        q = ReadParagraph.query.filter_by(doc_id=doc.doc_id)
        pars = doc.get_paragraphs()

        self.check_readlines(self.get_readings(doc), (UNREAD, UNREAD, UNREAD))
        self.mark_as_read(doc, pars[0].get_id())
        self.check_readlines(self.get_readings(doc), (READ, UNREAD, UNREAD))
        self.mark_as_read(doc, pars[1].get_id())
        self.check_readlines(self.get_readings(doc), (READ, READ, UNREAD))
        doc.modify_paragraph(pars[1].get_id(), 'a')
        self.check_readlines(self.get_readings(doc), (READ, MODIFIED, UNREAD))
        self.mark_as_read(doc, pars[2].get_id(), ReadParagraphType.click_par)
        self.check_readlines(self.get_readings(doc), (READ, MODIFIED, PAR_CLICK))
        doc.modify_paragraph(pars[2].get_id(), 'b')
        self.check_readlines(self.get_readings(doc), (READ, MODIFIED, PAR_CLICK_MODIFIED))
        self.mark_as_read(doc, pars[2].get_id())
        self.assertEqual(q.count(), 4)
        self.check_readlines(self.get_readings(doc), (READ, MODIFIED, PAR_CLICK_MODIFIED + ' ' + READ))
        self.mark_as_unread(doc, pars[2].get_id())
        self.assertEqual(q.count(), 3)
        self.check_readlines(self.get_readings(doc), (READ, MODIFIED, PAR_CLICK_MODIFIED))

    def test_readings_group(self):
        self.login_test1()
        self.login_test2(add=True)
        doc = self.create_doc(initial_par=['test', 'test2', 'test3', 'test4']).document
        q = ReadParagraph.query.filter_by(doc_id=doc.doc_id)
        self.check_readlines(self.get_readings(doc), (UNREAD, UNREAD, UNREAD, UNREAD))
        pars = doc.get_paragraphs()
        self.mark_as_read(doc, pars[0].get_id())

        self.check_readlines(self.get_readings(doc), (READ, UNREAD, UNREAD, UNREAD))
        self.mark_as_read(doc, pars[1].get_id())
        self.check_readlines(self.get_readings(doc), (READ, READ, UNREAD, UNREAD))

        self.login_test2()
        self.check_readlines(self.get_readings(doc), (READ, READ, UNREAD, UNREAD))
        self.mark_as_read(doc, pars[2].get_id())
        self.check_readlines(self.get_readings(doc), (READ, READ, READ, UNREAD))

        self.login_test1(add=True)
        self.check_readlines(self.get_readings(doc), (READ, READ, UNREAD, UNREAD))

        self.login_test1()
        self.mark_as_read(doc, pars[2].get_id())
        self.login_test2(add=True)
        self.check_readlines(self.get_readings(doc), (READ, READ, READ, UNREAD))
        doc.modify_paragraph(pars[2].get_id(), 'a')
        self.check_readlines(self.get_readings(doc), (READ, READ, MODIFIED, UNREAD))
        self.login_test2()
        self.mark_as_read(doc, pars[2].get_id())
        self.check_readlines(self.get_readings(doc), (READ, READ, READ, UNREAD))
        self.login_test1(add=True)
        self.check_readlines(self.get_readings(doc), (READ, READ, UNREAD, UNREAD))
        self.login_test1()
        self.check_readlines(self.get_readings(doc), (READ, READ, MODIFIED, UNREAD))
        self.assertEqual(q.count(), 7)

    def get_readings(self, doc):
        readlines = readline_selector(self.get(f'/view/{doc.doc_id}', as_tree=True))
        return readlines

    def mark_as_unread(self, doc, par_id):
        self.json_put(f'/unread/{doc.doc_id}/{par_id}')

    def mark_as_read(self, doc, par_id, read_type=ReadParagraphType.click_red, **kwargs):
        self.json_put(f'/read/{doc.doc_id}/{par_id}/{read_type.value}', **kwargs)

    def check_readlines(self, readlines, expected):
        self.assertEqual(len(readlines), len(expected))
        for r, e in zip(readlines, expected):
            classes = set(r.attrib['class'].split(' '))
            self.assertIn('readline', classes)
            if e:
                for c in e.split(' '):
                    self.assertIn(c, classes)

    def test_invalid_reference(self):
        self.login_test1()
        d = self.create_doc(initial_par='test')
        d.document.add_text(f'#- {{rd={d.id} ra={"nonexistent"}}}')
        d.document.add_text(f'#- {{rd={d.id} rp={"nonexistent"}}}')
        self.mark_as_read(d.document, d.document.get_paragraphs()[1].get_id(), expect_status=404)
        self.mark_as_read(d.document, d.document.get_paragraphs()[2].get_id(), expect_status=404)

    def test_mark_all_read(self):
        self.login_test1()
        d = self.create_doc(initial_par=['1', '2'])
        self.json_put(f'/read/{d.id}')
        self.check_readlines(self.get_readings(d.document), (READ, READ))
        q = ReadParagraph.query.filter_by(doc_id=d.id)
        self.assertEqual(q.count(), 2)
        self.json_put(f'/read/{d.id}')
        self.assertEqual(q.count(), 2)

    def test_expiry(self):
        self.login_test1()
        d = self.create_doc(initial_par=['1', '2'])
        self.json_put(f'/read/{d.id}')
        self.mark_as_read(d.document, d.document.get_paragraphs()[0].get_id(), ReadParagraphType.on_screen)
        rs = get_readings(self.current_user.get_personal_group().id, d.document,
                          get_read_expiry_condition(timedelta(seconds=10)))
        self.assertEqual(len(rs), 3)
        rs = get_readings(self.current_user.get_personal_group().id, d.document,
                          get_read_expiry_condition(timedelta(seconds=0)))
        self.assertEqual(len(rs), 2)

    def test_expiry_invalid(self):
        self.login_test1()
        d = self.create_doc()
        d.document.set_settings({'read_expiry': "a"})
        self.get(d.url)

    def test_readings_json(self):
        self.login_test1()
        ug_id = self.get_test_user_1_group_id()
        d = self.create_doc(initial_par=['1', '2'])
        pars = d.document.get_paragraphs()
        self.json_put(f'/read/{d.id}')
        rs = self.get(f'/read/{d.id}')
        self.assert_list_of_dicts_subset(rs, [{'doc_id': d.id,
                                               'type': 'read',
                                               'usergroup_id': ug_id},
                                              {'doc_id': d.id,
                                               'type': 'read',
                                               'usergroup_id': ug_id}])
        known_keys = {'doc_id', 'type', 'usergroup_id', 'id', 'timestamp', 'par_id', 'par_hash'}
        for r in rs:
            self.assertIsInstance(r['id'], int)
            self.assertIsInstance(r['timestamp'], str)
            self.assertEqual(set(r.keys()), known_keys)
        self.assertNotEqual(rs[0]['id'], rs[1]['id'])
        self.assertEqual(rs[0]['timestamp'], rs[1]['timestamp'])
        self.assertEqual(set(p.get_id() for p in pars), set(r['par_id'] for r in rs))
        self.assertEqual(set(p.get_hash() for p in pars), set(r['par_hash'] for r in rs))
