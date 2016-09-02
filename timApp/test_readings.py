from lxml.cssselect import CSSSelector

from timroutetest import TimRouteTest

readline_selector = CSSSelector('div.readline')

UNREAD = ''
READ = 'read'
MODIFIED = 'modified'


class ReadingsTest(TimRouteTest):
    def test_readings_normal(self):
        self.login_test1()
        doc = self.create_doc(initial_par='test\n#-\ntest2')
        pars = doc.get_paragraphs()

        self.check_readlines(self.get_readings(doc), (UNREAD, UNREAD))
        self.mark_as_read(doc, pars[0].get_id())
        self.check_readlines(self.get_readings(doc), (READ, UNREAD))
        self.mark_as_read(doc, pars[1].get_id())
        self.check_readlines(self.get_readings(doc), (READ, READ))
        doc.modify_paragraph(pars[1].get_id(), 'a')
        self.check_readlines(self.get_readings(doc), (READ, MODIFIED))

    def test_readings_group(self):
        self.login_test1()
        self.login_test2(add=True)
        doc = self.create_doc(initial_par='test\n#-\ntest2\n#-\ntest3\n#-\ntest4')
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

    def get_readings(self, doc):
        readlines = readline_selector(self.get('/view/{}'.format(doc.doc_id), as_tree=True))
        return readlines

    def mark_as_read(self, doc, par_id):
        self.json_put('/read/{}/{}'.format(doc.doc_id, par_id))

    def check_readlines(self, readlines, expected):
        self.assertEqual(len(readlines), len(expected))
        for r, e in zip(readlines, expected):
            self.assertEqual(r.attrib['class'], ('readline ' + e).strip())
