"""Server tests for preview."""
from timApp.tests.server.timroutetest import TimRouteTest


class PreviewTest(TimRouteTest):
    def test_translation_invalid_ref(self):
        self.login_test1()
        d = self.create_doc(initial_par="""#- {rd=9999 rp=xxxx}'""")
        t = self.create_translation(d)
        p = t.document.get_paragraphs()[0]
        md = f'#- {{r="tr" rp="{p.get_attr("rp")}"}}\n'
        self.get(f'/getBlock/{t.id}/{p.get_id()}', expect_content={'text': md})
        e = self.json_post(f'/preview/{t.id}', {'text': md}, json_key='texts', as_tree=True)
        self.assert_content(e, ['The referenced document does not exist.'])

    def test_help_par(self):
        self.login_test1()
        d = self.create_doc()
        e = self.json_post(f'/preview/{d.id}', {'text': 'test', 'par': 'HELP_PAR'}, json_key='texts', as_tree=True)
        self.assert_content(e, ['test'])

    def test_line_break(self):
        self.login_test1()
        d = self.create_doc()
        e = self.json_post(f'/preview/{d.id}', {'text': 'test\\\ntest2\\'}, json_key='texts', as_tree=True)
        self.assert_content(e, ['test\ntest2'])
