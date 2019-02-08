"""Server tests for getParDiff route."""
from lxml import html

from timApp.tests.server.timroutetest import TimRouteTest


class ParDiffTest(TimRouteTest):
    def test_par_diff(self):
        self.login_test1()
        self.get(f'/getParDiff/9999/0/0', expect_status=404)
        d = self.create_doc()
        self.get(f'/getParDiff/{d.id}/0/0', expect_content={'diff': [], 'live': 0, 'version': [0, 0]})
        d.document.add_paragraph('1')
        r = self.get(f'/getParDiff/{d.id}/0/0')
        self.assert_dict_subset(r, {'live': 0,
                                    'version': [1, 0]})
        self.assert_dict_subset(r['diff'][0], {'after_id': None,
                                               'type': 'insert'})
        self.assert_dict_subset(r['diff'][0]['content'], {'css': [],
                                                          'js': []})
        e = html.fromstring(r['diff'][0]['content']['texts'])
        self.assert_content(e, ['1'])

    def test_reference_par(self):
        """No exception is thrown when a reference par is in the "equal" section of the diff result."""
        self.login_test1()
        d = self.create_doc(initial_par="""#- {rp="yyyy" rd="9999"}""")
        pars = d.document.get_paragraphs()
        par = pars[0]
        par.set_attr('rp', 'xxxx')
        d.document.modify_paragraph_obj(par.get_id(), par)
        par.set_attr('rp', 'yyyy')
        d.document.modify_paragraph_obj(par.get_id(), par)
        self.get(f'/getParDiff/{d.id}/1/0')

    def test_diff_normal_to_reference(self):
        self.login_test1()
        d = self.create_doc(initial_par="""#- test""")
        par = d.document.get_paragraphs()[0]
        d.document.update(f"""#- {{id="{par.get_id()}" rp="yyyy" rd="9999"}}\ntest""", d.document.export_markdown())
        self.get(f'/getParDiff/{d.id}/1/0')
