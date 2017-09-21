from lxml import html

from timApp.tests.server.timroutetest import TimRouteTest


class EditTest(TimRouteTest):

    def test_nonexistent_edit(self):
        self.login_test1()
        d = self.create_doc(initial_par='test')
        par_id = d.document.get_paragraphs()[0].get_id()
        invalid_par = 'nonexistent'
        self.json_post(f'/deleteParagraph/{d.id}', {'par': invalid_par},
                       expect_status=400,
                       expect_content={'error': f'Paragraph {invalid_par} does not exist'})
        self.json_post(f'/deleteParagraph/{d.id}', {'area_start': invalid_par, 'area_end': par_id},
                       expect_status=400,
                       expect_content={'error': f'Paragraph {invalid_par} does not exist'})
        self.json_post(f'/deleteParagraph/{d.id}', {'area_start': par_id, 'area_end': invalid_par},
                       expect_status=400,
                       expect_content={'error': f'Paragraph {invalid_par} does not exist'})
        self.get(f'/getBlock/{d.id}/{invalid_par}', expect_status=404,
                 expect_content=f'Document {d.id}: Paragraph not found: {invalid_par}',
                 json_key='error')
        self.get(f'/getBlock/{d.id}/{par_id}', query_string={'area_start': par_id, 'area_end': invalid_par},
                 expect_status=404,
                 expect_content=f'Document {d.id}: Paragraph not found: {invalid_par}',
                 json_key='error')

    def test_duplicates(self):
        self.login_test1()
        d = self.create_doc()
        r = self.new_par(d.document, "``` {#test plugin=showVideo}\n```")
        self.assertEqual(r['duplicates'], [])
        r = self.new_par(d.document, "``` {#test plugin=showVideo}\n```")
        pars = d.document.get_paragraphs()
        self.assertEqual(r['duplicates'], [['test', pars[1].get_id()]])

    def test_get_updates_pars_translation(self):
        self.login_test1()
        d = self.create_doc(initial_par=['kissa'])
        t = self.create_translation(d, 'test', 'en')
        e = self.get(t.url, as_tree=True)
        self.assert_content(e, ['', 'kissa'])
        tr_pars = t.document.get_paragraphs()
        md = tr_pars[1].get_exported_markdown().replace('kissa', 'cat')
        e = self.post_par(t.document, md, tr_pars[1].get_id(), as_tree=True, json_key='texts')
        self.assert_content(e, ['cat'])
        updated = self.get_updated_pars(t, json_key='changed_pars')
        e = html.fromstring(updated[tr_pars[1].get_id()])
        self.assert_content(e, ['cat'])

    def test_update(self):
        """Editing a document with minor errors is possible in manage.
        Document must not contain any errors after editing.
        """
        self.login_test1()
        d = self.create_doc()
        for i in range(0, 2):
            d.document.add_paragraph("#- {area=a}")
            d.document.add_paragraph("#- {area_end=a}")
        par_ids = d.document.get_par_ids()
        self.json_post(f'/update/{d.id}',
                       {'fulltext': f"""
#- {{area=a id={par_ids[0]}}}
#- {{area_end=a id={par_ids[1]}}}
#- {{area=b id={par_ids[2]}}}
#- {{area_end=b id={par_ids[3]}}}
                       """,
                        'original': d.document.export_markdown()})
        orig_text = d.document.export_markdown()
        self.json_post(f'/update/{d.id}',
                       {'fulltext': f"""
#- {{area=a id={par_ids[0]}}}
#- {{area_end=a id={par_ids[1]}}}
#- {{area=a id={par_ids[2]}}}
#- {{area_end=a id={par_ids[3]}}}
                       """,
                        'original': orig_text},
                       expect_status=400,
                       expect_content=f'Multiple areas with same name noticed in paragraph {par_ids[2]}\nDuplicate area end noticed in paragraph {par_ids[3]}',
                       json_key='error')
        self.json_post(f'/update/{d.id}',
                       {'fulltext': f"""
#- {{id={par_ids[0]}}}
#- {{id={par_ids[0]}}}
                       """,
                        'original': orig_text},
                       expect_status=400,
                       expect_content=f'Duplicate paragraph id noticed in paragraph {par_ids[0]}',
                       json_key='error')
        self.json_post(f'/update/{d.id}',
                       {'fulltext': f"""
#- {{id=xxxx}}
                       """,
                        'original': orig_text},
                       expect_status=400,
                       expect_content=f'Invalid paragraph id noticed in paragraph xxxx',
                       json_key='error')
        self.json_post(f'/update/{d.id}',
                       {'fulltext': """
```
``` {a=b}
                       """,
                        'original': orig_text},
                       expect_status=400,
                       expect_content=f'Attributes at end of code block noticed in a paragraph',
                       json_key='error')

    def test_new_from_help_par(self):
        self.login_test1()
        d = self.create_doc()
        e = self.json_post(f'/newParagraph/', {'docId': d.id, 'text': 'test', 'par': 'HELP_PAR', 'par_next': None}, json_key='texts', as_tree=True)
        self.assert_content(e, ['test'])
