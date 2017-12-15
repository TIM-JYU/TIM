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

    def test_duplicate_task_ids(self):
        self.login_test1()
        d = self.create_doc()
        r = self.new_par(d.document, "``` {#test plugin=showVideo}\n```")
        self.assertEqual(r['duplicates'], [])
        r = self.new_par(d.document, "``` {#test plugin=showVideo}\n```")
        pars = d.document.get_paragraphs()
        self.assertEqual(r['duplicates'], [['test', pars[1].get_id()]])

    def test_area_editing(self):
        self.login_test1()
        d = self.create_doc(initial_par=['a1', 'a2', 'a3'])
        pars = d.document.get_paragraphs()
        new_text = d.document.export_markdown().replace('a1', 'b1').replace('a2', 'b2').replace('a3', 'b3')
        self.json_post('/postParagraph/', {
            "text": new_text,
            "docId": d.id,
            "par": pars[0].get_id(),
            "par_next": None,
            "area_start": pars[0].get_id(),
            "area_end": pars[-1].get_id(),
        })
        d.document.clear_mem_cache()
        self.assertEqual(d.document.export_markdown(), new_text)
        self.json_post(f'/deleteParagraph/{d.id}', {
            "area_start": pars[0].get_id(),
            "area_end": pars[-1].get_id(),
        })
        d.document.clear_mem_cache()
        self.assertEqual(d.document.get_paragraphs(), [])

    def test_get_updates_pars_translation(self):
        self.login_test1()
        d = self.create_doc(initial_par=['kissa'])
        t = self.create_translation(d)
        e = self.get(t.url, as_tree=True)
        self.assert_content(e, ['kissa'])
        tr_pars = t.document.get_paragraphs()
        par = tr_pars[0]
        md = par.get_exported_markdown().replace('kissa', 'cat')
        e = self.post_par(t.document, md, par.get_id(), as_tree=True, json_key='texts')
        self.assert_content(e, ['cat'])
        updated = self.get_updated_pars(t, json_key='changed_pars')
        e = html.fromstring(updated[par.get_id()])
        self.assert_content(e, ['cat'])

    def test_update(self):
        """Editing a document with minor errors is possible in manage.

        Document must not contain any errors after editing.

        """
        self.login_test1()
        d = self.create_doc()
        for i in range(0, 2):
            d.document.add_text("#- {area=a}")
            d.document.add_text("#- {area_end=a}")
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
                       expect_contains=f'Attributes at end of code block noticed in paragraph ',
                       json_key='error')

    def test_new_from_help_par(self):
        self.login_test1()
        d = self.create_doc()
        e = self.json_post(f'/newParagraph/', {'docId': d.id, 'text': 'test', 'par': 'HELP_PAR', 'par_next': None},
                           json_key='texts', as_tree=True)
        self.assert_content(e, ['test'])

    def test_duplicate_template(self):
        """Trying to load a template to a non-empty document is not possible."""
        self.login_test1()
        t = self.create_doc(initial_par=['p1', 'p2'])
        d = self.create_doc()
        template_pars = t.document.get_paragraphs()
        d.document.add_paragraph('p1', template_pars[0].get_id())
        d.document.add_paragraph('p2', template_pars[1].get_id())
        self.json_post(f'/update/{d.id}', {'template_name': t.path}, expect_status=400,
                       expect_content={'error': 'Cannot load a template because the document is not empty.'})
        self.get(d.url)

    def test_nonexistent_template(self):
        """Trying to load a non-existent template gives 404."""
        self.login_test1()
        d = self.create_doc()
        self.json_post(f'/update/{d.id}', {'template_name': 'xxx'}, expect_status=404,
                       expect_content={'error': 'Template not found'})

    def test_invalid_add(self):
        self.login_test1()
        d = self.create_doc()
        self.new_par(d.document, 'test', next_id='xxx', expect_status=400)

    def test_invalid_update(self):
        self.login_test1()
        d = self.create_doc(initial_par='test')
        par = d.document.get_paragraphs()[0]
        md = d.document.export_markdown()
        self.json_post(f'/update/{d.id}', {'fulltext': md, 'original': ''}, expect_status=400,
                       expect_content={'error': f'Duplicate paragraph id(s): {par.get_id()}'})
        self.get(d.url)

    def test_duplicate_par_ids(self):
        self.login_test1()
        d = self.create_doc(initial_par=['test1', 'test2'])
        pars = d.document.get_paragraphs()
        par1 = pars[0]
        par2 = pars[1]
        md1 = par1.get_exported_markdown(export_ids=True)
        self.new_par(d.document, md1, expect_status=400,
                     expect_content={'error': f'Duplicate paragraph id(s): {par1.get_id()}'})
        self.post_par(d.document, md1, par_id=par1.get_id())
        md2 = par2.get_exported_markdown(export_ids=True)
        self.post_par(d.document, md1 + md2, par_id=par1.get_id(), expect_status=400,
                      expect_content={'error': f'Duplicate paragraph id(s): {par2.get_id()}'})
        self.get(d.url)

    def test_version(self):
        self.login_test1()
        d = self.create_doc()
        j = self.new_par(d.document, 'test')
        self.assertEqual(j['version'], [1, 0])

    def test_mark_read(self):
        self.login_test1()
        d = self.create_doc()
        self.new_par(d.document, 'test', additional_data={'tags': {'markread': True}})
