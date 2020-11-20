from lxml import html

from timApp.document.docinfo import DocInfo
from timApp.document.viewcontext import default_view_ctx
from timApp.tests.server.timroutetest import TimRouteTest


class IndexTest(TimRouteTest):
    def test_index_one_heading_per_par(self):
        self.login_test1()
        doc = self.create_doc(initial_par="""
# Heading level 1
Lorem ipsum.

---

#-
## Heading level 2
#-
### Heading level 3
#-
# Second heading level 1
        """).document
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': 'Heading level 1'},
                           [{'id': 'heading-level-2', 'level': 2, 'text': 'Heading level 2'},
                            {'id': 'heading-level-3', 'level': 3, 'text': 'Heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': 'Second heading level 1'},
                           [])], doc.get_index(default_view_ctx))
        doc = self.create_doc(initial_par="""
# Heading level 1
Lorem ipsum.

---

```
# Not a header
```

#-
# Unnumbered {.nonumber}
#-
## Heading level 2
#-
## Second heading level 2
#-
### Heading level 3
#-
# Second heading level 1
        """).document
        doc.set_settings({'auto_number_headings': True, 'heading_format': {2: "{h1}.{h2}. {text}"}})
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': '1. Heading level 1'}, []),
                          ({'id': 'unnumbered', 'level': 1, 'text': 'Unnumbered'},
                           [{'id': 'heading-level-2', 'level': 2, 'text': '1.1. Heading level 2'},
                            {'id': 'second-heading-level-2', 'level': 2, 'text': '1.2. Second heading level 2'},
                            {'id': 'heading-level-3', 'level': 3, 'text': '1.2.1 Heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': '2. Second heading level 1'},
                           [])], doc.get_index(default_view_ctx))

        doc.set_settings({'auto_number_headings': False})
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': 'Heading level 1'}, []),
                          ({'id': 'unnumbered', 'level': 1, 'text': 'Unnumbered'},
                           [{'id': 'heading-level-2', 'level': 2, 'text': 'Heading level 2'},
                            {'id': 'second-heading-level-2', 'level': 2, 'text': 'Second heading level 2'},
                            {'id': 'heading-level-3', 'level': 3, 'text': 'Heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': 'Second heading level 1'},
                           [])], doc.get_index(default_view_ctx))

        doc.set_settings({'auto_number_headings': True,
                          'heading_format': {2: '{', 3: '{', 4: '{', 5: '{', 6: '{'}})
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': '1. Heading level 1'}, []),
                          ({'id': 'unnumbered', 'level': 1, 'text': 'Unnumbered'},
                           [{'id': 'heading-level-2', 'level': 2, 'text': '[ERROR] Heading level 2'},
                            {'id': 'second-heading-level-2', 'level': 2, 'text': '[ERROR] Second heading level 2'},
                            {'id': 'heading-level-3', 'level': 3, 'text': '[ERROR] Heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': '2. Second heading level 1'},
                           [])], doc.get_index(default_view_ctx))

    def test_index_many_headings_per_par(self):
        self.login_test1()
        doc = self.create_doc(initial_par="""
# Heading level 1
Lorem ipsum.

---

## Heading level 2

## Unnumbered {.nonumber}

### Heading level 3
#-
# Second heading level 1
        """).document
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': 'Heading level 1'},
                           [{'id': 'heading-level-2', 'level': 2, 'text': 'Heading level 2'},
                            {'id': 'unnumbered', 'level': 2, 'text': 'Unnumbered'},
                            {'id': 'heading-level-3', 'level': 3, 'text': 'Heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': 'Second heading level 1'},
                           [])], doc.get_index(default_view_ctx))

    def test_index_numeric_headings(self):
        self.login_test1()
        d = self.create_doc(initial_par=['# 1', '# 2', '# 3'])
        self.assertEqual(d.document.get_index(default_view_ctx), [({'id': 'section', 'level': 1, 'text': '1'}, []),
                                                                  ({'id': 'section-1', 'level': 1, 'text': '2'}, []),
                                                                  ({'id': 'section-2', 'level': 1, 'text': '3'}, [])])

    def test_index_skip_level(self):
        self.login_test1()
        doc = self.create_doc(initial_par="""
# Heading level 1
Lorem ipsum.

---

### Heading level 3

## Unnumbered {.nonumber}
#-
### Second heading level 3
#-
# Second heading level 1
        """).document
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': 'Heading level 1'},
                           [{'id': 'heading-level-3', 'level': 3, 'text': 'Heading level 3'},
                            {'id': 'unnumbered', 'level': 2, 'text': 'Unnumbered'},
                            {'id': 'second-heading-level-3', 'level': 3, 'text': 'Second heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': 'Second heading level 1'},
                           [])], doc.get_index(default_view_ctx))
        ins_pos = doc.get_paragraphs()[0].get_id()
        doc.set_settings({'auto_number_headings': True})
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': '1. Heading level 1'},
                           [{'id': 'heading-level-3', 'level': 3, 'text': '1.0.1 Heading level 3'},
                            {'id': 'unnumbered', 'level': 2, 'text': 'Unnumbered'},
                            {'id': 'second-heading-level-3', 'level': 3, 'text': '1.0.2 Second heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': '2. Second heading level 1'},
                           [])], doc.get_index(default_view_ctx))
        self.new_par(doc, """# New heading""", ins_pos)
        self.assertEqual([({'id': 'new-heading', 'level': 1, 'text': '1. New heading'}, []),
                          ({'id': 'heading-level-1', 'level': 1, 'text': '2. Heading level 1'},
                           [{'id': 'heading-level-3', 'level': 3, 'text': '2.0.1 Heading level 3'},
                            {'id': 'unnumbered', 'level': 2, 'text': 'Unnumbered'},
                            {'id': 'second-heading-level-3', 'level': 3, 'text': '2.0.2 Second heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': '3. Second heading level 1'},
                           [])], doc.get_index(default_view_ctx))

    def test_index_duplicate_headings(self):
        self.login_test1()
        doc = self.create_doc(initial_par="""
# Same

# Same
        """).document
        self.assertEqual([({'id': 'same', 'level': 1, 'text': 'Same'}, []),
                          ({'id': 'same-1', 'level': 1, 'text': 'Same'}, [])], doc.get_index(default_view_ctx))

        doc = self.create_doc(initial_par="""
# Same
#-
# Same

# Same
#-
# Same
        """).document
        self.assertEqual([({'id': 'same', 'level': 1, 'text': 'Same'}, []),
                          ({'id': 'same-1', 'level': 1, 'text': 'Same'}, []),
                          ({'id': 'same-1-1', 'level': 1, 'text': 'Same'}, []),
                          ({'id': 'same-3', 'level': 1, 'text': 'Same'}, [])], doc.get_index(default_view_ctx))

    def test_heading_preview(self):
        self.login_test1()
        d = self.create_doc(settings={'auto_number_headings': True}, initial_par=['# a', '# b', '# c'])
        self.check_doc_preview(d)

    def test_heading_preview_translation(self):
        self.login_test1()
        orig = self.create_doc(settings={'auto_number_headings': True}, initial_par=['# a', '# b', '# c'])
        d = self.create_translation(orig)
        self.check_doc_preview(d)

    def test_heading_preview_translation_nonumber(self):
        self.login_test1()
        orig = self.create_doc(settings={'auto_number_headings': True},
                               initial_par=['# a', '# b {.nonumber}', '# c'])
        d = self.create_translation(orig)
        pars = d.document.get_par_ids()
        self.get_updated_pars(d)

        e = self.post_preview(d, text='# d\n\n# e\n\n#-\n\n# f', json_key='texts', as_tree=True)
        self.assert_content(e, ['3. d\n4. e', '5. f'])

        e = self.post_preview(d, text='# d\n\n# e\n\n#-\n\n# f', par_next=pars[1],
                              json_key='texts', as_tree=True)
        self.assert_content(e, ['1. d\n2. e', '3. f'])

        e = self.post_preview(d, text='# d\n\n# e\n\n#-\n\n# f', par_next=pars[2],
                              json_key='texts', as_tree=True)
        self.assert_content(e, ['2. d\n3. e', '4. f'])

        e = self.post_preview(d, text='# d\n\n# e\n\n#-\n\n# f', par_next=pars[3],
                              json_key='texts', as_tree=True)
        self.assert_content(e, ['2. d\n3. e', '4. f'])

        orig_par = orig.document.get_paragraphs()[2]
        e = self.post_preview(d, text=f'# x {{r=tr rp={orig_par.get_id()}}}', par=pars[2],
                              json_key='texts', as_tree=True)
        self.assert_content(e, ['x'])

    def check_doc_preview(self, d: DocInfo):
        pars = d.document.get_par_ids()
        self.get_updated_pars(d)

        e = self.post_preview(d, text='# d\n\n# e\n\n#-\n\n# f', par_next=pars[1],
                              json_key='texts', as_tree=True)
        self.assert_content(e, ['1. d\n2. e', '3. f'])

        e = self.post_preview(d, text='# d\n\n# e\n\n#-\n\n# f', par=pars[1], par_next=pars[2],
                              json_key='texts', as_tree=True)
        self.assert_content(e, ['1. d\n2. e', '3. f'])

        e = self.post_preview(d, text='# d\n\n# e\n\n#-\n\n# f', par_next=pars[2],
                              json_key='texts', as_tree=True)
        self.assert_content(e, ['2. d\n3. e', '4. f'])

        e = self.post_preview(d, text='# d\n\n# e\n\n#-\n\n# f', par=pars[2], par_next=pars[3],
                              json_key='texts', as_tree=True)
        self.assert_content(e, ['2. d\n3. e', '4. f'])

        e = self.post_preview(d, text='# d\n\n# e\n\n#-\n\n# f', par_next=pars[3],
                              json_key='texts', as_tree=True)
        self.assert_content(e, ['3. d\n4. e', '5. f'])

        e = self.post_preview(d, text='# d\n\n# e\n\n#-\n\n# f', par=pars[3],
                              json_key='texts', as_tree=True)
        self.assert_content(e, ['3. d\n4. e', '5. f'])

        e = self.post_preview(d, text='# d\n\n# e\n\n#-\n\n# f', json_key='texts', as_tree=True)
        self.assert_content(e, ['4. d\n5. e', '6. f'])

    def test_translation_nonumber_edit(self):
        self.login_test1()
        orig = self.create_doc(settings={'auto_number_headings': True},
                               initial_par=['# a', '# b {.nonumber}', '# c'])
        t = self.create_translation(orig)
        tr_pars = t.document.get_paragraphs()
        md = tr_pars[2].get_exported_markdown().replace('# b', '# tr')
        self.get(t.url)  # refresh cache
        self.post_par(t.document, md, tr_pars[2].get_id())
        e = self.get_updated_pars(t)
        changed = e['changed_pars']
        self.assert_content(html.fromstring(changed[tr_pars[2].get_id()]), ['tr'])
        self.assertNotIn(tr_pars[3].get_id(), changed)
        e = self.get(t.url, as_tree=True)
        self.assert_content(e, ['', '1. a', 'tr', '2. c'])

    def test_too_deep_heading(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
####### a

#-""")
        self.get(d.url)

    def test_formatted_heading(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
# test ([test]{.red}) {#test}
        """)
        self.assertEqual(
            [({'id': 'test-test', 'level': 1, 'text': 'test (test)'}, [])],
            d.document.get_index(default_view_ctx),
        )

    def test_autonumber_link_only_heading(self):
        self.login_test1()
        d = self.create_doc(initial_par='# [Link only](https://example.com)')
        d.document.set_settings({'auto_number_headings': 1})
        r = self.get(d.url, as_tree=True)
        self.assert_content(r, ['', '1. Link only'])
