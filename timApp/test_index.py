from timroutetest import TimRouteTest


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
        """)
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': 'Heading level 1'},
                           [{'id': 'heading-level-2', 'level': 2, 'text': 'Heading level 2'},
                            {'id': 'heading-level-3', 'level': 3, 'text': 'Heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': 'Second heading level 1'},
                           [])], doc.get_index())
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
        """)
        doc.set_settings({'auto_number_headings': True, 'heading_format': {2: "{h1}.{h2}. {text}"}})
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': '1. Heading level 1'}, []),
                          ({'id': 'unnumbered', 'level': 1, 'text': 'Unnumbered'},
                           [{'id': 'heading-level-2', 'level': 2, 'text': '1.1. Heading level 2'},
                            {'id': 'second-heading-level-2', 'level': 2, 'text': '1.2. Second heading level 2'},
                            {'id': 'heading-level-3', 'level': 3, 'text': '1.2.1 Heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': '2. Second heading level 1'},
                           [])], doc.get_index())

        doc.set_settings({'auto_number_headings': False})
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': 'Heading level 1'}, []),
                          ({'id': 'unnumbered', 'level': 1, 'text': 'Unnumbered'},
                           [{'id': 'heading-level-2', 'level': 2, 'text': 'Heading level 2'},
                            {'id': 'second-heading-level-2', 'level': 2, 'text': 'Second heading level 2'},
                            {'id': 'heading-level-3', 'level': 3, 'text': 'Heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': 'Second heading level 1'},
                           [])], doc.get_index())

        doc.set_settings({'auto_number_headings': True,
                          'heading_format': {2: '{', 3: '{', 4: '{', 5: '{', 6: '{'}})
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': '1. Heading level 1'}, []),
                          ({'id': 'unnumbered', 'level': 1, 'text': 'Unnumbered'},
                           [{'id': 'heading-level-2', 'level': 2, 'text': '[ERROR] Heading level 2'},
                            {'id': 'second-heading-level-2', 'level': 2, 'text': '[ERROR] Second heading level 2'},
                            {'id': 'heading-level-3', 'level': 3, 'text': '[ERROR] Heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': '2. Second heading level 1'},
                           [])], doc.get_index())

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
        """)
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': 'Heading level 1'},
                           [{'id': 'heading-level-2', 'level': 2, 'text': 'Heading level 2'},
                            {'id': 'unnumbered', 'level': 2, 'text': 'Unnumbered'},
                            {'id': 'heading-level-3', 'level': 3, 'text': 'Heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': 'Second heading level 1'},
                           [])], doc.get_index())

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
        """)
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': 'Heading level 1'},
                           [{'id': 'heading-level-3', 'level': 3, 'text': 'Heading level 3'},
                            {'id': 'unnumbered', 'level': 2, 'text': 'Unnumbered'},
                            {'id': 'second-heading-level-3', 'level': 3, 'text': 'Second heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': 'Second heading level 1'},
                           [])], doc.get_index())
        ins_pos = doc.get_paragraphs()[0].get_id()
        doc.set_settings({'auto_number_headings': True})
        self.assertEqual([({'id': 'heading-level-1', 'level': 1, 'text': '1. Heading level 1'},
                           [{'id': 'heading-level-3', 'level': 3, 'text': '1.0.1 Heading level 3'},
                            {'id': 'unnumbered', 'level': 2, 'text': 'Unnumbered'},
                            {'id': 'second-heading-level-3', 'level': 3, 'text': '1.0.2 Second heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': '2. Second heading level 1'},
                           [])], doc.get_index())
        self.new_par(doc, """# New heading""", ins_pos)
        self.assertEqual([({'id': 'new-heading', 'level': 1, 'text': '1. New heading'}, []),
                          ({'id': 'heading-level-1', 'level': 1, 'text': '2. Heading level 1'},
                           [{'id': 'heading-level-3', 'level': 3, 'text': '2.0.1 Heading level 3'},
                            {'id': 'unnumbered', 'level': 2, 'text': 'Unnumbered'},
                            {'id': 'second-heading-level-3', 'level': 3, 'text': '2.0.2 Second heading level 3'}]),
                          ({'id': 'second-heading-level-1', 'level': 1, 'text': '3. Second heading level 1'},
                           [])], doc.get_index())

    def test_index_duplicate_headings(self):
        self.login_test1()
        doc = self.create_doc(initial_par="""
# Same

# Same
        """)
        self.assertEqual([({'id': 'same', 'level': 1, 'text': 'Same'}, []),
                          ({'id': 'same-1', 'level': 1, 'text': 'Same'}, [])], doc.get_index())

        doc = self.create_doc(initial_par="""
# Same
#-
# Same

# Same
#-
# Same
        """)
        self.assertEqual([({'id': 'same', 'level': 1, 'text': 'Same'}, []),
                          ({'id': 'same-1', 'level': 1, 'text': 'Same'}, []),
                          ({'id': 'same-1-1', 'level': 1, 'text': 'Same'}, []),
                          ({'id': 'same-2', 'level': 1, 'text': 'Same'}, [])], doc.get_index())
