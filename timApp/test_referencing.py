from timroutetest import TimRouteTest


class ReferencingTest(TimRouteTest):
    def test_reference(self):
        self.login_test1()
        text = 'Par 1'
        doc1 = self.create_doc(initial_par="#- {area=doc1_area}").document
        p1 = doc1.add_paragraph(text)
        p2 = doc1.add_paragraph('Par 2')
        ref_par = doc1.add_paragraph(text='', attrs={'rd': doc1.doc_id, 'rp': p2.get_id()})
        p3 = doc1.add_paragraph('Par 3')
        doc1.add_paragraph(text='', attrs={'area_end': 'doc1_area'})

        doc2 = self.create_doc().document
        doc2.add_paragraph(text='', attrs={'rd': doc1.doc_id, 'rp': p1.get_id()})
        tree = self.get('/view/{}'.format(doc2.doc_id), as_tree=True)
        result = tree.findall(r'.//div[@class="par"]/div[@class="parContent"]/p')
        self.assertEqual(1, len(result))
        self.assertEqual(p1.get_markdown(), result[0].text.strip())

        # Reference to reference
        doc2.add_paragraph(text='', attrs={'rd': doc1.doc_id, 'rp': ref_par.get_id()})
        tree = self.get('/view/{}'.format(doc2.doc_id), as_tree=True)
        result = tree.findall(r'.//div[@class="par"]/div[@class="parContent"]/p')
        self.assertEqual(2, len(result))
        self.assertEqual(p1.get_markdown(), result[0].text.strip())
        self.assertEqual(p2.get_markdown(), result[1].text.strip())

        doc2.add_paragraph(text='', attrs={'rd': doc1.doc_id, 'ra': 'doc1_area'})
        tree = self.get('/view/{}'.format(doc2.doc_id), as_tree=True)
        result = tree.findall(r'.//div[@class="par"]/div[@class="parContent"]/p')
        self.assertEqual(6, len(result))
        self.assertEqual(p1.get_markdown(), result[0].text.strip())  # Reference to normal
        self.assertEqual(p2.get_markdown(), result[1].text.strip())  # Reference to reference
        self.assertEqual(p1.get_markdown(), result[2].text.strip())  # Reference to area, first
        self.assertEqual(p2.get_markdown(), result[3].text.strip())
        self.assertEqual(p2.get_markdown(), result[4].text.strip())
        self.assertEqual(p3.get_markdown(), result[5].text.strip())  # Reference to area, last

    def test_cyclic_reference(self):
        self.login_test1()
        doc1 = self.create_doc().document
        p1 = doc1.add_paragraph('Par')
        p2 = doc1.add_paragraph(text='', attrs={'rd': doc1.doc_id, 'rp': p1.get_id()})
        p3 = doc1.add_paragraph(text='', attrs={'rd': doc1.doc_id, 'rp': p2.get_id()})
        doc1.modify_paragraph(p1.get_id(), '', new_attrs={'rd': doc1.doc_id, 'rp': p3.get_id()})
        tree = self.get('/view/{}'.format(doc1.doc_id), as_tree=True)
        result = tree.findall(r'.//div[@class="par"]/div[@class="parContent"]/div[@class="error"]')
        self.assertEqual(3, len(result))
        self.assertEqual('Infinite referencing loop detected: {}:{} -> {}:{} -> {}:{} -> {}:{}'
                         .format(doc1.doc_id, p1.get_id(),
                                 doc1.doc_id, p3.get_id(),
                                 doc1.doc_id, p2.get_id(),
                                 doc1.doc_id, p1.get_id()),
                         result[0].text)

    def test_cyclic_area_reference(self):
        self.login_test1()
        doc1 = self.create_doc().document
        doc1.add_paragraph('', attrs={'area': 'test'})
        doc1.add_paragraph(text='', attrs={'rd': doc1.doc_id, 'ra': 'test'})
        doc1.add_paragraph(text='', attrs={'area_end': 'test'})
        tree = self.get('/view/{}'.format(doc1.doc_id), as_tree=True)
        result = tree.findall(r'.//div[@class="par"]/div[@class="parContent"]/div[@class="error"]')
        self.assertEqual(1, len(result))

    def test_reference_self(self):
        self.login_test1()
        doc1 = self.create_doc().document
        p1 = doc1.add_paragraph('par')
        doc1.modify_paragraph(p1.get_id(), '', new_attrs={'rd': doc1.doc_id, 'rp': p1.get_id()})
        tree = self.get('/view/{}'.format(doc1.doc_id), as_tree=True)
        result = tree.findall(r'.//div[@class="par"]/div[@class="parContent"]/div[@class="error"]')
        self.assertEqual(1, len(result))
