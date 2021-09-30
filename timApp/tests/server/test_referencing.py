from timApp.tests.server.timroutetest import TimRouteTest


class ReferencingTest(TimRouteTest):
    def test_reference(self):
        self.login_test1()
        text = "Par 1"
        doc1 = self.create_doc(initial_par="#- {area=doc1_area}").document
        p1 = doc1.add_paragraph(text)
        p2 = doc1.add_paragraph("Par 2")
        ref_par = doc1.add_paragraph(
            text="", attrs={"rd": doc1.doc_id, "rp": p2.get_id()}
        )
        p3 = doc1.add_paragraph("Par 3")
        doc1.add_paragraph(text="", attrs={"area_end": "doc1_area"})

        doc2 = self.create_doc().document
        doc2.add_paragraph(text="", attrs={"rd": doc1.doc_id, "rp": p1.get_id()})
        tree = self.get(f"/view/{doc2.doc_id}", as_tree=True)
        result = tree.findall(r'.//div[@class="par"]/div[@class="parContent"]/p')
        self.assertEqual(1, len(result))
        self.assertEqual(p1.get_markdown(), result[0].text.strip())

        # Reference to reference
        doc2.add_paragraph(text="", attrs={"rd": doc1.doc_id, "rp": ref_par.get_id()})
        tree = self.get(f"/view/{doc2.doc_id}", as_tree=True)
        result = tree.findall(r'.//div[@class="par"]/div[@class="parContent"]/p')
        self.assertEqual(2, len(result))
        self.assertEqual(p1.get_markdown(), result[0].text.strip())
        self.assertEqual(p2.get_markdown(), result[1].text.strip())

        doc2.add_paragraph(text="", attrs={"rd": doc1.doc_id, "ra": "doc1_area"})
        tree = self.get(f"/view/{doc2.doc_id}", as_tree=True)
        result = tree.findall(r'.//div[@class="par"]/div[@class="parContent"]/p')
        self.assertEqual(6, len(result))
        self.assertEqual(
            p1.get_markdown(), result[0].text.strip()
        )  # Reference to normal
        self.assertEqual(
            p2.get_markdown(), result[1].text.strip()
        )  # Reference to reference
        self.assertEqual(
            p1.get_markdown(), result[2].text.strip()
        )  # Reference to area, first
        self.assertEqual(p2.get_markdown(), result[3].text.strip())
        self.assertEqual(p2.get_markdown(), result[4].text.strip())
        self.assertEqual(
            p3.get_markdown(), result[5].text.strip()
        )  # Reference to area, last

    def test_cyclic_reference(self):
        self.login_test1()
        doc1 = self.create_doc().document
        p1 = doc1.add_paragraph("Par")
        p2 = doc1.add_paragraph(text="", attrs={"rd": doc1.doc_id, "rp": p1.get_id()})
        p3 = doc1.add_paragraph(text="", attrs={"rd": doc1.doc_id, "rp": p2.get_id()})
        doc1.modify_paragraph(
            p1.get_id(), "", new_attrs={"rd": doc1.doc_id, "rp": p3.get_id()}
        )
        tree = self.get(f"/view/{doc1.doc_id}", as_tree=True)
        result = tree.cssselect(".parContent > span.error")
        self.assertEqual(3, len(result))
        self.assertEqual(
            f"Infinite referencing loop detected: {doc1.doc_id}:{p1.get_id()} -> {doc1.doc_id}:{p3.get_id()} -> {doc1.doc_id}:{p2.get_id()} -> {doc1.doc_id}:{p1.get_id()}",
            result[0].text,
        )

    def test_cyclic_area_reference(self):
        self.login_test1()
        doc1 = self.create_doc().document
        doc1.add_paragraph("", attrs={"area": "test"})
        doc1.add_paragraph(text="", attrs={"rd": doc1.doc_id, "ra": "test"})
        doc1.add_paragraph(text="", attrs={"area_end": "test"})
        tree = self.get(f"/view/{doc1.doc_id}", as_tree=True)
        self.assertTrue(tree.cssselect(".parContent > span.error"))

    def test_reference_self(self):
        self.login_test1()
        doc1 = self.create_doc().document
        p1 = doc1.add_paragraph("par")
        doc1.modify_paragraph(
            p1.get_id(), "", new_attrs={"rd": doc1.doc_id, "rp": p1.get_id()}
        )
        tree = self.get(f"/view/{doc1.doc_id}", as_tree=True)
        self.assertTrue(tree.cssselect(".parContent > span.error"))

    def test_invalid_reference_translation(self):
        self.login_test1()
        d = self.create_doc(initial_par="""#- {rd=9999 rp=xxxx}""")
        t = self.create_translation(d)
        e = self.get(t.url, as_tree=True)
        self.assert_content(e, ["The referenced document does not exist."])
        t.document.add_paragraph("new")
        invalid_par = t.document.add_text("#- {rd=9999 rp=xxxx}")[0]
        t.document.add_paragraph("new")
        t.document.modify_paragraph_obj(invalid_par.get_id(), invalid_par)
        e = self.get(t.url, as_tree=True)
        self.assert_content(
            e,
            [
                "The referenced document does not exist.",
                "new",
                "The referenced document does not exist.",
                "new",
            ],
        )

    def test_visible_attribute_reference(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
This is visible.

#- {visible=false}
This should not be visible.
        """
        )
        r = self.get(d.url, as_tree=True)
        self.assert_content(r, ["This is visible."])
        d2 = self.create_doc(initial_par="This is first of d2.")
        invis_par = d.document.get_paragraphs()[1]
        d2.document.add_paragraph_obj(invis_par.create_reference(d2.document))
        r = self.get(d2.url, as_tree=True)
        self.assert_content(r, ["This is first of d2."])
