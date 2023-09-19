"""Unit tests for testing paragraph referencing."""

from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document
from timApp.document.documentparser import DocumentParser
from timApp.document.viewcontext import default_view_ctx
from timApp.tests.db.timdbtest import TimDbTest
from timApp.timdb.exceptions import TimDbException


def add_ref_paragraph(
    doc: Document,
    src_par: DocParagraph,
    text: str | None = None,
    attrs: dict | None = None,
) -> DocParagraph:
    ref_attrs = {} if attrs is None else attrs.copy()
    ref_attrs["rp"] = src_par.get_id()
    ref_attrs["rt"] = src_par.get_hash()

    rd = src_par.get_doc_id()
    if doc.get_settings().get_source_document() != rd:
        ref_attrs["rd"] = str(rd)
    if text is not None:
        ref_attrs["r"] = "tr"
    else:
        text = ""

    return doc.add_paragraph(text, attrs=ref_attrs)


def add_area_ref_paragraph(
    doc: Document,
    src_doc: "Document",
    src_area_name: str,
    text: str | None = None,
    attrs: dict | None = None,
) -> DocParagraph:
    ref_attrs = {} if attrs is None else attrs.copy()
    ref_attrs["ra"] = src_area_name
    ref_attrs.pop("rt", None)

    if doc.get_settings().get_source_document() != src_doc.doc_id:
        ref_attrs["rd"] = str(src_doc.doc_id)
    if text is not None:
        ref_attrs["r"] = "tr"
    else:
        text = ""

    return doc.add_paragraph(text, attrs=ref_attrs)


class RefTest(TimDbTest):
    def dict_merge(self, a, b):
        c = a.copy()
        c.update(b)
        return c

    def dict_issubset(self, a, b):
        return set(a.items()).issubset(set(b.items()))

    def assert_dict_issubset(self, a, b):
        self.assertTrue(self.dict_issubset(a, b), f"{a} is not a subset of {b}")

    def setUp(self):
        super().setUp()
        self.init_testdb()

    def init_testdb(self):
        self.src_doc = self.create_doc().document
        self.ref_doc = self.create_doc().document

        self.src_par = self.src_doc.add_paragraph("testpar", attrs={"a": "1", "b": "2"})
        self.assertEqual(
            self.src_par.get_id(), self.src_doc.get_paragraphs()[0].get_id()
        )

    def test_simpleref(self):
        ref_par = add_ref_paragraph(self.ref_doc, self.src_par)
        self.assertEqual(1, len(self.ref_doc.get_paragraphs()))
        self.assertEqual(ref_par.get_id(), self.ref_doc.get_paragraphs()[0].get_id())
        self.assertEqual("", ref_par.get_markdown())

        rendered_pars = ref_par.get_referenced_pars()
        self.assertEqual(1, len(rendered_pars))
        self.assertEqual(self.src_par.get_id(), rendered_pars[0].get_id())
        self.assertEqual(self.src_par.get_markdown(), rendered_pars[0].get_markdown())
        self.assertEqual(
            self.src_par.get_html(default_view_ctx),
            rendered_pars[0].get_html(default_view_ctx),
        )
        self.assertEqual(self.src_par.get_attrs(), rendered_pars[0].get_attrs())

    def test_translation(self):
        ref_attrs = {"foo": "fffoooo", "bar": "baaaa"}
        ref_par = add_ref_paragraph(
            self.ref_doc, self.src_par, "translation", attrs=ref_attrs
        )
        self.assertEqual(1, len(self.ref_doc.get_paragraphs()))
        self.assertEqual(ref_par.get_id(), self.ref_doc.get_paragraphs()[0].get_id())
        self.assertEqual("translation", ref_par.get_markdown())

        rendered_pars = ref_par.get_referenced_pars()
        self.assertEqual(1, len(rendered_pars))
        self.assertEqual(self.src_par.get_id(), rendered_pars[0].get_id())
        self.assertEqual(ref_par.get_markdown(), rendered_pars[0].get_markdown())
        self.assertEqual(
            ref_par.get_html(default_view_ctx),
            rendered_pars[0].get_html(default_view_ctx),
        )
        self.assertEqual(
            self.dict_merge(self.src_par.get_attrs(), ref_attrs),
            rendered_pars[0].get_attrs(),
        )

    def test_circular(self):
        ref_par = add_ref_paragraph(self.ref_doc, self.src_par)
        self.assertEqual(1, len(self.ref_doc.get_paragraphs()))
        self.assertEqual(ref_par.get_id(), self.ref_doc.get_paragraphs()[0].get_id())
        self.assertEqual("", ref_par.get_markdown())

        self.src_par.set_attr("rd", str(self.ref_doc.doc_id))
        self.src_par.set_attr("rp", ref_par.get_id())
        self.src_doc.modify_paragraph_obj(self.src_par.get_id(), self.src_par)

        self.ref_doc.clear_mem_cache()
        ref_par.ref_pars = {}
        self.assertRaises(TimDbException, ref_par.get_referenced_pars)
        self.assertRaises(TimDbException, self.src_par.get_referenced_pars)

    def test_transitive(self):
        # Reference to the original paragraph
        ref_par1 = add_ref_paragraph(self.ref_doc, self.src_par)
        self.assertEqual(1, len(self.ref_doc.get_paragraphs()))
        self.assertEqual(ref_par1.get_id(), self.ref_doc.get_paragraphs()[0].get_id())
        self.assertEqual("", ref_par1.get_markdown())

        # Reference to the reference above
        ref_doc2 = self.create_doc().document
        ref_par2 = add_ref_paragraph(ref_doc2, ref_par1)
        self.assertEqual(1, len(ref_doc2.get_paragraphs()))
        self.assertEqual(ref_par2.get_id(), ref_doc2.get_paragraphs()[0].get_id())
        self.assertEqual("", ref_par2.get_markdown())

        # Render the reference to the reference
        rendered_pars = ref_par2.get_referenced_pars()
        self.assertEqual(1, len(rendered_pars))
        self.assertEqual(self.src_par.get_id(), rendered_pars[0].get_id())
        self.assertEqual(self.src_par.get_markdown(), rendered_pars[0].get_markdown())
        self.assertEqual(
            self.src_par.get_html(default_view_ctx),
            rendered_pars[0].get_html(default_view_ctx),
        )
        self.assertEqual(self.src_par.get_attrs(), rendered_pars[0].get_attrs())

        # Declare some new attributes
        ref_par1.set_attr("foo", "fffooo")
        ref_par2.set_attr("bar", "baaaa")
        self.ref_doc.modify_paragraph_obj(ref_par1.get_id(), ref_par1)
        ref_doc2.modify_paragraph_obj(ref_par2.get_id(), ref_par2)

        expected_attrs = self.src_par.get_attrs()
        expected_attrs["foo"] = "fffooo"
        expected_attrs["bar"] = "baaaa"

        rendered_pars = ref_par2.get_referenced_pars()
        self.assert_dict_issubset(expected_attrs, rendered_pars[0].get_attrs())

    def test_editparagraph_cite(self):
        src_md = self.src_par.get_exported_markdown()
        self.assertRegex(src_md, '^#- *\\{([ab]="[21]" ?){2}\\}\ntestpar\n$')

        ref_par = add_ref_paragraph(self.ref_doc, self.src_par)
        self.assertEqual(1, len(self.ref_doc.get_paragraphs()))
        self.assertEqual(ref_par.get_id(), self.ref_doc.get_paragraphs()[0].get_id())
        self.assertEqual("", ref_par.get_markdown())

        ref_md = ref_par.get_exported_markdown()
        dp = DocumentParser(ref_md)
        dp.validate_structure().raise_if_has_any_issues()
        ref_blocks = [
            DocParagraph.create(doc=ref_par.doc, md=par["md"], attrs=par.get("attrs"))
            for par in dp.get_blocks()
        ]

        self.assertEqual(1, len(ref_blocks))
        self.assertEqual("", ref_blocks[0].get_markdown())
        self.assertEqual(str(self.src_doc.doc_id), str(ref_blocks[0].get_attr("rd")))
        self.assertEqual(self.src_par.get_id(), ref_blocks[0].get_attr("rp"))
        self.assertEqual(self.src_par.get_hash(), ref_blocks[0].get_attr("rt"))

    def test_editparagraph_citearea(self):
        areastart_par = self.src_doc.add_paragraph("", attrs={"area": "testarea"})
        area_par1 = self.src_doc.add_paragraph("Testarea par 1", attrs={"x": 1, "y": 2})
        area_par2 = self.src_doc.add_paragraph("Testarea par 2", attrs={"a": 3, "b": 4})
        areaend_par = self.src_doc.add_paragraph("", attrs={"area_end": "testarea"})

        areastart_md = areastart_par.get_exported_markdown()
        areapar1_md = area_par1.get_exported_markdown()
        areapar2_md = area_par2.get_exported_markdown()
        areaend_md = areaend_par.get_exported_markdown()

        self.assertRegex(areastart_md, '^#- *\\{area="testarea" ?\\}\n+$')
        self.assertRegex(
            areapar1_md, '^#- *\\{([xy]="[12]" ?){2}\\}\nTestarea par 1\n$'
        )
        self.assertRegex(
            areapar2_md, '^#- *\\{([ab]="[34]" ?){2}\\}\nTestarea par 2\n$'
        )
        self.assertRegex(areaend_md, '^#- *\\{area_end="testarea" ?\\}\n+$')

        ref_par = add_area_ref_paragraph(self.ref_doc, self.src_doc, "testarea")
        ref_md = ref_par.get_exported_markdown()

        src_docid = str(self.src_doc.doc_id)
        self.assertRegex(
            ref_md, '^#- *\\{(((ra="testarea")|(rd="' + src_docid + '")) ?){2}\\}\n+$'
        )

        # todo: test the contents of the rendered area

    def test_editparagraph_translate(self):
        src_md = self.src_par.get_exported_markdown()
        self.assertRegex(src_md, '^#- *\\{([ab]="[21]" ?){2}\\}\ntestpar\n$')

        empty_refpar = add_ref_paragraph(self.ref_doc, self.src_par, "")
        self.assertEqual(1, len(self.ref_doc.get_paragraphs()))
        self.assertEqual(
            empty_refpar.get_id(), self.ref_doc.get_paragraphs()[0].get_id()
        )
        self.assertEqual("", empty_refpar.get_markdown())

        ref_md = empty_refpar.get_exported_markdown()
        dp = DocumentParser(ref_md)
        dp.validate_structure().raise_if_has_any_issues()
        ref_blocks = [
            DocParagraph.create(
                doc=empty_refpar.doc, md=par["md"], attrs=par.get("attrs")
            )
            for par in dp.get_blocks()
        ]

        self.assertEqual(1, len(ref_blocks))
        self.assertEqual(self.src_par.get_markdown(), ref_blocks[0].get_markdown())
        self.assertEqual(str(self.src_doc.doc_id), str(ref_blocks[0].get_attr("rd")))
        self.assertEqual(self.src_par.get_id(), ref_blocks[0].get_attr("rp"))
        self.assertEqual(self.src_par.get_hash(), ref_blocks[0].get_attr("rt"))
        self.assertEqual("tr", ref_blocks[0].get_attr("r"))

        ref_attrs = {"foo": "fffoooo", "bar": "baaaa"}
        ref_par = add_ref_paragraph(
            self.ref_doc, self.src_par, "translation", attrs=ref_attrs
        )
        self.assertEqual(2, len(self.ref_doc.get_paragraphs()))
        self.assertEqual(ref_par.get_id(), self.ref_doc.get_paragraphs()[1].get_id())
        self.assertEqual("translation", ref_par.get_markdown())

        ref_md = ref_par.get_exported_markdown()
        dp = DocumentParser(ref_md)
        dp.validate_structure().raise_if_has_any_issues()
        ref_blocks = [
            DocParagraph.create(doc=ref_par.doc, md=par["md"], attrs=par.get("attrs"))
            for par in dp.get_blocks()
        ]

        self.assertEqual(1, len(ref_blocks))
        self.assertEqual(ref_par.get_markdown(), ref_blocks[0].get_markdown())
        self.assertEqual(str(self.src_doc.doc_id), str(ref_blocks[0].get_attr("rd")))
        self.assertEqual(self.src_par.get_id(), ref_blocks[0].get_attr("rp"))
        self.assertEqual(self.src_par.get_hash(), ref_blocks[0].get_attr("rt"))
        self.assertEqual("tr", ref_blocks[0].get_attr("r"))

    def test_editparagraph_translatearea(self):
        areastart_par = self.src_doc.add_paragraph("", attrs={"area": "testarea"})
        area_par1 = self.src_doc.add_paragraph("Testarea par 1", attrs={"x": 1, "y": 2})
        area_par2 = self.src_doc.add_paragraph("Testarea par 2", attrs={"a": 3, "b": 4})
        areaend_par = self.src_doc.add_paragraph("", attrs={"area_end": "testarea"})

        areastart_md = areastart_par.get_exported_markdown()
        areapar1_md = area_par1.get_exported_markdown()
        areapar2_md = area_par2.get_exported_markdown()
        areaend_md = areaend_par.get_exported_markdown()

        self.assertRegex(areastart_md, '^#- *\\{area="testarea" ?\\}\n+$')
        self.assertRegex(
            areapar1_md, '^#- *\\{([xy]="[12]" ?){2}\\}\nTestarea par 1\n$'
        )
        self.assertRegex(
            areapar2_md, '^#- *\\{([ab]="[34]" ?){2}\\}\nTestarea par 2\n$'
        )
        self.assertRegex(areaend_md, '^#- *\\{area_end="testarea" ?\\}\n+$')

        ref_par = add_area_ref_paragraph(
            self.ref_doc, self.src_doc, "testarea", "translation"
        )
        ref_md = ref_par.get_exported_markdown()

        src_docid = str(self.src_doc.doc_id)
        self.assertRegex(
            ref_md,
            '^#- *\\{(((ra="testarea")|(rd="'
            + src_docid
            + '")|(r="tr")) ?){3}\\}\ntranslation\n+$',
        )

        ref_par = self.ref_doc.modify_paragraph(ref_par.get_id(), "")
        ref_md = ref_par.get_exported_markdown()
        self.assertRegex(
            ref_md,
            '^#- *\\{(((ra="testarea")|(rd="' + src_docid + '")|(r="tr")) ?){3}\\}\n+$',
        )

        # todo: test the contents of the rendered area

    def test_settings_not_inherited(self):
        """Settings attribute is not inherited from a referring paragraph."""
        ref = self.src_par.create_reference(self.ref_doc)
        ref.set_attr("settings", "")
        self.ref_doc.add_paragraph_obj(ref)
        deref = ref.get_referenced_pars()[0]
        self.assertNotIn("settings", deref.get_attrs())

        self.src_par.set_attr("settings", "")
        self.src_par.set_markdown("")
        self.src_par.save()
        ref.ref_pars = {}
        ref.doc.clear_mem_cache()
        deref = ref.get_referenced_pars()[0]
        self.assertIn("settings", deref.get_attrs())

    def test_reference_classes(self):
        """Classes of reference and source paragraphs are merged."""
        ref = self.src_par.create_reference(self.ref_doc)
        ref.add_class("red", "green")
        self.src_par.add_class("blue", "white", "orange")
        self.src_par.save()
        deref = ref.get_referenced_pars()[0]
        ref.ref_pars = {}
        self.assertEqual(
            {"blue", "white", "orange", "red", "green"}, set(deref.classes)
        )
        self.assertEqual(5, len(deref.classes))

        self.src_par.classes = None
        self.src_par.save()
        ref.doc.clear_mem_cache()
        deref = ref.get_referenced_pars()[0]
        self.assertEqual({"red", "green"}, set(deref.classes))
        self.assertEqual(2, len(deref.classes))
        ref.ref_pars = {}

        self.src_par.add_class("blue", "white", "orange")
        ref.classes = None
        self.src_par.save()
        ref.doc.clear_mem_cache()
        deref = ref.get_referenced_pars()[0]
        self.assertEqual({"blue", "white", "orange"}, set(deref.classes))
        self.assertEqual(3, len(deref.classes))
