from timApp.document.docparagraph import DocParagraph
from timApp.document.docsettings import DocSettings
from timApp.document.yamlblock import YamlBlock
from timApp.tests.db.timdbtest import TimDbTest


class SettingsTest(TimDbTest):
    def test_settings_reference(self):
        d = self.create_doc()
        d.document.set_settings({"a": "b"})
        par = d.document.get_paragraphs()[0]
        d2 = self.create_doc()
        d2.document.add_paragraph_obj(make_settings(par.create_reference(d2.document)))
        self.assertEqual(
            d.document.get_settings().get_dict(), d2.document.get_settings().get_dict()
        )

    def test_settings_multiple(self):
        d = self.create_doc()
        doc = d.document
        doc.set_settings({"a": "b"})
        pars = doc.get_paragraphs()
        self.assertTrue(pars[0].is_setting())
        doc.set_settings({"a": "c"})
        pars = doc.get_paragraphs()
        self.assertTrue(pars[0].is_setting())
        self.assertEqual(len(pars), 1)
        self.assertEqual(doc.get_settings().get_dict(), {"a": "c"})

        doc.set_settings({"b": "d"}, force_new_par=True)
        pars = doc.get_paragraphs()
        self.assertTrue(pars[0].is_setting())
        self.assertTrue(pars[1].is_setting())
        self.assertEqual(DocSettings.from_paragraph(pars[0]).get_dict(), {"a": "c"})
        self.assertEqual(DocSettings.from_paragraph(pars[1]).get_dict(), {"b": "d"})
        self.assertEqual(len(pars), 2)
        self.assertEqual(doc.get_settings().get_dict(), {"a": "c", "b": "d"})

        doc.insert_paragraph("hello", insert_after_id=pars[0].get_id())
        self.assertEqual(doc.get_settings().get_dict(), {"a": "c"})

    def test_settings_multiple_reference(self):
        doc = self.create_doc().document
        doc2 = self.create_doc().document

        p1 = doc2.add_paragraph_obj(
            DocSettings(doc2, YamlBlock(values={"a": "1"})).to_paragraph()
        )
        p2 = doc2.add_paragraph_obj(
            DocSettings(doc2, YamlBlock(values={"b": "2"})).to_paragraph()
        )
        doc.add_paragraph_obj(make_settings(p1.create_reference(doc)))
        doc.add_paragraph_obj(make_settings(p2.create_reference(doc)))
        self.assertEqual(doc.get_settings().get_dict(), {"a": "1", "b": "2"})
        self.assertEqual(doc.get_settings().get_dict(), doc2.get_settings().get_dict())

        doc2.insert_paragraph("hello", insert_before_id=p1.get_id())
        doc.clear_mem_cache()
        self.assertEqual(doc.get_settings().get_dict(), {"a": "1", "b": "2"})
        self.assertEqual(doc2.get_settings().get_dict(), {})

    def test_settings_area_reference(self):
        doc = self.create_doc().document
        doc2 = self.create_doc(
            initial_par="""
#-
first

#- {area=a settings=""}
a: 1
b: 2

#- {settings=""}
c: 3
d: 4

#- {area_end=a}
"""
        ).document
        ref = DocParagraph.create_area_reference(doc, area_name="a", rd=doc2.doc_id)
        doc.add_paragraph_obj(make_settings(ref))
        doc.add_paragraph_obj(
            DocSettings(doc, YamlBlock(values={"e": 5, "a": 0})).to_paragraph()
        )
        self.assertEqual(len(list(doc.get_settings_pars())), 2)
        self.assertEqual(
            {"a": 0, "b": 2, "c": 3, "d": 4, "e": 5}, doc.get_settings().get_dict()
        )
        self.assertEqual(doc2.get_settings().get_dict(), {})

    def test_settings_invalid_area_reference(self):
        doc = self.create_doc().document
        doc2 = self.create_doc(
            initial_par="""
#-
first

#- {area=a}
a: 1
b: 2

#- {settings=""}
c: 3
d: 4

#- {area_end=a}
"""
        ).document
        ref = DocParagraph.create_area_reference(doc, area_name="a", rd=doc2.doc_id)
        doc.add_paragraph_obj(make_settings(ref))
        doc.add_paragraph_obj(
            DocSettings(doc, YamlBlock(values={"e": 5, "a": 0})).to_paragraph()
        )
        self.assertEqual(len(list(doc.get_settings_pars())), 2)
        self.assertEqual(doc.get_settings().get_dict(), {})
        self.assertEqual(doc2.get_settings().get_dict(), {})

    def test_invalid_settings(self):
        doc = self.create_doc(initial_par="""#- {settings=""}\nx""").document
        self.assertEqual(doc.get_settings().get_dict(), {})


def make_settings(p: DocParagraph):
    p.set_attr("settings", "")
    return p
