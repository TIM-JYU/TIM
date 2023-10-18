from timApp.markdown.markdownconverter import GetDocumentSettingMacro
from timApp.tests.db.timdbtest import TimDbTest
from timApp.timdb.sqa import db


class DocumentTest(TimDbTest):
    def test_docsetting_macrofilter_inaccessible(self):
        doc = self.create_doc().document
        doc.set_settings({"test": "value"})
        db.session.commit()

        m = GetDocumentSettingMacro(doc)

        with self.assertRaises(Exception):
            m.get_document_setting("test")

        doc.add_setting("allowedDocsettingMacroAttributes", "test")
        db.session.commit()

        m = GetDocumentSettingMacro(doc)
        self.assertEqual(m.get_document_setting("test"), "value")

    def test_docsetting_macrofilter_basic(self):
        doc = self.create_doc().document
        settings = {
            "test": "value",
            "test2": {"subtest": "subvalue"},
            "test3": ["a", "b", "c"],
            "test4": [
                {
                    "subtest2": "subvalue2",
                }
            ],
            "allowedDocsettingMacroAttributes": ".*",
        }
        doc.set_settings(settings)
        db.session.commit()

        m = GetDocumentSettingMacro(doc)

        self.assertEqual(m.get_document_setting("test"), "value")

        self.assertEqual(m.get_document_setting("test2.subtest"), "subvalue")
        self.assertEqual(m.get_document_setting("test2.subtest_err"), None)
        self.assertEqual(m.get_document_setting("test2.subtest.invalid"), None)

        self.assertEqual(m.get_document_setting("test3.1"), "b")
        self.assertEqual(m.get_document_setting("test3.3"), None)

        self.assertEqual(m.get_document_setting("test4.0"), {"subtest2": "subvalue2"})
        self.assertEqual(m.get_document_setting("test4.0.subtest2"), "subvalue2")

        self.assertEqual(m.get_document_setting(""), settings)

    def test_docsetting_multiple_documents(self):
        doc1 = self.create_doc().document
        doc1.set_settings(
            {"testdoc1": "value", "allowedDocsettingMacroAttributes": ".*"}
        )
        doc2 = self.create_doc().document
        doc2.set_settings(
            {
                "testdoc2_1": "value",
                "testdoc2_2": "value",
                "allowedDocsettingMacroAttributes": "testdoc2_1",
            }
        )
        db.session.commit()

        m = GetDocumentSettingMacro(doc1)

        self.assertEqual(m.get_document_setting("testdoc1"), "value")
        self.assertEqual(m.get_document_setting(f"{doc1.id}.testdoc1"), "value")
        # Cannot access doc2's settings without explicit doc_id
        self.assertEqual(m.get_document_setting("testdoc2_1"), None)
        self.assertEqual(m.get_document_setting(f"{doc2.id}.testdoc2_1"), "value")

        with self.assertRaises(Exception):
            m.get_document_setting(f"{doc2.doc_id}.testdoc2_2")
