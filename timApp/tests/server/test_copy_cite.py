from tests.server.timroutetest import TimRouteTest


class CopyCiteTest(TimRouteTest):
    def test_copy(self):
        self.login_test1()
        d = self.create_doc(from_file='example_docs/multiple_mmcqs.md')
        d2 = self.create_doc(copy_from=d.id)
        self.assertEqual(d.document.export_markdown(), d2.document.export_markdown())
        self.login_test2()
        self.create_doc(copy_from=d.id, expect_status=403)

    def test_cite(self):
        self.login_test1()
        d = self.create_doc(from_file='example_docs/multiple_mmcqs.md')
        d2 = self.create_doc(cite=d.id)
        self.assertEqual({'source_document': d.id, 'macros': {}}, d2.document.get_settings().get_dict())
        d2_pars = d2.document.get_paragraphs()[1:]
        self.assertListEqual([p.get_id() for p in d.document.get_paragraphs()], [p.get_attr('rp') for p in d2_pars])
        self.assertTrue(all(p.get_attr('r') == 'c' for p in d2_pars))
        self.login_test2()
        self.create_doc(cite=d.id, expect_status=403)
