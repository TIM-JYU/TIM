from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder
from timApp.item.block import BlockType
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.userutils import get_default_right_document, grant_access
from timApp.util.utils import static_tim_doc


class CopyCiteTest(TimRouteTest):
    def test_copy(self):
        self.login_test1()
        d = self.create_doc(from_file=static_tim_doc("multiple_mmcqs.md"))
        d2 = self.create_doc(copy_from=d.id)
        self.assertEqual(d.document.export_markdown(), d2.document.export_markdown())
        self.login_test2()
        self.create_doc(copy_from=d.id, expect_status=403)

    def test_cite(self):
        self.login_test1()
        d = self.create_doc(from_file=static_tim_doc("multiple_mmcqs.md"))
        d.document.set_settings({"test": "hi"})
        d2 = self.create_doc(cite=d.id)
        self.assertEqual(
            {"source_document": d.id, "test": "hi"},
            d2.document.get_settings().get_dict(),
        )
        d2_pars = d2.document.get_paragraphs()
        self.assertListEqual(
            [p.get_id() for p in d.document.get_paragraphs()],
            [p.get_attr("rp") for p in d2_pars],
        )
        self.assertTrue(all(p.get_attr("r") == "c" for p in d2_pars))
        self.get(d2.url)
        self.login_test2()
        self.create_doc(cite=d.id, expect_status=403)

    def test_copy_translations(self):
        """Translations of a copied document are also copied."""
        self.login_test1()
        finnish_par = "suomalainen kappale"
        d = self.create_doc(initial_par=finnish_par)
        d.lang_id = "fi"
        d.title = "suomeksi"
        db.session.commit()
        tr_en = self.create_translation(d, "In English", "en")
        english_par = "an English paragraph"
        tr_en.document.modify_paragraph(
            tr_en.document.get_paragraphs()[0].get_id(), english_par
        )
        tr_de = self.create_translation(d, "Auf Deutsch", "de")
        german_par = "ein deutscher Absatz"
        tr_de.document.modify_paragraph(
            tr_de.document.get_paragraphs()[0].get_id(), german_par
        )
        copy = self.create_doc(copy_from=d.id)

        orig_trs = d.translations
        orig_trs.sort(key=lambda tr: tr.lang_id)
        orig_trs.sort(key=lambda tr: tr.is_original_translation)

        copy_trs = copy.translations
        copy_trs.sort(key=lambda tr: tr.lang_id)
        copy_trs.sort(
            key=lambda tr: tr.is_original_translation
        )  # original is the last in the list after this

        self.assertFalse({tr.id for tr in orig_trs} & {tr.id for tr in copy_trs})
        self.assertEqual(copy_trs[0].document.get_source_document().doc_id, copy.id)
        self.assertEqual(copy_trs[1].document.get_source_document().doc_id, copy.id)
        self.assertEqual(copy_trs[2].document.get_source_document(), None)
        self.assertEqual("de", copy_trs[0].lang_id)
        self.assertEqual("en", copy_trs[1].lang_id)
        self.assertEqual("fi", copy_trs[2].lang_id)
        self.assertEqual("Auf Deutsch", copy_trs[0].title)
        self.assertEqual("In English", copy_trs[1].title)
        self.assertEqual("suomeksi", copy_trs[2].title)
        self.assertEqual(
            german_par, copy_trs[0].document.get_paragraphs()[0].get_markdown()
        )
        self.assertEqual(
            english_par, copy_trs[1].document.get_paragraphs()[0].get_markdown()
        )
        self.assertEqual(
            finnish_par, copy_trs[2].document.get_paragraphs()[0].get_markdown()
        )

    def test_copy_translations_with_default_rights(self):
        """Translations of document are copied and default rights are set correctly."""
        self.login_test1()
        ug = self.test_user_1.get_personal_group()
        personal_folder = self.test_user_1.get_personal_folder().path

        f_from = Folder.create(f"{personal_folder}/translations_from", owner_groups=ug)
        f_to = Folder.create(f"{personal_folder}/translations_to", owner_groups=ug)
        db.session.commit()

        def_1 = get_default_right_document(
            f_from, BlockType.Document, create_if_not_exist=True
        )
        def_2 = get_default_right_document(
            f_to, BlockType.Document, create_if_not_exist=True
        )

        assert def_1 is not None
        assert def_2 is not None

        grant_access(ug, def_1, AccessType.owner)
        grant_access(ug, def_2, AccessType.owner)
        db.session.commit()

        d = self.create_doc(
            path=f"{f_from.path}/from_doc1", initial_par="suomalainen kappale"
        )
        d.lang_id = "fi"
        d.title = "suomeksi"
        db.session.commit()

        tr_en = self.create_translation(d, "In English", "en")
        tr_en.document.modify_paragraph(
            tr_en.document.get_paragraphs()[0].get_id(), "an English paragraph"
        )
        db.session.commit()

        copy = self.create_doc(f"{f_to.path}/to_doc1", copy_from=d.id)

        self.assertEqual(
            list(copy.block.accesses.keys()), [(ug.id, AccessType.owner.value)]
        )

    def test_copy_doc_with_issues(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {area=a}
#- {area_end=a}
#- {area=a}
#- {area_end=a}
        """
        )
        pars = d.document.get_paragraphs()
        self.create_doc(
            copy_from=d.id,
            expect_status=400,
            expect_content={
                "error": f"The following errors must be fixed before copying:\n"
                f"<ol><li>Multiple areas with same name noticed for area "
                f"&amp;#x27;a&amp;#x27; in paragraph {pars[0].get_id()}, {pars[2].get_id()}.</li>"
                f"<li>Duplicate area end noticed for area "
                f"&amp;#x27;a&amp;#x27; in paragraph {pars[1].get_id()}, {pars[3].get_id()}.</li></ol>"
            },
        )

    def test_copy_preamble(self):
        self.login_test1()
        f = self.test_user_1.get_personal_folder().path
        d = self.create_doc(f"{f}/folder/doc", initial_par="Test")
        # Create top-level preamble first
        p = self.create_preamble_for(d.parent, initial_par="Preamble")
        pars = d.document.get_paragraphs(include_preamble=True)
        self.assertEqual(len(pars), 2, "Created document must have preamble par")

        preamble_par_id = p.document.get_paragraphs()[0].get_id()
        # Create second preamble for document level that copies from top-level preamble
        p2 = self.create_preamble_for(d, copy_from=p.id)
        preamble2_par_id = p2.document.get_paragraphs()[0].get_id()
        self.assertNotEqual(
            preamble_par_id,
            preamble2_par_id,
            "Copied preamble must have fresh block IDs",
        )

        # Clear cached document object and regenerate it
        db.session.expunge(d)
        d = DocEntry.find_by_id(d.id)

        pars = d.document.get_paragraphs(include_preamble=True)
        self.assertEqual(
            len(pars), 3, "Document must have both original and copied preamble"
        )
