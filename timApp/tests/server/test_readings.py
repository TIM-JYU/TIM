from datetime import timedelta

from lxml.cssselect import CSSSelector
from sqlalchemy import select, func

from timApp.document.docinfo import DocInfo
from timApp.readmark.readings import get_readings, get_read_expiry_condition
from timApp.readmark.readparagraph import ReadParagraph
from timApp.readmark.readparagraphtype import ReadParagraphType
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db

readline_selector = CSSSelector("div.readline")

UNREAD = ""
READ = ReadParagraphType.click_red.class_str
MODIFIED = READ + "-modified"
PAR_CLICK = ReadParagraphType.click_par.class_str
PAR_CLICK_MODIFIED = PAR_CLICK + "-modified"


class ReadingsTest(TimRouteTest):
    def test_readings_normal(self):
        self.login_test1()
        doc = self.create_doc(initial_par=["test", "test2", "test3"])
        stmt = select(func.count(ReadParagraph.id)).filter_by(doc_id=doc.id)
        pars = doc.document.get_paragraphs()

        self.check_readlines(self.get_readings(doc), (UNREAD, UNREAD, UNREAD))
        self.mark_as_read(doc, pars[0].get_id())
        self.check_readlines(self.get_readings(doc), (READ, UNREAD, UNREAD))
        self.mark_as_read(doc, pars[1].get_id())
        self.check_readlines(self.get_readings(doc), (READ, READ, UNREAD))
        doc.document.modify_paragraph(pars[1].get_id(), "a")
        self.check_readlines(self.get_readings(doc), (READ, MODIFIED, UNREAD))
        self.mark_as_read(doc, pars[2].get_id(), ReadParagraphType.click_par)
        self.check_readlines(self.get_readings(doc), (READ, MODIFIED, PAR_CLICK))
        doc.document.modify_paragraph(pars[2].get_id(), "b")
        self.check_readlines(
            self.get_readings(doc), (READ, MODIFIED, PAR_CLICK_MODIFIED)
        )
        self.mark_as_read(doc, pars[2].get_id())
        self.assertEqual(db.session.scalar(stmt), 4)
        self.check_readlines(
            self.get_readings(doc), (READ, MODIFIED, PAR_CLICK_MODIFIED + " " + READ)
        )
        self.mark_as_unread(doc, pars[2].get_id())
        self.assertEqual(db.session.scalar(stmt), 3)
        self.check_readlines(
            self.get_readings(doc), (READ, MODIFIED, PAR_CLICK_MODIFIED)
        )

    def test_readings_group(self):
        self.login_test1()
        self.login_test2(add=True)
        doc = self.create_doc(initial_par=["test", "test2", "test3", "test4"])
        self.check_readlines(self.get_readings(doc), (UNREAD, UNREAD, UNREAD, UNREAD))
        pars = doc.document.get_paragraphs()
        self.mark_as_read(doc, pars[0].get_id())

        self.check_readlines(self.get_readings(doc), (READ, UNREAD, UNREAD, UNREAD))
        self.mark_as_read(doc, pars[1].get_id())
        self.check_readlines(self.get_readings(doc), (READ, READ, UNREAD, UNREAD))

        self.login_test2()
        self.check_readlines(self.get_readings(doc), (READ, READ, UNREAD, UNREAD))
        self.mark_as_read(doc, pars[2].get_id())
        self.check_readlines(self.get_readings(doc), (READ, READ, READ, UNREAD))

        self.login_test1(add=True)
        self.check_readlines(self.get_readings(doc), (READ, READ, UNREAD, UNREAD))

        self.login_test1()
        self.mark_as_read(doc, pars[2].get_id())
        self.login_test2(add=True)
        self.check_readlines(self.get_readings(doc), (READ, READ, READ, UNREAD))
        doc.document.modify_paragraph(pars[2].get_id(), "a")
        self.check_readlines(self.get_readings(doc), (READ, READ, MODIFIED, UNREAD))
        self.login_test2()
        self.mark_as_read(doc, pars[2].get_id())
        self.check_readlines(self.get_readings(doc), (READ, READ, READ, UNREAD))
        self.login_test1(add=True)
        self.check_readlines(self.get_readings(doc), (READ, READ, UNREAD, UNREAD))
        self.login_test1()
        self.check_readlines(self.get_readings(doc), (READ, READ, MODIFIED, UNREAD))
        self.assertEqual(
            db.session.scalar(
                select(func.count(ReadParagraph.id)).filter_by(doc_id=doc.id)
            ),
            7,
        )

        self.get(
            f"/read/stats/{doc.id}",
            expect_content=[
                {
                    "any_of_phs": 0,
                    "click_par": 0,
                    "click_red": 3,
                    "hover_par": 0,
                    "on_screen": 0,
                    "username": "testuser1",
                },
                {
                    "any_of_phs": 0,
                    "click_par": 0,
                    "click_red": 3,
                    "hover_par": 0,
                    "on_screen": 0,
                    "username": "testuser2",
                },
            ],
        )
        self.get(
            f"/read/stats/{doc.id}",
            query_string={"consent": "any"},
            expect_content=[
                {
                    "any_of_phs": 0,
                    "click_par": 0,
                    "click_red": 3,
                    "hover_par": 0,
                    "on_screen": 0,
                    "username": "testuser1",
                },
                {
                    "any_of_phs": 0,
                    "click_par": 0,
                    "click_red": 3,
                    "hover_par": 0,
                    "on_screen": 0,
                    "username": "testuser2",
                },
            ],
        )
        self.get(
            f"/read/stats/{doc.id}", query_string={"consent": "true"}, expect_content=[]
        )
        self.get(
            f"/read/stats/{doc.id}",
            query_string={"blocks": ";".join(p.get_id() for p in pars[0:2])},
            expect_content=[
                {
                    "any_of_phs": 0,
                    "click_par": 0,
                    "click_red": 2,
                    "hover_par": 0,
                    "on_screen": 0,
                    "username": "testuser1",
                },
                {
                    "any_of_phs": 0,
                    "click_par": 0,
                    "click_red": 2,
                    "hover_par": 0,
                    "on_screen": 0,
                    "username": "testuser2",
                },
            ],
        )
        self.get(
            f"/read/stats/{doc.id}",
            query_string={"format": "csv", "csv": "excel"},
            expect_content=(
                """
username,click_red,click_par,hover_par,on_screen,any_of_phs
testuser1,3,0,0,0,0
testuser2,3,0,0,0,0
""".strip()
                + "\n"
            ).replace("\n", "\r\n"),
        )

    def get_readings(self, doc: DocInfo):
        readlines = readline_selector(self.get(f"/view/{doc.id}", as_tree=True))
        return readlines

    def check_readlines(self, readlines, expected):
        self.assertEqual(len(readlines), len(expected))
        for r, e in zip(readlines, expected):
            classes = set(r.attrib["class"].split(" "))
            self.assertIn("readline", classes)
            if e:
                for c in e.split(" "):
                    self.assertIn(
                        c,
                        classes,
                        f'read status "{c}" not found in paragraph "{r.getparent().attrib["id"]}"',
                    )

    def test_mark_all_read(self):
        self.login_test1()
        d = self.create_doc(initial_par=["1", "2"])
        self.json_put(f"/read/{d.id}")
        self.check_readlines(self.get_readings(d), (READ, READ))
        stmt = select(func.count(ReadParagraph.id)).filter_by(doc_id=d.id)
        self.assertEqual(db.session.scalar(stmt), 2)
        self.json_put(f"/read/{d.id}")
        self.assertEqual(db.session.scalar(stmt), 2)

    def test_expiry(self):
        self.login_test1()
        d = self.create_doc(initial_par=["1", "2"])
        self.json_put(f"/read/{d.id}")
        self.mark_as_read(
            d, d.document.get_paragraphs()[0].get_id(), ReadParagraphType.on_screen
        )
        rs = get_readings(
            self.current_user.get_personal_group().id,
            d.document,
            get_read_expiry_condition(timedelta(seconds=10)),
        )
        self.assertEqual(len(rs), 3)
        rs = get_readings(
            self.current_user.get_personal_group().id,
            d.document,
            get_read_expiry_condition(timedelta(seconds=0)),
        )
        self.assertEqual(len(rs), 2)

    def test_expiry_invalid(self):
        self.login_test1()
        d = self.create_doc()
        d.document.set_settings({"read_expiry": "a"})
        self.get(d.url)

    def test_readings_json(self):
        self.login_test1()
        ug_id = self.get_test_user_1_group_id()
        d = self.create_doc(initial_par=["1", "2"])
        pars = d.document.get_paragraphs()
        self.json_put(f"/read/{d.id}")
        rs = self.get(f"/read/{d.id}")
        self.assert_list_of_dicts_subset(
            rs,
            [
                {"doc_id": d.id, "type": "read", "usergroup_id": ug_id},
                {"doc_id": d.id, "type": "read", "usergroup_id": ug_id},
            ],
        )
        known_keys = {
            "doc_id",
            "type",
            "usergroup_id",
            "id",
            "timestamp",
            "par_id",
            "par_hash",
            "usergroup",
        }
        for r in rs:
            self.assertIsInstance(r["id"], int)
            self.assertIsInstance(r["timestamp"], str)
            self.assertEqual(set(r.keys()), known_keys)
        self.assertNotEqual(rs[0]["id"], rs[1]["id"])
        self.assertEqual(rs[0]["timestamp"], rs[1]["timestamp"])
        self.assertEqual({p.get_id() for p in pars}, {r["par_id"] for r in rs})
        self.assertEqual({p.get_hash() for p in pars}, {r["par_hash"] for r in rs})

    def test_tr_separate_readings(self):
        self.login_test1()
        d = self.create_doc(initial_par="test")
        tr = self.create_translation(d)
        d_pars = d.document.get_paragraphs()
        tr_pars = tr.document.get_paragraphs()
        par = d_pars[0]
        d_parid = par.get_id()
        self.mark_as_read(d, d_parid)
        self.check_readlines(self.get_readings(d), (READ,))
        self.check_readlines(self.get_readings(tr), (UNREAD,))
        self.mark_as_unread(d, d_parid)
        tr_par = tr_pars[0]
        tr_parid = tr_par.get_id()
        self.mark_as_read(tr, tr_parid)
        self.check_readlines(self.get_readings(d), (UNREAD,))
        self.check_readlines(self.get_readings(tr), (READ,))
        self.mark_as_read(d, d_parid)
        self.check_readlines(self.get_readings(d), (READ,))
        self.check_readlines(self.get_readings(tr), (READ,))
        par.set_markdown("test edit")
        par.save()
        self.check_readlines(self.get_readings(d), (MODIFIED,))
        self.check_readlines(self.get_readings(tr), (READ,))
        tr_par.set_markdown("tr")
        tr_par.save()
        self.check_readlines(self.get_readings(d), (MODIFIED,))
        self.check_readlines(self.get_readings(tr), (MODIFIED,))
        self.mark_as_read(d, d_parid)
        self.check_readlines(self.get_readings(d), (READ,))
        self.check_readlines(self.get_readings(tr), (MODIFIED,))

    def test_same_par_id_diff_doc_reference(self):
        self.login_test1()
        d = self.create_doc(initial_par="test")
        d2 = self.create_doc(copy_from=d.id)
        d3 = self.create_doc()
        p1 = d.document.get_paragraphs()[0].create_reference(d3.document)
        p2 = d2.document.get_paragraphs()[0].create_reference(d3.document)
        d3.document.add_paragraph_obj(p1)
        d3.document.add_paragraph_obj(p2)
        self.assertEqual(p1.get_attr("rp"), p2.get_attr("rp"))
        self.mark_as_read(d, p1.get_attr("rp"))
        self.mark_as_read(d2, p2.get_attr("rp"))
        self.check_readlines(self.get_readings(d3), (READ, READ))

    def test_preamble_read(self):
        self.login_test1()
        d = self.create_doc(initial_par="test")
        p = self.create_preamble_for(d)
        p.document.add_text("p")
        d.document.clear_mem_cache()
        d.document.insert_preamble_pars()
        self.mark_as_read(d, d.document.get_paragraphs()[0].get_id())

    def test_unread_nonexistent(self):
        self.login_test1()
        d = self.create_doc(initial_par="test")
        parid = d.document.get_paragraphs()[0].get_id()
        self.mark_as_unread(d, parid, expect_status=400)

    def test_mark_all_read_par_id_conflict(self):
        """
        Situation:
         * 2 documents
         * both have 2 paragraphs with same ids
         * one of the documents has a ref paragraph to the other
         * make sure "mark all read" works for both
        """
        self.login_test1()
        d = self.create_doc(initial_par="x")
        d2 = self.create_doc()
        d2.document.add_text(d.document.export_markdown())
        par = d.document.get_paragraphs()[0]
        d2.document.add_paragraph_obj(par.create_reference(d2.document))
        self.assertEqual(par.get_id(), d2.document.get_paragraphs()[0].get_id())
        self.json_put(f"/read/{d.id}")
        self.json_put(f"/read/{d2.id}")
        self.check_readlines(self.get_readings(d), (READ,))
        self.check_readlines(
            self.get_readings(d2),
            (
                READ,
                READ,
            ),
        )
