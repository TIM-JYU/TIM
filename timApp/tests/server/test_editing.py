from lxml import html
from lxml.html import HtmlElement

from timApp.auth.accesstype import AccessType
from timApp.document.docparagraph import DocParagraph
from timApp.document.viewcontext import default_view_ctx
from timApp.plugin.plugin import Plugin
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db


class EditTest(TimRouteTest):
    def test_nonexistent_edit(self):
        self.login_test1()
        d = self.create_doc(initial_par="test")
        par_id = d.document.get_paragraphs()[0].get_id()
        invalid_par = "nonexistent"
        self.delete_par(
            d,
            invalid_par,
            expect_status=400,
            expect_content={"error": f"Paragraph {invalid_par} does not exist"},
        )
        self.delete_area(
            d,
            invalid_par,
            par_id,
            expect_status=400,
            expect_content={"error": f"Paragraph {invalid_par} does not exist"},
        )
        self.delete_area(
            d,
            par_id,
            invalid_par,
            expect_status=400,
            expect_content={"error": f"Paragraph {invalid_par} does not exist"},
        )
        self.get(
            f"/getBlock/{d.id}/{invalid_par}",
            expect_status=404,
            expect_content="Paragraph not found. It may have been deleted.",
        )
        self.get(
            f"/getBlock/{d.id}/{par_id}",
            query_string={"area_start": par_id, "area_end": invalid_par},
            expect_status=404,
            expect_content="Area not found. It may have been deleted.",
        )

    def test_duplicate_task_ids(self):
        self.login_test1()
        d = self.create_doc()
        r = self.new_par(d.document, "``` {#test plugin=showVideo}\n```")
        self.assertEqual(r["duplicates"], [])
        r = self.new_par(d.document, "``` {#test plugin=showVideo}\n```")
        pars = d.document.get_paragraphs()
        self.assertEqual(r["duplicates"], [["test", pars[1].get_id()]])

    def test_area_editing(self):
        self.login_test1()
        d = self.create_doc(initial_par=["a1par", "a2par", "a3par"])
        pars = d.document.get_paragraphs()
        new_text = (
            d.document.export_markdown()
            .replace("a1par", "b1par")
            .replace("a2par", "b2par")
            .replace("a3par", "b3par")
        )
        self.json_post(
            "/postParagraph/",
            {
                "text": new_text,
                "docId": d.id,
                "par": pars[0].get_id(),
                "par_next": None,
                "area_start": pars[0].get_id(),
                "area_end": pars[-1].get_id(),
            },
        )
        d.document.clear_mem_cache()
        self.assertEqual(d.document.export_markdown(), new_text)
        self.delete_area(d, pars[0].get_id(), pars[-1].get_id())
        d.document.clear_mem_cache()
        self.assertEqual(d.document.get_paragraphs(), [])

    def test_get_updates_pars_translation(self):
        self.login_test1()
        d = self.create_doc(initial_par=["kissa"])
        t = self.create_translation(d)
        e = self.get(t.url, as_tree=True)
        self.assert_content(e, ["kissa"])
        tr_pars = t.document.get_paragraphs()
        par = tr_pars[0]
        md = par.get_exported_markdown().replace("kissa", "cat")
        e = self.post_par(t.document, md, par.get_id(), as_tree=True, json_key="texts")
        self.assert_content(e, ["cat"])
        updated = self.get_updated_pars(t, json_key="changed_pars")
        e = html.fromstring(updated[par.get_id()])
        self.assert_content(e, ["cat"])

    def test_update(self):
        """Editing a document with minor errors is possible in manage.

        Document must not contain any errors after editing.

        """
        self.login_test1()
        d = self.create_doc()
        for i in range(0, 2):
            d.document.add_text("#- {area=a}")
            d.document.add_text("#- {area_end=a}")
        par_ids = d.document.get_par_ids()
        self.update_whole_doc(
            d,
            f"""
#- {{area=a id={par_ids[0]}}}
#- {{area_end=a id={par_ids[1]}}}
#- {{area=b id={par_ids[2]}}}
#- {{area_end=b id={par_ids[3]}}}
                       """,
        )
        orig_text = d.document.export_markdown()
        self.json_post(
            f"/update/{d.id}",
            {
                "fulltext": f"""
#- {{area=a id={par_ids[0]}}}
#- {{area_end=a id={par_ids[1]}}}
#- {{area=a id={par_ids[2]}}}
#- {{area_end=a id={par_ids[3]}}}
                       """,
                "original": orig_text,
            },
            expect_status=400,
            expect_content=f"Multiple areas with same name noticed in paragraph {par_ids[2]}\nDuplicate area end noticed in paragraph {par_ids[3]}",
        )
        self.json_post(
            f"/update/{d.id}",
            {
                "fulltext": f"""
#- {{id={par_ids[0]}}}
#- {{id={par_ids[0]}}}
                       """,
                "original": orig_text,
            },
            expect_status=400,
            expect_content=f"Duplicate paragraph id noticed in paragraph {par_ids[0]}",
        )
        self.json_post(
            f"/update/{d.id}",
            {
                "fulltext": f"""
#- {{id=xxxx}}
                       """,
                "original": orig_text,
            },
            expect_status=400,
            expect_content=f"Invalid paragraph id noticed in paragraph xxxx",
        )
        self.json_post(
            f"/update/{d.id}",
            {
                "fulltext": """
```
``` {a=b}
                       """,
                "original": orig_text,
            },
            expect_status=400,
            expect_contains=f"Attributes at end of code block noticed in paragraph ",
        )

    def test_new_from_help_par(self):
        self.login_test1()
        d = self.create_doc()
        e = self.json_post(
            f"/newParagraph/",
            {"docId": d.id, "text": "test", "par": "HELP_PAR", "par_next": None},
            json_key="texts",
            as_tree=True,
        )
        self.assert_content(e, ["test"])

    def test_duplicate_template(self):
        """Trying to load a template to a non-empty document is not possible."""
        self.login_test1()
        t = self.create_doc(initial_par=["p1", "p2"])
        d = self.create_doc()
        template_pars = t.document.get_paragraphs()
        d.document.add_paragraph("p1", template_pars[0].get_id())
        d.document.add_paragraph("p2", template_pars[1].get_id())
        self.json_post(
            f"/update/{d.id}",
            {"template_name": t.path},
            expect_status=400,
            expect_content={
                "error": "Cannot load a template because the document is not empty."
            },
        )
        self.get(d.url)

    def test_nonexistent_template(self):
        """Trying to load a non-existent template gives 404."""
        self.login_test1()
        d = self.create_doc()
        self.json_post(
            f"/update/{d.id}",
            {"template_name": "xxx"},
            expect_status=404,
            expect_content={"error": "Template not found"},
        )

    def test_invalid_add(self):
        self.login_test1()
        d = self.create_doc()
        self.new_par(d.document, "test", next_id="xxx", expect_status=400)

    def test_invalid_update(self):
        self.login_test1()
        d = self.create_doc(initial_par="test")
        par = d.document.get_paragraphs()[0]
        md = d.document.export_markdown()
        self.json_post(
            f"/update/{d.id}",
            {"fulltext": md, "original": ""},
            expect_status=400,
            expect_content={"error": f"Duplicate paragraph id(s): {par.get_id()}"},
        )
        self.get(d.url)

    def test_duplicate_par_ids(self):
        self.login_test1()
        d = self.create_doc(initial_par=["test1", "test2"])
        pars = d.document.get_paragraphs()
        par1 = pars[0]
        par2 = pars[1]
        md1 = par1.get_exported_markdown(export_ids=True)
        self.new_par(
            d.document,
            md1,
            expect_status=400,
            expect_content={"error": f"Duplicate paragraph id(s): {par1.get_id()}"},
        )
        self.post_par(d.document, md1, par_id=par1.get_id())
        md2 = par2.get_exported_markdown(export_ids=True)
        self.post_par(
            d.document,
            md1 + md2,
            par_id=par1.get_id(),
            expect_status=400,
            expect_content={"error": f"Duplicate paragraph id(s): {par2.get_id()}"},
        )
        self.get(d.url)

    def test_version(self):
        self.login_test1()
        d = self.create_doc()
        j = self.new_par(d.document, "test")
        self.assertEqual(j["version"], [1, 0])

    def test_mark_read(self):
        self.login_test1()
        d = self.create_doc()
        self.new_par(d.document, "test", additional_data={"tags": {"markread": True}})

    def test_xml_encoding(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""<?xml version="1.0" encoding="iso-8859-1"?>"""
        )
        self.get(d.url)

    def test_edit_attribute(self):
        self.login_test1()
        d = self.create_doc(
            initial_par=[
                "test",
                "#- {edit=manage}",
                "test",
                "#- {area=a edit=manage}",
                "in area",
                "#- {area_end=a}",
            ]
        )
        pars = d.document.get_paragraphs()
        par1 = pars[0]
        par_manage = pars[1]
        par3 = pars[2]
        self.post_par(d.document, "#- {edit=manage}\ntest", par_manage.get_id())
        self.test_user_2.grant_access(d, AccessType.edit)
        db.session.commit()
        self.login_test2()
        self.post_par(d.document, "asd", par1.get_id())
        self.post_par(
            d.document,
            "testing",
            par_manage.get_id(),
            expect_status=403,
            expect_content=f"Only users with manage access can edit this paragraph ({par_manage.get_id()}).",
        )
        self.update_whole_doc(d, "", expect_status=403)
        self.post_area(d, "", par1.get_id(), par3.get_id(), expect_status=403)
        self.delete_area(d, par1.get_id(), par3.get_id(), expect_status=403)
        self.delete_area(d, par1.get_id(), par_manage.get_id(), expect_status=403)
        self.delete_area(d, par_manage.get_id(), par3.get_id(), expect_status=403)
        self.delete_area(d, par_manage.get_id(), par_manage.get_id(), expect_status=403)
        self.delete_par(d, par_manage.get_id(), expect_status=403)
        self.cut(d, par1, par3, expect_status=403)
        self.cut(d, par1, par_manage, expect_status=403)
        self.cut(d, par_manage, par3, expect_status=403)
        self.cut(d, par_manage, par_manage, expect_status=403)
        self.json_post(
            "/postNewTaskNames/",
            {"docId": d.id, "duplicates": [[None, None, par_manage.get_id()]]},
            expect_status=403,
        )
        self.json_post(
            "/cancelChanges/",
            {
                "docId": d.id,
                "originalPar": {"md": "asd"},
                "parId": par_manage.get_id(),
                "newPars": [],
            },
            expect_status=403,
        )
        self.json_post(
            "/cancelChanges/",
            {
                "docId": d.id,
                "originalPar": None,
                "parId": par_manage.get_id(),
                "newPars": [par_manage.get_id()],
            },
            expect_status=403,
        )
        self.json_post(f"/unwrap_area/{d.id}/a", expect_status=403)

        self.test_user_2.grant_access(d, AccessType.manage)
        db.session.commit()
        self.post_par(d.document, "#- {edit=manage}\nedited", par1.get_id())
        self.update_whole_doc(d, d.document.export_markdown() + "new")
        self.post_area(
            d,
            d.document.export_section(par1.get_id(), par3.get_id()) + "new",
            par1.get_id(),
            par3.get_id(),
        )
        self.delete_area(d, par1.get_id(), par3.get_id())
        self.json_post(f"/unwrap_area/{d.id}/a")

    def test_download(self):
        self.login_test1()
        d = self.create_doc(initial_par="test")
        par_id = d.document.get_paragraphs()[0].get_id()
        self.get(
            f"/download/{d.id}",
            expect_content=f"""
#- {{id="{par_id}"}}
test
        """.strip()
            + "\n",
        )
        self.get(
            f"/download/{d.id}",
            query_string={"format": "json"},
            expect_content=d.document.export_raw_data(),
        )
        self.login_test2()
        self.get(f"/download/{d.id}", expect_status=403)
        self.get(
            f"/download/{d.id}", query_string={"format": "json"}, expect_status=403
        )

    def test_no_unnecessary_update(self):
        self.login_test1()
        d = self.create_doc(initial_par="test")
        self.get(d.url)
        self.new_par(
            d.document,
            """
#- {settings=""}
macros:
 a: b""",
            next_id=d.document.get_paragraphs()[0].get_id(),
        )
        r = self.get_updated_pars(d)
        self.assertFalse(r["changed_pars"])

    def test_no_unnecessary_update_with_preamble(self):
        self.login_test1()
        d = self.create_doc(initial_par="test")
        p = self.create_preamble_for(d, initial_par="pr")
        self.get(d.url)
        pid = d.document.get_paragraphs()[0].get_id()
        self.post_par(d.document, """hi""", par_id=pid)
        r = self.get_updated_pars(d)
        self.assertEqual({pid}, set(r["changed_pars"].keys()))

    def test_no_area_start_update(self):
        """getUpdatedPars shouldn't return area start or end pars because it will break the DOM structure."""
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {area=a collapse=true}
title

#-
1

#- {area_end=a}
        """
        )
        self.get_updated_pars(d)  # refresh cache
        pars = d.document.get_paragraphs()
        md = d.document.export_markdown().replace("title", "titleedit")
        self.post_area(d, md, area_start=pars[0].get_id(), area_end=pars[-1].get_id())
        r = self.get_updated_pars(d)
        self.assertEqual({}, r["changed_pars"])

    def test_cache_no_extra_div(self):
        self.login_test1()
        d = self.create_doc()
        p = DocParagraph.from_dict(
            d.document,
            {
                "attrs": {},
                "h": {"thisIsSomeOldHash": '<h2 id="hi">hi</h2>'},
                "id": "eHdzfXQbvcIY",
                "md": "## hi",
            },
        )
        d.document.add_paragraph_obj(p)
        e = self.get(d.url, as_tree=True)
        pc: HtmlElement = e.cssselect(".parContent")[0]
        self.assertEqual("h2", pc.getchildren()[0].tag)

    def test_html_heading_same_as_md(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
# x
#-
<h1>x</h1>
        """
        )
        e = self.get(d.url, as_tree=True)
        content = e.cssselect(".parContent")
        p1: HtmlElement = content[0].getchildren()[0]
        p2: HtmlElement = content[1].getchildren()[0]
        self.assertEqual("x", p1.get("id"))
        self.assertEqual("x-1", p2.get("id"))

    def test_getblock(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {plugin=pali}
#- {defaultplugin=pali}
#-
test
        """
        )
        d_id = d.id
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()
        par_ids = [p.get_id() for p in d.document.get_paragraphs()]
        self.login_test2()
        self.get(f"/getBlock/{d_id}/{par_ids[0]}", expect_status=403)
        self.get(f"/getBlock/{d_id}/{par_ids[1]}", expect_status=403)
        self.get(f"/getBlock/{d_id}/{par_ids[2]}")


class TimTableEditTest(TimRouteTest):
    def test_timtable_edit(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
``` {plugin="timTable"}
table:
    countRow: 1
    countCol: 1
    rows:
      - row:
        - cell: 'hi'
```"""
        )
        par = d.document.get_paragraphs()[0]
        par_id = par.get_id()
        ver1 = d.document.get_version()
        self.json_post(
            f"/timTable/saveCell",
            {
                "cellContent": "hey",
                "docId": d.id,
                "parId": par_id,
                "row": 0,
                "col": 0,
            },
        )
        d.document.clear_mem_cache()
        ver2 = d.document.get_version()
        self.assertNotEqual(ver1, ver2)
        par = d.document.get_paragraphs()[0]
        p = Plugin.from_paragraph(par, default_view_ctx)
        self.assertEqual(
            {
                "table": {
                    "countCol": 1,
                    "countRow": 1,
                    "rows": [{"row": [{"cell": "hi"}]}],
                    "tabledatablock": {"cells": {"A1": "hey"}, "type": "relative"},
                }
            },
            p.values,
        )
        self.json_post(
            f"/timTable/saveCell",
            {
                "cellContent": "hey",
                "docId": d.id,
                "parId": par_id,
                "row": 0,
                "col": 0,
            },
        )
        d.document.clear_mem_cache()
        ver3 = d.document.get_version()
        self.assertEqual(ver2, ver3)
