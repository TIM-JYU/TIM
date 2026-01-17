"""Server tests for preview."""

from lxml.html import HtmlElement

from timApp.auth.accesstype import AccessType
from timApp.tests.server.timroutetest import TimRouteTest, get_content
from timApp.timdb.sqa import db


class PreviewTest(TimRouteTest):
    def test_normal_preview(self):
        self.login_test1()
        d = self.create_doc(initial_par="a1par")
        pars = d.document.get_paragraphs()
        r = self.json_post(
            f"/preview/{d.id}",
            {
                "text": "a2par",
                "docId": d.id,
                "par": pars[0].get_id(),
                "proofread": False,
                "tags": {},
            },
        )
        self.assertIn("<p>a2par</p>", r["texts"])
        self.assertEqual("", r["warnings"])

    def test_illegal_task_id_preview(self):
        self.login_test1()
        d = self.create_doc(initial_par="a1par")
        pars = d.document.get_paragraphs()
        r = self.json_post(
            f"/preview/{d.id}",
            {
                "text": "#- {#test1+}\na2par",
                "docId": d.id,
                "par": pars[0].get_id(),
                "proofread": False,
                "tags": {},
            },
        )
        self.assertIn("<p>a2par</p>", r["texts"])
        self.assertEqual(
            "Illegal chars in taskId &amp;#x27;test1+&amp;#x27;", r["warnings"]
        )

    def test_illegal_par_id_and_duplicate_task_id_preview(self):
        self.login_test1()
        d = self.create_doc(initial_par="a1par")
        pars = d.document.get_paragraphs()
        r = self.json_post(
            f"/preview/{d.id}",
            {
                "text": '#- {#test1 id="aaa"}\na2par\n#- {#test1}\na3par',
                "docId": d.id,
                "par": pars[0].get_id(),
                "proofread": False,
                "tags": {},
            },
        )
        self.assertIn("<p>a2par</p>", r["texts"])
        self.assertIn("<p>a3par</p>", r["texts"])
        self.assertRegex(
            r["warnings"],
            "<ol><li>Invalid paragraph id noticed in paragraph aaa</li>"
            "<li>Duplicate task id &#x27;test1&#x27; noticed in paragraph aaa, .*</li></ol>",
        )

    def test_preview_permission(self):
        self.login_test1()
        d = self.create_doc(initial_par="Secret")
        p = d.document.get_last_par()
        self.assertEqual(p.get_markdown(), "Secret")
        res = self.post_preview(d, "", par=p.get_id())
        self.assertEqual(res["original_par"]["md"], "Secret")

        self.logout()
        res = self.post_preview(d, "", par=p.get_id())
        self.assertIsNone(res["original_par"])

        self.login_test2()
        res = self.post_preview(d, "", par=p.get_id())
        self.assertIsNone(res["original_par"])

        self.login_test1()
        self.test_user_2.grant_access(d, AccessType.edit)
        db.session.commit()

        self.login_test2()
        res = self.post_preview(d, "", par=p.get_id())
        self.assertEqual(res["original_par"]["md"], "Secret")

    def test_translation_invalid_ref(self):
        self.login_test1()
        d = self.create_doc(initial_par="""#- {rd=9999 rp=xxxx}""")
        t = self.create_translation(d)
        p = t.document.get_paragraphs()[0]
        md = f'#- {{r="tr" rd="{p.get_attr("rd")}" rp="{p.get_attr("rp")}"}}\n'
        self.get(f"/getBlock/{t.id}/{p.get_id()}", expect_content={"text": md})
        e = self.post_preview(t, text=md, json_key="texts", as_tree=True)
        self.assert_content(e, ["The referenced document does not exist."])

    def test_help_par(self):
        self.login_test1()
        d = self.create_doc()
        e = self.post_preview(
            d, text="test", par="HELP_PAR", json_key="texts", as_tree=True
        )
        self.assert_content(e, ["test"])

    def test_line_break(self):
        self.login_test1()
        d = self.create_doc()
        e = self.post_preview(d, text="test\\\ntest2\\", json_key="texts", as_tree=True)
        self.assert_content(e, ["test\ntest2"])

    def test_attributes_at_end_of_code_block(self):
        """Gives warning for attributes at end of code block"""
        self.login_test1()
        d = self.create_doc(initial_par="a1par")
        pars = d.document.get_paragraphs()
        r = self.json_post(
            f"/preview/{d.id}",
            {
                "text": "```\n``` {}",
                "docId": d.id,
                "par": pars[0].get_id(),
                "proofread": False,
                "tags": {},
            },
        )
        self.assertIn("<p><code></code> {}</p>", r["texts"])
        self.assertIn(
            "Attributes at end of code block noticed in paragraph", r["warnings"]
        )

    def test_preamble_preview_first(self):
        """Make sure an exception won't occur when editing the first paragraph of a document with a preamble."""
        self.login_test1()
        d = self.create_doc(initial_par="test")
        p = self.create_preamble_for(d)
        p.document.add_paragraph("test2")
        first = d.document.get_paragraphs()[0]
        self.post_preview(
            d, text="asd", par=first.get_id(), json_key="texts", as_tree=True
        )

    def test_spellcheck(self):
        self.login_test1()
        d = self.create_doc()
        self.check_spelling(
            d,
            [
                """<p><tim-spell-error bind-sugg='["koira", "Korria", "koitta"]'>koirra</tim-spell-error></p>""",
            ],
            "koirra",
        )
        self.check_spelling(
            d,
            [
                """<p><tim-spell-error bind-sugg="[]">koirrra</tim-spell-error></p>""",
            ],
            "koirrra",
        )
        self.check_spelling(
            d,
            [
                r"""<p>astia juusto <tim-spell-error bind-sugg='["leip\u00e4", "Leopa", "Leila", "leipoa"]'>leipa</tim-spell-error> <tim-spell-error bind-sugg='["sieni", "siteeni", "sieneni", "sireeni", "siseni"]'>sieeni</tim-spell-error> omena <tim-spell-error bind-sugg='["kasvi", "kiva", "k\u00e4vi", "kavio", "kahvi"]'>kavi</tim-spell-error> kissa</p>""",
            ],
            "astia juusto leipa sieeni omena kavi kissa",
        )
        self.check_spelling(
            d,
            [
                """<p><tim-spell-error bind-sugg='["juostu", "juusto", "juutu"]'>juustu</tim-spell-error> <tim-spell-error bind-count="2" bind-sugg='["juostu", "juusto", "juutu"]'>juustu</tim-spell-error></p>""",
                """<p><tim-spell-error bind-sugg='["juostu", "juusto", "juutu"]'>juustu</tim-spell-error></p>""",
            ],
            "juustu juustu\n#-\njuustu",
        )
        self.check_spelling(
            d,
            ["""<p>Maitoa 0.5 litraa</p>"""],
            "Maitoa 0.5 litraa",
        )
        self.check_spelling(
            d,
            [""],
            "#- {defaultplugin=xxx}",
        )
        self.check_spelling(
            d,
            [],
            "",
        )

    def check_spelling(self, d, expected, markdown):
        e: list[HtmlElement] = self.post_preview(
            d, text=markdown, json_key="texts", as_tree="fragments", spellcheck=True
        )
        self.assertEqual(len(e), len(expected))
        for i, ex in enumerate(expected):
            children = e[i].cssselect(".parContent")[0].getchildren()
            if not children:
                self.assertEqual("", ex)
                continue
            self.assert_same_html(
                children[0],
                ex,
            )
