"""Server tests for cbcountfield."""
from timApp.auth.accesstype import AccessType
from timApp.tests.server.timroutetest import TimRouteTest


class CbCountFieldTest(TimRouteTest):
    def expect_count(self, r, count):
        self.assertEqual(count, r["web"]["count"])

    def test_cbcountfield(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {#t plugin=cbcountfield}
        """
        )
        self.test_user_2.grant_access(d, AccessType.view)
        self.test_user_3.grant_access(d, AccessType.view)
        self.commit_db()
        r = self.post_answer(
            "cbcountfield",
            f"{d.id}.t",
            user_input={"c": "1"},
        )
        self.expect_count(r, 1)
        r = self.post_answer(
            "cbcountfield",
            f"{d.id}.t",
            user_input={"c": "1"},
        )
        self.expect_count(r, 1)
        r = self.post_answer(
            "cbcountfield",
            f"{d.id}.t",
            user_input={"c": "0"},
        )
        self.expect_count(r, 0)
        r = self.post_answer(
            "cbcountfield",
            f"{d.id}.t",
            user_input={"c": "1"},
        )
        self.expect_count(r, 1)
        self.login_test2()
        r = self.post_answer(
            "cbcountfield",
            f"{d.id}.t",
            user_input={"c": "1"},
        )
        self.expect_count(r, 2)
        r = self.post_answer(
            "cbcountfield",
            f"{d.id}.t",
            user_input={"c": "1"},
        )
        self.expect_count(r, 2)
        r = self.post_answer(
            "cbcountfield",
            f"{d.id}.t",
            user_input={"c": "0"},
        )
        self.expect_count(r, 1)
        self.login_test3()
        with self.internal_container_ctx():
            r = self.get(d.url, as_tree=True)
            par_id = d.document.get_paragraphs()[0].get_id()
            self.assert_plugin_json(
                r.cssselect(".parContent cbcountfield-runner")[0],
                self.create_plugin_json(
                    d,
                    "t",
                    state=None,
                    info=None,
                    par_id=par_id,
                    toplevel={"count": 1},
                    markup={"autoUpdateTables": True},
                ),
            )

    def test_cbcountfield_grouplogin(self):
        self.login_test1()
        self.login_test2(add=True)
        d = self.create_doc(
            initial_par="""
#- {#t plugin=cbcountfield}"""
        )
        r = self.post_answer(
            "cbcountfield",
            f"{d.id}.t",
            user_input={"c": "1"},
        )
        self.expect_count(r, 1)
