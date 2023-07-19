"""Server tests for showing authors for paragraphs."""
from timApp.auth.accesstype import AccessType
from timApp.tests.server.timroutetest import TimRouteTest, get_content
from timApp.timdb.sqa import db


class AuthorsTest(TimRouteTest):
    def test_authors(self):
        self.login_test1()
        d = self.create_doc()
        url = d.url
        self.test_user_2.grant_access(d, AccessType.edit)
        self.test_user_3.grant_access(d, AccessType.edit)
        db.session.commit()
        # db.session.expire_all()
        self.new_par(d.document, "par 1")
        self.new_par(d.document, "par 2")
        self.new_par(d.document, "par 3")
        d.document.set_settings({"show_authors": True})
        pars = d.document.get_paragraphs()
        username_selector = ".authorinfo .username"
        authors = get_content(self.get(url, as_tree=True), username_selector)
        self.assertEqual(
            authors, ["user 1 Test", "user 1 Test", "user 1 Test", "user 1 Test"]
        )
        self.post_par(d.document, "edit", pars[1].get_id())
        authors = get_content(self.get(url, as_tree=True), username_selector)
        self.assertEqual(
            authors,
            ["user 1 Test", "user 1 Test (2 edits)", "user 1 Test", "user 1 Test"],
        )
        self.login_test2()
        self.post_par(d.document, "edit2", pars[1].get_id())
        authors = get_content(self.get(url, as_tree=True), username_selector)
        self.assertEqual(
            authors,
            [
                "user 1 Test",
                "user 2 Test; user 1 Test (2 edits)",
                "user 1 Test",
                "user 1 Test",
            ],
        )
        self.post_par(d.document, "edit3", pars[1].get_id())
        self.post_par(d.document, "edit3", pars[3].get_id())
        authors = get_content(self.get(url, as_tree=True), username_selector)
        self.assertEqual(
            authors,
            [
                "user 1 Test",
                "user 2 Test (2 edits); user 1 Test (2 edits)",
                "user 1 Test",
                "user 2 Test; user 1 Test",
            ],
        )
        d.document.set_settings({})
        authors = get_content(self.get(url, as_tree=True), username_selector)
        self.assertEqual(authors, [])
