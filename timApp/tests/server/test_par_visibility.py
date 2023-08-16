"""Server tests for user/view-specific document rendering."""
from timApp.auth.accesstype import AccessType
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db


class ParVisibilityTest(TimRouteTest):
    def test_belongs(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#-
a

#- {nocache=true visible="%%'testuser1'|belongs%%"}
testuser1 only

#-
anyone
"""
        )
        self.assert_content(
            self.get(d.url, as_tree=True), ["a", "testuser1 only", "anyone"]
        )
        p = d.document.get_paragraphs()[1]
        p.set_markdown("testuser1 only edited")
        d.document.modify_paragraph_obj(p.get_id(), p)
        self.assert_content(
            self.get(d.url, as_tree=True), ["a", "testuser1 only edited", "anyone"]
        )

        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()
        self.login_test2()
        self.assert_content(self.get(d.url, as_tree=True), ["a", "anyone"])
        self.test_user_2.grant_access(d, AccessType.edit)
        db.session.commit()
        self.assert_content(
            self.get(d.url, as_tree=True), ["a", "anyone"]
        )  # TODO shouldn't editors always see everything?
        p = d.document.get_paragraphs()[1]
        p.set_attr("visible", "%%'testuser2'|belongs%%")
        p.set_markdown("testuser2 only")
        d.document.modify_paragraph_obj(p.get_id(), p)
        self.assert_content(
            self.get(d.url, as_tree=True), ["a", "testuser2 only", "anyone"]
        )
        self.login_test1()
        self.assert_content(self.get(d.url, as_tree=True), ["a", "anyone"])

    def test_isview(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {nocache=true visible="%%False|isview%%"}
only teacher
        """
        )
        self.assert_content(self.get(d.url, as_tree=True), [])
        self.assert_content(self.get(d.get_url_for_view("slide"), as_tree=True), [])
        self.assert_content(
            self.get(d.get_url_for_view("answers"), as_tree=True), ["only teacher"]
        )
        self.assert_content(
            self.get(d.get_url_for_view("teacher"), as_tree=True), ["only teacher"]
        )
        self.assert_content(
            self.get(d.url, as_tree=True), []
        )  # make sure the teacher route is not cached incorrectly

    def test_belongs_in_text(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {nocache=true}
I am testuser1: %%'testuser1'|belongs%%

#-
I am testuser1: %%'testuser1'|belongs%%
        """
        )
        self.assert_content(
            self.get(d.url, as_tree=True),
            [
                "I am testuser1: True",
                "I am testuser1: The belongs filter requires nocache=true attribute.",
            ],
        )

    def test_area_visible(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
1

#- {area=a visible="%%'testuser1'|belongs%%" nocache=true}
a title
#-
2

#- {area=b collapse=true nocache=true}
b title
#-
2.5
#- {area_end=b}

#- {area_end=a}

#-
3
        """
        )
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()
        self.assert_content(
            self.get(d.url, as_tree=True),
            ["1", "a title", "2", "2.5", "", "", "3"],
        )
        self.login_test2()
        self.assert_content(
            self.get(d.url, as_tree=True),
            [
                "1",
                "",
                "",
                "3",
            ],
        )
