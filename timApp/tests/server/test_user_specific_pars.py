"""Server tests for user-specific document rendering."""
from timApp.auth.accesstype import AccessType
from timApp.tests.server.timroutetest import TimRouteTest


class UserSpecificTest(TimRouteTest):
    def test_belongs(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
#-
a

#- {nocache=true visible="%%'testuser1'|belongs%%"}
testuser1 only

#-
anyone
""")
        self.assert_content(self.get(d.url, as_tree=True), ['a', 'testuser1 only', 'anyone'])
        p = d.document.get_paragraphs()[1]
        p.set_markdown('testuser1 only edited')
        d.document.modify_paragraph_obj(p.get_id(), p)
        self.assert_content(self.get(d.url, as_tree=True), ['a', 'testuser1 only edited', 'anyone'])

        self.test_user_2.grant_access(d, AccessType.view)
        self.login_test2()
        self.assert_content(self.get(d.url, as_tree=True), ['a', 'anyone'])
        self.test_user_2.grant_access(d, AccessType.edit)
        self.assert_content(self.get(d.url, as_tree=True), ['a', 'anyone'])  # TODO shouldn't editors always see everything?
        p = d.document.get_paragraphs()[1]
        p.set_attr('visible', "%%'testuser2'|belongs%%")
        p.set_markdown('testuser2 only')
        d.document.modify_paragraph_obj(p.get_id(), p)
        self.assert_content(self.get(d.url, as_tree=True), ['a', 'testuser2 only', 'anyone'])
        self.login_test1()
        self.assert_content(self.get(d.url, as_tree=True), ['a', 'anyone'])
