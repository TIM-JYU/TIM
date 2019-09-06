"""Server tests for showing authors for paragraphs."""
from timApp.tests.server.timroutetest import TimRouteTest, get_content
from timApp.user.userutils import grant_edit_access


class AuthorsTest(TimRouteTest):
    def test_authors(self):
        self.login_test1()
        d = self.create_doc()
        url = d.url
        grant_edit_access(self.test_user_2.get_personal_group(), d)
        grant_edit_access(self.test_user_3.get_personal_group(), d)
        self.new_par(d.document, 'par 1')
        self.new_par(d.document, 'par 2')
        self.new_par(d.document, 'par 3')
        d.document.set_settings({'show_authors': True})
        pars = d.document.get_paragraphs()
        username_selector = '.authorinfo .username'
        authors = get_content(self.get(url, as_tree=True), username_selector)
        self.assertEqual(authors, ['Logged-in users', 'Test user 1', 'Test user 1', 'Test user 1'])
        self.post_par(d.document, 'edit', pars[1].get_id())
        authors = get_content(self.get(url, as_tree=True), username_selector)
        self.assertEqual(authors, ['Logged-in users', 'Test user 1 (2 edits)', 'Test user 1', 'Test user 1'])
        self.login_test2()
        self.post_par(d.document, 'edit2', pars[1].get_id())
        authors = get_content(self.get(url, as_tree=True), username_selector)
        self.assertEqual(authors, ['Logged-in users',
                                   'Test user 2; Test user 1 (2 edits)',
                                   'Test user 1',
                                   'Test user 1'])
        self.post_par(d.document, 'edit3', pars[1].get_id())
        self.post_par(d.document, 'edit3', pars[3].get_id())
        authors = get_content(self.get(url, as_tree=True), username_selector)
        self.assertEqual(authors, ['Logged-in users',
                                   'Test user 2 (2 edits); Test user 1 (2 edits)',
                                   'Test user 1',
                                   'Test user 2; Test user 1'])
        d.document.set_settings({})
        authors = get_content(self.get(url, as_tree=True), username_selector)
        self.assertEqual(authors, [])
