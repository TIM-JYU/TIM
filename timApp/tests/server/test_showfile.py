from timApp.tests.browser.browsertest import BrowserTest


class ShowfileTest(BrowserTest):
    def test_no_local_file_access(self):
        self.login_test1()
        d = self.create_doc(initial_par=f"""
#- {{#t plugin=showCode}}
file: {self.get_browser_url()}/ping
        """)
        self.assert_content(self.get(d.url, as_tree=True), ['{"status":"ok"}\nping'])

        d = self.create_doc(initial_par="""
#- {#t plugin=showCode}
file: file:///tmp/cache/something
        """)
        self.assert_content(self.get(d.url, as_tree=True), ["URL scheme must be http or https, got 'file'something"])
