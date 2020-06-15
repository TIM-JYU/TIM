from timApp.tests.browser.browsertest import BrowserTest


class JsrunnerTest(BrowserTest):

    def test_jsrunner_utf8_multihtml(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {#t plugin="jsrunner"}
button: 'åäöÅÄÖ“”'
showInView: true
groups: []
```""")
        self.goto_document(d)
        btn = self.find_element("#t button")
        self.assertEqual('åäöÅÄÖ“”', btn.text)
