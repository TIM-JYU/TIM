"""Server tests for xxx."""
from typing import List

from lxml.html import HtmlElement

from timApp.tests.server.timroutetest import TimRouteTest


class DocSassTest(TimRouteTest):
    def test_sass(self):
        self.login_test1()
        d = self.create_doc(settings={'css': """
.test {
  .inner1 { display: block; }
  .inner2 { display: block; }
}
        """})
        r = self.get(d.url, as_tree=True)
        s: List[HtmlElement] = r.cssselect('style')
        self.assertEqual("""
.test .inner1 { display: block; }

.test .inner2 { display: block; }
        """.strip(), s[0].text.strip())

        d.document.set_settings({'css': '{'})
        r = self.get(d.url, as_tree=True)
        self.assertEqual("""
Document stylesheet has errors: Error: Invalid CSS after "{": expected 1 selector or at-rule, was "{"
        on line 1:1 of stdin
>> {
   ^
        """.strip(), self.get_message(r))

    def get_message(self, r: HtmlElement):
        m = r.cssselect('.alert.alert-info')[0]
        return m.text_content().strip()
