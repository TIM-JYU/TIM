"""Server tests for verifying the absence of XSS or other DOM sanitization issues in certain scenarios."""
from timApp.tests.server.timroutetest import TimRouteTest


class SanitizeTest(TimRouteTest):
    def test_nocache_xss(self):
        self.login_test1()
        d = self.create_doc(initial_par='#- {nocache=true}\n<script class="evil">alert("hi")</script>')
        r = self.get(d.url, as_tree=True)
        self.assertFalse(r.cssselect('script.evil'))

    def test_html_tag_sanitize(self):
        self.login_test1()
        d = self.create_doc(initial_par='<html></html>')
        self.get(d.url, as_tree=True)
        d.document.update('<html >Test</html >', d.document.export_markdown())
        self.get(d.url, as_tree=True)
