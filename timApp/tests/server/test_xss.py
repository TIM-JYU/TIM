"""Server tests for verifying the absence of XSS in certain scenarios."""
from timApp.tests.server.timroutetest import TimRouteTest


class XssTest(TimRouteTest):
    def test_nocache_xss(self):
        self.login_test1()
        d = self.create_doc(initial_par='#- {nocache=true}\n<script class="evil">alert("hi")</script>')
        r = self.get(d.url, as_tree=True)
        self.assertFalse(r.cssselect('script.evil'))
