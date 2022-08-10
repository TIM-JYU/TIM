"""Server tests for verifying the absence of XSS or other DOM sanitization issues in certain scenarios."""
from lxml import html
from lxml.html import Element

from timApp.document.docentry import DocEntry
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup


class SanitizeTest(TimRouteTest):
    def test_nocache_xss(self):
        self.login_test1()
        d = self.create_doc(
            initial_par='#- {nocache=true}\n<script class="evil">alert("hi")</script>'
        )
        r = self.get(d.url, as_tree=True)
        self.assertFalse(r.cssselect("script.evil"))

    def test_html_tag_sanitize(self):
        self.login_test1()
        d = self.create_doc(initial_par="<html></html>")
        self.get(d.url, as_tree=True)
        d.document.update("<html >Test</html >", d.document.export_markdown())
        self.get(d.url, as_tree=True)

    def test_html_print_sanitize(self):
        DocEntry.create(
            "templates/printing/empty",
            UserGroup.get_anonymous_group(),
            initial_par="""
``` {.latex printing_template=""}
$body$
```""",
        )
        db.session.commit()
        self.login_test1()
        d = self.create_doc(
            initial_par="<script>console.log(123)</script> <iframe src='https://www.google.com'></iframe> <p>Test</p>"
        )
        r: Element = self.get(
            f"{d.get_relative_url_for_view('print')}?file_type=html&template_doc_id=0",
            as_tree=True,
        )
        self.assert_elements_equal(
            r.find("body"),
            html.fromstring("<html><body><p>Test</p></body></html>").find("body"),
        )
