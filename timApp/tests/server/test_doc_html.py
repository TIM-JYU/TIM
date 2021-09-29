import json

from lxml.html import HtmlElement

from timApp.tests.server.timroutetest import TimRouteTest


class DocHtmlTest(TimRouteTest):
    def test_attrs_visible_attr_no_replace(self):
        """Make sure "visible" attribute is not rendered as boolean (true/false) in HTML after processing."""
        # TODO: Maybe it could/should be, but it requires some TS changes, so let's leave it like this as it has been.
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {area=a visible=%%true%%}

#-
test

#- {area_end=a}

#- {visible=%%true%%}
test2
        """)
        r = self.get(d.url, as_tree=True)
        pars: list[HtmlElement] = r.cssselect('.par')
        self.assertEqual({'area': 'a', 'visible': '%%true%%'}, json.loads(pars[0].attrib['attrs']))
        self.assertEqual({'visible': '%%true%%'}, json.loads(pars[-1].attrib['attrs']))
