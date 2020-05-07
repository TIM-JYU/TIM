from timApp.tests.server.timroutetest import TimRouteTest
from timApp.user.preferences import Preferences
from lxml.html import HtmlElement
from typing import List
from urllib.parse import urlparse, ParseResult
from os.path import basename


def get_theme_style_name(html: HtmlElement):
    s: List[HtmlElement] = html.cssselect("link[rel='stylesheet'][href^='/static/generated/']")
    href: str = s[0].attrib["href"]
    url: ParseResult = urlparse(href)
    return basename(url.path)


class DocThemesTest(TimRouteTest):
    def test_theme_overrides(self):
        self.login_test1()
        self.current_user.set_prefs(Preferences(css_files={
            'bluetheme': True
        }))

        d = self.create_doc()

        d.document.set_settings({
            "themes": ["hide_focus"],
            "override_user_themes": True
        })
        t = self.get(d.url, as_tree=True)
        self.assertEqual("hide_focus.css", get_theme_style_name(t))

        d.document.set_settings({
            "themes": ["hide_focus"],
            "override_user_themes": False
        })
        t = self.get(d.url, as_tree=True)
        self.assertEqual("bluetheme-hide_focus.css", get_theme_style_name(t))
