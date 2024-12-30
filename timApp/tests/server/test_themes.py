from os.path import basename
from typing import Optional
from urllib.parse import urlparse, ParseResult

from lxml.html import HtmlElement

from timApp.document.docentry import DocEntry
from timApp.document.documents import import_document_from_file
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.preferences import Preferences
from timApp.user.settings.style_utils import OFFICIAL_STYLES_PATH
from timApp.user.usergroup import UserGroup
from timApp.util.utils import static_tim_doc


def get_theme_style_name(html: HtmlElement, style_origin: str) -> str | None:
    s: list[HtmlElement] = html.cssselect(
        f"link[rel='stylesheet'][data-style-origin='{style_origin}']"
    )
    if not s:
        return None
    href: str = s[0].attrib["href"]
    url: ParseResult = urlparse(href)
    return basename(url.path)


class DocThemesTest(TimRouteTest):
    def test_theme_overrides(self):
        import_document_from_file(
            static_tim_doc("style_docs/lighttheme.md"),
            f"{OFFICIAL_STYLES_PATH}/lighttheme",
            UserGroup.get_anonymous_group(),
            title="lighttheme",
        )

        import_document_from_file(
            static_tim_doc("style_docs/hide_focus.md"),
            f"{OFFICIAL_STYLES_PATH}/hide_focus",
            UserGroup.get_anonymous_group(),
            title="hide_focus",
        )

        db.session.commit()

        self.login_test1()

        light_theme_doc = DocEntry.find_by_path(f"{OFFICIAL_STYLES_PATH}/lighttheme")
        self.current_user.set_prefs(Preferences(style_doc_ids=[light_theme_doc.id]))
        db.session.commit()

        t = self.get("/", as_tree=True)
        personal_theme = get_theme_style_name(t, "user-prefs-style")
        self.assertIsNotNone(personal_theme)
        self.assertNotEqual(personal_theme, "default.css")

        d = self.create_doc()
        d.document.set_settings(
            {"themes": ["hide_focus"], "override_user_themes": True}
        )
        t = self.get(d.url, as_tree=True)
        self.assertIsNone(get_theme_style_name(t, "user-prefs-style"))
        doc_theme1 = get_theme_style_name(t, "document-style")
        self.assertNotEqual(personal_theme, doc_theme1)

        d.document.set_settings(
            {"themes": ["hide_focus"], "override_user_themes": False}
        )
        t = self.get(d.url, as_tree=True)
        # Wile we don't override the theme, we move user themes to document themes for the document
        self.assertIsNone(get_theme_style_name(t, "user-prefs-style"))
        doc_theme2 = get_theme_style_name(t, "document-style")
        self.assertNotEqual(personal_theme, doc_theme1)
        self.assertNotEqual(doc_theme1, doc_theme2)
