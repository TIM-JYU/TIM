import re
from typing import Any

import lxml
import lxml.etree
from lxml.etree import ParserError
from lxml.html import tostring, fragment_fromstring, document_fromstring
from lxml.html.clean import Cleaner

TIM_SAFE_TAGS = [
    "a",
    "abbr",
    "acronym",
    "aside",
    "b",
    "blockquote",
    "button",
    "code",
    "em",
    "figcaption",
    "figure",
    "i",
    "li",
    "ol",
    "strong",
    "ul",
    "video",
    "p",
    "code",
    "div",
    "span",
    "br",
    "pre",
    "img",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "h7",
    "hr",
    "input",
    "label",
    "table",
    "tbody",
    "thead",
    "tfoot",
    "td",
    "tr",
    "th",
    "caption",
    "colgroup",
    "col",
    "sub",
    "sup",
    "u",
    "s",
    "tim-login-menu",
    "tim-plugin-loader",
    # plugin components:
    "cs-comtest-runner",
    "cs-comtest-runner-input",
    "cs-console",
    "cs-jsav-runner",
    "cs-jypeli-runner",
    "cs-jypeli-runner-input",
    "cs-parsons-runner",
    "cs-runner",
    "cs-runner-input",
    "cs-sage-runner",
    "tim-geogebra",
    "cs-simcir-runner",
    "cs-tauno-runner",
    "cs-tauno-runner-input",
    "cs-text-runner",
    "dropdown-runner",
    "feedback-runner",
    "drag-runner",
    "imagex-runner",
    "js-runner",
    "mcq",
    "mmcq",
    "pali-runner",
    "tim-multisave",
    "textfield-runner",
    "cbcountfield-runner",
    "cbfield-runner",
    "rbfield-runner",
    "numericfield-runner",
    "goaltable-runner",
    "jsframe-runner",
    "tim-video",
    "tim-images",
    "importdata-runner",
    "tim-table",
    "group-join",
    "symbolbutton-runner"
    # raw AngularJS components:
    "tim-rights-editor",
    "tim-self-expire",
    "tim-mark-all-as-read",
    "tim-add-member",
    "tim-print-button",
    "tim-goto-link",
    "tim-graph-viz",
    "tim-variables",
    "tim-message-list-admin",
    "tim-message-view",
    "manage-read-receipt",
    "tim-archive-header",
    "tim-archive-footer",
    "tim-style-preview",
    "tim-message-send",
    "tim-notification-options",
    "tim-search-button",
    "tim-user-profile",
    "tim-steps",
    "tim-participant-list",
    "tim-course-manager",
    "tim-todo",
    "tim-badge",
    "tim-badge-creator",
    "tim-badge-viewer",
    "tim-badge-giver",
    "tim-badge-withdraw",
    "tim-group-name",
]

TIM_SAFE_ATTRS_MAP = {
    "*": ["class", "id", "align"],
    "video": ["src", "controls"],
    "abbr": ["title"],
    "acronym": ["title"],
    "img": ["src", "width", "height"],
    "a": ["href", "title", "target"],
}

TIM_SAFE_ATTRS = frozenset(
    [
        "abbr",
        "accept",
        "accept-charset",
        "accesskey",
        "action",
        "align",
        "alt",
        "axis",
        "border",
        "cellpadding",
        "cellspacing",
        "char",
        "charoff",
        "charset",
        "checked",
        "cite",
        "class",
        "clear",
        "cols",
        "colspan",
        "color",
        "compact",
        "coords",
        "datetime",
        "dir",
        "disabled",
        "enctype",
        "for",
        "frame",
        "headers",
        "height",
        "href",
        "hreflang",
        "hspace",
        "id",
        "ismap",
        "label",
        "lang",
        "longdesc",
        "maxlength",
        "media",
        "method",
        "multiple",
        "name",
        "nohref",
        "noshade",
        "nowrap",
        "prompt",
        "readonly",
        "rel",
        "rev",
        "rows",
        "rowspan",
        "rules",
        "scope",
        "selected",
        "shape",
        "size",
        "span",
        "src",
        "start",
        "style",
        "summary",
        "tabindex",
        "target",
        "title",
        "type",
        "usemap",
        "valign",
        "value",
        "vspace",
        "width",
        "controls",
        "plugin",
        "json",
        "plugin-type",
        "data-answer-id",
        "answer-id",
        "task-id",
        "placeholder",
        "data-html",
        # tim-rights-editor
        "item-id",
        "allow-select-action",
        "barcode-mode",
        "restrict-rights",
        "hide-remove",
        "hide-edit",
        "hide-expire",
        "confirm-expire",
        "force-duration",
        "force-duration-start",
        "force-duration-end",
        "force-confirm",
        # tim-self-expire
        "button-text",
        "confirm",
        "redirect-href",
        "set-field",
        # tim-table
        "bind-data",
        # tim-add-member
        # tim-group-name
        "group",
        # tim-goto-link
        "auto-open",
        "check-unsaved",
        "close-at",
        "countdown-text",
        "is-button",
        "max-wait",
        "open-at",
        "past-due-text",
        "reset-time",
        "stop-after-countdown",
        "no-countdown",
        "time-lang",
        "unauthorized-text",
        "unsaved-changes-text",
        "wait-text",
        # viz and vars:
        "usercode",
        "vizcmd",
        "height",
        "jsparams",
        # tim-message-list-admin
        "list",
        # tim-archive-header
        # tim-archive-footer
        "message",
        "ng-non-bindable",
        # tim-message-send
        "send-global",
        # tim-search-button
        "folder",
        "button-text",
        "wrapper",
        "document-id",
        "profile-id",
        "view-mode",
        "badgegroup-context",
        "badgeuser-context",
    ]
)

c_no_style = Cleaner(
    allow_tags=TIM_SAFE_TAGS,
    comments=False,
    forms=False,
    remove_unknown_tags=False,
    safe_attrs=TIM_SAFE_ATTRS,
)

c_with_styles = Cleaner(
    allow_tags=TIM_SAFE_TAGS + ["style"],
    comments=False,
    forms=False,
    remove_unknown_tags=False,
    safe_attrs=TIM_SAFE_ATTRS,
)


# NOTE: lxml cleaner is a LOT faster than bleach.
def sanitize_html(html_string: str, allow_styles: bool = False) -> str:
    cleaner = c_with_styles if allow_styles else c_no_style
    return sanitize_with_cleaner(html_string, cleaner)


# Copied from LXML to match the same pattern
_replace_css_import = re.compile(r"@\s*import", re.I).sub
_css_style_prefix = "<style type='text/css'>"
_css_style_suffix = "</style>"
_css_style_prefix_len = len(_css_style_prefix)
_css_style_suffix_len = len(_css_style_suffix)


def sanitize_css(css_string: str, allow_imports: bool = False) -> str:
    """
    Sanitizes the given CSS style string.
    :param css_string: Style string to sanitize.
    :param allow_imports: Whether to allow @import statements.
                          Note that this means that users can try to import something nasty!
    :return: Sanitized CSS
    """
    if allow_imports:
        css_string = _replace_css_import("#import", css_string)
    css_string = sanitize_html(
        f"{_css_style_prefix}{css_string}{_css_style_suffix}", allow_styles=True
    )
    css_string = css_string[_css_style_prefix_len:-_css_style_suffix_len]
    if allow_imports:
        css_string = css_string.replace("#import", "@import")
    return css_string


# Taken from LXML
looks_like_full_html = re.compile(r"^\s*<(?:html|!doctype)", re.I).match

# NOTE: lxml now removes data:image/svg+xml because of possible XSS:
# See: https://github.com/lxml/lxml/commit/f2330237440df7e8f39c3ad1b1aa8852be3b27c0
# However, in TIM, data URLs are used for plugins and Tex2SVG math content
# In our case, we can generally be pretty sure that the data URL contains only SVG
# Moreover, scripts embedded in SVG as data URLs are not executed unless the user opens the image:
# See: https://security.stackexchange.com/a/212960
# This is enough for our use case. Because lxml does not provide a switch to disable removing data URLs
# (or even sanitizing them), we have to do it manually.
replace_data_svg = re.compile(r"data:image/svg\+xml;base64,", re.I).sub
replace_data_escaped = re.compile(r"data:image/escaped;base64,", re.I).sub


def escape_data_svg(svg_string: str) -> str:
    """Converts data:image/svg+xml;base64, to data:image/safe;base64,"""
    return replace_data_svg("data:image/escaped;base64,", svg_string)


def unescape_data_svg(svg_string: str) -> str:
    """Converts data:image/safe;base64, back to data:image/svg+xml;base64,"""
    return replace_data_escaped("data:image/svg+xml;base64,", svg_string)


def fromstring(html_string: str) -> Any:
    """
    Parses string into an LXML document or element.
    Unlike LXML's fromstring, calls document_fromstring or fragment_fromstring, based on whether the string looks
    like a full document, or just a fragment.

    :param html_string: String to parse
    :return: An LXML document
    """
    if looks_like_full_html(html_string):
        return document_fromstring(html_string)
    try:
        return fragment_fromstring(html_string)
    except (
        ParserError,
        TypeError,
    ):  # TypeError is a hack to deal with a bug in lxml
        return fragment_fromstring(html_string, create_parent="div")


def sanitize_with_cleaner(html_string: str, cleaner: Cleaner) -> str:
    try:
        html_string = escape_data_svg(html_string)
        doc = fromstring(html_string)
        cleaner(doc)
        cleaned = tostring(doc, encoding="ascii").decode("ascii")
        cleaned = unescape_data_svg(cleaned)
        return strip_div(cleaned)
    except lxml.etree.ParserError:  # Thrown if the HTML string is empty
        return ""
    except lxml.etree.XMLSyntaxError:  # Not yet sure why thrown
        return ""
    except ValueError:  # Thrown if XML has an encoding declaration
        return ""


def strip_div(s: str) -> str:
    if s.startswith("<div>") and s.endswith("</div>"):
        return s[5:-6]
    else:
        return s


def presanitize_html_body(html_string: str) -> str:
    """
    Apply basic <html> tag sanitization.
    This may be needed in cases where user-given yet un-sanitized HTML is parsed by lxml before proper sanitization.

    :param html_string: HTML to sanitize
    :return: HTML string with <html> tag sanitized in a basic way for LXML to parse it
    """
    return html_string.replace("<html", "&lt;html")
