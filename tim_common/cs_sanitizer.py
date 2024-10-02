"""
Functions for HTML sanitization used by csplugin.
"""
from typing import Optional

import html5lib
from html5lib.constants import namespaces
from html5lib.filters import sanitizer
from html5lib.serializer import HTMLSerializer
from lxml.html.clean import Cleaner

from tim_common.html_sanitize import sanitize_html, sanitize_with_cleaner


cs_allowed_tags = [
    "em",
    "strong",
    "tt",
    "a",
    "b",
    "code",
    "i",
    "kbd",
    "span",
    "li",
    "ul",
    "ol",
]
cs_allowed_attrs = frozenset(
    [
        "href",
        "class",
    ]
)


cs_cleaner = Cleaner(
    allow_tags=cs_allowed_tags,
    comments=False,
    forms=False,
    remove_unknown_tags=False,
    safe_attrs=cs_allowed_attrs,
)


def allow_minimal(s: str | None) -> str | None:
    if not s:
        return s
    return sanitize_with_cleaner(s, cs_cleaner)


cs_svg_tim_allowed_elements = {
    (namespaces["svg"], "feBlend"),
    (namespaces["svg"], "feColorMatrix"),
    (namespaces["svg"], "feGaussianBlur"),
    (namespaces["svg"], "feOffset"),
    (namespaces["svg"], "filter"),
    (namespaces["svg"], "style"),
}

cs_svg_allowed_attributes = {
    (None, "filter"),
    (None, "in"),
    (None, "in2"),
    (None, "lengthAdjust"),
    (None, "mode"),
    (None, "result"),
    (None, "stdDeviation"),
    (None, "textLength"),
}

tim_allowed_css_props = {"stroke-dasharray"}


def svg_sanitize(s: str) -> str:
    dom = html5lib.parseFragment(s, treebuilder="dom")
    walker = html5lib.getTreeWalker("dom")
    stream = walker(dom)
    stream = sanitizer.Filter(stream)
    stream.allowed_elements = set(stream.allowed_elements).union(
        cs_svg_tim_allowed_elements
    )
    stream.allowed_attributes = set(stream.allowed_attributes).union(
        cs_svg_allowed_attributes
    )
    stream.allowed_css_properties = set(stream.allowed_css_properties).union(
        tim_allowed_css_props
    )

    serializer = HTMLSerializer(quote_attr_values="always")
    s = serializer.render(stream)
    return s


def cs_min_sanitize(s: str) -> str:
    if s.find("<svg") >= 0:
        return svg_sanitize(s)
    s = s.replace("<", "&lt;").replace(">", "&gt;")

    # s = tim_sanitize(s)
    return s


def tim_sanitize(s: str | None) -> str | None:
    if not s:
        return s
    return sanitize_html(s)
