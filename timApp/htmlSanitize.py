# -*- coding: utf-8 -*-
import lxml
import lxml.etree
from lxml.html.clean import Cleaner


TIM_SAFE_TAGS = ['a',
                 'abbr',
                 'acronym',
                 'b',
                 'blockquote',
                 'code',
                 'em',
                 'i',
                 'li',
                 'ol',
                 'strong',
                 'ul',
                 'video',
                 'p',
                 'code',
                 'div',
                 'span',
                 'br',
                 'pre',
                 'img',
                 'h1',
                 'h2',
                 'h3',
                 'h4',
                 'h5',
                 'h6',
                 'h7',
                 'table',
                 'tbody',
                 'thead',
                 'tfoot',
                 'td',
                 'tr',
                 'th',
                 'caption',
                 'colgroup',
                 'col',
                 'sub',
                 'sup',
                 'u',
                 's']

TIM_SAFE_ATTRS_MAP = {'*': ['class', 'id', 'align'],
                      'video': ['src', 'controls'],
                      'abbr': ['title'],
                      'acronym': ['title'],
                      'img': ['src', 'width', 'height'],
                      'a': ['href', 'title', 'target']}

TIM_SAFE_ATTRS = frozenset([
    'abbr', 'accept', 'accept-charset', 'accesskey', 'action', 'align',
    'alt', 'axis', 'border', 'cellpadding', 'cellspacing', 'char', 'charoff',
    'charset', 'checked', 'cite', 'class', 'clear', 'cols', 'colspan',
    'color', 'compact', 'coords', 'datetime', 'dir', 'disabled', 'enctype',
    'for', 'frame', 'headers', 'height', 'href', 'hreflang', 'hspace', 'id',
    'ismap', 'label', 'lang', 'longdesc', 'maxlength', 'media', 'method',
    'multiple', 'name', 'nohref', 'noshade', 'nowrap', 'prompt', 'readonly',
    'rel', 'rev', 'rows', 'rowspan', 'rules', 'scope', 'selected', 'shape',
    'size', 'span', 'src', 'start', 'summary', 'tabindex', 'target', 'title',
    'type', 'usemap', 'valign', 'value', 'vspace', 'width', 'controls', 'plugin'])

c = Cleaner(allow_tags=TIM_SAFE_TAGS, remove_unknown_tags=False, safe_attrs=TIM_SAFE_ATTRS)


# NOTE: lxml cleaner is a LOT faster than bleach.
def sanitize_html(html_string):
    try:
        return c.clean_html(html_string)
    except lxml.etree.ParserError:  # Thrown if the HTML string is empty
        return ""
    except lxml.etree.XMLSyntaxError:  # Not yet sure why thrown
        return ""
