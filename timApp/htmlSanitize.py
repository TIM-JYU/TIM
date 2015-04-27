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
                 'sup']

TIM_SAFE_ATTRS_MAP = {'*': ['class', 'id', 'align'],
                      'video': ['src', 'controls'],
                      'abbr': ['title'],
                      'acronym': ['title'],
                      'img': ['src', 'width', 'height'],
                      'a': ['href', 'title', 'target']}

TIM_SAFE_ATTRS = ['class',
                  'id',
                  'align',
                  'src',
                  'controls',
                  'title',
                  'width',
                  'height',
                  'href',
                  'target']


# NOTE: lxml cleaner is a LOT faster than bleach.
def sanitize_html(html_string):
    try:
        c = Cleaner(allow_tags=TIM_SAFE_TAGS, remove_unknown_tags=False)
        return c.clean_html(html_string)
    except lxml.etree.ParserError:  # Thrown if the HTML string is empty
        return ""
    except lxml.etree.XMLSyntaxError:  # Not yet sure why thrown
        return ""
