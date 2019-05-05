# -*- coding: utf-8 -*-
import lxml
import lxml.etree
from lxml.html import fromstring, tostring
from lxml.html.clean import Cleaner

TIM_SAFE_TAGS = [
    'a',
    'abbr',
    'acronym',
    'aside',
    'b',
    'blockquote',
    'button',
    'code',
    'em',
    'figcaption',
    'figure',
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
    'hr',
    'input',
    'label',
    'login-menu',
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
    's',
    'style',
    'tim-plugin-loader',
    # plugin components:
    'cs-comtest-runner',
    'cs-comtest-runner-input',
    'cs-console',
    'cs-jypeli-runner',
    'cs-jypeli-runner-input',
    'cs-parsons-runner',
    'cs-runner',
    'cs-runner-input',
    'cs-sage-runner',
    'cs-simcir-runner',
    'cs-tauno-runner',
    'cs-tauno-runner-input',
    'cs-text-runner',
    'dropdown-runner',
    'feedback-runner',
    'imagex-runner',
    'jsav-runner',
    'list-video-runner',
    'mcq',
    'mmcq',
    'pali-runner',
    'savearea-runner',
    'small-video-runner',
    'textfield-runner',
    'video-runner',
]

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
    'size', 'span', 'src', 'start', 'style', 'summary', 'tabindex', 'target', 'title',
    'type', 'usemap', 'valign', 'value', 'vspace', 'width', 'controls', 'plugin',
    'json', 'data-plugin', 'data-answer-id', 'answer-id', 'task-id', 'placeholder', 'data-html'])

c = Cleaner(
    allow_tags=TIM_SAFE_TAGS,
    comments=False,
    forms=False,
    remove_unknown_tags=False,
    safe_attrs=TIM_SAFE_ATTRS,
)


# NOTE: lxml cleaner is a LOT faster than bleach.
def sanitize_html(html_string):
    try:
        doc = fromstring(html_string)
        c(doc)
        cleaned = tostring(doc, encoding='ascii').decode('ascii')
        return strip_div(cleaned)
    except lxml.etree.ParserError:  # Thrown if the HTML string is empty
        return ""
    except lxml.etree.XMLSyntaxError:  # Not yet sure why thrown
        return ""
    except ValueError:  # Thrown if XML has an encoding declaration
        return ""


def strip_div(s: str):
    if s.startswith('<div>') and s.endswith('</div>'):
        return s[5:-6]
    else:
        return s
