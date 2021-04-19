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
    'tim-login-menu',
    'tim-plugin-loader',
    # plugin components:
    'cs-comtest-runner',
    'cs-comtest-runner-input',
    'cs-console',
    'cs-jsav-runner',
    'cs-jypeli-runner',
    'cs-jypeli-runner-input',
    'cs-parsons-runner',
    'cs-runner',
    'cs-runner-input',
    'cs-sage-runner',
    'tim-geogebra',
    'cs-simcir-runner',
    'cs-tauno-runner',
    'cs-tauno-runner-input',
    'cs-text-runner',
    'dropdown-runner',
    'feedback-runner',
    'drag-runner',
    'imagex-runner',
    'js-runner',
    'mcq',
    'mmcq',
    'pali-runner',
    'tim-multisave',
    'textfield-runner',
    'cbcountfield-runner',
    'cbfield-runner',
    'rbfield-runner',
    'numericfield-runner',
    'goaltable-runner',
    'jsframe-runner',
    'tim-video',
    'importdata-runner',
    'tim-table',

    # raw AngularJS components:
    'tim-rights-editor',
    'tim-self-expire',
    'tim-mark-all-as-read',
    'tim-add-member',
    'tim-goto-link',
    'tim-graph-viz',
    'tim-variables',
    'tim-message-list-admin',
    'tim-message',
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
    'json', 'data-plugin', 'data-answer-id', 'answer-id', 'task-id', 'placeholder', 'data-html',

    # tim-rights-editor
    'item-id', 'allow-select-action', 'barcode-mode', 'restrict-rights', 'hide-remove',
    'hide-edit', 'hide-expire', 'confirm-expire',
    'force-duration', 'force-duration-start', 'force-duration-end', 'force-confirm',

    # tim-self-expire
    'button-text', 'confirm',

    # tim-table
    'bind-data',

    # tim-add-member
    'group',

    # tim-goto-link
    'auto-open',
    'check-unsaved',
    'close-at',
    'countdown-text',
    'is-button',
    'max-wait',
    'open-at',
    'past-due-text',
    'reset-time',
    'stop-after-countdown',
    'time-lang',
    'unauthorized-text',
    'unsaved-changes-text',
    'wait-text',

    # viz and vars:
    'usercode',
    'vizcmd',
    'height',
    'jsparams',

])

c_no_style = Cleaner(
    allow_tags=TIM_SAFE_TAGS,
    comments=False,
    forms=False,
    remove_unknown_tags=False,
    safe_attrs=TIM_SAFE_ATTRS,
)

c_with_styles = Cleaner(
    allow_tags=TIM_SAFE_TAGS + ['style'],
    comments=False,
    forms=False,
    remove_unknown_tags=False,
    safe_attrs=TIM_SAFE_ATTRS,
)


# NOTE: lxml cleaner is a LOT faster than bleach.
def sanitize_html(html_string: str, allow_styles: bool = False) -> str:
    cleaner = c_with_styles if allow_styles else c_no_style
    try:
        doc = fromstring(html_string)
        cleaner(doc)
        cleaned = tostring(doc, encoding='ascii').decode('ascii')
        return strip_div(cleaned)
    except lxml.etree.ParserError:  # Thrown if the HTML string is empty
        return ""
    except lxml.etree.XMLSyntaxError:  # Not yet sure why thrown
        return ""
    except ValueError:  # Thrown if XML has an encoding declaration
        return ""


def strip_div(s: str) -> str:
    if s.startswith('<div>') and s.endswith('</div>'):
        return s[5:-6]
    else:
        return s
