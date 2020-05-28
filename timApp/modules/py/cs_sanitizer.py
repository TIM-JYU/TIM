import html5lib
from html5lib.constants import namespaces
from html5lib.filters import sanitizer
from html5lib.filters.sanitizer import allowed_elements, allowed_attributes, allowed_css_properties
from html5lib.serializer import HTMLSerializer
import bleach
import html

'''
"kirjasto cs-pluginissa tarvittaville sanitoinneille
'''

cs_allowed_tags = ['em', 'strong', 'tt', 'a', 'b', 'code', 'i', 'kbd', 'span', 'li', 'ul', 'ol']
cs_allowed_attrs = {
    'a': ['href'],
    'span': ['class']
}


def allow_minimal(s: str) -> str:
    if not s:
        return s
    return bleach.clean(s, cs_allowed_tags, cs_allowed_attrs)

cs_svg_tim_allowed_elements = set(allowed_elements).union({
    (namespaces['svg'], 'feBlend'),
    (namespaces['svg'], 'feColorMatrix'),
    (namespaces['svg'], 'feGaussianBlur'),
    (namespaces['svg'], 'feOffset'),
    (namespaces['svg'], 'filter'),
    (namespaces['svg'], 'style'),
})


cs_svg_allowed_attributes = set(allowed_attributes).union({
    (None, 'filter'),
    (None, 'in'),
    (None, 'in2'),
    (None, 'lengthAdjust'),
    (None, 'mode'),
    (None, 'result'),
    (None, 'stdDeviation'),
    (None, 'textLength'),
})

tim_allowed_css_props = set(allowed_css_properties).union({
    'stroke-dasharray'
})


def svg_sanitize(s: str) -> str:
    dom = html5lib.parseFragment(s, treebuilder="dom")
    walker = html5lib.getTreeWalker("dom")
    stream = walker(dom)
    stream = sanitizer.Filter(stream,
                              allowed_elements=cs_svg_tim_allowed_elements,
                              allowed_attributes=cs_svg_allowed_attributes,
                              allowed_css_properties=tim_allowed_css_props)
    serializer = HTMLSerializer(quote_attr_values='always')
    s = serializer.render(stream)
    return s


def cs_min_sanitize(s: str) -> str:
    if s.find('<svg') >= 0: return svg_sanitize(s);
    s = s.replace('<', '&lt;').replace('>', '&gt;')

    # s = tim_sanitize(s)
    return s

TIM_SAFE_TAGS = ['a',
                 'abbr',
                 'acronym',
                 'b',
                 'blockquote',
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
                 'svg']

TIM_SAFE_ATTRS_MAP = {'*': ['class', 'id', 'align'],
                      'video': ['src', 'controls'],
                      'abbr': ['title'],
                      'acronym': ['title'],
                      'img': ['src', 'width', 'height', 'style', 'title'],
                      'a': ['href', 'title', 'target'],
                      'svg': ['*']
                      }

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
    'type', 'usemap', 'valign', 'value', 'vspace', 'width', 'controls', 'plugin'])

TIM_SAFE_PROTOCOLS=['http', 'https', 'smb', 'data']
TIM_SAFE_STYLES = ['width', 'height', 'vertical-align']


def tim_sanitize(s: str) -> str:
    if not s:
        return s
    return bleach.clean(s, TIM_SAFE_TAGS, TIM_SAFE_ATTRS_MAP, protocols=TIM_SAFE_PROTOCOLS, styles=TIM_SAFE_STYLES)
