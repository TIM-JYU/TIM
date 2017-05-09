#!/usr/bin/env python

"""
Pandoc filter to convert class values to commands of same name in latex. Leaves
(should leave...) ids, other, predefined classes and key-values intact.
"""

from pandocfilters import toJSONFilter, Span, attributes, Str, RawInline

def classes_to_latex_cmds(key, value, format, meta):
  if key == 'Span' and format == 'latex':
      [[_id, classes, kvs], contents] = value

    _classes_to_wrap = []
    for _class in classes:
        if _class not in ["csl-no-emph", "csl-no-strong", "csl-no-smallcaps"]:
            _classes_to_wrap.append(_class)

    return Span(attributes({'id': _id,'class': ""}),
                           wrap_with_latex_cmds(contents, _classes_to_wrap))


def wrap_with_latex_cmds(element, classes_to_wrap):
    if len(classes_to_wrap) <= 0:
        return ''
    else:
        c = classes_to_wrap[0]
        return '\\%s{%s}' % (c, wrap_with_latex_cmds(classes_to_wrap[1:]))


if __name__ == "__main__":
    toJSONFilter(classes_to_latex_cmds)
