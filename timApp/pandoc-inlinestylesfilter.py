#!/usr/bin/env python3

"""
Pandoc filter to convert class values to commands of same name in latex. Leaves
(should leave...) ids, other, predefined classes and key-values intact.
"""

from pandocfilters import toJSONFilter, Span, RawInline


def classes_to_latex_cmds(key, value, fmt, meta):
    if key == 'Span' and fmt == 'latex':
        ([ident, classes, kvs], contents) = value

        #debugging
        #return Span([ident, classes, kvs], contents)

        classes_to_wrap = []
        for c in classes:
            if c not in ["csl-no-emph", "csl-no-strong", "csl-no-smallcaps"]:
                classes_to_wrap.append(c)

        # TODO: should preserve also the aforementioned predef styles

        # TODO: the input 'contents' is a list, output should be a list of inline elements

        content = wrap_with_latex_cmds(contents, classes_to_wrap)

        return Span([ident, list(set(classes) - set(classes_to_wrap)), kvs], content)


def wrap_with_latex_cmds(content, classes_to_wrap):
    if len(classes_to_wrap) <= 0:
        return content
    else:
        #c = classes_to_wrap[0]
        #if len(classes_to_wrap) == 1:
        #    return [latex("\\%s{" % c)] + content + [latex("}")]
        #else:
        #    wrap_with_latex_cmds(content, classes_to_wrap[1:])
        c = classes_to_wrap[0]
        if len(classes_to_wrap) > 1:
            content = wrap_with_latex_cmds(content, classes_to_wrap[1:])
        return [latex("\\%s{" % c)] + content + [latex("}")]

        # return [latex("\\%s{" % c)] + content + [latex("}")]


def latex(content):
    return RawInline('latex', content)

if __name__ == "__main__":
    toJSONFilter(classes_to_latex_cmds)
