#!/usr/bin/env python

"""
Pandoc filter to remove numbering from header elements that have the .nonumber class.
"""

from pandocfilters import toJSONFilter, Header, RawInline


def remove_header_numbering(key, value, fmt, meta):
    if key == 'Header' and fmt == 'latex':
        (level, [ident, classes, kvs], contents) = value

        if 'nonumber' in classes:
            classes.remove('nonumber')  # remove the unnecessary TIM specific class
            if not 'unnumbered' in classes:
                classes.append('unnumbered')  # add the class that pandoc understands

        return Header(level, [ident, classes, kvs], contents)

if __name__ == "__main__":
    toJSONFilter(remove_header_numbering)
