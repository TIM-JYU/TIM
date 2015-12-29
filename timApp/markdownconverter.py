"""Provides functions for converting markdown-formatted text to HTML."""
import re
from copy import copy

from contracts import contract
from jinja2 import Environment
from lxml import html

from dumboclient import call_dumbo
from htmlSanitize import sanitize_html


def has_macros(text, macros, macro_delimiter=None):
    return macro_delimiter is not None and len(macros) > 0 and macro_delimiter in text


def expand_macros_regex(text, macros, macro_delimiter=None):
    if not has_macros(text, macros, macro_delimiter):
        return text
    return re.sub('{0}([a-zA-Z]+){0}'.format(re.escape(macro_delimiter)),
                  lambda match: macros.get(match.group(1), 'UNKNOWN MACRO: ' + match.group(1)),
                  text)


def expand_macros_jinja2(text, macros, macro_delimiter=None):
    if not has_macros(text, macros, macro_delimiter):
        return text
    env = Environment(variable_start_string=macro_delimiter,
                      variable_end_string=macro_delimiter,
                      comment_start_string='{#',
                      comment_end_string='#}',
                      block_start_string='{%',
                      block_end_string='%}',
                      lstrip_blocks=True,
                      trim_blocks=True)
    return env.from_string(text).render(macros)


expand_macros = expand_macros_jinja2
#expand_macros = expand_macros_regex

@contract
def md_to_html(text: str,
               sanitize: bool=True,
               macros: 'dict(str:str)|None'=None,
               macro_delimiter=None,
               auto_macros=None,
               auto_number_headings=None) -> str:
    """
    Converts the specified markdown text to HTML.

    :param sanitize: Whether the HTML should be sanitized. Default is True.
    :type text: str
    :param text: The text to be converted.
    """

    text = expand_macros(text, macros, macro_delimiter)

    raw = call_dumbo([text])

    if auto_macros:
        raw[0] = insert_heading_numbers(auto_macros, raw[0], auto_number_headings)
    if sanitize:
        return sanitize_html(raw[0])
    else:
        return raw[0]


@contract
def md_list_to_html_list(pars,
                         settings,
                         sanitize: bool=True,
                         auto_macros=None
                         ):
    """
    Converts the specified list of markdown texts to an HTML list.

    :param sanitize: Whether the HTML should be sanitized. Default is True.
    :type pars: list[DocParagraph]
    :param pars: The list of markdown texts to be converted.
    """
    #from time import time

    #t0 = time()
    texts = [expand_macros(p.get_markdown(), settings.get_macros(), settings.get_macro_delimiter()) for p in pars]
    #t1 = time()
    #print("expand_macros for {} paragraphs took {} seconds.".format(len(texts), t1 - t0))

    #t0 = time()
    raw = call_dumbo(texts)
    #t1 = time()
    #print("Dumbo call for {} paragraphs took {} seconds.".format(len(texts), t1 - t0))

    if auto_macros:
        processed = []
        for pre_html, m, attrs in zip(raw, auto_macros, (p.get_attrs() for p in pars)):
            if 'nonumber' in attrs.get('classes', {}):
                final_html = pre_html
            else:
                final_html = insert_heading_numbers(m,
                                                    pre_html,
                                                    settings.auto_number_headings(),
                                                    settings.heading_format())
            processed.append(final_html)
        raw = processed

    if sanitize:
        return [sanitize_html(p) for p in raw]
    else:
        return raw


def insert_heading_numbers(auto_macros, pre_html, auto_number_headings=True, heading_format=''):
    tree = html.fragment_fromstring(pre_html, create_parent=True)
    deltas = auto_macros['h']
    used = auto_macros['usedh']
    for e in tree.iterchildren():
        hcount = used.get(e.text, 0)
        if hcount > 0:
            e.attrib['id'] += '-' + str(hcount)
        if auto_number_headings and e.tag in HEADING_TAGS:
            level = int(e.tag[1])
            deltas[level] += 1
            for i in range(level + 1, 7):
                deltas[i] = 0
            for i in range(6, 0, -1):
                if deltas[i] != 0:
                    break
            hvals = {'h1': '', 'h2': '', 'h3': '', 'h4': '', 'h5': '', 'h6': ''}
            for i in range(1, i + 1):
                hvals['h' + str(i)] = deltas[i]
            try:
                heading_num = heading_format.format(**hvals)
                # The heading number is usually of the form '1.2.3....' so we want to strip those extra dots
                last_format_char = heading_format[-1]
                heading_num = heading_num.rstrip(heading_num[-1])
                if last_format_char != '}':
                    heading_num += last_format_char
            except (KeyError, ValueError, IndexError) as ex:
                heading_num = '[ERROR]'

            e.text = heading_num + ' ' + e.text
    final_html = ''.join(map(lambda x: html.tostring(x).decode('utf-8'), tree.iterchildren()))
    return final_html


HEADING_TAGS = {'h1', 'h2', 'h3', 'h4', 'h5', 'h6'}
