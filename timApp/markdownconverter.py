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
        if auto_number_headings:
            raw[0] = insert_heading_numbers(auto_macros, raw[0])
    if sanitize:
        return sanitize_html(raw[0])
    else:
        return raw[0]


@contract
def md_list_to_html_list(texts: 'list(str)',
                         sanitize: bool=True,
                         macros: 'dict(str:str)|None'=None,
                         macro_delimiter=None,
                         auto_macros=None,
                         auto_number_headings=False) -> 'list(str)':
    """
    Converts the specified list of markdown texts to an HTML list.

    :param sanitize: Whether the HTML should be sanitized. Default is True.
    :type texts: list[str]
    :param texts: The list of markdown texts to be converted.
    """
    #from time import time

    #t0 = time()
    texts = [expand_macros(text, macros, macro_delimiter) for text in texts]
    #t1 = time()
    #print("expand_macros for {} paragraphs took {} seconds.".format(len(texts), t1 - t0))

    #t0 = time()
    raw = call_dumbo(texts)
    #t1 = time()
    #print("Dumbo call for {} paragraphs took {} seconds.".format(len(texts), t1 - t0))

    if auto_macros:
        if auto_number_headings:
            processed = []
            for pre_html, m in zip(raw, auto_macros):
                final_html = insert_heading_numbers(m, pre_html)
                processed.append(final_html)
            raw = processed

    if sanitize:
        return [sanitize_html(p) for p in raw]
    else:
        return raw


def insert_heading_numbers(m, pre_html):
    tree = html.fragment_fromstring(pre_html, create_parent=True)
    deltas = m['h']
    for e in tree.iterchildren():
        if e.tag in HEADING_TAGS:
            level = int(e.tag[1])
            deltas[level] += 1
            for i in range(level + 1, 7):
                deltas[i] = 0
            full_heading = ''
            for i in range(1, 7):
                if deltas[i] == 0:
                    break
                full_heading += str(deltas[i]) + '.'
            e.text = full_heading + ' ' + e.text
    final_html = ''.join(map(lambda x: html.tostring(x).decode('utf-8'), tree.iterchildren()))
    return final_html


HEADING_TAGS = {'h1', 'h2', 'h3', 'h4', 'h5', 'h6'}
