"""Provides functions for converting markdown-formatted text to HTML."""
import re

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
                      comment_start_string='{!!!',
                      comment_end_string='!!!}',
                      block_start_string='{%',
                      block_end_string='%}',
                      lstrip_blocks=True,
                      trim_blocks=True)
    return env.from_string(text).render(macros)


expand_macros = expand_macros_jinja2


# expand_macros = expand_macros_regex


@contract
def md_to_html(text: str,
               sanitize: bool = True,
               macros: 'dict(str:str)|None' = None,
               macro_delimiter=None) -> str:
    """
    Converts the specified markdown text to HTML.

    :param macros: The macros to use.
    :param macro_delimiter: The macro delimiter.
    :param sanitize: Whether the HTML should be sanitized. Default is True.
    :param text: The text to be converted.
    :return: A HTML string.
    """

    text = expand_macros(text, macros, macro_delimiter)

    raw = call_dumbo([text])

    if sanitize:
        return sanitize_html(raw[0])
    else:
        return raw[0]


def par_list_to_html_list(pars,
                          settings,
                          auto_macros=None
                          ):
    """
    Converts the specified list of DocParagraphs to an HTML list.

    :return: A list of HTML strings.
    :type auto_macros: list(dict)
    :type settings: DocSettings
    :param settings: The document settings.
    :param auto_macros: Currently a list(dict) containing the heading information ('h': dict(int,int) of heading counts
           and 'headings': dict(str,int) of so-far used headings and their counts).
    :type pars: list[DocParagraph]
    :param pars: The list of DocParagraphs to be converted.
    """

    texts = [expand_macros(p.get_markdown(), settings.get_macros(), settings.get_macro_delimiter()) for p in pars]
    raw = call_dumbo(texts)

    if auto_macros:
        processed = []
        for pre_html, m, attrs in zip(raw, auto_macros, (p.get_attrs() for p in pars)):
            if 'nonumber' in attrs.get('classes', {}):
                final_html = pre_html
            else:
                final_html = insert_heading_numbers(pre_html, m, settings.auto_number_headings(),
                                                    settings.heading_format())
            processed.append(final_html)
        raw = processed

    return raw


def insert_heading_numbers(html_str, heading_info, auto_number_headings=True, heading_format=''):
    """
    Applies the given heading_format to the HTML if it is a heading, based on the given heading_info.
    Additionally corrects the id attribute of the heading in case it has been used earlier.

    :param heading_info: A dict containing the heading information ('h': dict(int,int) of heading counts
           and 'headings': dict(str,int) of so-far used headings and their counts).
    :param html_str: The HTML string to be processed.
    :param auto_number_headings: Whether the headings should be formatted at all.
    :param heading_format: A dict(int,str) of the heading formats to be used.
    :return: The HTML with the formatted headings.
    """
    tree = html.fragment_fromstring(html_str, create_parent=True)
    counts = heading_info['h']
    used = heading_info['headings']
    for e in tree.iterchildren():
        hcount = used.get(e.text, 0)
        is_heading = e.tag in HEADING_TAGS
        if hcount > 0 and is_heading:
            e.attrib['id'] += '-' + str(hcount)
        if auto_number_headings and is_heading:
            level = int(e.tag[1])
            counts[level] += 1
            for i in range(level + 1, 7):
                counts[i] = 0
            for i in range(6, 0, -1):
                if counts[i] != 0:
                    break
            values = {'text': e.text}
            # noinspection PyUnboundLocalVariable
            for i in range(1, i + 1):
                values['h' + str(i)] = counts[i]
            try:
                formatted = heading_format[level].format(**values)
            except (KeyError, ValueError, IndexError):
                formatted = '[ERROR] ' + e.text

            e.text = formatted
    final_html = ''.join(map(lambda x: html.tostring(x).decode('utf-8'), tree.iterchildren()))
    return final_html


HEADING_TAGS = {'h1', 'h2', 'h3', 'h4', 'h5', 'h6'}
