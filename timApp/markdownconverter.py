"""Provides functions for converting markdown-formatted text to HTML."""
import re
from typing import Optional, Dict

from jinja2 import Environment, TemplateSyntaxError
from lxml import html

from dumboclient import call_dumbo
from htmlSanitize import sanitize_html
from utils import get_error_html


def has_macros(text, macros, macro_delimiter=None):
    return macro_delimiter is not None


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
    try:
        return env.from_string(text).render(macros)
    except TemplateSyntaxError as e:
        return get_error_html('Syntax error in template: {}'.format(e))


expand_macros = expand_macros_jinja2


# expand_macros = expand_macros_regex


def md_to_html(text: str,
               sanitize: bool = True,
               macros: Optional[Dict[str, str]] = None,
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

    # Edit html after dumbo
    raw = edit_html_with_own_syntax(raw)

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


# Does changes to html after Dumbo and returns edited html
def edit_html_with_own_syntax(raw: list) -> list:
    index = 0
    while index < len(raw):
        html_text = raw[index]
        raw[index] = make_slide_fragments(html_text)
        #raw[index] = check_and_edit_html_if_surrounded_with(text, fragment_string, change_classes_to_fragment)
        index += 1
    return raw


# Adds the necessary html to make slide fragments work with reveal.js
def make_slide_fragments(html_text: str) -> str:
    # TODO: Make algorithm work with more than 2 levels of fragments
    # TODO: Make different styles of fragments available, possible syntax could be §§{shrink} or something
    # TODO: Refactor to make this more reusable
    # TODO: Make sure that this doesn't break latex conversion

    # Split from fragment area start tag <§
    fragments = html_text.split("&lt;§")
    # If no fragment areas were found we look for fragment pieces
    if len(fragments) < 2:
        new_html = check_and_edit_html_if_surrounded_with(html_text, "§§", change_classes_to_fragment)
        return new_html
    else:
        index = 1
        # For every fragment area
        while index < len(fragments):
            # Try to find area end
            index_of_area_end = fragments[index].find("§&gt;")
            # If not found
            if index_of_area_end == -1:
                # Look for normal fragments
                fragments[index] = check_and_edit_html_if_surrounded_with(fragments[index], "§§", change_classes_to_fragment)
            else:
                # Make a new fragment area if start and end found
                fragments[index] = '<div class="fragment"><p>' + fragments[index]
                fragments[index] = fragments[index].replace("§&gt;", "</div>", 1)
                # Look for inner fragments
                fragments[index] = check_and_edit_html_if_surrounded_with(fragments[index], "§§", change_classes_to_fragment)
            index += 1
        new_html = "".join(fragments)
        return new_html


# Checks if html element's content is surrounded with given string and edits it accordingly
def check_and_edit_html_if_surrounded_with(html_content: str, string_delimeter: str, editing_function) -> str:
    # List of strings after splitting html from
    html_list = html_content.split(string_delimeter)
    if len(html_list) < 2:
        return html_content
    else:
        # Edit the list with given function
        new_html = editing_function(html_list)
    return new_html


def change_classes_to_fragment(html_list: list) -> str:
    """
    If found, html_list[1] will have the content that we need to make a fragment of
    and html_list[0] might have the element tag that will have "fragment" added to it's class.
    There might be multiple fragments in the html list.
    """
    # Start from 1, the previous will contain the html tag to change
    index = 1
    while index < len(html_list):
        # Changes html element's class to fragment
        new_htmls = change_class(html_list[index - 1], html_list[index], "fragment")
        # Apply changes
        html_list[index-1] = new_htmls[0]
        html_list[index] = new_htmls[1]
        index += 2

    # Join the list into a string
    new_html = "".join(html_list)
    return new_html


def change_class(text_containing_html_tag: str, text_content: str, new_class: str) -> list:
    """
    Find the last html tag in the list and change that element's class to new_class
    or add the new class to element's classes or surround the new content with span element with the new class
    """
    try:
        # Find where the html tag supposedly ends
        index_of_tag_end = text_containing_html_tag.rfind(">")
        # Find where the html tag starts
        index_of_tag_start = text_containing_html_tag.rfind("<", 0, index_of_tag_end)
        # If the previous text ends a html tag
        if index_of_tag_end == len(text_containing_html_tag) - 1:
            # Html tag content is between those 2 indices
            html_tag = text_containing_html_tag[index_of_tag_start:index_of_tag_end]
            # Check if element already has atleast one class, if it does then add new_class
            if "class=" in html_tag:
                # Add the new class to html element classes
                index_of_class = html_tag.rfind("class=")
                text_containing_html_tag = text_containing_html_tag[:(
                    index_of_tag_start + index_of_class + 7)] + new_class + " " + text_containing_html_tag[(
                    index_of_tag_start + index_of_class + 7):]
            else:
                # If there isn't class in html tag we add that and the new class
                text_containing_html_tag = text_containing_html_tag[
                                           :index_of_tag_end] + ' class="' + new_class + '"' + text_containing_html_tag[
                                                                                               index_of_tag_end:]
        else:
            text_content = '<span class="' + new_class + '">' + text_content + '</span>'
    # If there is an error we do nothing but return the original text
    except ValueError as e:
        pass
    return [text_containing_html_tag, text_content]


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
