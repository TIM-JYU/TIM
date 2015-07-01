"""Provides functions for converting markdown-formatted text to HTML."""
from contracts import contract

from dumboclient import call_dumbo
from htmlSanitize import sanitize_html


@contract
def md_to_html(text: str, sanitize: bool=True) -> str:
    """
    Converts the specified markdown text to HTML.

    :param sanitize: Whether the HTML should be sanitized. Default is True.
    :type text: str
    :param text: The text to be converted.
    """

    raw = call_dumbo([text])
    if sanitize:
        return sanitize_html(raw[0])
    else:
        return raw[0]


@contract
def md_list_to_html_list(texts: 'list(str)', sanitize: bool=True) -> 'list(str)':
    """
    Converts the specified list of markdown texts to an HTML list.

    :param sanitize: Whether the HTML should be sanitized. Default is True.
    :type texts: list[str]
    :param texts: The list of markdown texts to be converted.
    """

    raw = call_dumbo(texts)
    if sanitize:
        return [sanitize_html(p) for p in raw]
    else:
        return raw
