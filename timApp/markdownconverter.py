"""Provides functions for converting markdown-formatted text to HTML."""
from contracts import contract

from dumboclient import call_dumbo


@contract
def md_to_html(text: str) -> str:
    """
    Converts the specified markdown text to HTML.

    :type text: str
    :param text: The text to be converted.
    """

    return call_dumbo([text])[0]


@contract
def md_list_to_html_list(texts: 'list(str)') -> 'list(str)':
    """
    Converts the specified list of markdown texts to an HTML list.

    :type texts: list[str]
    :param texts:
    """

    return call_dumbo(texts)
