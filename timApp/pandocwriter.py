"""
Functions for calling pandoc and constructing the calls
"""

import pypandoc


def call_pandoc(markdown: str, to_format: str) -> str:
    """
    Calls for a new system subprocess to run pandoc.

    :param to_format: Specifies the format to which the input is converted
    :param markdown: The documents markdown
    :return: LaTeX produced by pandoc
    """

    return pypandoc.convert_text(markdown, to_format, format='md')


def get_print_options() -> [str]:
    # TODO: actually get the printing options,
    # needs to have the printing opts implemented in the system first
    return ["-t latex"]

