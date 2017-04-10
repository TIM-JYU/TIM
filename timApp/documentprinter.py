"""
Functions for calling pandoc and constructing the calls
"""
import datetime
import os

import pypandoc

from typing import List

from documentmodel.document import Document
from timdb.models.docentry import DocEntry
from timdb.printsettings import PrintSettings
from timdb.tim_models import db, PrintedDoc
from dbaccess import get_timdb


class DocumentPrinter():

    def __init__(self, doc_entry: DocEntry, print_settings: PrintSettings):
        self._doc_entry = doc_entry
        self._settings = print_settings

    def _convert_from_md(self, to_format: str) -> str:
        """
        Calls for a new system subprocess to run pandoc.

        :param to_format: Specifies the format to which the input is converted
        :param markdown: The documents markdown
        :return: LaTeX produced by pandoc
        """

        content = '\n\n'.join(par.get_markdown() for par in self._doc_entry.document.get_paragraphs())

        return pypandoc.convert_text(content, to_format, format='md')

    def _as_latex(self):
        return self._convert_from_md('latex')

    def _as_pdf(self):
        return self._convert_from_md('pdf')

    def write_tex(self) -> bytearray:
        """
        Writes the document as pdf on disk and returns the file path
        :return: Converted document as bytearray
        """

        return bytearray().extend(self._as_latex())
