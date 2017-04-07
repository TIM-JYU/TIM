"""
Functions for calling pandoc and constructing the calls
"""
import os

import pypandoc

from documentmodel.document import Document
from timdb import printed_docs
from timdb.models.docentry import DocEntry
from timdb.printed_docs import PrintedDocs
from timdb.printsettings import PrintSettings
from timdb.tim_models import db
from dbaccess import get_timdb


class DocumentPrinter():

    def __init__(self, doc: Document, print_settings: PrintSettings):
        self._document = doc
        self._settings = print_settings

    def _convert_from_md(self, to_format: str) -> str:
        """
        Calls for a new system subprocess to run pandoc.

        :param to_format: Specifies the format to which the input is converted
        :param markdown: The documents markdown
        :return: LaTeX produced by pandoc
        """

        content = '\n\n'.join(par.get_markdown() for par in self._document.get_paragraphs())

        return pypandoc.convert_text(content, to_format, format='md')

    def _as_latex(self):
        return self._convert_from_md('latex')

    def _as_pdf(self):
        return self._convert_from_md('pdf')

    def _hash_settings(self):
        return hash(self._settings)

    def write_pdf(self) -> str:
        """
        Writes the document as pdf on disk and returns the file path
        :return: File path
        """

        doc_id = self._document.doc_id
        doc_name = DocEntry.find_by_id(doc_id).name
        doc_path = os.path.join(get_timdb().blocks_path, 'printed_docs', str(doc_id), doc_name + '.pdf')

        #os.makedirs(os.path.dirname(doc_path))  # TODO: Set mode. Does not take into account if file already exists.

        if os.path.exists(doc_path):
            os.remove(doc_path)
        pypandoc.convert_text(self._as_latex(), to='pdf', format='latex', outputfile=doc_path)

        return doc_path


    def write_tex(self) -> str:
        """
        Writes the document as pdf on disk and returns the file path
        :return: File path
        """

        doc_id = self._document.doc_id
        doc_name = DocEntry.find_by_id(doc_id).name
        doc_path = os.path.join(get_timdb().blocks_path, 'printed_docs', str(doc_id), doc_name + '.tex')

        #os.makedirs(os.path.dirname(doc_path))  # TODO: Set mode. Does not take into account if file already exists.

        if os.path.exists(doc_path):
            os.remove(doc_path)

        with open(doc_path, 'w') as f:
            f.write(self._as_latex())
        f.close()

        return doc_path



