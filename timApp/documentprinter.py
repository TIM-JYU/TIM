"""
Functions for calling pandoc and constructing the calls
"""
import tempfile

import pypandoc

from timdb.models.docentry import DocEntry
from timdb.printsettings import PrintSettings


class PrintingError(Exception):
    pass

class DocumentPrinter():

    def __init__(self, doc_entry: DocEntry, print_settings: PrintSettings):
        self._doc_entry = doc_entry
        self._settings = print_settings

    @property
    def _content(self) -> str:
        """
        Calls for a new system subprocess to run pandoc.

        :param to_format: Specifies the format to which the input is converted
        :param markdown: The documents markdown
        :return: LaTeX produced by pandoc
        """

        content = '\n\n'.join(par.get_markdown() for par in self._doc_entry.document.get_paragraphs())
        # print(content)
        return content


    def _write_tex(self) -> bytearray:
        """
        Converts the document to latex and returns the converted document as a bytearray
        :return: Converted document as bytearray
        """

        as_latex = pypandoc.convert_text(source=self._content, format='markdown', to='latex')

        return bytearray(source=as_latex, encoding='utf-8')

    def _write_pdf(self) -> bytearray:
        """
        Converts the document to pdf and returns the converted document as a bytearray
        :return: Converted document as bytearray
        """

        tmp_file = tempfile.NamedTemporaryFile(suffix='.pdf', delete=True)
        try:
            pypandoc.convert_text(source=self._content, format='markdown', to='pdf', outputfile=tmp_file.name)
            bytarr = bytearray(source=tmp_file.read())
            return bytarr
        # TODO: Except block to handle exceptions
        finally:
            tmp_file.close()

    def write_to_format(self, file_type: str) -> bytearray:
        if file_type == 'latex':
            return self._write_tex()
        elif file_type == 'pdf':
            return self._write_pdf()