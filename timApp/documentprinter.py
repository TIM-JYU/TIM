"""
Functions for calling pandoc and constructing the calls
"""
import os
import tempfile
from typing import List, Tuple, Optional

import pypandoc

from timdb.models.docentry import DocEntry
from timdb.models.folder import Folder
from timdb.printsettings import PrintSettings
from timdb.printsettings import PrintFormat

TEMPLATES_FOLDER = 'Templates'
DEFAULT_TEMPLATE_NAME = 'default'

class PrintingError(Exception):
    pass


class DocumentPrinter:
    default_files_root = 'tim_files'
    default_printing_folder = 'printed_documents'
    temporary_docs_folder = 'temp'

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

    def _write_latex(self) -> bytearray:
        """
        Converts the document to latex and returns the converted document as a bytearray
        :return: Converted document as bytearray
        """

        as_latex = pypandoc.convert_text(source=self._content, format='markdown', to='latex')

        return bytearray(as_latex, encoding='utf-8')

    def _write_pdf(self) -> bytearray:
        """
        Converts the document to pdf and returns the converted document as a bytearray
        :return: Converted document as bytearray
        """

        tmp_file = tempfile.NamedTemporaryFile(suffix='.pdf', delete=True)

        template_path = os.path.join(self.default_files_root,
                                     self.default_printing_folder,
                                     'basic_template.latex')
            # TODO sensible handling of templates
        images_root = os.path.join(self.default_files_root, 'blocks')
            # TODO sensible locating of images root folder

        try:
            pypandoc.convert_text(source=self._content,
                                  format='markdown',
                                  to='pdf',
                                  outputfile=tmp_file.name,
                                  extra_args=['--template=' + template_path, '-V', 'graphics-root:' + images_root])
            bytarr = bytearray(tmp_file.read())
            return bytarr
        # TODO: Except block to handle exceptions
        finally:
            tmp_file.close()

    def write_to_format(self, file_type: PrintFormat) -> bytearray:
        if file_type == PrintFormat.LATEX:
            return self._write_latex()
        elif file_type == PrintFormat.PDF:
            return self._write_pdf()

    def get_print_path(self, file_type: PrintFormat, temp: bool = True) -> str:
        """
        Formulates the printing path for the given document

        :param file_type: File format for the output
        :param temp: 
        :return: 
        """

        path = os.path.join(self.default_files_root,
                            self.default_printing_folder,
                            self.temporary_docs_folder if temp else "",
                            self._doc_entry.name + "." + file_type)

        return path

    @staticmethod
    def _recurse_search_default_template_from_folder(folder: Folder) -> Optional[DocEntry]:
        print("Searching for default template in folder %s" % folder.path)
        template_path = os.path.join(TEMPLATES_FOLDER, DEFAULT_TEMPLATE_NAME)
        template_doc = folder.get_document(template_path)

        if template_doc is not None:
            print("Found template @ %s" % template_doc.path)
            return template_doc
        else:
            if folder.is_root():
                print("Reached root...")
                return None
            else:
                return DocumentPrinter._recurse_search_default_template_from_folder(folder=folder.parent)

    @staticmethod
    def get_default_template(doc_entry: DocEntry) -> Optional[DocEntry]:
        doc_folder = Folder.find_by_path(doc_entry.location)
        return DocumentPrinter._recurse_search_default_template_from_folder(doc_folder)

    @staticmethod
    def get_custom_template(doc_entry: DocEntry) -> DocEntry:
        print("Getting custom template...")
        folder = Folder.find_by_path(doc_entry.location)
        doc_name = os.path.split(doc_entry.name)[1] # get the filename i.e. the last name in path
        print("Doc name is %s " % doc_name)
        template_path = os.path.join(TEMPLATES_FOLDER, doc_name)
        template_doc = folder.get_document(relative_path=template_path)

        if template_doc is not None:
            print("Found document template at %s " % template_doc.location)
            return template_doc
        else:
            print("The document template does not exist. Creating one...")
            default_template = DocumentPrinter.get_default_template(doc_entry=doc_entry)
            if default_template is None:
                raise PrintingError("No default template is defined in the document tree.")
            print("Using default template @ %s to create document template" % default_template.path)
            template_doc = folder.get_document(relative_path=template_path, create_if_not_exist=True)
            template_doc.document.update(default_template.document.export_markdown(),
                                         template_doc.document.export_markdown())
            return template_doc


