"""
Functions for calling pandoc and constructing the calls
"""
import os
import tempfile
from typing import Optional, List
import re
import sys

import pypandoc
from flask import jsonify

from accesshelper import has_view_access
from documentmodel.documentversion import DocumentVersion
from timdb.documents import Documents
from timdb.models.docentry import DocEntry
from timdb.models.folder import Folder
from timdb.models.printeddoc import PrintedDoc
from timdb.models.user import User
from timdb.printsettings import PrintFormat
from timdb.tim_models import db

FILES_ROOT = os.path.abspath('tim_files')
DEFAULT_PRINTING_FOLDER = os.path.join(FILES_ROOT, 'printed_documents')
TEMPORARY_PRINTING_FOLDER = os.path.join(DEFAULT_PRINTING_FOLDER, 'tmp')
TEMPLATES_FOLDER = os.path.join('Templates', 'Printing')
DEFAULT_TEMPLATE_NAME = 'Default'

class PrintingError(Exception):
    pass


class DocumentPrinter:

    def __init__(self, doc_entry: DocEntry, template_to_use: DocEntry):
        self._doc_entry = doc_entry
        if template_to_use is None:
            template_to_use = DocumentPrinter.get_default_template(doc_entry)
        self._template_to_use = template_to_use

    @property
    def _content(self) -> str:
        """
        Gets the content of the DocEntry assigned for this DocumentPrinter object.
        Fetches the markdown for the documents paragraphs, checks whether the
        paragraph should be printed (determined by a boolean 'print'-attribute,
        and returns the markdown for all the paragraphs that should be printed.

        Returns the (markdown) contents of the file as a single string, as that's the
        format pypandoc likes to handle.

        :return: The TIM documents contents in markdown format. Excludes the paragraphs that have attribute
                 print="false"
        """

        pars_to_print = []

        for par in self._doc_entry.document.get_paragraphs():
            print_flag = par.get_attr('print')

            # print("Attritube 'print' of par %s has value %s" % (par.get_id(), print_flag))
            if print_flag is not None and str.lower(print_flag) is not 'false':
                continue

            pars_to_print.append(par.get_markdown())

        content = '\n\n'.join(pars_to_print)
        # print(content)
        return content

    @property
    def _images_root(self) -> str:
        return os.path.join(os.path.abspath(FILES_ROOT), 'blocks')

    def write_to_format(self, target_format: PrintFormat) -> bytearray:
        """
        Converts the document to latex and returns the converted document as a bytearray
        :param target_format: The target file format
        :return: Converted document as bytearray
        """

        #### TEST SECTION

        toc = False

        ####

        output_bytes = None

        with tempfile.NamedTemporaryFile(suffix='.latex', delete=True) as template_file,\
             tempfile.NamedTemporaryFile(suffix='.' + target_format.value, delete=True) as output_file:

            if self._template_to_use is None:
                raise PrintingError("No template chosen for the printing. Printing was cancelled.")

            template_content = DocumentPrinter.parse_template_content(self._template_to_use)
            #print("template-content:\n\n" + template_content)

            if template_content is None:
                raise PrintingError("The content in the template document %s is not valid." % self._template_to_use.path)

            self._template_doc = self._template_to_use
            template_file.write(bytearray(template_content, encoding='utf-8'))
            # template_file.seek(0)
            # print("Wrote template:\n %s" % template_file.read().decode(encoding='utf-8'))
            # print("Printing to format: %s" % target_format.value)

            # print("Image root set @ %s" % self._images_root)
            # print("####### SYSPATH:\n" + "\n".join(sys.path))

            # print("pypandoc module filename:\n" + pypandoc.__file__)
            # print("####### PANDOCFILTERS INSTALLED:\n")
            # print("pandocfilters" in sys.modules)

            # TODO: getting the path could probably be done with more finesse
            filters = [os.path.join(os.getcwd(), "pandoc-inlinestylesfilter.py")]
            # print("Python path is: %r" % sys.path)
            # print("Flask installed: %s" % ("flask" in sys.modules))
            # print("Pandocfilters installed: %s" % ("pandocfilters" in sys.modules))
            # print("Pip freeze:\n%s" % os.system("pip list"))
            # rint("Pip3 freeze:\n%s" % os.system("pip3 list"))
            try:
                pypandoc.convert_text(source=self._content,
                                      format='markdown',
                                      to=target_format.value,
                                      outputfile=output_file.name,
                                      extra_args=['--template=' + template_file.name,
                                                  '-V', 'graphics-root:' + self._images_root],
                                      filters=filters)
                template_file.seek(0)
                output_bytes = bytearray(output_file.read())
            except Exception as ex:
                # print(str(ex))

                # TODO: logging of errors
                # Might be a good idea to log these?
                # might also be a good idea to separate between errors that should be shown to the user, and
                # ones that only get written to the log file.

                # TODO: selection of errors that should be routed to the UI

                raise PrintingError(str(ex))

        return output_bytes

    def get_print_path(self, file_type: PrintFormat, temp: bool = True) -> str:
        """
        Formulates the printing path for the given document

        :param file_type: File format for the output
        :param temp: 
        :return: 
        """

        doc_version = self._doc_entry.document.get_latest_version().get_version()
        print("Document id: %s, version (%s, %s)" % (self._doc_entry.document.doc_id, doc_version[0], doc_version[1]))
        path = os.path.join(TEMPORARY_PRINTING_FOLDER if temp else DEFAULT_PRINTING_FOLDER,
                            self._doc_entry.name + "." + file_type.value)

        return path

    @staticmethod
    def _recurse_search_default_template_from_folder(folder: Folder) -> Optional[DocEntry]:
        print("Searching for default template in folder %s%s" % (os.sep, folder.path))
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
    def _recurse_search_default_templates_from_folder(folder: Folder) -> List[DocEntry]:
        # print("Searching for default templates in folder %s%s" % (os.sep, folder.path))

        Folder.query.filter(Folder.location)

        docs_in_folder = folder.get_all_documents()

        template_path = os.path.join(TEMPLATES_FOLDER, DEFAULT_TEMPLATE_NAME)
        template_doc = folder.get_document(template_path)

        if template_doc is not None:
            # print("Found template @ %s" % template_doc.path)
            return [template_doc].append(template_doc)
        else:
            if folder.is_root():
                # print("Reached root...")
                return []
            else:
                return DocumentPrinter._recurse_search_default_template_from_folder(folder=folder.parent)


    @staticmethod
    def get_default_template(doc_entry: DocEntry) -> Optional[DocEntry]:
        doc_folder = Folder.find_by_path(doc_entry.location)
        return DocumentPrinter._recurse_search_default_template_from_folder(doc_folder)

    @staticmethod
    def get_custom_template(doc_entry: DocEntry) -> DocEntry:
        # print("Getting custom template...")
        folder = Folder.find_by_path(doc_entry.location)
        doc_name = os.path.split(doc_entry.name)[1] # get the filename i.e. the last name in path
        # print("Doc name is %s " % doc_name)
        template_path = os.path.join(TEMPLATES_FOLDER, doc_name)
        template_doc = folder.get_document(relative_path=template_path)

        if template_doc is not None:
            # print("Found document template at %s " % template_doc.location)
            return template_doc
        else:
            # print("The document template does not exist. Creating one...")
            default_template = DocumentPrinter.get_default_template(doc_entry=doc_entry)
            if default_template is None:
                raise PrintingError("No default template is defined in the document tree.")
            print("Using default template @ %s to create document template @ %s" % (default_template.path, template_path))
            template_doc = folder.get_document(relative_path=template_path, create_if_not_exist=True)
            template_doc.document.update(default_template.document.export_markdown(),
                                         template_doc.document.export_markdown())
            return template_doc

    @staticmethod
    def get_user_templates(doc_entry: DocEntry, current_user: User) -> List[DocEntry]:
        templates = []

        if doc_entry is None or current_user is None:
            raise PrintingError("You need to supply both the DocEntry and User to fetch the printing templates.")

        print("Searching for user's own templates")
        path = os.path.join(current_user.get_personal_folder().get_full_path(), TEMPLATES_FOLDER)

        print("Trying path " + path)
        templates_folder = Folder.find_by_path(path)

        if templates_folder is not None and has_view_access(templates_folder.id):

            print("Looking into the template folder " + templates_folder.get_full_path())

            docs = templates_folder.get_all_documents()

            if docs is not None:
                for d in docs:
                    if has_view_access(d.id):
                        print("Found the document " + d.name)
                        templates.append(d)

        return templates

    @staticmethod
    def get_all_templates(doc_entry: DocEntry, current_user: User) -> List[DocEntry]:
        templates = []

        if doc_entry is None or current_user is None:
            raise PrintingError("You need to supply both the DocEntry and User to fetch the printing templates.")


        # print("Crawling up the document tree searching for all accessible templates")
        current_folder = doc_entry.parent
        while (current_folder is not None):

            # print("Searching for templates in " + current_folder.get_full_path())
            path = os.path.join(current_folder.get_full_path(),
                                TEMPLATES_FOLDER)

            # print("Trying path " + path)
            templates_folder = Folder.find_by_path(path)

            if templates_folder is not None and has_view_access(templates_folder.id):

                # print("Looking into the template folder " + templates_folder.get_full_path())

                docs = templates_folder.get_all_documents()

                if docs is None:
                    continue

                for d in docs:
                    if has_view_access(d.id):
                        # print("Found the document " + d.name)
                        templates.append(d)

            current_folder = current_folder.parent

        return templates


    @staticmethod
    def get_templates_as_dict(doc_entry: DocEntry, current_user: User):

        try:
            user_templates = DocumentPrinter.get_all_templates(doc_entry=doc_entry, current_user=current_user)
            all_templates = DocumentPrinter.get_all_templates(doc_entry=doc_entry, current_user=current_user)
        except PrintingError as err:
            raise PrintingError(str(err))

        default_templates = list(set(all_templates) - set(user_templates))

        templates_dict = {}

        user_templates_list = []
        for t in user_templates:
            user_templates_list.append({'doc_id':t.id,'doc_path':t.name})

        default_templates_list = []
        for t in default_templates:
            default_templates_list.append({'doc_id':t.id,'doc_path':t.name})

        templates_dict.update({'userTemplates': user_templates_list})
        templates_dict.update({'defaultTemplates': default_templates_list})

        return templates_dict


    @staticmethod
    def remove_block_markers(template_md: str) -> str:
        out = []
        p = re.compile('```')
        for line in template_md.splitlines():
            if p.match(line) is None:
                out.append(line)

        return "\n".join(out)

    @staticmethod
    def parse_template_content(template_doc: DocEntry) -> str:
        pars = []
        for par in template_doc.document.get_paragraphs():
            if par.get_attr('printing_template') is not None:
                par_content = par.get_markdown()
                pars.append(DocumentPrinter.remove_block_markers(par_content))

        return "\n\n".join(pars)

    def get_document_version_as_float(self) -> float:
        doc_v = self._doc_entry.document.get_latest_version().get_version()
        doc_v_fst, doc_v_snd = doc_v[0], doc_v[1]
        return doc_v_fst + doc_v_snd/10

    def get_template_version_as_float(self) -> Optional[float]:
        if self._template_to_use is None:
            return None

        doc_v = self._template_to_use.document.get_latest_version().get_version()
        doc_v_fst, doc_v_snd = doc_v[0], doc_v[1]
        return doc_v_fst + doc_v_snd/10

    def get_printed_document_path_from_db(self, file_type: PrintFormat) -> Optional[str]:
        current_doc_version = self.get_document_version_as_float()
        current_template_version = self.get_template_version_as_float()

        existing_print = db.session.query(PrintedDoc). \
            filter(PrintedDoc.doc_id == self._doc_entry.id).\
            filter(PrintedDoc.template_doc_id == self._template_to_use.id).\
            filter(PrintedDoc.file_type == file_type.value).\
            filter(PrintedDoc.doc_version == current_doc_version).\
            filter(PrintedDoc.template_doc_version == current_template_version).\
            first()

        if existing_print is None or not os.path.exists(existing_print.path_to_file):
            return None

        return existing_print.path_to_file
