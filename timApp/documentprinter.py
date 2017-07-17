"""
Functions for calling pandoc and constructing the calls
"""
import os
import tempfile
import timeit
from typing import Optional, List
import re
import sys

import pypandoc
from flask import jsonify

from documentmodel.docparagraph import DocParagraph
from documentmodel.document import dereference_pars
from documentmodel.macroinfo import MacroInfo
from markdownconverter import expand_macros, create_environment
from plugin import Plugin, parse_plugin_values
from pluginOutputFormat import PluginOutputFormat
from accesshelper import has_view_access
from dbaccess import get_timdb
from documentmodel.documentversion import DocumentVersion
from pluginControl import pluginify
from sessioninfo import get_current_user_object
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

    def get_content(self, plugins_user_print: bool = False) -> str:
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


        pdoc_plugin_attrs = self._doc_entry.document.get_settings().global_plugin_attrs()
        pdoc_macroinfo = self._doc_entry.document.get_settings().get_macroinfo(get_current_user_object())
        pdoc_macro_delimiter = pdoc_macroinfo.get_macro_delimiter()
        pdoc_macros = pdoc_macroinfo.get_macros()
        pdoc_macro_env = create_environment(pdoc_macro_delimiter)

        # Remove paragraphs that are not to be printed and replace plugin pars,
        # that have a defined 'print' block in their yaml, with the 'print'-blocks content
        pars = self._doc_entry.document.get_paragraphs()
        pars_to_print = []
        for par in pars:

            # do not print document settings pars
            if 'settings' in par.get_attrs():
                continue

            par_classes = par.get_attr('classes')

            if par_classes is not None and 'hidden-print' in par_classes:
                continue

            ppar = par
            # Replace plugin- and question pars with regular docpars with the md defined in the 'print' block
            # of their yaml as the md content of the replacement par
            if par.is_plugin() or par.is_question():
                plugin_yaml = parse_plugin_values(par=par,
                                                  global_attrs=pdoc_plugin_attrs,
                                                  macroinfo=pdoc_macroinfo)
                plugin_yaml_print = plugin_yaml.get('markup').get('print') if (plugin_yaml.get('markup') is not None) else None

                if plugin_yaml_print is not None:
                    ppar = DocParagraph.create(doc=self._doc_entry.document, md=plugin_yaml_print)

            pars_to_print.append(ppar)

        # Dereference pars and render markdown for plugins
        # Only the 1st return value (the pars) is needed here
        par_dicts, _, _, _ = pluginify(doc=self._doc_entry.document,
                                       pars=pars_to_print,
                                       timdb=get_timdb(),
                                       user=get_current_user_object(),
                                       output_format=PluginOutputFormat.MD,
                                       wrap_in_div=False,
                                       user_print=plugins_user_print)

        export_pars = []

        pdoc = self._doc_entry.document
        # Get the markdown for each par dict
        for pd in par_dicts:
            if not pd['is_plugin'] and not pd['is_question']:
                if pd['md'].startswith('#'):
                    pd['md'] += ' {{ {} }}'.format(' '.join(['.{}'.format(class_name) for class_name in pd['attrs'].get('classes', [])]))
                pd['md'] = expand_macros(text=pd['md'],
                                         macros=pdoc_macros,
                                         macro_delimiter=pdoc_macro_delimiter,
                                         env=pdoc_macro_env,
                                         ignore_errors=True)

            export_pars.append(pd['md'])

        content = '\n\n'.join(export_pars)
        # print(content)
        return content

    def write_to_format(self, target_format: PrintFormat, plugins_user_print: bool = False) -> bytearray:
        """
        Converts the document to latex and returns the converted document as a bytearray
        :param target_format: The target file format
        :param plugins_user_print: Whether or not to print user input from plugins (instead of default values)
        :return: Converted document as bytearray
        """

        output_bytes = None

        with tempfile.NamedTemporaryFile(suffix='.latex', delete=True) as template_file,\
             tempfile.NamedTemporaryFile(suffix='.' + target_format.value, delete=True) as output_file:

            if self._template_to_use is None:
                raise PrintingError("No template chosen for the printing. Printing was cancelled.")

            template_content = DocumentPrinter.parse_template_content(doc_to_print=self._doc_entry, template_doc=self._template_to_use)
            #print("template-content:\n\n" + template_content)

            if template_content is None:
                raise PrintingError("The content in the template document %s is not valid." % self._template_to_use.path)

            self._template_doc = self._template_to_use
            template_file.write(bytearray(template_content, encoding='utf-8'))

            # TODO: getting the path could probably be done with more finesse
            cwd = os.getcwd()
            filters = [os.path.join(cwd, "pandoc-inlinestylesfilter.py"),
                       os.path.join(cwd, "pandoc-imagefilepathsfilter.py"),
                       os.path.join(cwd, "pandoc-headernumberingfilter.py")]

            src = self.get_content(plugins_user_print=plugins_user_print)
            try:
                pypandoc.convert_text(source=src,
                                      format='markdown',
                                      to=target_format.value,
                                      outputfile=output_file.name,
                                      extra_args=['--template=' + template_file.name],
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

    def get_print_path(self, file_type: PrintFormat, temp: bool = True, plugins_user_print: bool = False) -> str:
        """
        Formulates the printing path for the given document

        :param file_type: File format for the output
        :param temp: 
        :return: 
        """

        doc_version = self._doc_entry.document.get_latest_version().get_version()
        print("Document id: %s, version (%s, %s)" % (self._doc_entry.document.doc_id, doc_version[0], doc_version[1]))
        path = os.path.join(TEMPORARY_PRINTING_FOLDER if temp else DEFAULT_PRINTING_FOLDER,
                            self._doc_entry.name +
                            ('-' + get_current_user_object().name if plugins_user_print else '') +
                            "." + file_type.value)

        return path

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

        templates_list = []

        user_templates_list = []
        for t in user_templates:
            user_templates_list.append({'id':t.id,'path':t.name,'origin':'user','name':t.title})

        default_templates_list = []
        for t in default_templates:
            default_templates_list.append({'id':t.id,'path':t.name,'origin':'doctree','name':t.title})

        templates_list = user_templates_list + default_templates_list

        return templates_list


    @staticmethod
    def remove_block_markers(template_md: str) -> str:
        out = []
        p = re.compile('```')
        for line in template_md.splitlines():
            if p.match(line) is None:
                out.append(line)

        return "\n".join(out)

    @staticmethod
    def parse_template_content(template_doc: DocEntry, doc_to_print: DocEntry) -> str:
        pars = template_doc.document.get_paragraphs()

        pars = dereference_pars(pars, source_doc=template_doc.document.get_original_document())

        # attach macros from target document to template
        macroinfo = MacroInfo()

        macros = macroinfo.get_macros()
        macros.update(template_doc.document.get_settings().get_macroinfo().get_macros())
        macros.update(doc_to_print.document.get_settings().get_macroinfo().get_macros())

        out_pars = []

        # go through doc pars to get all the template pars
        for par in pars:
            if par.get_attr('printing_template') is not None:
                exp_md = par.get_expanded_markdown(macroinfo=macroinfo, ignore_errors=True)
                out_pars.append(DocumentPrinter.remove_block_markers(exp_md))

        return "\n\n".join(out_pars)

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
