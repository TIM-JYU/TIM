"""
Functions for calling pandoc and constructing the calls
"""
import os
import re
import tempfile
from typing import Optional, List

from timApp.accesshelper import has_view_access
from timApp.dbaccess import get_timdb
from timApp.documentmodel.docparagraph import DocParagraph
from timApp.documentmodel.document import dereference_pars
from timApp.documentmodel.macroinfo import MacroInfo
from documentmodel.randutils import hashfunc
from timApp.markdownconverter import expand_macros, create_environment
from timApp.pluginControl import pluginify
from timApp.pluginOutputFormat import PluginOutputFormat
from timApp.sessioninfo import get_current_user_object
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.folder import Folder
from timApp.timdb.models.printeddoc import PrintedDoc
from timApp.timdb.models.user import User
from timApp.timdb.printsettings import PrintFormat
from timApp.timdb.tim_models import db
from timApp.plugin import get_value
from timApp.plugin import parse_plugin_values_macros

import subprocess
from pypandoc import _as_unicode, _validate_formats
from pypandoc.py3compat import string_types, cast_bytes

FILES_ROOT = os.path.abspath('tim_files')
DEFAULT_PRINTING_FOLDER = os.path.join(FILES_ROOT, 'printed_documents')
TEMPORARY_PRINTING_FOLDER = os.path.join(DEFAULT_PRINTING_FOLDER, 'tmp')
TEMPLATES_FOLDER = os.path.join('Templates', 'Printing')
DEFAULT_TEMPLATE_NAME = 'Default'
TEX_MACROS_KEY = "texmacros"


class PrintingError(Exception):
    pass


def add_nonumber(md: str) -> str:
    """
    add's {.unnumbered} after every heading line that starts with #
        Special cases:
            - many # lines in same md
                - before #-line there must be at least two cr
                - split bteween two cr
            - line statring with # may continue by ordinary line
                - the unnumbered must be added before first cr
            - line starting with # may continue next line and have \ at the end
                 - undefined
    :param md: markdown to be converted
    :return: markdown with headings marked as unnumbered
    """
    mds = md.split("\n\n")
    result = ""
    for m in mds:
        if m.startswith("#"):
            ms = m.split("\n")
            if not ms[0].endswith("\\"):
                ms[0] += "{.unnumbered}"
            m = "\n".join(ms)
        result += m + "\n\n"

    return result


class DocumentPrinter:
    def __init__(self, doc_entry: DocEntry, template_to_use: DocEntry):
        self._doc_entry = doc_entry
        # if template_to_use is None:
        #    template_to_use = DocumentPrinter.get_default_template(doc_entry)
        self._template_to_use = template_to_use
        self._template_to_use_id = -1
        if template_to_use:
            self._template_to_use_id = template_to_use.id
        self._template_doc = None
        self._content = None
        self._print_hash = None
        self._macros = {}

    def get_template_id(self):
        return self._template_to_use_id

    def get_content(self, plugins_user_print: bool = False, target_format: str = '') -> str:
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

        if self._content is not None:
            return self._content
        pdoc_plugin_attrs = self._doc_entry.document.get_settings().global_plugin_attrs()
        pdoc_macroinfo = self._doc_entry.document.get_settings().get_macroinfo(get_current_user_object())
        pdoc_macro_delimiter = pdoc_macroinfo.get_macro_delimiter()
        pdoc_macros = pdoc_macroinfo.get_macros()
        pdoc_macro_env = create_environment(pdoc_macro_delimiter)
        pdoc_macros.update(self._doc_entry.document.get_settings().get_macroinfo(key=TEX_MACROS_KEY).get_macros())
        self._macros = pdoc_macros

        # Remove paragraphs that are not to be printed and replace plugin pars,
        # that have a defined 'texprint' block in their yaml, with the 'texprint'-blocks content
        pars = self._doc_entry.document.get_paragraphs()
        pars_to_print = []
        texplain = self._doc_entry.document.get_settings().is_texplain()
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
                plugin_yaml = parse_plugin_values_macros(par=par,
                                                         global_attrs=pdoc_plugin_attrs,
                                                         macros=pdoc_macros,
                                                         macro_delimiter=pdoc_macro_delimiter)
                plugin_yaml_beforeprint = get_value(plugin_yaml.get('markup'), 'texbeforeprint')
                if plugin_yaml_beforeprint is not None:
                    bppar = DocParagraph.create(doc=self._doc_entry.document, md=plugin_yaml_beforeprint)
                    pars_to_print.append(bppar)

                plugin_yaml_print = get_value(plugin_yaml.get('markup'), 'texprint')
                if plugin_yaml_print is not None:
                    ppar = DocParagraph.create(doc=self._doc_entry.document, md=plugin_yaml_print)
                pars_to_print.append(ppar)

                plugin_yaml_afterprint = get_value(plugin_yaml.get('markup'), 'texafterprint')
                if plugin_yaml_afterprint is not None:
                    appar = DocParagraph.create(doc=self._doc_entry.document, md=plugin_yaml_afterprint)
                    pars_to_print.append(appar)

            else:
                pars_to_print.append(ppar)

        tformat = target_format
        if target_format == 'pdf' or target_format == 'json':
            tformat = 'latex'

        # Dereference pars and render markdown for plugins
        # Only the 1st return value (the pars) is needed here
        par_dicts, _, _, _ = pluginify(doc=self._doc_entry.document,
                                       pars=pars_to_print,
                                       timdb=get_timdb(),
                                       user=get_current_user_object(),
                                       output_format=PluginOutputFormat.MD,
                                       wrap_in_div=False,
                                       user_print=plugins_user_print,
                                       target_format=tformat)

        export_pars = []

        # Get the markdown for each par dict
        for pd in par_dicts:
            md = pd['md']
            if not pd['is_plugin'] and not pd['is_question']:
                md = expand_macros(text=md,
                                   macros=pdoc_macros,
                                   macro_delimiter=pdoc_macro_delimiter,
                                   env=pdoc_macro_env,
                                   ignore_errors=True)
                classes = pd['attrs'].get('classes', [])
                if len(classes):
                    endraw = ""
                    beginraw = ""
                    nonumber = ""
                    for cls in classes:
                        if cls == 'visible-print':
                            continue
                        if cls == "nonumber":
                            nonumber = "{.unnumbered}"
                        else:
                            if target_format == "html":
                                beginraw += '<div class="' + cls + '">'
                                endraw += "</div>"
                            elif target_format == "plain":
                                beginraw = ''
                            else:
                                beginraw += "RAWTEX" + cls + "\n\n"
                                endraw += "\n\nENDRAWTEX"
                    if nonumber:
                        md = add_nonumber(md)
                    md = beginraw + md + endraw

                if texplain:
                    if md.startswith('```'):
                        md = md[3:-3]

                '''
                if pd['md'].startswith('#'):
                    pd['md'] += ' {{ {} }}'.format(
                        ' '.join(['.{}'.format(class_name) for class_name in pd['attrs'].get('classes', [])]))
                pd['md'] = expand_macros(text=pd['md'],
                                         macros=pdoc_macros,
                                         macro_delimiter=pdoc_macro_delimiter,
                                         env=pdoc_macro_env,
                                         ignore_errors=True)
                '''
            export_pars.append(md)

        content = '\n\n'.join(export_pars)
        self._content = content
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

        with tempfile.NamedTemporaryFile(suffix='.latex', delete=True) as template_file, \
                tempfile.NamedTemporaryFile(suffix='.' + target_format.value, delete=True) as output_file:

            if self._template_to_use is None:
                raise PrintingError("No template chosen for the printing. Printing was cancelled.")

            if self._template_to_use:
                template_content = DocumentPrinter.parse_template_content(doc_to_print=self._doc_entry,
                                                                          template_doc=self._template_to_use)
            else:
                template_content = '$body$\n'
            # print("template-content:\n\n" + template_content)

            if template_content is None:
                raise PrintingError(
                    "The content in the template document %s is not valid." % self._template_to_use.path)

            top_level = 'section'
            if re.search("^\\\\documentclass\[[^\n]*(book|report)\}", template_content, flags=re.S):
                top_level = 'chapter'

            self._template_doc = self._template_to_use
            templbyte = bytearray(template_content, encoding='utf-8')
            # template_file.write(templbyte) # for some reason does not write small files
            open(template_file.name, 'wb').write(templbyte)

            # TODO: getting the path could probably be done with more finesse
            cwd = os.getcwd()
            filters = [
                os.path.join(cwd, "pandoc-inlinestylesfilter.py"),
                os.path.join(cwd, "pandoc-imagefilepathsfilter.py"),
                #  os.path.join(cwd, "pandoc-headernumberingfilter.py")  # handled allready when making md
            ]

            src = self.get_content(plugins_user_print=plugins_user_print, target_format=target_format.value)
            ftop = self._macros.get('texforcetoplevel', None)
            if ftop:
                top_level = ftop

            print("docid from env ", os.environ.get("texdocid", None))
            os.environ["texdocid"] = str(self._doc_entry.document.doc_id)

            # print(top_level)
            # TODO: add also variables from texpandocvariables document setting, but this may leed to security hole???
            try:
                tim_convert_text(source=src,
                                 from_format='markdown',
                                 to=target_format.value,
                                 outputfile=output_file.name,
                                 extra_args=['--template=' + template_file.name,
                                             '--variable=TTrue:1',
                                             '--variable=T1:1',
                                             '--top-level-division=' + top_level,
                                             '--atx-headers',
                                             # '--verbose',  # this gives non UTF8 results sometimes
                                             '--latex-engine-opt=-interaction=nonstopmode',
                                             '-Mtexdocid=' + str(self._doc_entry.document.doc_id),
                                             # '--latex-engine=xelatex'
                                             ],
                                 filters=filters
                                 )
                template_file.seek(0)
                output_bytes = bytearray(output_file.read())
                print("docid from env ", os.environ.get("texdocid", None))
            except Exception as ex:
                # print(str(ex))

                # TODO: logging of errors
                # Might be a good idea to log these?
                # might also be a good idea to separate between errors that should be shown to the user, and
                # ones that only get written to the log file.

                # TODO: selection of errors that should be routed to the UI

                raise PrintingError('<pre>' + str(ex) + '</pre>')
                # finally:
                #    os.remove(template_file.name)

        return output_bytes

    def get_print_path(self, file_type: PrintFormat, temp: bool = True, plugins_user_print: bool = False) -> str:
        """
        Formulates the printing path for the given document

        :param file_type: File format for the output
        :param temp:
        :param plugins_user_print: should print user answers
        :return:
        """

        """ # Old version
        doc_version = self._doc_entry.document.get_latest_version().get_version()
        print("Document id: %s, version (%s, %s)" % (self._doc_entry.document.doc_id, doc_version[0], doc_version[1]))
        path = os.path.join(TEMPORARY_PRINTING_FOLDER if temp else DEFAULT_PRINTING_FOLDER,
                            self._doc_entry.name +
                            ('-' + get_current_user_object().name if plugins_user_print else '') +
                            "." + file_type.value)
        """
        print_hash = self.hash_doc_print(plugins_user_print=plugins_user_print)

        # print("Document id: %s, version (%s, %s)" % (self._doc_entry.document.doc_id, doc_version[0], doc_version[1]))
        path = os.path.join(DEFAULT_PRINTING_FOLDER,
                            str(self._doc_entry.id),
                            str(self._template_to_use_id),
                            str(print_hash) + "." + file_type.value)

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
                    if has_view_access(d.id) and not re.search("/Printing/.*Templates/", d.name):
                        # print("Found the document " + d.name)
                        templates.append(d)

        return templates

    @staticmethod
    def get_all_templates(doc_entry: DocEntry, current_user: User) -> List[DocEntry]:
        templates = []

        if doc_entry is None or current_user is None:
            raise PrintingError("You need to supply both the DocEntry and User to fetch the printing templates.")

        # print("Crawling up the document tree searching for all accessible templates")
        current_folder = doc_entry.parent
        while current_folder is not None:

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
                    if has_view_access(d.id) and not re.search("/Printing/.*Templates/", d.name):
                        # print("Found the document " + d.name)
                        templates.append(d)

            current_folder = current_folder.parent

        return templates

    @staticmethod
    def get_templates_as_dict(doc_entry: DocEntry, current_user: User):

        try:
            user_templates = DocumentPrinter.get_user_templates(doc_entry=doc_entry, current_user=current_user)
            all_templates = DocumentPrinter.get_all_templates(doc_entry=doc_entry, current_user=current_user)
        except PrintingError as err:
            raise PrintingError(str(err))

        default_templates = list(set(all_templates) - set(user_templates))

        user_templates_list = []
        for t in user_templates:
            user_templates_list.append({'id': t.id, 'path': t.name, 'origin': 'user', 'name': t.title})

        default_templates_list = []
        for t in default_templates:
            default_templates_list.append({'id': t.id, 'path': t.name, 'origin': 'doctree', 'name': t.title})

        user_templates_list.sort(key=lambda x: x['name'])
        default_templates_list.sort(key=lambda x: x['name'])

        templates_list = user_templates_list + default_templates_list

        # if doc_entry.document.get_settings().is_texplain():  # does not work because no block entry
        #    templates_list.append({'id': -1, 'path': 'empty', 'origin': 'doctree', 'name': 'empty'})

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
        macros.update(template_doc.document.get_settings().get_macroinfo(key=TEX_MACROS_KEY).get_macros())
        macros.update(doc_to_print.document.get_settings().get_macroinfo().get_macros())
        macros.update(doc_to_print.document.get_settings().get_macroinfo(key=TEX_MACROS_KEY).get_macros())

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
        return doc_v_fst + doc_v_snd / 10

    def get_template_version_as_float(self) -> Optional[float]:
        if self._template_to_use is None:
            return None

        doc_v = self._template_to_use.document.get_latest_version().get_version()
        doc_v_fst, doc_v_snd = doc_v[0], doc_v[1]
        return doc_v_fst + doc_v_snd / 10

    def hash_doc_print(self, plugins_user_print: bool = False) -> str:
        thash = ''
        if self._template_to_use:
            thash = self._template_to_use.last_modified
        content = str(self._doc_entry.id) + " " + str(self._doc_entry.last_modified) + \
                  str(self._template_to_use_id) + " " + str(thash)
        if plugins_user_print:
            content += str(plugins_user_print) + str(get_current_user_object().id)

        # self.get_content(plugins_user_print=plugins_user_print)
        return hashfunc(content)

    def get_printed_document_path_from_db(self, file_type: PrintFormat, plugins_user_print: bool = False) -> \
            Optional[str]:
        #  current_doc_version = self.get_document_version_as_float()
        #  current_template_version = self.get_template_version_as_float()
        existing_print = db.session.query(PrintedDoc). \
            filter(PrintedDoc.doc_id == self._doc_entry.id). \
            filter(PrintedDoc.template_doc_id == self._template_to_use_id). \
            filter(PrintedDoc.file_type == file_type.value). \
            filter(PrintedDoc.version == self.hash_doc_print(plugins_user_print=plugins_user_print)). \
            first()
        # filter(PrintedDoc.doc_version == current_doc_version). \
        # filter(PrintedDoc.template_doc_version == current_template_version). \

        if existing_print is None or not os.path.exists(existing_print.path_to_file):
            return None

        return existing_print.path_to_file


# ------------------------ copied from pypandoc / Juho Vepsäläinen ---------------------------------
# use own version, because the original fall down if scandinavian chars in erros messages

def tim_convert_text(source, to, from_format, extra_args=(), encoding='utf-8',
                     outputfile=None, filters=None):
    """Converts given `source` from `format` to `to`.
    :param str source: Unicode string or bytes (see encoding)
    :param str to: format into which the input should be converted; can be one of
            `pypandoc.get_pandoc_formats()[1]`
    :param str from_format: the format of the inputs; can be one of `pypandoc.get_pandoc_formats()[1]`
    :param list extra_args: extra arguments (list of strings) to be passed to pandoc
            (Default value = ())
    :param str encoding: the encoding of the input bytes (Default value = 'utf-8')
    :param str outputfile: output will be written to outfilename or the converted content
            returned if None (Default value = None)
    :param list filters: pandoc filters e.g. filters=['pandoc-citeproc']
    :returns: converted string (unicode) or an empty string if an outputfile was given
    :rtype: unicode

    :raises RuntimeError: if any of the inputs are not valid of if pandoc fails with an error
    :raises OSError: if pandoc is not found; make sure it has been installed and is available at
            path.
    """
    source = _as_unicode(source, encoding)
    return tim_convert_input(source, from_format, 'string', to, extra_args=extra_args,
                             outputfile=outputfile, filters=filters)


def tim_convert_input(source, from_format, input_type, to, extra_args=(), outputfile=None,
                      filters=None):
    pandoc_path = '/usr/bin/pandoc'

    from_format, to = _validate_formats(from_format, to, outputfile)

    string_input = input_type == 'string'
    input_file = [source] if not string_input else []
    args = [pandoc_path, '--from=' + from_format, '--to=' + to]

    args += input_file

    if outputfile:
        args.append("--output=" + outputfile)

    args.extend(extra_args)

    # adds the proper filter syntax for each item in the filters list
    if filters is not None:
        if isinstance(filters, string_types):
            filters = filters.split()
        f = ['--filter=' + x for x in filters]
        args.extend(f)

    # To get access to pandoc-citeproc when we use a included copy of pandoc,
    # we need to add the pypandoc/files dir to the PATH
    new_env = os.environ.copy()
    files_path = os.path.join(os.path.dirname(os.path.realpath(__file__)), "files")
    new_env["PATH"] = new_env.get("PATH", "") + os.pathsep + files_path

    p = subprocess.Popen(
        args,
        stdin=subprocess.PIPE if string_input else None,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=new_env)

    # something else than 'None' indicates that the process already terminated
    if not (p.returncode is None):
        raise RuntimeError(
            'Pandoc died with exitcode "%s" before receiving input: %s' % (p.returncode,
                                                                           p.stderr.read())
        )

    try:
        source = cast_bytes(source, encoding='utf-8')
    except (UnicodeDecodeError, UnicodeEncodeError):
        # assume that it is already a utf-8 encoded string
        pass
    try:
        stdout, stderr = p.communicate(source if string_input else None)
    except OSError:
        # this is happening only on Py2.6 when pandoc dies before reading all
        # the input. We treat that the same as when we exit with an error...
        raise RuntimeError('Pandoc died with exitcode "%s" during conversion.' % (p.returncode))

    stdout = _decode_result(stdout)
    stderr = _decode_result(stderr)

    # check that pandoc returned successfully
    if p.returncode != 0:
        raise RuntimeError(
            'Pandoc died with exitcode "%s" during conversion: \n%s' % (p.returncode, stderr)
        )

    # if there is an outputfile, then stdout is likely empty!
    return stdout


def _decode_result(s):
    try:
        s = s.decode('utf-8')
    except UnicodeDecodeError:
        # this shouldn't happen: pandoc more or less garantees that the output is utf-8!
        # raise RuntimeError('Pandoc output was not utf-8.')
        # noinspection PyBroadException
        try:
            s = s.decode('iso-8859-15')
        except Exception:
            pass
    s = s.replace('\\n', '\n')
    return s
