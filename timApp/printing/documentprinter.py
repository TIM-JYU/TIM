"""
Functions for calling pandoc and constructing the calls
"""
import json
import os
import re
import subprocess
import tempfile
from pathlib import Path

from flask import current_app
from pypandoc import _as_unicode, _validate_formats
from pypandoc.py3compat import string_types, cast_bytes
from sqlalchemy import select

from timApp.auth.accesshelper import has_view_access
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import (
    DocParagraph,
    add_heading_numbers,
    add_headings_to_counters,
)
from timApp.document.docsettings import DocSettings
from timApp.document.document import dereference_pars, Document
from timApp.document.docviewparams import DocPrintParams
from timApp.document.macroinfo import MacroInfo
from timApp.document.post_process import process_areas
from timApp.document.preloadoption import PreloadOption
from timApp.document.randutils import hashfunc
from timApp.document.specialnames import TEMPLATE_FOLDER_NAME, PRINT_FOLDER_NAME
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import (
    default_view_ctx,
    ViewContext,
)
from timApp.document.yamlblock import strip_code_block
from timApp.folder.folder import Folder
from timApp.item.partitioning import get_area_range, RequestedViewRange
from timApp.item.routes import get_document
from timApp.markdown.autocounters import AutoCounters
from timApp.markdown.markdownconverter import (
    expand_macros,
    create_environment,
    TimSandboxedEnvironment,
)
from timApp.plugin.plugin import get_value, PluginWrap
from timApp.plugin.plugin import parse_plugin_values_macros
from timApp.plugin.pluginControl import pluginify
from timApp.plugin.pluginOutputFormat import PluginOutputFormat
from timApp.plugin.pluginexception import PluginException
from timApp.printing.printeddoc import PrintedDoc
from timApp.printing.printsettings import PrintFormat
from timApp.tim_app import app
from timApp.timdb.dbaccess import get_files_path
from timApp.timdb.sqa import run_sql
from timApp.user.user import User
from timApp.util.flask.responsehelper import to_json_str
from timApp.util.utils import cache_folder_path
from tim_common.html_sanitize import sanitize_html

DEFAULT_PRINTING_FOLDER = cache_folder_path / "printed_documents"
TEMPLATES_FOLDER = Path(TEMPLATE_FOLDER_NAME) / PRINT_FOLDER_NAME
TEX_MACROS_KEY = "texmacros"

REGSLIDESEP = re.compile("^-{3,}$")  # slide separator


class PrintingError(Exception):
    pass


class LaTeXError(Exception):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return repr(self.value)


def add_nonumber(md: str) -> str:
    r"""
    Adds {.unnumbered} after every heading line that starts with #
        Special cases:
            - many # lines in same md
                - before #-line there must be at least two cr
                - split between two cr
            - line starting with # may continue by ordinary line
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


TexSettingsAndMacros = tuple[
    DocSettings, dict[str, str], TimSandboxedEnvironment, dict[str, object], str
]


def get_tex_settings_and_macros(
    d: Document,
    view_ctx: ViewContext,
    user_ctx: UserContext,
    template_doc: DocEntry | None = None,
    tformat: PrintFormat = PrintFormat.PLAIN,
) -> TexSettingsAndMacros:
    settings = d.get_settings()
    pdoc_plugin_attrs = settings.global_plugin_attrs()
    pdoc_macroinfo = settings.get_macroinfo(view_ctx, user_ctx)
    pdoc_macro_delimiter = pdoc_macroinfo.get_macro_delimiter()
    pdoc_macros = pdoc_macroinfo.get_macros()
    if tformat == PrintFormat.LATEX:
        pdoc_macros["tex"] = True
    pdoc_macro_env = create_environment(
        pdoc_macro_delimiter,
        user_ctx,
        view_ctx,
        pdoc_macros,
        d,
    )

    if template_doc:
        template_settings = template_doc.document.get_settings()
        pdoc_macros.update(template_settings.get_texmacroinfo(view_ctx).get_macros())

    pdoc_macros.update(settings.get_texmacroinfo(view_ctx).get_macros())

    return (
        settings,
        pdoc_plugin_attrs,
        pdoc_macro_env,
        pdoc_macros,
        pdoc_macro_delimiter,
    )


def get_tex_macros(d: Document):
    settings = d.get_settings()
    # texmacros = settings.get_texmacroinfo(default_view_ctx).get_macros()
    texmacros = settings.get_setting_or_default("texmacros", {})

    return texmacros


class DocumentPrinter:
    def __init__(
        self,
        doc_entry: DocInfo,
        template_to_use: DocInfo | None,
        urlroot: str,
    ):
        self._doc_entry = doc_entry
        self._template_to_use = template_to_use
        self._content = None
        self._print_hash = None
        self._macros = {}
        self.texplain = False
        self.textplain = False
        self.texfiles = None
        self.urlroot = urlroot
        self.allow_texplain_macros = None

    def get_template_id(self) -> int | None:
        if self._template_to_use:
            return self._template_to_use.id
        return None

    def get_content(
        self,
        user_ctx: UserContext,
        view_ctx: ViewContext,
        plugins_user_print: bool = False,
        target_format: PrintFormat = PrintFormat.PLAIN,
        urlparams: DocPrintParams = DocPrintParams(),
    ) -> str:
        """
        Gets the content of the DocEntry assigned for this DocumentPrinter object.
        Fetches the markdown for the documents paragraphs, checks whether the
        paragraph should be printed determined by a boolean 'print'-attribute,
        and returns the markdown for all the paragraphs that should be printed.

        Returns the (markdown) contents of the file as a single string, as that's the
        format pypandoc likes to handle.

        :return: The TIM documents contents in markdown format. Excludes the paragraphs that have attribute
                 print="false"
        """

        tformat = target_format
        if target_format in (PrintFormat.PDF, PrintFormat.JSON):
            tformat = PrintFormat.LATEX

        if self._content is not None:
            return self._content

        (
            settings,
            _,
            pdoc_macro_env,
            pdoc_macros,
            pdoc_macro_delimiter,
        ) = get_tex_settings_and_macros(
            self._doc_entry.document, view_ctx, user_ctx, self._template_to_use, tformat
        )

        self._macros = pdoc_macros

        # TODO: tries to change soft hyphens to LaTeX \-
        #       but Pandoc removes the \???
        """
        if tformat == PrintFormat.LATEX:
            charmacros = settings.get_charmacros() if settings else {}
            charmacros = charmacros | {"&shy;": "\\-"}
            if settings:
                settings.set_charmacros(charmacros)
        """
        # Remove paragraphs that are not to be printed and replace plugin pars,
        # that have a defined 'texprint' block in their yaml, with the 'texprint'-blocks content
        r_view_range = None
        if urlparams.area:
            area = get_area_range(self._doc_entry, urlparams.area, view_ctx)
            if area is not None:
                # In some print cases (e.g. video subtitles) empty paragraphs can break some functionality
                # thus we don't print area start or area end paragraphs (RequestedViewRange(b=area[0]+1, e=area[1])
                r_view_range = RequestedViewRange(b=area[0] + 1, e=area[1], size=None)
        if r_view_range is not None:
            pars, _ = get_document(self._doc_entry, r_view_range)
        else:
            pars = self._doc_entry.document.get_paragraphs(include_preamble=True)
        self._doc_entry.document.preload_option = PreloadOption.all
        pars = dereference_pars(
            pars, context_doc=self._doc_entry.document, view_ctx=view_ctx
        )
        pars_to_print = []
        self.texplain = settings.is_texplain()
        self.allow_texplain_macros = settings.allow_texplain_macros()
        self.textplain = (
            urlparams.textplain
            if urlparams.textplain is not None
            else settings.is_textplain()
        )

        self.texfiles = settings.get_texmacroinfo(view_ctx).get_macros().get("texfiles")
        if self.texfiles and self.texfiles is str:
            self.texfiles = [self.texfiles]

        texmacros = get_tex_macros(self._doc_entry.document)

        if texmacros:
            view_ctx = view_ctx.copy(extramacros=json.dumps(texmacros))
        if tformat == PrintFormat.LATEX:  # ensure tex macro is set
            view_ctx = view_ctx.copy(extramacros=json.dumps({"tex": True}))

        # Process areas to determine what is visible to the user who is printing
        # TODO: We don't need to process all the areas, just need to find the IDs of the visible items
        processed_par_ids = {
            p.target_data.id
            for p in process_areas(
                settings,
                pars,
                pdoc_macros,
                pdoc_macro_delimiter,
                pdoc_macro_env,
                view_ctx,
                use_md=True,
                cache=False,
            )
        }

        par_infos: [  # TODO: Why this was list[]
            tuple[
                DocParagraph,
                DocSettings,
                dict,
                TimSandboxedEnvironment,
                dict[str, object],
                str,
            ]
        ] = []
        for par in pars:
            # do not print document settings pars
            if par.is_setting():
                continue

            if par.id not in processed_par_ids:
                continue

            tex_settings = get_tex_settings_and_macros(
                par.doc, view_ctx, user_ctx, self._template_to_use, tformat
            )
            p_info = par, *tex_settings
            _, pdoc_plugin_attrs, env, pdoc_macros, pdoc_macro_delimiter = tex_settings

            if self.texplain or self.textplain:
                if par.get_markdown().find("#") == 0:
                    continue

            if par.has_class("hidden-print"):
                continue

            ppar = par
            # Replace plugin- and question pars with regular docpars with the md defined in the 'print' block
            # of their yaml as the md content of the replacement par
            if par.is_plugin():
                try:
                    plugin_yaml = parse_plugin_values_macros(
                        par=par,
                        global_attrs=pdoc_plugin_attrs,
                        macros=pdoc_macros,
                        env=env,
                    )
                except PluginException:
                    plugin_yaml = {}
                plugin_yaml_beforeprint = get_value(plugin_yaml, "texbeforeprint")
                if plugin_yaml_beforeprint is not None:
                    bppar = DocParagraph.create(
                        doc=self._doc_entry.document, md=plugin_yaml_beforeprint
                    )
                    par_infos.append(p_info)
                    pars_to_print.append(bppar)

                plugin_yaml_print = get_value(plugin_yaml, "texprint")
                if plugin_yaml_print is not None:
                    ppar = DocParagraph.create(
                        doc=self._doc_entry.document, md=plugin_yaml_print
                    )
                par_infos.append(p_info)
                pars_to_print.append(ppar)

                plugin_yaml_afterprint = get_value(plugin_yaml, "texafterprint")
                if plugin_yaml_afterprint is not None:
                    appar = DocParagraph.create(
                        doc=self._doc_entry.document, md=plugin_yaml_afterprint
                    )
                    par_infos.append(p_info)
                    pars_to_print.append(appar)

            else:
                par_infos.append(p_info)
                pars_to_print.append(ppar)

        # render markdown for plugins
        presult = pluginify(
            doc=self._doc_entry.document,
            pars=pars_to_print,
            user_ctx=user_ctx,
            view_ctx=view_ctx,
            pluginwrap=PluginWrap.Nothing,
            output_format=PluginOutputFormat.MD,
            user_print=plugins_user_print,
            target_format=tformat,
            protect_raw_inline_plugins=True,
        )
        pars_to_print = presult.pars

        export_pars = []

        # TODO: Instead, convert all paragraph classes into environments and always emit \begin-\end for them
        environment_classes = set(pdoc_macros.get("texenvironment_classes", []))

        # Get the markdown for each par dict
        for p, (
            _,
            settings,
            pdoc_plugin_attrs,
            pdoc_macro_env,
            pdoc_macros,
            pdoc_macro_delimiter,
        ) in zip(pars_to_print, par_infos):
            md = p.prepare(view_ctx, use_md=True).output
            if not p.is_plugin() and not p.is_question():
                if (self.texplain and self.allow_texplain_macros) or (
                    not p.get_nomacros() and not self.texplain and not self.textplain
                ):
                    env = pdoc_macro_env
                    counters = env.counters
                    if counters:
                        counters.task_id = p.get_auto_id()
                        counters.is_plugin = p.is_plugin()
                    md = expand_macros(
                        text=md,
                        macros=pdoc_macros,
                        settings=settings,
                        env=pdoc_macro_env,
                        ignore_errors=False,
                    )

                classes = None
                # Don't add classes when in tex(t)plain mode, as they can mess up the output
                if not self.texplain and not self.textplain:
                    classes = p.classes

                if classes:
                    endraw = ""
                    beginraw = ""
                    nonumber = ""
                    for cls in classes:
                        if cls == "visible-print":
                            continue
                        if cls == "nonumber":
                            nonumber = "{.unnumbered}"
                        else:
                            if target_format == "html":
                                beginraw += '<div class="' + cls + '">'
                                endraw += "</div>"
                            elif target_format == "plain":
                                beginraw = ""
                            else:
                                is_env = cls in environment_classes
                                raw_type = "RAWTEXENV" if is_env else "RAWTEX"
                                beginraw += raw_type + cls + "\n\n"
                                endraw += f"\n\nEND" + raw_type
                                if is_env:
                                    endraw += cls

                    if nonumber:
                        md = add_nonumber(md)
                    md = beginraw + md + endraw

                if self.texplain or self.textplain:
                    if md.startswith("```"):
                        md = md[3:-3]
                if (
                    not pdoc_macros.get("texautonumber")
                    and settings.auto_number_headings()
                ):
                    md = add_heading_numbers(
                        md,
                        p,
                        settings.heading_format(),
                        initial_heading_counts=settings.auto_number_start(),
                    )

                """
                if pd['md'].startswith('#'):
                    pd['md'] += ' {{ {} }}'.format(
                        ' '.join(['.{}'.format(class_name) for class_name in pd['attrs'].get('classes', [])]))
                pd['md'] = expand_macros(text=pd['md'],
                                         macros=pdoc_macros,
                                         macro_delimiter=pdoc_macro_delimiter,
                                         env=pdoc_macro_env,
                                         ignore_errors=True)
                """
            if md.find("§") >= 0:  # check if slide fragments
                md = md.replace("<§", "").replace("§>", "").replace("§§", "")
            if md.find("---") >= 0:  # check if slide separator
                if REGSLIDESEP.match(md):
                    continue
            export_pars.append(md)

        if self.texplain or self.textplain:
            # Paragraphs are separated by a blank line in the Markdown format.
            content = "\n\n".join(export_pars)
        else:
            content = settings.get_doctexmacros() + "\n" + "\n\n".join(export_pars)

        self._content = content
        return content

    def get_autocounters(
        self,
        user_ctx: UserContext,
        view_ctx: ViewContext,
    ) -> AutoCounters:
        """
        Gets the content of the DocEntry assigned for this
        DocumentPrinter object. Builds autonumber counters
        from %%"name"|c_????%% filters.

        :return: counters for autonumbering
        """

        (
            settings,
            _,
            pdoc_macro_env,
            pdoc_macros,
            pdoc_macro_delimiter,
        ) = get_tex_settings_and_macros(
            self._doc_entry.document, view_ctx, user_ctx, self._template_to_use
        )

        self._macros = pdoc_macros
        counters = pdoc_macro_env.get_counters()
        counters.set_renumbering(True)
        counters.set_auto_number_headings(
            self._doc_entry.document.get_settings().auto_number_headings()
        )

        # Remove paragraphs that are not to be printed and replace plugin pars,
        # that have a defined 'texprint' block in their yaml, with the 'texprint'-blocks content
        # TODO: Check if this needs to be checked also for autonumbering
        pars = self._doc_entry.document.get_paragraphs(include_preamble=True)
        self._doc_entry.document.preload_option = PreloadOption.all
        pars = dereference_pars(
            pars, context_doc=self._doc_entry.document, view_ctx=view_ctx
        )

        for par in pars:
            counters.task_id = par.get_auto_id()
            # do not count document settings pars
            if par.is_setting():
                continue

            tex_settings = get_tex_settings_and_macros(
                par.doc, view_ctx, user_ctx, self._template_to_use
            )
            p_info = par, *tex_settings
            _, pdoc_plugin_attrs, env, pdoc_macros, pdoc_macro_delimiter = tex_settings
            env.set_counters(counters)
            counters.par = par

            # Replace plugin- and question pars with regular docpars
            # with the md defined in the 'print' block
            # of their yaml as the md content of the replacement par
            if par.is_plugin():
                try:
                    plugin_yaml = parse_plugin_values_macros(
                        par=par,
                        global_attrs=pdoc_plugin_attrs,
                        macros=pdoc_macros,
                        env=env,
                    )
                except PluginException:
                    pass
                continue

            # Get the markdown
            p = par

            md = p.prepare(view_ctx, use_md=True).output

            classes = p.classes
            nonumber = False

            if classes:
                for cls in classes:
                    if cls == "nonumber":
                        md = add_nonumber(md)
                        nonumber = True
                    else:
                        pass

            jump_name = p.attrs.get("taskId", None)

            # TODO: Make counters also for nonumbered and no auto_number
            if settings.auto_number_headings() and not nonumber:
                md = add_heading_numbers(
                    md,
                    p,
                    settings.heading_format(),
                    settings.heading_ref_format(),
                    jump_name,
                    counters,
                    initial_heading_counts=settings.auto_number_start(),
                )
            else:
                add_headings_to_counters(md, jump_name, counters)

            if not p.is_plugin() and not p.is_question():
                if not p.get_nomacros():
                    md = expand_macros(
                        text=md,
                        macros=pdoc_macros,
                        settings=settings,
                        env=env,
                        ignore_errors=False,
                    )

        return counters

    def write_to_format(
        self,
        user_ctx: UserContext,
        view_ctx: ViewContext,
        target_format: PrintFormat,
        path: Path,
        plugins_user_print: bool = False,
        eol_type: str = "native",
        urlparams: DocPrintParams = DocPrintParams(),
    ):
        """
        Converts the document to latex and returns the converted document as a bytearray

        :param user_ctx: The user context.
        :param view_ctx: The view context.
        :param target_format: The target file format
        :param plugins_user_print: Whether or not to print user input from plugins (instead of default values)
        :param path:  filepath to write
        :param eol_type: EOL type. Allows same option as Pandoc (crlf, lf, native)
        :return: Converted document as bytearray
        """

        with tempfile.NamedTemporaryFile(suffix=".latex", delete=True) as template_file:
            if self._template_to_use:
                template_content = DocumentPrinter.parse_template_content(
                    doc_to_print=self._doc_entry, template_doc=self._template_to_use
                )
            else:
                template_content = "$body$\n"

            if template_content is None:
                raise PrintingError(
                    f"The content in the template document {self._template_to_use.path} is not valid."
                )

            top_level = "section"
            if re.search(
                "^\\\\documentclass\\[[^\n]*(book|report)}",
                template_content,
                flags=re.S,
            ):
                top_level = "chapter"

            src = self.get_content(
                user_ctx,
                view_ctx,
                plugins_user_print=plugins_user_print,
                target_format=target_format,
                urlparams=urlparams,
            )

            # see: https://regex101.com/r/latest
            # src = re.sub(r'\{width=[^ }]* +([^}]*scale=[^%]*%[^}]*\})',                         r'{\1', src)

            templbyte = bytearray(template_content, encoding="utf-8")
            # template_file.write(templbyte) # for some reason does not write small files
            with open(template_file.name, "wb") as f:
                f.write(templbyte)

            print_dir = os.path.dirname(os.path.realpath(__file__))
            filters = [
                os.path.join(print_dir, "pandoc_inlinestylesfilter.py"),
                os.path.join(print_dir, "pandoc_imagefilepathsfilter.py"),
                # os.path.join(print_dir, "pandoc_headernumberingfilter.py")  # handled already when making md
            ]

            ftop = self._macros.get("texforcetoplevel", None)
            if ftop:
                top_level = ftop

            from_format = "markdown"
            if self.texplain:
                from_format = "latex"
            if self.textplain:
                from_format = "latex"

            texfiles = None
            if self.texfiles:
                texfiles = []
                for texfile in self.texfiles:
                    if texfile.startswith("http"):
                        texfiles.append(texfile)
                    else:
                        if texfile.find("/") < 0:  # add path if missing
                            texfile = (
                                self._doc_entry.document.docinfo.location
                                + "/"
                                + texfile
                            )
                        texfiles.append(
                            self.urlroot
                            + texfile
                            + "?file_type=latex&template_doc_id=0"
                        )

            # TODO: add also variables from texpandocvariables document setting, but this may lead to security hole?

            # TODO: if textplain use easier conversion?
            to_format = str(target_format.value)
            if target_format == PrintFormat.SVG:
                to_format = "plain"

            try:
                tim_convert_text(
                    source=src,
                    from_format=from_format,
                    to=to_format,
                    outputfile=path.absolute().as_posix(),  # output_file.name,
                    extra_args=[
                        "--template=" + template_file.name,
                        "--variable=TTrue:1",
                        "--variable=T1:1",
                        "--top-level-division=" + top_level,
                        "--markdown-headings=atx",
                        '--metadata=pagetitle:""',
                        # '--verbose',  # this gives non UTF8 results sometimes
                        "-Mtexdocid=" + str(self._doc_entry.id),
                    ],
                    filters=filters,
                    texfiles=texfiles,
                    eol_type=eol_type,
                )
            except LaTeXError as ex:
                raise LaTeXError(ex.value)
            except Exception as ex:
                raise PrintingError(f"<pre>{sanitize_html(str(ex))}</pre>")

    def get_print_path(
        self,
        view_ctx: ViewContext,
        file_type: PrintFormat,
        plugins_user_print: bool = False,
        urlparams: DocPrintParams | None = None,
    ) -> Path:
        """
        Formulates the printing path for the given document

        :param view_ctx: The view context.
        :param file_type: File format for the output
        :param plugins_user_print: should print user answers
        :return:
        """

        print_hash = self.hash_doc_print(
            plugins_user_print=plugins_user_print,
            url_macros=view_ctx.url_macros_dict,
            urlparams=urlparams,
        )

        path = (
            DEFAULT_PRINTING_FOLDER
            / str(self._doc_entry.id)
            / str(self.get_template_id())
            / str(print_hash + "." + str(file_type.value))
        )

        return path

    @staticmethod
    def get_user_templates(doc_entry: DocEntry, current_user: User) -> list[DocInfo]:
        templates = []

        if doc_entry is None or current_user is None:
            raise PrintingError(
                "You need to supply both the DocEntry and User to fetch the printing templates."
            )

        path = os.path.join(
            current_user.get_personal_folder().get_full_path(), TEMPLATES_FOLDER
        )

        templates_folder = Folder.find_by_path(path)

        if templates_folder is not None and has_view_access(templates_folder):
            docs = templates_folder.get_all_documents()
            if docs is not None:
                for d in docs:
                    if has_view_access(d) and not re.search(
                        f"/{PRINT_FOLDER_NAME}/.*{TEMPLATE_FOLDER_NAME}/", d.path
                    ):
                        templates.append(d)

        return templates

    @staticmethod
    def get_all_templates(doc_entry: DocEntry, current_user: User) -> list[DocInfo]:
        templates = []

        if doc_entry is None or current_user is None:
            raise PrintingError(
                "You need to supply both the DocEntry and User to fetch the printing templates."
            )

        current_folder = doc_entry.parent
        while current_folder is not None:
            path = os.path.join(current_folder.get_full_path(), TEMPLATES_FOLDER)

            templates_folder = Folder.find_by_path(path)

            if templates_folder is not None and has_view_access(templates_folder):
                docs = templates_folder.get_all_documents()

                if docs is None:
                    continue

                for d in docs:
                    if has_view_access(d) and not re.search(
                        f"/{PRINT_FOLDER_NAME}/.*{TEMPLATE_FOLDER_NAME}/", d.path
                    ):
                        templates.append(d)

            current_folder = current_folder.parent

        return templates

    @staticmethod
    def get_templates_as_dict(doc_entry: DocEntry, current_user: User):
        settings = doc_entry.document.get_settings()
        tex_template = settings.get("texTemplate", "")
        try:
            user_templates = DocumentPrinter.get_user_templates(
                doc_entry=doc_entry, current_user=current_user
            )
            all_templates = DocumentPrinter.get_all_templates(
                doc_entry=doc_entry, current_user=current_user
            )
            if tex_template:
                all_templates.append(DocEntry.find_by_path(tex_template))
        except PrintingError as err:
            raise PrintingError(str(err))

        default_templates = list(set(all_templates) - set(user_templates))

        user_templates_list: list[DocInfo] = []
        for t in user_templates:
            user_templates_list.append(t)

        default_templates_list: list[DocInfo] = []
        for t in default_templates:
            default_templates_list.append(t)

        user_templates_list.sort(key=lambda x: x.title)
        default_templates_list.sort(key=lambda x: x.title)

        templates_list = user_templates_list + default_templates_list

        return templates_list

    @staticmethod
    def parse_template_content(template_doc: DocInfo, doc_to_print: DocInfo) -> str:
        # attach macros from target document to template
        # Note: we fetch settings before full dereference so that blinded settings are included
        template_settings = template_doc.document.get_settings()
        doc_settings = doc_to_print.document.get_settings()

        pars = template_doc.document.get_paragraphs()

        pars = dereference_pars(
            pars, context_doc=template_doc.document, view_ctx=default_view_ctx
        )

        macros = template_settings.get_macroinfo(default_view_ctx).get_macros()
        macros.update(template_settings.get_texmacroinfo(default_view_ctx).get_macros())
        macros.update(doc_settings.get_macroinfo(default_view_ctx).get_macros())
        macros.update(doc_settings.get_texmacroinfo(default_view_ctx).get_macros())

        out_pars = []
        macroinfo = MacroInfo(default_view_ctx, macro_map=macros).with_field_macros()
        # go through doc pars to get all the template pars
        for par in pars:
            if par.get_attr("printing_template") is not None:
                exp_md = par.get_expanded_markdown(
                    macroinfo=macroinfo, ignore_errors=True
                )
                out_pars.append(strip_code_block(exp_md.strip()))

        return "\n\n".join(out_pars)

    def get_document_version_as_float(self) -> float:
        doc_v = self._doc_entry.document.get_latest_version().get_version()
        doc_v_fst, doc_v_snd = doc_v[0], doc_v[1]
        return doc_v_fst + doc_v_snd / 10

    def get_template_version_as_float(self) -> float | None:
        if self._template_to_use is None:
            return None

        doc_v = self._template_to_use.document.get_latest_version().get_version()
        doc_v_fst, doc_v_snd = doc_v[0], doc_v[1]
        return doc_v_fst + doc_v_snd / 10

    def hash_doc_print(
        self,
        plugins_user_print: bool = False,
        url_macros: dict[str, str] | None = None,
        urlparams: DocPrintParams | None = None,
    ) -> str:
        thash = ""
        if self._template_to_use:
            thash = self._template_to_use.last_modified
        content = (
            str(self._doc_entry.id)
            + " "
            + str(self._doc_entry.last_modified)
            + str(self.get_template_id())
            + " "
            + str(thash)
        )
        if url_macros:
            # dict key ordering is stable in starting from python 3.7
            content += str(url_macros)
        if urlparams:
            content += to_json_str(urlparams)
        if plugins_user_print:
            content += str(plugins_user_print) + str(get_current_user_object().id)

        return hashfunc(content)

    def get_printed_document_path_from_db(
        self,
        file_type: PrintFormat,
        plugins_user_print: bool = False,
        url_macros: dict[str, str] | None = None,
        urlparams: DocPrintParams | None = None,
    ) -> str | None:
        existing_print: PrintedDoc | None = (
            run_sql(
                select(PrintedDoc)
                .filter_by(
                    doc_id=self._doc_entry.id,
                    template_doc_id=self.get_template_id(),
                    file_type=file_type.value,
                    version=self.hash_doc_print(
                        plugins_user_print=plugins_user_print,
                        url_macros=url_macros,
                        urlparams=urlparams,
                    ),
                )
                .order_by(PrintedDoc.id.desc())
                .limit(1)
            )
            .scalars()
            .first()
        )
        if existing_print is None or not os.path.exists(existing_print.path_to_file):
            return None

        return existing_print.path_to_file


def number_lines(s: str, start: int = 1):
    lines = s.split("\n")
    i = start
    result = ""
    for line in lines:
        result += f"{i:3}: {line}\n"
        i += 1
    return result


# ------------------------ copied from pypandoc / Juho Vepsäläinen ---------------------------------
# Use own version because the original breaks if there are non-ASCII chars in error messages


def tim_convert_text(
    source: str,
    to: str,
    from_format: str,
    extra_args: list[str] = (),
    encoding: str = "utf-8",
    outputfile: str | None = None,
    filters: list[str] | None = None,
    removethis: list[str] | None = None,
    texfiles: list[str] | None = None,
    eol_type: str = "native",
):
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
    :param removethis: lines that contains this text are removed from generated LaTeX file
    :param texfiles: what files need to copy
    :returns: converted string (unicode) or an empty string if an outputfile was given
    :param eol_type: EOL type to use. Allowed values are same as Pandoc (crlf, lf, native)
    :rtype: unicode

    :raises RuntimeError: if any of the inputs are not valid of if pandoc fails with an error
    :raises OSError: if pandoc is not found; make sure it has been installed and is available at
            path.
    """
    source = _as_unicode(source, encoding)
    return tim_convert_input(
        source,
        from_format,
        "string",
        to,
        extra_args=extra_args,
        outputfile=outputfile,
        filters=filters,
        removethis=removethis,
        texfiles=texfiles,
        eol_type=eol_type,
    )


def tim_convert_input(
    source,
    from_format,
    input_type,
    to,
    extra_args=(),
    outputfile=None,
    filters=None,
    removethis=None,
    texfiles=None,
    eol_type="native",
):
    pandoc_path = "/usr/bin/pandoc"
    stdout = ""

    from_format, to = _validate_formats(from_format, to, outputfile)
    is_pdf = outputfile and outputfile.find(".pdf") >= 0
    latex_file = outputfile

    # To get access to pandoc-citeproc when we use a included copy of pandoc,
    # we need to add the pypandoc/files dir to the PATH
    new_env = os.environ.copy()
    files_path = os.path.join(os.path.dirname(os.path.realpath(__file__)), "files")
    new_env["PATH"] = (
        new_env.get(
            "PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
        )
        + os.pathsep
        + files_path
    )
    string_input = input_type == "string"
    new_env["TIM_HOST"] = current_app.config["TIM_HOST"]

    if is_pdf:
        latex_file = outputfile.replace(".pdf", ".latex")

    for texfile in texfiles or []:
        get_file(latex_file, texfile, new_env)

    if from_format == "latex":
        with open(latex_file, "w", encoding="utf-8") as f:
            if eol_type == "native":
                # Resolve the EOL type to the native one of the OS
                eol_type = "lf" if os.linesep == "\n" else "crlf"
            if eol_type == "crlf":
                f.write(source.replace("\n", "\r\n"))
            elif eol_type == "lf":
                f.write(source.replace("\r\n", "\n"))
            else:
                f.write(source)
    else:
        input_file = [source] if not string_input else []
        args = [pandoc_path, "--from=" + from_format, "--to=" + to]

        args += input_file

        if outputfile:
            args.append("--output=" + latex_file)

        args.extend(extra_args)
        args.append(f"--eol={eol_type}")

        # adds the proper filter syntax for each item in the filters list
        if filters is not None:
            if isinstance(filters, string_types):
                filters = filters.split()
            f = ["--filter=" + x for x in filters]
            args.extend(f)
        try:  # Hack because images in mmcqs is not found
            Path("/images").symlink_to(get_files_path() / "blocks/images")
        except:
            pass
        p = subprocess.Popen(
            args,
            stdin=subprocess.PIPE if string_input else None,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=new_env,
        )

        # something else than 'None' indicates that the process already terminated
        if not (p.returncode is None):
            raise RuntimeError(
                f'Pandoc died with exitcode "{p.returncode}" before receiving input: {p.stderr.read()}'
            )

        try:
            bsource = cast_bytes(source, encoding="utf-8")
        except (UnicodeDecodeError, UnicodeEncodeError):
            # assume that it is already a utf-8 encoded string
            bsource = source
        try:
            stdout, stderr = p.communicate(bsource if string_input else None)
        except OSError:
            # this is happening only on Py2.6 when pandoc dies before reading all
            # the input. We treat that the same as when we exit with an error...
            raise RuntimeError(
                'Pandoc died with exitcode "%s" during conversion.' % p.returncode
            )

        stdout = _decode_result(stdout)
        stderr = _decode_result(stderr)
        if stdout or stderr:
            raise RuntimeError(
                f'Pandoc died with error "{stderr}" during conversion.\nOutput: {stdout}.\nSource=\n{number_lines(source)}'
            )

        with open(latex_file, encoding="utf-8") as r:
            lines = r.readlines()
        with open(latex_file, "w", encoding="utf-8") as f:
            for line in lines:
                if removethis:
                    if line.find(removethis) >= 0:
                        continue
                # line = line.replace(']{ ', ']{')  # correct "]{ %%" problem caused by Jinja 2 macros
                f.write(line)

    if is_pdf:
        p, stdout = run_latex(outputfile, latex_file, new_env, "")

    # if there is an outputfile, then stdout is likely empty!
    return stdout


def _decode_result(s):
    try:
        s = s.decode("utf-8")
    except UnicodeDecodeError:
        # this shouldn't happen: pandoc more or less guarantees that the output is utf-8!
        # raise RuntimeError('Pandoc output was not utf-8.')
        # noinspection PyBroadException
        try:
            s = s.decode("iso-8859-15")
        except Exception:
            pass
    s = s.replace("\\n", "\n")
    return s


def run_latex(outputfile, latex_file, new_env, string_input):
    with app.app_context():
        max_memory = app.config["PRINT_MAX_LATEX_MEMORY"]
    try:
        filedir = os.path.dirname(outputfile)
        args = [
            "latexmk",
            "-g",
            "-f",
            "-pdfxe",
            f"-pdfxelatex=xelatex -cnf-line=extra_mem_top={max_memory} -cnf-line=extra_mem_bot={max_memory} -cnf-line=main_memory={max_memory}",
            f"-output-directory={filedir}",  # '-file-line-error',
            "-interaction=nonstopmode",
            latex_file,
        ]
        p = subprocess.Popen(
            args,
            stdin=subprocess.PIPE if string_input else None,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            cwd=filedir,
            env=new_env,
        )

        # something else than 'None' indicates that the process already terminated
        if not (p.returncode is None):
            raise RuntimeError(
                f'LaTeX died with exitcode "{p.returncode}" before receiving input: {p.stderr.read()}'
            )

        stdout, stderr = p.communicate(None)
        stdout = _decode_result(stdout)
        stderr = _decode_result(stderr)
        # check if latex returned successfully
        if p.returncode != 0:
            # Find errors:
            i = stdout.find("\n!")  # find possible error line from normal output
            # i = stdout.find('\n'+ latex_file + ':') # find possible error line in file:line:error format
            line = ""
            if i >= 0:
                stdout = stdout[i:]
                stdout = re.sub("\n\\(/usr/.*[^\n]", "", stdout)
                stdout = stdout.replace(latex_file, "")
                i = stdout.find("/var/lib")
                if i >= 0:
                    stdout = stdout[: i - 3]
                i = stdout.find("\nl.")
                if i >= 0:
                    i2 = stdout.find(" ", i)
                    if i2 >= 0:
                        line = stdout[i + 3 : i2]
            raise LaTeXError(
                {"line": line, "latex": latex_file, "pdf": outputfile, "error": stdout}
                # 'LINE: %s\nLATEX:%s\nPDF:%s\nLaTeX died with exitcode "%s" during conversion: \n%s\n%s' %
                # (line, latex_file, outputfile, p.returncode, stdout, stderr)
            )
        return p, stdout
    except OSError as ex:
        # this is happening only on Py2.6 when pandoc dies before reading all
        # the input. We treat that the same as when we exit with an error...
        raise RuntimeError('LaTeX died with error "%s" during conversion.' % str(ex))


def get_file(latex_file, fileurl, new_env):
    filedir = os.path.dirname(latex_file)
    end = fileurl.find("?")
    if end < 0:
        end = len(fileurl)
    filename = fileurl[fileurl.rfind("/") + 1 : end]
    dot = filename.rfind(".")  # change last - to . if there is no dot at the end
    minus = filename.rfind("-")
    if dot < minus:
        filename = (
            filename[:minus] + "." + filename[minus + 1 :]
        )  # TODO: Remove this when all texfiles are changed and renamed
    args = ["wget", fileurl, "-O", filename]
    p = subprocess.Popen(
        args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=filedir, env=new_env
    )

    code = p.returncode
    if code is not None:
        raise RuntimeError(
            f'Get file died with exitcode "{code}" before receiving input: {p.stderr.read()}'
        )

    stdout, stderr = p.communicate()
    stdout = _decode_result(stdout)
    stderr = _decode_result(stderr)
    if p.returncode > 0:
        raise RuntimeError(
            f'Get file {fileurl} failed: "{p.returncode}": {stdout} {stderr}'
        )
