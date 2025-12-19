"""
Routes for printing a document
"""
import json
import os
import shutil
import tempfile
from dataclasses import field
from pathlib import Path
from urllib.parse import urlencode

from flask import current_app, render_template
from flask import g
from flask import make_response
from flask import request
from flask import send_file, Response
from marshmallow import EXCLUDE

from timApp.auth import sessioninfo
from timApp.auth.accesshelper import (
    verify_view_access,
    verify_edit_access,
    has_edit_access,
)
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.docviewparams import DocPrintParams, PrintModelSchema
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import default_view_ctx, ViewRoute, ViewContext
from timApp.document.yamlblock import YamlBlock
from timApp.markdown.autocounters import (
    REMOTE_REFS_KEY,
    AUTOCNTS_KEY,
    AUTOCNTS_PREFIX,
    COUNTERS_SETTINGS_KEY,
)
from timApp.printing.documentprinter import DocumentPrinter, PrintingError, LaTeXError
from timApp.printing.printeddoc import PrintedDoc
from timApp.printing.printsettings import PrintFormat
from timApp.timdb.sqa import db
from timApp.upload.upload import add_csp_if_not_script_safe
from timApp.util.flask.requesthelper import (
    RouteException,
    NotExist,
    view_ctx_with_urlmacros,
)
from timApp.util.flask.responsehelper import (
    json_response,
    add_no_cache_headers,
    add_csp_header,
    ok_response,
)
from timApp.util.flask.typedblueprint import TypedBlueprint
from tim_common.html_sanitize import sanitize_html, sanitize_svg

TEXPRINTTEMPLATE_KEY = "texprinttemplate"
DEFAULT_PRINT_TEMPLATE_NAME = "templates/printing/runko"
EMPTY_PRINT_TEMPLATE_NAME = "templates/printing/empty"
TEMP_DIR_PATH = tempfile.gettempdir()
DOWNLOADED_IMAGES_ROOT = os.path.join(TEMP_DIR_PATH, "tim-img-dls")

print_blueprint = TypedBlueprint("print", __name__, url_prefix="/print")


@print_blueprint.before_request
def do_before_requests() -> None:
    g.user = sessioninfo.get_current_user_object()


@print_blueprint.url_value_preprocessor
def pull_doc_path(endpoint: str | None, values: dict[str, str] | None) -> None:
    if not endpoint or not values:
        return
    if current_app.url_map.is_endpoint_expecting(endpoint, "doc_path"):
        doc_path = values["doc_path"]
        if doc_path is None:
            raise RouteException()
        g.doc_path = doc_path
        g.doc_entry = DocEntry.find_by_path(doc_path)
        if not g.doc_entry:
            raise NotExist("Document not found")
        verify_view_access(g.doc_entry)


def template_by_name(
    template_name: str, isdef: bool = False
) -> tuple[DocInfo | None, int, str | None, bool]:
    template_doc = DocEntry.find_by_path(template_name)
    if template_doc is None:
        return None, 0, f"Template not found: {template_name}", False
    return template_doc, template_doc.id, None, isdef


def get_doc_template_name(doc: DocInfo) -> str | None:
    texmacros = (
        doc.document.get_settings().get_texmacroinfo(default_view_ctx).get_macros()
    )
    if not texmacros:
        return None
    template_name = texmacros.get(TEXPRINTTEMPLATE_KEY)
    if template_name is None:
        return None
    return str(template_name)


def get_template_doc(
    doc: DocInfo, template_doc_id: int
) -> tuple[DocInfo | None, int, str | None, bool]:
    template_name = get_doc_template_name(doc)
    if template_name:
        return template_by_name(template_name, True)

    if template_doc_id == -1:
        return template_by_name(DEFAULT_PRINT_TEMPLATE_NAME, True)
    if template_doc_id == 0:
        return template_by_name(EMPTY_PRINT_TEMPLATE_NAME)

    template_doc = DocEntry.find_by_id(template_doc_id)
    if template_doc is None:
        return None, 0, f"There is no template with id {str(template_doc_id)}", False

    def_template = DocEntry.find_by_path(DEFAULT_PRINT_TEMPLATE_NAME)
    isdef = def_template is not None and def_template.id == template_doc_id

    return template_doc, template_doc_id, None, isdef


@print_blueprint.post("/<path:doc_path>")
def print_document(
    doc_path: str,
    file_type: str = field(metadata={"data_key": "fileType"}),
    template_doc_id: int = field(metadata={"data_key": "templateDocId"}),
    plugins_user_print: bool = field(metadata={"data_key": "printPluginsUserCode"}),
    remove_old_images: bool = field(
        metadata={"data_key": "removeOldImages"}, default=False
    ),
    force: bool = False,
    url_macros: dict[str, str]
    | None = field(metadata={"data_key": "urlMacros"}, default=None),
) -> Response:
    if not file_type:
        file_type = "pdf"

    if file_type.lower() not in [f.value for f in PrintFormat]:
        raise RouteException("The supplied parameter 'fileType' is invalid.")

    doc: DocInfo = g.doc_entry
    doc_settings = doc.document.get_settings()
    template_doc, template_doc_id, template_error, template_doc_def = get_template_doc(
        doc, template_doc_id
    )
    if template_error:
        raise RouteException(template_error)

    print_type = PrintFormat[file_type.upper()]

    if remove_old_images:
        remove_images(doc.id)

    if doc_settings.urlmacros():
        view_ctx = view_ctx_with_urlmacros(ViewRoute.View, urlmacros=url_macros)
    else:
        view_ctx = default_view_ctx
        url_macros = None

    urlparams: DocPrintParams = PrintModelSchema.load(request.args, unknown=EXCLUDE)

    existing_doc = check_print_cache(
        doc_entry=doc,
        template=template_doc,
        file_type=print_type,
        plugins_user_print=plugins_user_print,
        url_macros=url_macros,
        urlparams=urlparams,
    )

    #  print_access_url = f'{request.url}?file_type={str(print_type.value).lower()}&template_doc_id={template_doc_id}&plugins_user_code={plugins_user_print}'
    print_access_url = f"{request.url}"  # create url for printed page
    sep = "?"
    if str(print_type.value).lower() != "pdf":
        print_access_url += f"{sep}file_type={str(print_type.value).lower()}"
        sep = "&"
    if not template_doc_def:
        print_access_url += f"{sep}template_doc_id={template_doc_id}"
        sep = "&"
    if plugins_user_print:
        print_access_url += f"{sep}plugins_user_code={plugins_user_print}"
        sep = "&"
    if url_macros:
        print_access_url += f"{sep}{urlencode(url_macros)}"
        sep = "&"

    if force:
        existing_doc = None

    if existing_doc is not None and not plugins_user_print:  # never cache user print
        return json_response(
            {"success": True, "url": print_access_url}, status_code=200
        )

    if template_doc is None:
        raise RouteException("The template doc was not found.")

    try:
        create_printed_doc(
            doc_entry=doc,
            template_doc=template_doc,
            file_type=print_type,
            temp=True,
            user_ctx=UserContext.from_one_user(g.user),
            view_ctx=view_ctx,
            plugins_user_print=plugins_user_print,
            urlroot="http://localhost:5000/print/",
        )  # request.url_root + 'print/')
    except LaTeXError as err:
        try:
            print("Error occurred: " + str(err))
            e = err.value
            latex_access_url = f"{request.url}?file_type=latex&template_doc_id={template_doc_id}&plugins_user_code={plugins_user_print}"
            if url_macros:
                latex_access_url += f"&{urlencode(url_macros)}"
            line = e.get("line", "")
            return json_response(
                {
                    "success": True,
                    "url": print_access_url,
                    "errormsg": "<pre>" + e.get("error", "") + "</pre>",
                    "latex": latex_access_url,
                    "latexline": latex_access_url + "&line=" + line + "#L" + line,
                },
                status_code=201,
            )
        except Exception as err:
            print("General error occurred: " + str(err))
            raise RouteException(str(err))  # TODO: maybe there's a better error code?
        # raise RouteException(str(err))
    except PrintingError as err:
        print("Printing occurred: " + str(err))
        raise RouteException(str(err))  # TODO: maybe there's a better error code?
    except Exception as err:
        print("General error occurred: " + str(err))
        raise RouteException(str(err))  # TODO: maybe there's a better error code?

    # print_access_url = f'{request.url}?file_type={str(print_type.value).lower()}&template_doc_id={template_doc_id}&plugins_user_code={plugins_user_print}'
    db.session.commit()
    return json_response({"success": True, "url": print_access_url}, status_code=201)


@print_blueprint.get("/<path:doc_path>")
def get_printed_document(
    doc_path: str,
    file_type: str | None = None,
    plugins_user_code: bool = False,
    template_doc_id: int = -1,
    force: bool = False,
    showerror: bool = False,
) -> Response:
    doc: DocInfo = g.doc_entry
    doc_settings = doc.document.get_settings()
    def_file_type = "pdf"
    urlparams: DocPrintParams = PrintModelSchema.load(request.args, unknown=EXCLUDE)
    if urlparams.textplain is not None:
        if urlparams.textplain:
            def_file_type = "plain"
    elif doc_settings.is_textplain():
        def_file_type = "plain"
    suffix = Path(doc_path).suffix
    if suffix:
        def_file_type = suffix[1:].lower()

    file_type = file_type or def_file_type
    # if doc_path != doc.name and doc_path.rfind('.') >= 0:  # name have been changed because . in name
    #    file_type = 'plain'

    line = request.args.get("line")

    if file_type.lower() not in (f.value for f in PrintFormat):
        raise RouteException("The supplied query parameter 'file_type' was invalid.")

    print_type = PrintFormat(file_type)
    template_doc = None
    original_print_type = print_type
    eol_type = "native"
    if print_type == PrintFormat.ICS:
        print_type = PrintFormat.PLAIN
        eol_type = "crlf"
    if (
        print_type != PrintFormat.PLAIN
        and print_type != PrintFormat.RST
        and print_type != PrintFormat.ICS
    ):
        template_doc, template_doc_id, template_error, _ = get_template_doc(
            doc, template_doc_id
        )
        if template_error:
            raise RouteException(template_error)

    if doc_settings.urlmacros():
        url_macros = dict(request.args)
        # Remove any possibly colliding names from args
        url_macros.pop("file_type", None)
        url_macros.pop("plugins_user_code", None)
        url_macros.pop("template_doc_id", None)
        url_macros.pop("force", None)
        url_macros.pop("showerror", None)
        view_ctx = view_ctx_with_urlmacros(ViewRoute.View, urlmacros=url_macros)
    else:
        view_ctx = default_view_ctx
        url_macros = None
    cached = check_print_cache(
        doc_entry=doc,
        template=template_doc,
        file_type=print_type,
        plugins_user_print=plugins_user_code,
        url_macros=url_macros,
        urlparams=urlparams,
    )

    if force or showerror:
        cached = None

    pdferror = None

    if cached is None:
        try:
            create_printed_doc(
                doc_entry=doc,
                template_doc=template_doc,
                file_type=print_type,
                temp=True,
                user_ctx=UserContext.from_one_user(g.user),
                view_ctx=view_ctx,
                plugins_user_print=plugins_user_code,
                urlroot="http://localhost:5000/print/",
                eol_type=eol_type,
                urlparams=urlparams,
            )  # request.url_root+'print/')
        except PrintingError as err:
            raise RouteException(str(err))
        except LaTeXError as err:
            pdferror = err.value
        except Exception as err:
            raise RouteException(str(err))

    cached = check_print_cache(
        doc_entry=doc,
        template=template_doc,
        file_type=print_type,
        plugins_user_print=plugins_user_code,
        url_macros=url_macros,
        urlparams=urlparams,
    )
    if (pdferror and showerror) or not cached:
        if not pdferror:
            pdferror = {
                "error": "Unknown error (LaTeX did not return an error but still no PDF was generated.)"
            }
        rurl = request.url
        i = rurl.find("?")
        rurl = rurl[:i]
        latex_access_url = f"{rurl}?file_type=latex&template_doc_id={template_doc_id}&plugins_user_code={plugins_user_code}"
        pdf_access_url = f"{rurl}?file_type=pdf&template_doc_id={template_doc_id}&plugins_user_code={plugins_user_code}"
        if url_macros:
            latex_access_url += f"&{urlencode(url_macros)}"
            pdf_access_url += f"&{urlencode(url_macros)}"
        line = pdferror.get("line", "")
        result = (
            "<!DOCTYPE html>\n"
            + "<html>"
            + "<head>\n"
            + "</head>\n"
            + "<body>\n"
            + '<div class="error">\n'
        )

        result += (
            "LaTeX error: <pre>"
            + pdferror.get("error", "")
            + "</pre>"
            + '<p><a href="'
            + latex_access_url
            + "&line="
            + line
            + "#L"
            + line
            + '" target="_blank">Erroneous LaTeX file</a></p>'
            + '<p><a href="'
            + latex_access_url
            + '" target="_blank">Created LaTeX file</a></p>'
            + '<p><a href="'
            + pdf_access_url
            + '" target="_blank">Possibly broken PDF file</a></p>'
            + '<p><a href="'
            + pdf_access_url
            + '&showerror=true">Recreate PDF</a></p>'
        )
        result += "\n</div>\n</body>\n</html>"
        result = sanitize_html(result)
        response = make_response(result)
        add_no_cache_headers(response)
        add_csp_header(response)
        return response

    mime = get_mimetype_for_format(original_print_type)

    if not line:
        if (
            original_print_type == PrintFormat.HTML
            or original_print_type == PrintFormat.SVG
        ):
            with open(cached, "r", encoding="utf-8") as f:
                result = f.read()
            # TODO: This sanitizes the HTML, including PDF iframes.
            #       Those should be added back by rendering plugins as HTML.
            if original_print_type == PrintFormat.HTML:
                result = sanitize_html(result, allow_styles=True)
            else:
                result = sanitize_svg(result)
            response = make_response(
                render_template("html_print.jinja2", content=result, title=doc.path)
            )
        else:
            response = make_response(send_file(path_or_file=cached, mimetype=mime))
        add_csp_if_not_script_safe(response, mime, "sandbox allow-scripts")
    else:  # show LaTeX with line numbers
        styles = "p.red { color: red; }\n"
        styles += (
            ".program {font-family: monospace; line-height: 1.0; }\n"
            + ".program p { -webkit-margin-before: 0em; -webkit-margin-after: 0.2em;}\n"
        )
        result = (
            "<!DOCTYPE html>\n"
            + "<html>"
            + "<head>\n"
            + "<style>\n"
            + styles
            + "</style>\n"
            + "</head>\n"
            + "<body>\n"
            + '<div class="program">\n'
        )
        n = 1
        with open(cached, encoding="utf8") as f:
            for rivi in f:
                cl = ""
                if str(n) == line:
                    cl = ' class="red" '
                result += (
                    "<p"
                    + cl
                    + ">"
                    + '<a name="L'
                    + str(n)
                    + '" >'
                    + format(n, "04d")
                    + "</a> "
                    + rivi.strip()
                    + "</p>\n"
                )
                n += 1
        result += "\n</div>\n</body>\n</html>"
        result = sanitize_html(result, allow_styles=True)
        response = make_response(result)
        add_csp_header(response)

    add_no_cache_headers(response)
    db.session.commit()
    return response


def get_setting_and_counters_par(
    doc_info: DocInfo,
) -> tuple[DocParagraph | None, DocParagraph | None]:
    # TODO: Make this more effective!
    settings_par: DocParagraph | None = None
    counters_par: DocParagraph | None = None
    for par in doc_info.document:  # type: DocParagraph
        s = par.get_attr("settings", None)
        if s is not None:
            if s == "":
                settings_par = par
                continue
            if s == COUNTERS_SETTINGS_KEY:
                counters_par = par
                break
    return settings_par, counters_par


def add_counters_par(
    doc_info: DocInfo,
    settings_par: DocParagraph,
    counters_par: DocParagraph | None,
    values: str,
) -> DocParagraph:
    new_values = f"```\n{values}```"
    if counters_par:
        return doc_info.document.modify_paragraph(counters_par.id, new_values)
    return doc_info.document.insert_paragraph(
        new_values,
        insert_after_id=settings_par.id,
        attrs={"settings": COUNTERS_SETTINGS_KEY},
    )


def handle_doc_numbering(
    view_ctx: ViewContext, doc_info: DocInfo, used_names: list[str] | None
) -> str:
    """
    Create automatic counters for document and all referenced documents.

    :param view_ctx: View context to use for rendering.
    :param doc_info: document to handle
    :param used_names: list of already used names to avoid endless recursion
    :return: Possible error string
    """
    errors = ""
    settings_par, counters_par = get_setting_and_counters_par(doc_info)
    if not settings_par:
        return f"{doc_info.short_name}: Add settings par first: Press Edit settings under Cogwheel"

    autocounters = doc_info.document.get_settings().autocounters()
    remote_refs = autocounters.get(REMOTE_REFS_KEY, {})
    remote_counter_macros = ""

    for remote_ref in remote_refs:
        remote_doc_path = remote_refs.get(remote_ref).get("doc", None)
        if not remote_doc_path:
            continue
        if remote_doc_path.startswith("/"):
            remote_doc_path = remote_doc_path[1:]
        else:
            remote_doc_path = f"{doc_info.location}/{remote_doc_path}"
        remote_doc_entry = DocEntry.find_by_path(remote_doc_path)
        if not remote_doc_entry:
            errors += f"\n Missing: {remote_doc_path}<br>\n"
            continue

        # check if recurse and name not used yet
        if used_names is not None and remote_doc_path not in used_names:
            used_names.append(remote_doc_path)
            if not has_edit_access(doc_info):
                errors += f"\n No edit access to {doc_info.location}<br>\n"
            else:
                error = handle_doc_numbering(view_ctx, remote_doc_entry, used_names)
                if error:
                    errors += error + "<br>\n"

        _, remote_counters = get_setting_and_counters_par(remote_doc_entry)
        if not remote_counters:
            continue
        counters_settings = YamlBlock.from_markdown(remote_counters.get_markdown())
        cnts = counters_settings.get("macros", {}).get(AUTOCNTS_KEY, {})
        if not cnts:
            continue
        rcnts = f"  {AUTOCNTS_PREFIX}{remote_ref}: {json.dumps(cnts)}\n"
        remote_counter_macros += rcnts

    printer = DocumentPrinter(doc_info, template_to_use=None, urlroot="")

    fullname = f"Error in {doc_info.location}/{doc_info.short_name}:<br>\n"

    try:
        counters = printer.get_autocounters(UserContext.from_one_user(g.user), view_ctx)
    except PrintingError as err:
        return f"{fullname}{errors}<br>\n{err}<br>\n"

    new_counter_macro_values = counters.get_counter_macros() + remote_counter_macros
    add_counters_par(doc_info, settings_par, counters_par, new_counter_macro_values)
    if not errors:
        return ""
    return fullname + errors


@print_blueprint.post("/numbering/<path:doc_path>")
def get_numbering(doc_path: str, recurse: bool = False) -> Response:
    """
    renumber autocounters

    :param doc_path: from what document
    :param recurse: Should the referenced documents be renumbered as well?
    :return: ok-response
    """
    doc_entry = DocEntry.find_by_path(doc_path)
    if doc_entry is None:
        raise NotExist(doc_path)
    verify_edit_access(doc_entry)  # throws exception
    used_names = None
    if recurse:
        used_names = [doc_path]
    view_ctx = view_ctx_with_urlmacros(ViewRoute.View)
    errors = handle_doc_numbering(view_ctx, doc_entry, used_names)

    if errors:
        raise RouteException(errors)

    return ok_response()


@print_blueprint.get("/templates/<path:doc_path>")
def get_templates(doc_path: str) -> Response:
    doc = g.doc_entry

    template_name = get_doc_template_name(doc)
    if template_name:  # do not give choices if template fixed in doc
        return json_response({"templates": [], "doctemplate": template_name})

    user = g.user

    templates = DocumentPrinter.get_templates_as_dict(doc, user)
    return json_response({"templates": templates, "doctemplate": ""})


def get_mimetype_for_format(file_type: PrintFormat) -> str:
    if file_type == PrintFormat.PDF:
        return "application/pdf"
    elif file_type == PrintFormat.HTML:
        return "text/html"
    elif file_type == PrintFormat.ICS:
        return "text/calendar"
    elif file_type == PrintFormat.SVG:
        return "image/svg+xml"
    else:
        return "text/plain"


def check_print_cache(
    doc_entry: DocInfo,
    template: DocInfo | None,
    file_type: PrintFormat,
    plugins_user_print: bool = False,
    url_macros: dict[str, str] | None = None,
    urlparams: DocPrintParams | None = None,
) -> str | None:
    """
    Fetches the given document from the database.

    :param url_macros:
    :param doc_entry:
    :param template:
    :param file_type:
    :param plugins_user_print:
    :return:
    """

    printer = DocumentPrinter(doc_entry=doc_entry, template_to_use=template, urlroot="")

    # if plugins_user_print:
    #     path = printer.get_print_path(file_type=file_type, plugins_user_print=plugins_user_print)
    #     if path is not None and os.path.exists(path):
    #         return path
    #     return None

    path = printer.get_printed_document_path_from_db(
        file_type=file_type,
        plugins_user_print=plugins_user_print,
        url_macros=url_macros,
        urlparams=urlparams,
    )
    if path is not None and os.path.exists(path):
        return path

    return None

    # if plugins_user_print:
    #    return printer.get_print_path(file_type=file_type, plugins_user_print=plugins_user_print)

    # return printer.get_printed_document_path_from_db(file_type=file_type)


def create_printed_doc(
    doc_entry: DocInfo,
    template_doc: DocInfo | None,
    file_type: PrintFormat,
    temp: bool,
    user_ctx: UserContext,
    view_ctx: ViewContext,
    plugins_user_print: bool = False,
    urlroot: str = "",
    eol_type: str = "native",
    urlparams: DocPrintParams = DocPrintParams(),
) -> str:
    """
    Adds a marking for a printed document to the db

    :param user_ctx: The user context.
    :param view_ctx: The view context.
    :param doc_entry: Document that is being printed
    :param template_doc: printing template used
    :param file_type: File type for the document
    :param temp: Is the document stored only temporarily (gets deleted after some time)
    :param plugins_user_print: use users answers for plugins or not
    :param urlroot: url root for this route
    :param eol_type: EOL type. Same option as Pandoc (crlf, lf, native)
    :return str: path to the created file
    """

    printer = DocumentPrinter(
        doc_entry=doc_entry, template_to_use=template_doc, urlroot=urlroot
    )
    path = printer.get_print_path(
        view_ctx,
        file_type=file_type,
        plugins_user_print=plugins_user_print,
        urlparams=urlparams,
    )
    if path.exists():
        path.unlink()

    folder: Path = path.parent
    folder.mkdir(parents=True, exist_ok=True)
    try:
        printer.write_to_format(
            user_ctx,
            view_ctx,
            target_format=file_type,
            path=path,
            plugins_user_print=plugins_user_print,
            eol_type=eol_type,
            urlparams=urlparams,
        )
        pdferror = None
    except LaTeXError as err:
        pdferror = err.value
    except PrintingError as err:
        raise PrintingError(str(err))

    p_doc = PrintedDoc(
        doc_id=doc_entry.id,
        template_doc_id=printer.get_template_id(),
        version=printer.hash_doc_print(
            plugins_user_print=plugins_user_print,
            url_macros=view_ctx.url_macros_dict,
            urlparams=urlparams,
        ),
        path_to_file=path.as_posix(),
        file_type=file_type.value,
        temp=temp,
    )

    db.session.add(p_doc)

    if pdferror:
        raise LaTeXError(pdferror)
    return p_doc.path_to_file


def remove_images(doc_id: int) -> None:
    # noinspection PyBroadException
    try:
        shutil.rmtree(os.path.join(DOWNLOADED_IMAGES_ROOT, str(doc_id)))
    except:
        pass
