"""
Routes for printing a document
"""
import os
import shutil
import tempfile
from pathlib import Path
from typing import Optional

from flask import Blueprint, send_file
from flask import current_app
from flask import g
from flask import make_response
from flask import request

from timApp.auth import sessioninfo
from timApp.auth.accesshelper import verify_view_access
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import default_view_ctx
from timApp.printing.documentprinter import DocumentPrinter, PrintingError, LaTeXError
from timApp.printing.printeddoc import PrintedDoc
from timApp.printing.printsettings import PrintFormat
from timApp.timdb.sqa import db
from timApp.upload.upload import add_csp_if_not_pdf
from timApp.util.flask.requesthelper import verify_json_params, get_option, RouteException, NotExist
from timApp.util.flask.responsehelper import json_response, add_no_cache_headers, add_csp_header

TEXPRINTTEMPLATE_KEY = "texprinttemplate"
DEFAULT_PRINT_TEMPLATE_NAME = "templates/printing/runko"
EMPTY_PRINT_TEMPLATE_NAME = "templates/printing/empty"
TEMP_DIR_PATH = tempfile.gettempdir()
DOWNLOADED_IMAGES_ROOT = os.path.join(TEMP_DIR_PATH, 'tim-img-dls')

print_blueprint = Blueprint('print',
                            __name__,
                            url_prefix='/print')


@print_blueprint.before_request
def do_before_requests():
    g.user = sessioninfo.get_current_user_object()


@print_blueprint.url_value_preprocessor
def pull_doc_path(endpoint, values):
    if current_app.url_map.is_endpoint_expecting(endpoint, 'doc_path'):
        doc_path = values['doc_path']
        if doc_path is None:
            raise RouteException()
        g.doc_path = doc_path
        g.doc_entry = DocEntry.find_by_path(doc_path)
        if not g.doc_entry:
            raise NotExist('Document not found')
        verify_view_access(g.doc_entry)


def template_by_name(template_name: str, isdef: bool = False):
    template_doc = DocEntry.find_by_path(template_name)
    if template_doc is None:
        return None, 0, "Template not found: " + template_name, False
    return template_doc, template_doc.id, None, isdef


def get_doc_template_name(doc: DocInfo) -> Optional[str]:
    texmacros = doc.document.get_settings().get_texmacroinfo(default_view_ctx).get_macros()
    if not texmacros:
        return None
    template_name = texmacros.get(TEXPRINTTEMPLATE_KEY)
    return template_name


def get_template_doc(doc: DocInfo, template_doc_id):
    template_name = get_doc_template_name(doc)
    if template_name:
        return template_by_name(template_name, True)

    # template_doc_id = get_option(trequest, 'templateDocId', -1)
    try:
        template_doc_id = int(template_doc_id)
    except:
        return "", 0, "Invalid template doc id", False

    if template_doc_id == -1:
        return template_by_name(DEFAULT_PRINT_TEMPLATE_NAME, True)
    if template_doc_id == 0:
        return template_by_name(EMPTY_PRINT_TEMPLATE_NAME)

    template_doc = DocEntry.find_by_id(template_doc_id)
    if template_doc is None:
        return None, 0, "There is no template with id " + str(template_doc_id), False

    def_template = DocEntry.find_by_path(DEFAULT_PRINT_TEMPLATE_NAME)
    isdef = (def_template and def_template.id == template_doc_id)

    return template_doc, template_doc_id, None, isdef


@print_blueprint.post("/<path:doc_path>")
def print_document(doc_path):
    file_type, template_doc_id, plugins_user_print = verify_json_params('fileType', 'templateDocId',
                                                                        'printPluginsUserCode',
                                                                        error_msgs=['No filetype selected.',
                                                                                    'No template doc selected.',
                                                                                    'No value for printPluginsUserCode submitted.'])
    remove_old_images, force = verify_json_params('removeOldImages', 'force', require=False)

    if not file_type:
        file_type = 'pdf'

    if file_type.lower() not in [f.value for f in PrintFormat]:
        raise RouteException("The supplied parameter 'fileType' is invalid.")

    doc: DocInfo = g.doc_entry
    template_doc, template_doc_id, template_error, template_doc_def = get_template_doc(doc, template_doc_id)
    if template_error:
        raise RouteException(template_error)

    print_type = PrintFormat[file_type.upper()]

    if remove_old_images:
        remove_images(doc.id)

    existing_doc = check_print_cache(doc_entry=doc, template=template_doc, file_type=print_type,
                                     plugins_user_print=plugins_user_print)

    #  print_access_url = f'{request.url}?file_type={str(print_type.value).lower()}&template_doc_id={template_doc_id}&plugins_user_code={plugins_user_print}'
    print_access_url = f'{request.url}'  # create url for printed page
    sep = '?'
    if str(print_type.value).lower() != 'pdf':
        print_access_url += f'{sep}file_type={str(print_type.value).lower()}'
        sep = '&'
    if not template_doc_def:
        print_access_url += f'{sep}template_doc_id={template_doc_id}'
        sep = '&'
    if plugins_user_print:
        print_access_url += f'{sep}plugins_user_code={plugins_user_print}'

    if force == 'true' or force == True:
        existing_doc = None

    if existing_doc is not None and not plugins_user_print:  # never cache user print
        return json_response({'success': True, 'url': print_access_url}, status_code=200)

    if template_doc is None:
        raise RouteException("The template doc was not found.")

    try:
        create_printed_doc(
            doc_entry=doc,
            template_doc=template_doc,
            file_type=print_type,
            temp=True,
            user_ctx=UserContext.from_one_user(g.user),
            plugins_user_print=plugins_user_print,
            urlroot='http://localhost:5000/print/',
        )  # request.url_root + 'print/')
    except LaTeXError as err:
        try:
            print("Error occurred: " + str(err))
            e = err.value
            latex_access_url = \
                f'{request.url}?file_type=latex&template_doc_id={template_doc_id}&plugins_user_code={plugins_user_print}'
            line = e.get('line', '')
            return json_response({'success': True,
                                  'url': print_access_url,
                                  'errormsg': '<pre>' + e.get('error', '') + '</pre>',
                                  'latex': latex_access_url,
                                  'latexline': latex_access_url + '&line=' + line + '#L' + line
                                  },
                                 status_code=201)
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
    return json_response({'success': True, 'url': print_access_url}, status_code=201)


@print_blueprint.get("/<path:doc_path>")
def get_printed_document(doc_path):
    doc = g.doc_entry

    def_file_type = 'pdf'
    if doc.document.get_settings().is_textplain():
        def_file_type = 'plain'

    file_type = get_option(request, 'file_type', def_file_type)
    # if doc_path != doc.name and doc_path.rfind('.') >= 0:  # name have been changed because . in name
    #    file_type = 'plain'

    # template_doc_id = get_option(request, 'template_doc_id', -1)
    plugins_user_print = get_option(request, 'plugins_user_code', False)
    line = request.args.get('line')

    if file_type.lower() not in (f.value for f in PrintFormat):
        raise RouteException("The supplied query parameter 'file_type' was invalid.")

    print_type = PrintFormat(file_type)
    template_doc = None
    template_doc_id = get_option(request, 'template_doc_id', -1)
    orginal_print_type = print_type
    if print_type == PrintFormat.ICS:
        print_type = PrintFormat.PLAIN
    if print_type != PrintFormat.PLAIN and \
            print_type != PrintFormat.RST and \
            print_type != PrintFormat.ICS:
        template_doc, template_doc_id, template_error, _ = get_template_doc(doc, template_doc_id)
        if template_error:
            raise RouteException(template_error)

    cached = check_print_cache(doc_entry=doc,
                               template=template_doc,
                               file_type=print_type,
                               plugins_user_print=plugins_user_print)

    force = get_option(request, 'force', False)
    showerror = get_option(request, 'showerror', False)

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
                plugins_user_print=plugins_user_print,
                urlroot='http://localhost:5000/print/',
            )  # request.url_root+'print/')
        except PrintingError as err:
            raise RouteException(str(err))
        except LaTeXError as err:
            pdferror = err.value
        except Exception as err:
            raise RouteException(str(err))

    cached = check_print_cache(doc_entry=doc,
                               template=template_doc,
                               file_type=print_type,
                               plugins_user_print=plugins_user_print)
    if (pdferror and showerror) or not cached:
        if not pdferror:
            pdferror = {'error': 'Unknown error (LaTeX did not return an error but still no PDF was generated.)'}
        rurl = request.url
        i = rurl.find('?')
        rurl = rurl[:i]
        latex_access_url = \
            f'{rurl}?file_type=latex&template_doc_id={template_doc_id}&plugins_user_code={plugins_user_print}'
        pdf_access_url = \
            f'{rurl}?file_type=pdf&template_doc_id={template_doc_id}&plugins_user_code={plugins_user_print}'
        line = pdferror.get('line', '')
        result = '<!DOCTYPE html>\n' + \
                 '<html>' + \
                 '<head>\n' + \
                 '</head>\n' + \
                 '<body>\n' + \
                 '<div class="error">\n'

        result += 'LaTeX error: <pre>' + pdferror.get('error', '') + '</pre>' + \
                  '<p><a href="' + latex_access_url + '&line=' + line + '#L' + line + \
                  '" target="_blank">Erronous LaTeX file</a></p>' + \
                  '<p><a href="' + latex_access_url + '" target="_blank">Created LaTeX file</a></p>' + \
                  '<p><a href="' + pdf_access_url + '" target="_blank">Possibly broken PDF file</a></p>' + \
                  '<p><a href="' + pdf_access_url + '&showerror=true">Recreate PDF</a></p>'
        result += "\n</div>\n</body>\n</html>"
        response = make_response(result)
        add_no_cache_headers(response)
        add_csp_header(response)
        return response

    mime = get_mimetype_for_format(orginal_print_type)

    if not line:
        response = make_response(send_file(path_or_file=cached, mimetype=mime))
        add_csp_if_not_pdf(response, mime, 'sandbox allow-scripts')
    else:  # show LaTeX with line numbers
        styles = "p.red { color: red; }\n"
        styles += ".program {font-family: monospace; line-height: 1.0; }\n" + \
                  ".program p { -webkit-margin-before: 0em; -webkit-margin-after: 0.2em;}\n"
        result = '<!DOCTYPE html>\n' + \
                 '<html>' + \
                 '<head>\n' + \
                 '<style>\n' + \
                 styles + \
                 '</style>\n' + \
                 '</head>\n' + \
                 '<body>\n' + \
                 '<div class="program">\n'
        n = 1
        with open(cached, "r", encoding='utf8') as f:
            for rivi in f:
                cl = ""
                if str(n) == line:
                    cl = ' class="red" '
                result += "<p" + cl + ">" + '<a name="L' + str(n) + '" >' + format(n,
                                                                                   '04d') + "</a> " + rivi.strip() + "</p>\n"
                n += 1
        result += "\n</div>\n</body>\n</html>"
        response = make_response(result)
        add_csp_header(response)

    add_no_cache_headers(response)
    db.session.commit()
    return response


@print_blueprint.get("/templates/<path:doc_path>")
def get_templates(doc_path):
    doc = g.doc_entry

    template_name = get_doc_template_name(doc)
    if template_name:  # do not give choices if template fixed in doc
        return json_response({'templates': [], 'doctemplate': template_name})

    user = g.user

    templates = DocumentPrinter.get_templates_as_dict(doc, user)
    return json_response({'templates': templates, 'doctemplate': ''})


def get_mimetype_for_format(file_type: PrintFormat) -> str:
    if file_type == PrintFormat.PDF:
        return 'application/pdf'
    elif file_type == PrintFormat.HTML:
        return 'text/html'
    elif file_type == PrintFormat.ICS:
        return 'text/calendar'
    else:
        return 'text/plain'


def check_print_cache(doc_entry: DocInfo,
                      template: DocInfo,
                      file_type: PrintFormat,
                      plugins_user_print: bool = False) -> Optional[str]:
    """
    Fetches the given document from the database.

    :param doc_entry:
    :param template:
    :param file_type:
    :param plugins_user_print:
    :return:
    """

    printer = DocumentPrinter(doc_entry=doc_entry, template_to_use=template, urlroot='')

    # if plugins_user_print:
    #     path = printer.get_print_path(file_type=file_type, plugins_user_print=plugins_user_print)
    #     if path is not None and os.path.exists(path):
    #         return path
    #     return None

    path = printer.get_printed_document_path_from_db(file_type=file_type, plugins_user_print=plugins_user_print)
    if path is not None and os.path.exists(path):
        return path

    return None

    # if plugins_user_print:
    #    return printer.get_print_path(file_type=file_type, plugins_user_print=plugins_user_print)

    # return printer.get_printed_document_path_from_db(file_type=file_type)


def create_printed_doc(
        doc_entry: DocInfo,
        template_doc: Optional[DocInfo],
        file_type: PrintFormat,
        temp: bool,
        user_ctx: UserContext,
        plugins_user_print: bool = False,
        urlroot: str = '',
) -> str:
    """
    Adds a marking for a printed document to the db

    :param user_ctx: The user context.
    :param doc_entry: Document that is being printed
    :param template_doc: printing template used
    :param file_type: File type for the document
    :param temp: Is the document stored only temporarily (gets deleted after some time)
    :param plugins_user_print: use users answers for plugins or not
    :param urlroot: url root for this route
    :return str: path to the created file
    """

    printer = DocumentPrinter(doc_entry=doc_entry,
                              template_to_use=template_doc,
                              urlroot=urlroot)
    path = printer.get_print_path(file_type=file_type,
                                  plugins_user_print=plugins_user_print)
    if path.exists():
        path.unlink()

    folder: Path = path.parent
    folder.mkdir(parents=True, exist_ok=True)
    try:
        printer.write_to_format(user_ctx, target_format=file_type, path=path, plugins_user_print=plugins_user_print)
        pdferror = None
    except LaTeXError as err:
        pdferror = err.value
    except PrintingError as err:
        raise PrintingError(str(err))

    p_doc = PrintedDoc(doc_id=doc_entry.id,
                       template_doc_id=printer.get_template_id(),
                       version=printer.hash_doc_print(plugins_user_print=plugins_user_print),
                       path_to_file=path.as_posix(),
                       file_type=file_type.value,
                       temp=temp)

    db.session.add(p_doc)

    if pdferror:
        raise LaTeXError(pdferror)
    return p_doc.path_to_file


def remove_images(docid):
    # noinspection PyBroadException
    try:
        shutil.rmtree(os.path.join(DOWNLOADED_IMAGES_ROOT, str(docid)))
    except:
        pass
