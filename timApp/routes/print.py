"""
Routes for printing a document
"""
import os
import json
import shutil
import tempfile
from typing import Optional

from flask import Blueprint, send_file
from flask import abort
from flask import current_app
from flask import g
from flask import make_response
from flask import request

from timApp import sessioninfo
from timApp.accesshelper import verify_logged_in, verify_view_access
from timApp.documentprinter import DocumentPrinter, PrintingError, PDFLaTeXError
from timApp.requesthelper import verify_json_params
from timApp.responsehelper import json_response
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.printeddoc import PrintedDoc
from timApp.timdb.printsettings import PrintFormat
from timApp.timdb.tim_models import db

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
            abort(400)
        g.doc_path = doc_path
        g.doc_entry = DocEntry.find_by_path(doc_path, try_translation=True)
        if not g.doc_entry:
            abort(404, 'Document not found')
        verify_view_access(g.doc_entry)


@print_blueprint.route("/<path:doc_path>", methods=['POST'])
def print_document(doc_path):
    file_type, template_doc_id, plugins_user_print = verify_json_params('fileType', 'templateDocId',
                                                                        'printPluginsUserCode',
                                                                        error_msgs=['No filetype selected.',
                                                                                    'No template doc selected.',
                                                                                    'No value for printPluginsUserCode submitted.'])
    remove_old_images, force = verify_json_params('removeOldImages', 'force', require=False)

    if not isinstance(plugins_user_print, bool):
        abort(400, 'Invalid printPluginsUserCode value')
    try:
        template_doc_id = int(template_doc_id)
    except ValueError:
        abort(400, 'Invalid template doc id')

    if file_type.lower() not in [f.value for f in PrintFormat]:
        abort(400, "The supplied parameter 'fileType' is invalid.")

    doc = g.doc_entry
    template_doc = DocEntry.find_by_id(template_doc_id)
    print_type = PrintFormat[file_type.upper()]

    if remove_old_images:
        remove_images(doc.document.doc_id)

    existing_doc = check_print_cache(doc_entry=doc, template=template_doc, file_type=print_type,
                                     plugins_user_print=plugins_user_print)

    print_access_url = f'{request.url}?file_type={str(print_type.value).lower()}&template_doc_id={template_doc_id}&plugins_user_code={plugins_user_print}'

    if force == 'true' or force == True:
        existing_doc = None

    if existing_doc is not None and not plugins_user_print:  # never cache user print
        return json_response({'success': True, 'url': print_access_url}, status_code=200)

    if os.environ.get('TIM_HOST', None) != request.url_root:
        os.environ['TIM_HOST'] = request.url_root

    if template_doc is None:
        abort(400, "The template doc was not found.")

    try:
        create_printed_doc(doc_entry=doc,
                           file_type=print_type,
                           template_doc=template_doc,
                           temp=True,
                           plugins_user_print=plugins_user_print)
    except PDFLaTeXError as err:
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
            abort(400, str(err))  # TODO: maybe there's a better error code?
        # abort(400, str(err))
    except PrintingError as err:
        print("Printing occurred: " + str(err))
        abort(400, str(err))  # TODO: maybe there's a better error code?
    except Exception as err:
        print("General error occurred: " + str(err))
        abort(400, str(err))  # TODO: maybe there's a better error code?

    print_access_url = f'{request.url}?file_type={str(print_type.value).lower()}&template_doc_id={template_doc_id}&plugins_user_code={plugins_user_print}'

    return json_response({'success': True, 'url': print_access_url}, status_code=201)


@print_blueprint.route("/<path:doc_path>", methods=['GET'])
def get_printed_document(doc_path):
    doc = g.doc_entry

    file_type = request.args.get('file_type')
    template_doc_id = request.args.get('template_doc_id')
    plugins_user_print = request.args.get('plugins_user_code')
    line = request.args.get('line')

    if file_type is None:
        file_type = 'pdf'

    if file_type.lower() not in [f.value for f in PrintFormat]:
        abort(400, "The supplied query parameter 'file_type' was invalid.")

    if template_doc_id is None:
        template_doc = DocEntry.find_by_path('templates/printing/runko')
        if template_doc is None:
            abort(400, "The supplied query parameter 'template_doc_id' was invalid.")
        template_doc_id = template_doc.id

    if plugins_user_print is None or isinstance(plugins_user_print, bool):
        plugins_user_print = 'false'
        # abort(400, "The supplied query parameter 'plugins_user_code' was invalid.")

    plugins_user_print = plugins_user_print.lower() == 'true'

    template_doc_id = int(float(template_doc_id))
    template_doc = DocEntry.find_by_id(template_doc_id)

    if template_doc is None:
        abort(400, "The supplied parameter 'template_doc_id' was invalid.")

    print_type = PrintFormat[file_type.upper()]

    cached = check_print_cache(doc_entry=doc,
                               template=template_doc,
                               file_type=print_type,
                               plugins_user_print=plugins_user_print)

    force = str(request.args.get('force', 'false')).lower() == 'true'
    showerror = str(request.args.get('showerror', 'false')).lower() == 'true'

    if force or showerror:
        cached = None

    pdferror = None

    if cached is None:
        try:
            create_printed_doc(doc_entry=doc,
                               file_type=print_type,
                               template_doc=template_doc,
                               temp=True,
                               plugins_user_print=plugins_user_print)
        except PrintingError as err:
            print("Error occurred: " + str(err))
            abort(400, str(err))  # TODO: maybe there's a better error code?
        except PDFLaTeXError as err:
            print("PdfLaTeX error occurred: " + str(err))
            pdferror = err.value

    cached = check_print_cache(doc_entry=doc,
                               template=template_doc,
                               file_type=print_type,
                               plugins_user_print=plugins_user_print)
    if pdferror and showerror:
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

        result += 'PDFLaTeX error: <pre>' + pdferror.get('error', '') + '</pre>' + \
                  '<p><a href="' + latex_access_url + '&line=' + line + '#L' + line + \
                  '" target="_blank">Erronous LaTeX file</a></p>' + \
                  '<p><a href="' + latex_access_url + '" target="_blank">Created LaTeX file</a></p>' + \
                  '<p><a href="' + pdf_access_url + '" target="_blank">Possibly broken PDF file</a></p>' + \
                  '<p><a href="' + pdf_access_url + '&showerror=true">Recreate PDF</a></p>'
        result += "\n</div>\n</body>\n</html>"
        response = make_response(result)

        # Add headers to stop the documents from caching
        # This is needed for making sure the current version of the document is actually retrieved
        response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
        response.headers['Pragma'] = 'no-cache'
        response.headers['Expires'] = '-1'

        return response

    mime = get_mimetype_for_format(print_type)

    if mime is None:
        abort(400, "An unexpected error occurred.")

    if not line:
        response = make_response(send_file(filename_or_fp=cached, mimetype=mime))
    else:  # show LaTeX with line numbers
        styles = "p.red { color: red; }\n"
        styles += ".program {font-family: monospace; line-height: 0.5; }\n"
        result = '<!DOCTYPE html>\n' + \
                 '<html>' + \
                 '<head>\n' + \
                 '<style>\n' + \
                 styles + \
                 '</style>\n' + \
                 '</head>\n' + \
                 '<body>\n' + \
                 '<div class="program">\n'
        f = open(cached, "r")
        n = 1
        for rivi in f:
            cl = ""
            if str(n) == line:
                cl = ' class="red" '
            result += "<p" + cl + ">" + '<a name="L' + str(n) + '" >' + format(n,
                                                                               '04d') + "</a> " + rivi.strip() + "</p>\n"
            n += 1
        f.close()
        result += "\n</div>\n</body>\n</html>"
        response = make_response(result)

    # Add headers to stop the documents from caching
    # This is needed for making sure the current version of the document is actually retrieved
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    response.headers['Pragma'] = 'no-cache'
    response.headers['Expires'] = '-1'

    return response


@print_blueprint.route("/templates/<path:doc_path>", methods=['GET'])
def get_templates(doc_path):
    doc = g.doc_entry
    user = g.user

    templates = DocumentPrinter.get_templates_as_dict(doc, user)
    return json_response(templates)


@print_blueprint.route("/hash/<path:doc_path>", methods=['GET'])
def get_hash(doc_path):
    doc = g.doc_entry
    user = g.user
    template_doc = DocEntry.find_by_id(50)  # TODO: what 50???

    printer = DocumentPrinter(doc_entry=doc, template_to_use=template_doc)
    return printer.hash_doc_print(plugins_user_print=True)


def get_mimetype_for_format(file_type: PrintFormat):
    if file_type == PrintFormat.PDF:
        return 'application/pdf'
    elif file_type == PrintFormat.HTML:
        return 'text/html'
    else:
        return 'text/plain'


def check_print_cache(doc_entry: DocEntry,
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

    printer = DocumentPrinter(doc_entry=doc_entry, template_to_use=template)

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


def create_printed_doc(doc_entry: DocEntry,
                       template_doc: DocInfo,
                       file_type: PrintFormat,
                       temp: bool,
                       plugins_user_print: bool = False) -> str:
    """
    Adds a marking for a printed document to the db


    :param doc_entry: Document that is being printed
    :param file_type: File type for the document
    :param temp: Is the document stored only temporarily (gets deleted after some time)
    :return str: path to the created file
    """

    if template_doc is None:
        raise PrintingError("No template file was specified for the printing!")

    printer = DocumentPrinter(doc_entry=doc_entry,
                              template_to_use=template_doc)

    try:
        path = printer.get_print_path(temp=temp,
                                      file_type=file_type,
                                      plugins_user_print=plugins_user_print)

        if os.path.exists(path):
            os.remove(path)

        folder = os.path.split(path)[0]  # gets only the head of the head, tail -tuple
        if not os.path.exists(folder):
            os.makedirs(folder)
        # with open(path, mode='wb') as doc_file:
        #    doc_file.write(printer.write_to_format(target_format=file_type, plugins_user_print=plugins_user_print))
        printer.write_to_format(target_format=file_type, plugins_user_print=plugins_user_print, path=path)

        # if plugins_user_print:
        #    return path

        pdferror = None
    except PDFLaTeXError as err:
        pdferror = err.value
    except PrintingError as err:
        raise PrintingError(str(err))

    p_doc = PrintedDoc(doc_id=doc_entry.document.doc_id,
                       template_doc_id=printer.get_template_id(),
                       version=printer.hash_doc_print(plugins_user_print=plugins_user_print),
                       path_to_file=path,
                       file_type=file_type.value,
                       temp=temp)

    db.session.add(p_doc)
    db.session.commit()

    if pdferror:
        raise PDFLaTeXError(pdferror)
    return p_doc.path_to_file


def remove_images(docid):
    # noinspection PyBroadException
    try:
        shutil.rmtree(os.path.join(DOWNLOADED_IMAGES_ROOT, str(docid)))
    except:
        pass
