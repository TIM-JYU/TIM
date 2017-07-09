"""
Routes for printing a document
"""
import os
from typing import Optional

from datetime import datetime
from flask import Blueprint, send_file, jsonify, json
from flask import Response
from flask import g
from flask import abort
from flask import current_app
from flask import make_response
from flask import request

import pluginControl
import sessioninfo
from accesshelper import verify_logged_in
from routes.view import get_document
from timdb.models.docentry import DocEntry
from documentprinter import DocumentPrinter, PrintingError
from timdb.printsettings import PrintSettings
from timdb.printsettings import PrintFormat
from timdb.models.printeddoc import PrintedDoc
from timdb.tim_models import db

print_blueprint = Blueprint('print',
                   __name__,
                   url_prefix='/print')


@print_blueprint.before_request
def do_before_requests():
    verify_logged_in()
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
            abort(404)


@print_blueprint.route("/<path:doc_path>", methods=['POST'])
def print_document(doc_path):

    data = request.get_json(silent=True)
    file_type = data.get('fileType')
    template_doc_id = data.get('templateDocId')

    if file_type is None:
        abort(400, "No filetype selected.")

    if template_doc_id is None:
        abort(400, "No template doc selected.")

    file_type = str(file_type)
    template_doc_id = int(float(template_doc_id))

    if (file_type.lower() not in [f.value for f in PrintFormat]):
        abort(400, "The supplied parameter 'fileType' is invalid.")

    doc = g.doc_entry
    template_doc = DocEntry.find_by_id(template_doc_id)
    type = PrintFormat[file_type.upper()]

    if get_document_print(doc_entry=doc, template=template_doc, file_type=type) is not None:
        return json.dumps({'success': True}), 200, {'ContentType': 'application/json'}

    if os.environ.get('TIM_HOST', None) != request.url_root:
        os.environ['TIM_HOST'] = request.url_root

    if template_doc is None:
        abort(400, "The supplied parameter 'templateDocId' is invalid.")

    try:
        create_printed_doc(doc_entry=doc, file_type=type, template_doc=template_doc, temp=True)
    except PrintingError as err:
        abort(500, str(err))

    return json.dumps({'success': True}), 200, {'ContentType': 'application/json'}


@print_blueprint.route("/<path:doc_path>", methods=['GET'])
def get_printed_document(doc_path):
    doc = g.doc_entry

    file_type = request.args.get('file_type')
    template_doc_id = request.args.get('template_doc_id')

    if file_type is None or file_type.lower() not in [f.value for f in PrintFormat]:
        abort(400, "The supplied query parameter 'file_type' was invalid.")

    if template_doc_id is None:
        abort(400, "The supplied query parameter 'template_doc_id' was invalid.")

    template_doc_id = int(float(template_doc_id))
    template_doc = DocEntry.find_by_id(template_doc_id)

    if template_doc is None:
        abort(400, "The supplied parameter 'template_doc_id' was invalid.")

    type = PrintFormat[file_type.upper()]

    path_to_doc = get_document_print(doc_entry=doc, template=template_doc, file_type=type)

    if path_to_doc is None:
        abort(404, "The document you tried to fetch does not exist.")

    mime = get_mimetype_for_format(type)

    if mime is None:
        abort(400, "An unexpected error occurred.")

    response = make_response(send_file(filename_or_fp=path_to_doc, mimetype=mime))

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
    return jsonify(templates)


def get_mimetype_for_format(file_type: PrintFormat):
    if file_type == PrintFormat.PDF:
        return 'application/pdf'
    elif file_type == PrintFormat.LATEX:
        return 'text/plain'
    else:
        return None


def get_document_print(doc_entry: DocEntry, template: DocEntry, file_type: PrintFormat) -> Optional[str]:
    """
    Fetches the given document from the database.

    :param doc_entry:
    :param template:
    :param file_type:
    :return:
    """

    printer = DocumentPrinter(doc_entry=doc_entry, template_to_use=template)

    return printer.get_printed_document_path_from_db(file_type=file_type)


def create_printed_doc(doc_entry: DocEntry, template_doc: DocEntry, file_type: PrintFormat, temp: bool) -> str:
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

    existing_doc_path = printer.get_printed_document_path_from_db(file_type=file_type)

    if existing_doc_path is not None:
        print("already exists:\n" + existing_doc_path)
        return existing_doc_path

    try:
        path = printer.get_print_path(temp=temp, file_type=file_type)

        if os.path.exists(path):
            os.remove(path)

        folder = os.path.split(path)[0] # gets only the head of the head, tail -tuple
        if not os.path.exists(folder):
            os.makedirs(folder)
        with open(path, mode='wb') as doc_file:
            doc_file.write(printer.write_to_format(target_format=file_type))

        doc_version = printer.get_document_version_as_float()

        p_doc = PrintedDoc(doc_id=doc_entry.document.doc_id,
                           doc_version=doc_version,
                           template_doc_id = printer._template_to_use.document.doc_id,
                           template_doc_version = printer.get_template_version_as_float(),
                           path_to_file=path,
                           file_type = file_type.value,
                           temp=temp)

        db.session.add(p_doc)
        db.session.commit()

        return p_doc.path_to_file
    except PrintingError as err:
        abort(500, str(err))
