"""
Routes for printing a document
"""
import os
from typing import Optional

from flask import Blueprint, send_file, jsonify
from flask import Response
from flask import g
from flask import abort
from flask import current_app
from flask import request

import sessioninfo
from accesshelper import verify_logged_in
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


@print_blueprint.route("/<path:doc_path>", methods=['GET'])
def print_document(doc_path):
    doc = g.doc_entry

    file_type = request.args.get('file_type')
    template_doc_id = request.args.get('template_doc_id')
    #print("file_type: %s" % file_type)
    if file_type is None or file_type.lower() not in [f.value for f in PrintFormat]:
        abort(404, "The supplied parameter 'file_type' was not valid.")

    if template_doc_id is None:
        abort(404, "You need to supply a value for parameter 'template_doc_id'.")

    template_doc_id = int(float(template_doc_id))
    template_doc = DocEntry.query.filter(DocEntry.id == template_doc_id).first()

    if template_doc is None:
        abort(404, "The supplied parameter 'template_doc_id' was not valid.")

    type = PrintFormat[file_type.upper()]
    path_to_doc = create_printed_doc(doc_entry=doc, file_type=type, temp=True)
    mime = get_mimetype_for_format(type)

    if mime is None:
        abort(404, "The supplied parameter 'file_type' was not valid.")

    return send_file(filename_or_fp=path_to_doc, mimetype=mime)


@print_blueprint.route("/getTemplatesJSON/<path:doc_path>", methods=['GET'])
def get_templates(doc_path):
    doc = g.doc_entry
    user = g.user

    templates = DocumentPrinter.get_templates_as_dict(doc, user)
    return jsonify(templates)


@print_blueprint.route("/editSettings", methods=['POST'])
def edit_settings():
    return


def get_mimetype_for_format(file_type: PrintFormat):
    if file_type == PrintFormat.PDF:
        return 'application/pdf'
    elif file_type == PrintFormat.LATEX:
        return 'text/plain'
    else:
        return None


def fetch_document_from_db(doc_entry: DocEntry, file_type: PrintFormat) -> Optional[str]:
    """
    Fetches the given document from the database.

    :param doc_entry:
    :param file_type:
    :return:
    """

    # TODO: Do something meaningful!

    return None


def create_printed_doc(doc_entry: DocEntry, file_type: PrintFormat, temp: bool) -> str:
    """
    Adds a marking for a printed document to the db


    :param doc_entry: Document that is being printed
    :param file_type: File type for the document
    :param temp: Is the document stored only temporarily (gets deleted after some time)
    :return str: path to the created file
    """

    printer = DocumentPrinter(doc_entry=doc_entry,
                              template_to_use=DocumentPrinter.get_custom_template(doc_entry=doc_entry))

    existing_doc_path = printer.get_printed_document_path_from_db(file_type=file_type)

    if existing_doc_path is not None:
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
        abort(403, str(err))
