"""
Routes for printing a document
"""
import os
from typing import Optional

from flask import Blueprint, send_file
from flask import Response
from flask import g
from flask import abort
from flask import jsonify

from accesshelper import verify_logged_in
from dbaccess import get_timdb
from timdb.models.docentry import DocEntry
from documentprinter import DocumentPrinter, PrintingError
from timdb.printsettings import PrintSettings
from timdb.tim_models import PrintedDoc

print_blueprint = Blueprint('print',
                   __name__,
                   url_prefix='/print')


@print_blueprint.before_request
def do_before_requests():
    verify_logged_in()
    # g.PrintSettings = PrintSettings(get_current_user_object())
    g.PrintSettings = PrintSettings()


@print_blueprint.route("/latex/<path:doc_path>", methods=['GET'])
def print_document_as_latex(doc_path):
    doc = DocEntry.find_by_path(doc_path)
    settings_used = PrintSettings()

    path_to_doc = fetch_document_from_db(doc_entry=doc, file_type='latex', settings=settings_used)
    if path_to_doc is "":
        path_to_doc = create_printed_doc(doc_entry=doc, file_type='latex', temp=True, settings=PrintSettings())
    return send_file(path_to_doc, mimetype='application/x-latex')


@print_blueprint.route("/pdf/<path:doc_path>", methods=['GET'])
def print_document_as_pdf(doc_path):
    doc = DocEntry.find_by_path(doc_path)
    settings_used = PrintSettings()

    path_to_doc = fetch_document_from_db(doc_entry=doc, file_type='pdf', settings=settings_used)
    if path_to_doc is "":
        path_to_doc = create_printed_doc(doc_entry=doc, file_type='pdf', temp=True, settings=settings_used)
    return send_file(filename_or_fp=path_to_doc, mimetype='application/pdf')


@print_blueprint.route("/edit_settings", methods=['POST'])
def edit_settings():
    return

@print_blueprint.route("/get_settings/<path:doc_path>", methods=['GET'])
def get_settings(doc_path):
    """
    :return: Print settings
    :TODO: Get real settings
    """

    settings = {
        "doc_settings" : {
            "fontSize": 12,
            "paperSize": "A4"
        }
    }
    return jsonify(settings)


def fetch_document_from_db(doc_entry: DocEntry, file_type: str, settings: PrintSettings) -> str:
    """
    Fetches the given document from the database.

    :param doc_entry:
    :param file_type:
    :param settings:
    :return:
    """

    db = get_timdb()
    path = DocumentPrinter(doc_entry=doc_entry, print_settings=settings).get_print_path(file_type=file_type)
    identical_document = db.session.query(PrintedDoc).\
        filter(PrintedDoc.doc_id == doc_entry.document.doc_id).\
        filter(PrintedDoc.path_to_file == path).\
        filter(PrintedDoc.settings_hash == settings.hash_value).first()
    if identical_document is None:
        return ""
    elif not os.path.exists(identical_document.path_to_file):
        return ""
    else:
        return identical_document.path_to_file


def create_printed_doc(doc_entry: DocEntry, file_type: str, temp: bool, settings: PrintSettings) -> str:
    """
    Adds a marking for a printed document to the db


    :param doc_entry: Document that is being printed
    :param file_type: File type for the document
    :param temp: Is the document stored only temporarily (gets deleted after some time)
    :param settings: The settings object of the printing transaction
    :return: path to the created file
    """

    try:
        printer = DocumentPrinter(doc_entry=doc_entry, print_settings=settings)
        path = printer.get_print_path(temp=temp, file_type=file_type)

        if os.path.exists(path):
            return path

        folder = os.path.split(path)[0] # gets only the head of the head, tail -tuple
        if not os.path.exists(folder):
            os.makedirs(folder)
        with open(path, mode='wb') as doc_file:
            doc_file.write(printer.write_to_format(file_type=file_type))

        db = get_timdb()

        p_doc = PrintedDoc(doc_id=doc_entry.document.doc_id,
                           path_to_file=path,
                           temp=True,
                           settings_hash=settings.hash_value)

        db.session.add(p_doc)
        db.commit()

        return p_doc.path_to_file
    except PrintingError as err:
        abort(403, str(err))


def get_printed_document_from_db(printed_doc_id: int):
    db = get_timdb()
    return db.session.query(PrintedDoc).filter(PrintedDoc.id == printed_doc_id).first()