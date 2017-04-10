"""
Routes for printing a document
"""

from flask import Blueprint
from flask import Response
from flask import g
from flask import abort

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
def print_document_to_latex(doc_path):
    doc = DocEntry.find_by_path(doc_path)
    settings_used = PrintSettings()

    printed_doc = fetch_document_from_db(doc_entry=doc, file_type='latex', settings=settings_used)
    if printed_doc is None:
        printed_doc = add_printed_doc_to_db(doc_entry=doc, file_type='latex', temp=True, settings=PrintSettings())
    return Response(printed_doc, mimetype='text/plain')


@print_blueprint.route("/pdf/<path:doc_path>", methods=['GET'])
def print_document_to_pdf(doc_path):
    doc = DocEntry.find_by_path(doc_path)
    settings_used = PrintSettings()

    printed_doc = fetch_document_from_db(doc_entry=doc, file_type='pdf', settings=settings_used)
    if printed_doc is None:
        printed_doc = add_printed_doc_to_db(doc_entry=doc, file_type='pdf', temp=True, settings=settings_used)
    return Response(printed_doc, mimetype='application/pdf')



@print_blueprint.route("/edit_settings", methods=['POST'])
def edit_settings():
    return


def fetch_document_from_db(doc_entry: DocEntry, file_type: str, settings: PrintSettings) -> bytearray:
    """
    Fetches the given document from the database.

    :param doc_entry:
    :param file_type:
    :param settings:
    :return:
    """

    db = get_timdb()
    identical_document = db.session.query(PrintedDoc).\
        filter(PrintedDoc.doc_id == doc_entry.document.doc_id).\
        filter(PrintedDoc.filetype == file_type).\
        filter(PrintedDoc.settings_hash == settings.hash_value).first()
    return None if identical_document is None else identical_document.content


def add_printed_doc_to_db(doc_entry: DocEntry, file_type: str, temp: bool, settings: PrintSettings) -> bytearray:
    """
    Adds a marking for a printed document to the db


    :param doc_entry: Document that is being printed
    :param file_type: File type for the document
    :param temp: Is the document stored only temporarily (gets deleted after some time)
    :param settings: The settings object of the printing transaction
    :return: id of the newly-created db-entry
    """

    try:
        doc_content = DocumentPrinter(doc_entry=doc_entry, print_settings=settings).write_to_format(file_type=file_type)

        p_doc = PrintedDoc(doc_id=doc_entry.document.doc_id,
                           content=doc_content,
                           filetype=str(file_type),
                           temp=True,
                           settings_hash=settings.hash_value)
        db = get_timdb()
        db.session.add(p_doc)
        db.commit()

        return p_doc.content

    except PrintingError as err:
        abort(403, str(err))


def get_printed_document_from_db(printed_doc_id: int):
    db = get_timdb()
    return db.session.query(PrintedDoc).filter(PrintedDoc.id == printed_doc_id).first()