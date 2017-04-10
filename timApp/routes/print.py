"""
Routes for printing a document
"""
import datetime
import string

from flask import Blueprint
from flask import abort
from flask import Response
from flask import stream_with_context
from flask import g

from accesshelper import verify_logged_in
from dbaccess import get_timdb
from timdb.models.docentry import DocEntry
from sessioninfo import get_current_user_object
from documentprinter import DocumentPrinter
from timdb.printsettings import PrintSettings, PrintFormats
from timdb.tim_models import PrintedDoc

print_blueprint = Blueprint('print',
                   __name__,
                   url_prefix='/print')


@print_blueprint.before_request
def do_before_requests():
    verify_logged_in()
    # g.PrintSettings = PrintSettings(get_current_user_object())
    g.PrintSettings = PrintSettings()


@print_blueprint.route("/<path:doc_path>", methods=['GET'])
def print_document(doc_path):
    doc = DocEntry.find_by_path(doc_path)
    printed_doc = add_printed_doc_to_db(doc_entry=doc, file_type=PrintFormats.LATEX, temp=True, settings=PrintSettings())
    return Response(printed_doc, mimetype='text/plain')


@print_blueprint.route("/edit_settings", methods=['POST'])
def edit_settings():
    return


def get_markdown(doc_path):
    # Define a generator that assembles the response content piece by piece.
    """Returns the paragraphs as plain markdown for the specified document.

    :param doc_path: The document path.
    :return: The document markdown.

    """

    # Search for the document in the database based on its path. We also search the translations.
    d = DocEntry.find_by_path(doc_path, try_translation=True)
    # If the document is not found, an error is returned.
    if not d:
        abort(404)

    doc = '\n\n'.join(par.get_markdown() for par in d.document.get_paragraphs())

    return doc


def add_printed_doc_to_db(doc_entry: DocEntry, file_type: PrintFormats, temp: bool, settings: PrintSettings) -> bytearray:
    """
    Adds a marking for a printed document to the db


    :param doc_entry: Document that is being printed
    :param file_type: File type for the document
    :param temp: Is the doc saved on a temporary basis
    :param settings: The settings object of the printing transaction
    :return: id of the newly-created db-entry
    """

    p_doc = PrintedDoc(doc_id=doc_entry.document.doc_id,
                       temp=temp,
                       content=DocumentPrinter(doc_entry=doc_entry, print_settings=settings).write_tex(),
                       filetype=file_type,
                       settings_hash=settings.hash_value,
                       created=datetime.date())
    db = get_timdb()
    db.session.add(p_doc)
    db.commit()

    return p_doc.content


def get_printed_document_from_db(printed_doc_id: int):
    db = get_timdb()
    return db.session.query(PrintedDoc).filter(PrintedDoc.id == printed_doc_id).first()