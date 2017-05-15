"""
Routes for printing a document
"""
import os
from typing import Optional

from flask import Blueprint, send_file
from flask import Response
from flask import g
from flask import abort
from flask import current_app

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


@print_blueprint.route("/latex/<path:doc_path>", methods=['GET'])
def print_document_as_latex(doc_path):
    doc = DocEntry.find_by_path(doc_path)

#    path_to_doc = fetch_document_from_db(doc_entry=doc, file_type='latex')
#    if path_to_doc is "":
#        path_to_doc = create_printed_doc(doc_entry=doc, file_type='latex', temp=True)
    return Response(PrintSettings.read_settings(doc, g.user), mimetype='text/plain')


@print_blueprint.route("/pdf/<path:doc_path>", methods=['GET'])
def print_document_as_pdf(doc_path):
    doc = g.doc_entry

    path_to_doc = fetch_document_from_db(doc_entry=doc, file_type=PrintFormat.PDF)
    if path_to_doc is None:
        path_to_doc = create_printed_doc(doc_entry=doc, file_type=PrintFormat.PDF, temp=True)
    return send_file(filename_or_fp=path_to_doc, mimetype='application/pdf')


@print_blueprint.route("/testing/latex/<path:doc_path>", methods=['GET'])
def test_latex(doc_path):
    doc = g.doc_entry

    printer = DocumentPrinter(doc_entry=doc)
    path = printer.get_print_path(file_type=PrintFormat.LATEX)
    if os.path.exists(path):
        os.remove(path)

    path_to_doc = create_printed_doc(doc_entry=doc, file_type=PrintFormat.LATEX, temp=True)
    return send_file(filename_or_fp=path_to_doc, mimetype='text/plain')


@print_blueprint.route("/testing/pdf/<path:doc_path>", methods=['GET'])
def test_pdf(doc_path):
    doc = g.doc_entry

    printer = DocumentPrinter(doc_entry=doc)
    path = printer.get_print_path(file_type=PrintFormat.PDF)
    if os.path.exists(path):
        os.remove(path)

    path_to_doc = create_printed_doc(doc_entry=doc, file_type=PrintFormat.PDF, temp=True)
    return send_file(filename_or_fp=path_to_doc, mimetype='application/pdf')


@print_blueprint.route("/getDefaultTemplate/<path:doc_path>", methods=['GET'])
def get_default_template(doc_path):
    # Gets the latest default printing template from the tree
    # Precedence is determined such that newest equals closest to the document
    # atm. only for testing
    doc = g.doc_entry
    template_doc = None
    if doc is not None:
        template_doc = DocumentPrinter.get_default_template(doc_entry=doc)
    template_content = None
    if template_doc is not None:
        template_content = DocumentPrinter(doc_entry=template_doc)._content

    if template_content is None:
        abort(404)
    else:
        return Response(template_content, mimetype='text/plain')


@print_blueprint.route("/getCustomTemplate/<path:doc_path>", methods=['GET'])
def get_custom_template(doc_path):
    # Gets the custom template file for the document
    # The template is presumed to be located at <doc_folder>/Templates/<doc_name>
    # If the template does not exist, such a file is created by copying the latest default template
    # and the new document is returned.
    doc = g.doc_entry
    template_doc = None
    if doc is not None:
        template_doc = DocumentPrinter.get_custom_template(doc_entry=doc)
    template_content = None
    if template_doc is not None:
        template_content = DocumentPrinter(doc_entry=template_doc)._content

    if template_content is None:
        abort(404)
    else:
        return Response(template_content, mimetype='text/plain')



@print_blueprint.route("/editSettings", methods=['POST'])
def edit_settings():
    return


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

    try:
        printer = DocumentPrinter(doc_entry=doc_entry)
        path = printer.get_print_path(temp=temp, file_type=file_type)

        if os.path.exists(path):
            os.remove(path)

        folder = os.path.split(path)[0] # gets only the head of the head, tail -tuple
        if not os.path.exists(folder):
            os.makedirs(folder)
        with open(path, mode='wb') as doc_file:
            doc_file.write(printer.write_to_format(target_format=file_type))

        p_doc = PrintedDoc(doc_id=doc_entry.document.doc_id,
                           path_to_file=path,
                           temp=temp,
                           settings_hash='TESTING#####')

        db.session.add(p_doc)
        db.session.commit()

        return p_doc.path_to_file
    except PrintingError as err:
        abort(403, str(err))
