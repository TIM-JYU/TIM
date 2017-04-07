"""
Routes for printing a document
"""
import string

from flask import Blueprint
from flask import abort
from flask import Response
from flask import stream_with_context
from flask import g

from accesshelper import verify_logged_in
from timdb.models.docentry import DocEntry
from sessioninfo import get_current_user_object
from documentprinter import DocumentPrinter
from timdb.printsettings import PrintSettings

print_blueprint = Blueprint('print',
                   __name__,
                   url_prefix='/print')

#@print_blueprint.before_request
def do_before_requests():
    verify_logged_in()
    g.PrintSettings = PrintSettings(get_current_user_object())


@print_blueprint.route("/<path:doc_path>", methods=['GET'])
def print_document(doc_path):
    printer = DocumentPrinter(DocEntry.find_by_path(doc_path).document, PrintSettings())
    doc_path = printer.write_tex()
    return Response(doc_path, mimetype='text/plain')


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

    # This way we don't have to construct any long string in memory.
    #def generate_response():
     #   for p in d.document.get_paragraphs():
      #      yield call_pandoc(p.get_markdown() + '\n\n', to_format='latex')

    # Return the response as plain text using the above generator.
    return doc
