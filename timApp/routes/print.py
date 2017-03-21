"""
Routes for printing a document
"""
import subprocess

from flask import Blueprint
from flask import abort
from flask import Response

from accesshelper import verify_logged_in
from timdb.models.docentry import DocEntry

print = Blueprint('print',
                   __name__,
                   url_prefix='/print')


@print.route("/<path:doc_path>", methods=['GET'])
def print_document(doc_path):
    # verify_logged_in()

    md = get_markdown(doc_path)

    return Response(call_pandoc(md), mimetype='text/plain')


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

    # This way we don't have to construct any long string in memory.
    def generate_response():
        for p in d.document.get_paragraphs():
            yield p.get_markdown() + '\n\n'

    # Return the response as plain text using the above generator.
    return generate_response()


def call_pandoc(md):
    return subprocess.check_output(["pandoc", "-t latex"])