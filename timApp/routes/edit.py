"""Routes for editing a document."""
from bs4 import UnicodeDammit
from flask import Blueprint

from .common import *
from documentmodel.docparagraph import DocParagraph
from markdownconverter import md_to_html
import pluginControl
from timdb.docidentifier import DocIdentifier

edit_page = Blueprint('edit_page',
                      __name__,
                      url_prefix='')  # TODO: Better URL prefix.


@edit_page.route('/update/<int:doc_id>/<version>', methods=['POST'])
def update_document(doc_id, version):
    """Route for updating a document as a whole.

    :param doc_id: The id of the document to be modified.
    :param version: The version string of the current document version.
    :return: A JSON object containing the versions of the document.
    """
    timdb = getTimDb()
    doc_identifier = DocIdentifier(doc_id, version)
    if not timdb.documents.documentExists(doc_id):
        abort(404)
    if not timdb.users.userHasEditAccess(getCurrentUserId(), doc_id):
        abort(403)
    # verify_document_version(doc_id, version)
    if 'file' in request.files:
        doc = request.files['file']
        raw = doc.read()

        # UnicodeDammit gives incorrect results if the encoding is UTF-8 without BOM,
        # so try the built-in function first.
        try:
            content = raw.decode('utf-8')
        except UnicodeDecodeError:
            content = UnicodeDammit(raw).unicode_markup
    else:
        request_json = request.get_json()
        if 'fulltext' not in request_json:
            return jsonResponse({'message': 'Malformed request - fulltext missing.'}, 400)
        content = request_json['fulltext']

    if content is None:
        return jsonResponse({'message': 'Failed to convert the file to UTF-8.'}, 400)
    timdb.documents.updateDocument(doc_identifier, content)
    return jsonResponse(timdb.documents.getDocumentVersions(doc_id))


@edit_page.route("/postParagraph/", methods=['POST'])
def modify_paragraph():
    """
    Route for modifying a paragraph in a document.

    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.
    """
    timdb = getTimDb()
    doc_id, md, par_id = verify_json_params('docId', 'text', 'par')
    verifyEditAccess(doc_id)
    current_app.logger.info("Editing file: {}, paragraph {}".format(doc_id, par_id))
    version = request.headers.get('Version', '')
    # verify_document_version(doc_id, version)
    identifier = get_newest_document(doc_id)

    blocks, doc = timdb.documents.modify_paragraph(identifier, par_id, md)
    # Replace appropriate elements with plugin content, load plugin requirements to template
    pars, js_paths, css_paths, modules = pluginControl.pluginify(blocks,
                                                                 getCurrentUserName(),
                                                                 timdb.answers,
                                                                 doc_id,
                                                                 getCurrentUserId())
    return jsonResponse({'texts': pars,
                         'js': js_paths,
                         'css': css_paths,
                         'angularModule': modules,
                         'version': doc.get_version()})


@edit_page.route("/preview/<int:doc_id>", methods=['POST'])
def preview(doc_id):
    """Route for previewing a paragraph.

    :param doc_id: The id of the document in which the preview will be renderer. Unused so far.
    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.
    """
    timdb = getTimDb()
    md, = verify_json_params('text')
    blocks = [DocParagraph(html=md_to_html(md))]
    pars, js_paths, css_paths, modules = pluginControl.pluginify(blocks,
                                                                 getCurrentUserName(),
                                                                 timdb.answers,
                                                                 doc_id,
                                                                 getCurrentUserId())
    return jsonResponse({'texts': pars,
                         'js': js_paths,
                         'css': css_paths,
                         'angularModule': modules})


@edit_page.route("/newParagraph/", methods=["POST"])
def add_paragraph():
    """Route for adding a new paragraph to a document.

    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.
    """
    timdb = getTimDb()
    md, doc_id, paragraph_id = verify_json_params('text', 'docId', 'par_next')
    verifyEditAccess(doc_id)
    version = request.headers.get('Version', '')
    # verify_document_version(doc_id, version)
    blocks, new_doc = timdb.documents.add_paragraph(get_newest_document(doc_id), md, paragraph_id)
    pars, js_paths, css_paths, modules = pluginControl.pluginify(blocks,
                                                                 getCurrentUserName(),
                                                                 timdb.answers,
                                                                 doc_id,
                                                                 getCurrentUserId())
    return jsonResponse({'texts': pars,
                         'js': js_paths,
                         'css': css_paths,
                         'angularModule': modules,
                         'version': new_doc.get_version()})


@edit_page.route("/deleteParagraph/<int:doc_id>/<par_id>", methods=["POST"])
def delete_paragraph(doc_id, par_id):
    """Route for deleting a paragraph from a document.

    :param doc_id: The id of the document.
    :param par_id: The id of the paragraph.
    :return: A JSON object containing the version of the new document.
    """
    timdb = getTimDb()
    verifyEditAccess(doc_id)
    version = request.headers.get('Version', '')
    # verify_document_version(doc_id, version)
    new_doc = timdb.documents.delete_paragraph(get_newest_document(doc_id), par_id)
    return jsonResponse({'version': new_doc.get_version()})
