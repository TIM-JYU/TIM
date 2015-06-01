"""Routes for editing a document."""
import os
from bs4 import UnicodeDammit
import cssutils
from flask import Blueprint, render_template, request, redirect, url_for
from .common import *
import pluginControl
from timdb.docidentifier import DocIdentifier

edit_page = Blueprint('edit_page',
                      __name__,
                      url_prefix='')  # TODO: Better URL prefix.

@edit_page.route('/update/<int:doc_id>/<version>', methods=['POST'])
def updateDocument(doc_id, version):
    timdb = getTimDb()
    docId = DocIdentifier(doc_id, version)
    if not timdb.documents.documentExists(doc_id):
        abort(404)
    if not timdb.users.userHasEditAccess(getCurrentUserId(), doc_id):
        abort(403)
    verify_document_version(doc_id, version)
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
        json = request.get_json()
        if not 'fulltext' in json:
            return jsonResponse({'message': 'Malformed request - fulltext missing.'}, 400)
        content = json['fulltext']

    if content is None:
        return jsonResponse({'message': 'Failed to convert the file to UTF-8.'}, 400)
    newId = timdb.documents.updateDocument(docId, content)
    return jsonResponse(timdb.documents.getDocumentVersions(doc_id))

@edit_page.route("/postParagraph/", methods=['POST'])
def postParagraph():
    timdb = getTimDb()
    doc_id, paragraphText, parIndex = verify_json_params('docId', 'text', 'par')
    verifyEditAccess(doc_id)
    current_app.logger.info("Editing file: {}, paragraph {}".format(doc_id, parIndex ))
    version = request.headers.get('Version', '')
    verify_document_version(doc_id, version)
    identifier = DocIdentifier(doc_id, version)

    try:
        blocks, doc = timdb.documents.modifyMarkDownBlock(identifier, int(parIndex), paragraphText)
    except IOError as err:
        print(err)
        abort('Failed to modify block', 400)
    # Replace appropriate elements with plugin content, load plugin requirements to template
    preparedBlocks, jsPaths, cssPaths, modules = pluginControl.pluginify(blocks, getCurrentUserName(), timdb.answers, doc_id, getCurrentUserId())
    return jsonResponse({'texts': preparedBlocks,
                         'js': jsPaths,
                         'css': cssPaths,
                         'angularModule': modules,
                         'version': doc.hash})

@edit_page.route("/preview/<int:doc_id>", methods=['POST'])
def preview(doc_id):
    timdb = getTimDb()
    paragraphText, = verify_json_params('text')

    blocks = timdb.documents.previewBlock(paragraphText)

    preparedBlocks, jsPaths, cssPaths, modules = pluginControl.pluginify(blocks, getCurrentUserName(), timdb.answers, doc_id, getCurrentUserId())
    return jsonResponse({'texts': preparedBlocks,
                         'js': jsPaths,
                         'css': cssPaths,
                         'angularModule': modules})

@edit_page.route("/newParagraph/", methods=["POST"])
def addBlock():
    timdb = getTimDb()
    blockText, doc_id, paragraph_id = verify_json_params('text', 'docId', 'par')
    verifyEditAccess(doc_id)
    version = request.headers.get('Version', '')
    verify_document_version(doc_id, version)
    blocks, new_doc = timdb.documents.addMarkdownBlock(getNewest(doc_id), blockText, int(paragraph_id))
    preparedBlocks, jsPaths, cssPaths, modules = pluginControl.pluginify(blocks, getCurrentUserName(), timdb.answers, doc_id, getCurrentUserId())
    return jsonResponse({'texts': preparedBlocks,
                         'js': jsPaths,
                         'css': cssPaths,
                         'angularModule': modules,
                         'version': new_doc.hash})

@edit_page.route("/deleteParagraph/<int:doc_id>/<int:blockId>", methods=["POST"])
def removeBlock(doc_id, blockId):
    timdb = getTimDb()
    verifyEditAccess(doc_id)
    version = request.headers.get('Version', '')
    verify_document_version(doc_id, version)
    new_doc = timdb.documents.deleteParagraph(getNewest(doc_id), blockId)
    return jsonResponse({'version': new_doc.hash})
