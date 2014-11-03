"""Routes for editing a document."""
import os
from bs4 import UnicodeDammit
import cssutils
from flask import Blueprint, render_template, request, redirect, url_for
from .common import *
import pluginControl
from timdb.timdbbase import DocIdentifier

edit_page = Blueprint('edit_page',
                      __name__,
                      url_prefix='')  # TODO: Better URL prefix.

@edit_page.route('/update/<int:doc_id>/<version>', methods=['POST'])
def updateDocument(doc_id, version):
    timdb = getTimDb()
    docId = DocIdentifier(doc_id, version)
    if not timdb.documents.documentExists(docId):
        abort(404)
    if not timdb.users.userHasEditAccess(getCurrentUserId(), doc_id):
        abort(403)
    newestVersion = timdb.documents.getDocumentVersions(doc_id, 1)[0]['hash']
    if version != newestVersion:
        return jsonResponse({'message': 'The document has been modified by someone else. Please refresh the page.'},
                            400)
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
    docId = request.get_json()['docId']
    verifyEditAccess(docId)
    paragraphText = request.get_json()['text']
    parIndex = request.get_json()['par']
    current_app.logger.info("Editing file: {}, paragraph {}".format(docId, parIndex ))
    version = request.headers.get('Version')
    identifier = getNewest(docId)#DocIdentifier(docId, version)

    try:
        blocks, version = timdb.documents.modifyMarkDownBlock(identifier, int(parIndex), paragraphText)
    except IOError as err:
        print(err)
        return "Failed to modify block."
    # Replace appropriate elements with plugin content, load plugin requirements to template
    preparedBlocks, jsPaths, cssPaths, modules = pluginControl.pluginify(blocks, getCurrentUserName(), timdb.answers, docId, getCurrentUserId())
    return jsonResponse({'texts' : preparedBlocks, 'js':jsPaths,'css':cssPaths,'angularModule':modules})

@edit_page.route('/edit/<int:doc_id>')
@edit_page.route("/documents/<int:doc_id>")
def editDocument(doc_id):
    timdb = getTimDb()
    if not timdb.documents.documentExists(DocIdentifier(doc_id, '')):
        abort(404)
    if not hasEditAccess(doc_id):
        if not loggedIn():
            return redirect(url_for('loginWithKorppi', came_from=request.path))
        else:
            abort(403)
    newest = getNewest(doc_id)
    doc_metadata = timdb.documents.getDocument(newest)
    xs = timdb.documents.getDocumentAsHtmlBlocks(newest)
    texts, jsPaths, cssPaths, modules = pluginControl.pluginify(xs, getCurrentUserName(), timdb.answers, doc_id, getCurrentUserId())
    modules.append("ngSanitize")
    modules.append("angularFileUpload")
    return render_template('editing.html', docId=doc_metadata['id'], docName=doc_metadata['name'], text=json.dumps(texts), version={'hash' : newest.hash}, js=jsPaths, cssFiles=cssPaths, jsMods=modules)

@edit_page.route("/newParagraph/", methods=["POST"])
def addBlock():
    timdb = getTimDb()
    jsondata = request.get_json()
    blockText = jsondata['text']
    docId = jsondata['docId']
    verifyEditAccess(docId)
    paragraph_id = jsondata['par']
    blocks, version = timdb.documents.addMarkdownBlock(getNewest(docId), blockText, int(paragraph_id))
    preparedBlocks, jsPaths, cssPaths, modules = pluginControl.pluginify(blocks, getCurrentUserName(), timdb.answers, docId, getCurrentUserId())
    return jsonResponse({'texts' : preparedBlocks, 'js':jsPaths,'css':cssPaths,'angularModule':modules})

@edit_page.route("/deleteParagraph/<int:docId>/<int:blockId>")
def removeBlock(docId, blockId):
    timdb = getTimDb()
    verifyEditAccess(docId)
    timdb.documents.deleteParagraph(getNewest(docId), blockId)
    return "Successfully removed paragraph"

