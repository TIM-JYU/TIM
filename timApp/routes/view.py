"""Routes for document view."""

from flask import Blueprint, render_template, redirect, url_for
from .common import *
import pluginControl

view_page = Blueprint('view_page',
                      __name__,
                      url_prefix='')


@view_page.route("/view/<int:doc_id>")
def view_document(doc_id):
    return view(doc_id, 'view.html')


@view_page.route("/view_html/<int:doc_id>")
def view_document_html(doc_id):
    return view(doc_id, 'view_html.html')


def view(doc_id, template_name):
    timdb = getTimDb()
    if not timdb.documents.documentExists(doc_id):
        abort(404)
    if not hasViewAccess(doc_id):
        if not loggedIn():
            return redirect(url_for('loginWithKorppi', came_from=request.path))
        else:
            abort(403)
    if not loggedIn():
        return redirect(url_for('loginWithKorppi', came_from=request.path))
    version = timdb.documents.getNewestVersion(doc_id)
    xs = timdb.documents.getDocumentAsHtmlBlocks(DocIdentifier(doc_id, version['hash']))
    doc = timdb.documents.getDocument(doc_id)
    texts, jsPaths, cssPaths, modules = pluginControl.pluginify(xs, getCurrentUserName(), timdb.answers, doc_id, getCurrentUserId())
    modules.append("ngSanitize")
    modules.append("angularFileUpload")
    prefs = timdb.users.getPrefs(getCurrentUserId())
    custom_css_files = json.loads(prefs).get('css_files', {}) if prefs is not None else {}
    if custom_css_files:
        custom_css_files = {key: value for key, value in custom_css_files.items() if value}
    custom_css = json.loads(prefs).get('custom_css', '') if prefs is not None else ''
    return render_template(template_name,
                           docID=doc['id'],
                           docName=doc['name'],
                           text=texts,
                           version=version,
                           js=jsPaths,
                           cssFiles=cssPaths,
                           jsMods=modules,
                           custom_css_files=custom_css_files,
                           custom_css=custom_css)
