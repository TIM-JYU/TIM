"""Routes for document view."""

from flask import Blueprint, render_template, redirect, url_for
from .common import *
import pluginControl

view_page = Blueprint('view_page',
                      __name__,
                      url_prefix='')


@view_page.route("/view_old/<path:doc_name>")
def view_document_old(doc_name):
    return view(doc_name, 'view.html')

@view_page.route("/view/<path:doc_name>")
def view_document(doc_name):
    return view(doc_name, 'view_html.html')

@view_page.route("/view_html/<path:doc_name>")
def view_document_html(doc_name):
    return view(doc_name, 'view_html.html')


@view_page.route("/view_html/<int:doc_id>/<int:start_index>/<int:end_index>")
def view_document_part(doc_id, start_index, end_index):
    return view(doc_id, 'view_html.html', (start_index, end_index))


def view(doc_name, template_name, view_range=None):
    timdb = getTimDb()
    doc_id = timdb.documents.getDocumentId(doc_name)
    
    if doc_id is None or not timdb.documents.documentExists(doc_id):
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
    start_index = 0
    if view_range is not None:
        start_index = max(view_range[0], 0)
        end_index = min(view_range[1], len(xs))
        xs = xs[start_index:end_index + 1]
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
                           custom_css=custom_css,
                           start_index=start_index)
