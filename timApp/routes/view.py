"""Routes for document view."""

from flask import Blueprint, render_template, redirect, url_for
from .common import *

import pluginControl


view_page = Blueprint('view_page',
                      __name__,
                      url_prefix='')


@view_page.route("/view_old/<path:doc_name>")
def view_document_old(doc_name):
    view_range = parse_range(request.args.get('b'), request.args.get('e'))
    return view(doc_name, 'view.html', view_range)

@view_page.route("/view/<path:doc_name>")
@view_page.route("/view_html/<path:doc_name>")
@view_page.route("/doc/<path:doc_name>")
def view_document(doc_name):
    try:
        view_range = parse_range(request.args.get('b'), request.args.get('e'))
    except (ValueError, TypeError):
        abort(400, "Invalid start or end index specified.")

    return view(doc_name, 'view_html.html', view_range)

@view_page.route("/teacher/<path:doc_name>")
def teacher_view(doc_name):
    try:
        view_range = parse_range(request.args.get('b'), request.args.get('e'))
        userstr = request.args.get('user')
        user = int(userstr) if userstr is not None and userstr != '' else 0
    except (ValueError, TypeError):
        abort(400, "Invalid start or end index specified.")

    return view(doc_name, 'view_html.html', view_range, user, teacher=True)


def parse_range(start_index, end_index):
    if start_index is None and end_index is None:
        return None

    return( int(start_index), int(end_index) )


def view(doc_name, template_name, view_range=None, user=0, teacher=False):
    timdb = getTimDb()
    doc_id = timdb.documents.getDocumentId(doc_name)
    
    if teacher:
        verifyOwnership(doc_id)
    
    if doc_id is None or not timdb.documents.documentExists(doc_id):
        # Backwards compatibility: try to use as document id
        try:
            doc_id = int(doc_name)
            if not timdb.documents.documentExists(doc_id):
                abort(404)
        except ValueError:
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
        
    if teacher:
        texts, jsPaths, cssPaths, modules = pluginControl.pluginify(xs, timdb.users.getUser(user)[1], timdb.answers, doc_id, user, browseAnswers=True)
        task_ids = pluginControl.find_task_ids(xs, doc_id)
        users = timdb.answers.getUsersForTasks(task_ids)
    else:
        texts, jsPaths, cssPaths, modules = pluginControl.pluginify(xs, getCurrentUserName(), timdb.answers, doc_id, getCurrentUserId())
        users = []
               
    modules.append("ngSanitize")
    modules.append("angularFileUpload")
    prefs = timdb.users.getPrefs(getCurrentUserId())
    custom_css_files = json.loads(prefs).get('css_files', {}) if prefs is not None else {}
    if custom_css_files:
        custom_css_files = {key: value for key, value in custom_css_files.items() if value}
    custom_css = json.loads(prefs).get('custom_css', '') if prefs is not None else ''
    editable = hasEditAccess(doc_id)
    return render_template(template_name,
                           docID=doc['id'],
                           docName=doc['name'],
                           text=texts,
                           plugin_users=users,
                           version=version,
                           js=jsPaths,
                           cssFiles=cssPaths,
                           jsMods=modules,
                           custom_css_files=custom_css_files,
                           custom_css=custom_css,
                           start_index=start_index,
                           editable=editable)
