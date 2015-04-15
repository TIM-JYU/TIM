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
        user = int(userstr) if userstr is not None and userstr != '' else None
    except (ValueError, TypeError):
        abort(400, "Invalid start or end index specified.")

    return view(doc_name, 'view_html.html', view_range, user, teacher=True)


def parse_range(start_index, end_index):
    if start_index is None and end_index is None:
        return None

    return( int(start_index), int(end_index) )

def try_return_folder(doc_name):
    timdb = getTimDb()
    #docs = timdb.documents.getDocuments(historylimit=0)
    #allowedDocs = [doc for doc in docs if timdb.users.userHasViewAccess(getCurrentUserId(), doc['id'])]
    folder_name = doc_name.rstrip('/')
    #path_name = folder_name + '/'

    if timdb.folders.getFolderId(folder_name) is None:
        abort(404)

    possible_groups = timdb.users.getUserGroups(getCurrentUserId())
    return render_template('index.html',
                           userName=getCurrentUserName(),
                           userId=getCurrentUserId(),
                           userGroups=possible_groups,
                           folder=folder_name)

    #for doc in allowedDocs:
    #    if doc['name'].startswith(path_name):
    #        possible_groups = timdb.users.getUserGroups(getCurrentUserId())
    #        return render_template('index.html',
    #                               userName=getCurrentUserName(),
    #                               userId=getCurrentUserId(),
    #                               userGroups=possible_groups,
    #                               folder=folder_name)



def view(doc_name, template_name, view_range=None, user=None, teacher=False):
    timdb = getTimDb()
    doc_id = timdb.documents.getDocumentId(doc_name)
    
    if doc_id is None or not timdb.documents.documentExists(doc_id):
        # Backwards compatibility: try to use as document id
        try:
            doc_id = int(doc_name)
            if not timdb.documents.documentExists(doc_id):
                #abort(404)
                return try_return_folder(doc_name)
        except ValueError:
            return try_return_folder(doc_name)
            #abort(404)

    if teacher:
        verifyOwnership(doc_id)

    if not hasViewAccess(doc_id):
        if not loggedIn():
            session['came_from'] = request.url
            return render_template('loginpage.html', target_url=url_for('login_page.loginWithKorppi'), came_from=request.url)
        else:
            abort(403)

    version = timdb.documents.getNewestVersion(doc_id)
    xs = timdb.documents.getDocumentAsHtmlBlocks(DocIdentifier(doc_id, version['hash']))
    doc = timdb.documents.getDocument(doc_id)
    start_index = 0
    if view_range is not None:
        start_index = max(view_range[0], 0)
        end_index = min(view_range[1], len(xs))
        xs = xs[start_index:end_index + 1]

    if teacher:
        task_ids = pluginControl.find_task_ids(xs, doc_id)
        users = timdb.answers.getUsersForTasks(task_ids)
        if user is None:
            user = getCurrentUserId()
    else:
        user = getCurrentUserId()
        users = []
    current_user = timdb.users.getUser(user)
    texts, jsPaths, cssPaths, modules = pluginControl.pluginify(xs,
                                                                current_user['name'],
                                                                timdb.answers,
                                                                doc_id,
                                                                current_user['id'])
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
                           plugin_users=users,
                           current_user=current_user,
                           version=version,
                           js=jsPaths,
                           cssFiles=cssPaths,
                           jsMods=modules,
                           custom_css_files=custom_css_files,
                           custom_css=custom_css,
                           start_index=start_index,
                           teacher_mode=teacher,
                           rights={'editable': hasEditAccess(doc_id),
                                   'can_mark_as_read': hasReadMarkingRight(doc_id),
                                   'can_comment': hasCommentRight(doc_id),
                                   'browse_own_answers': loggedIn()
                                   })
