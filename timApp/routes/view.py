"""Routes for document view."""

from flask import Blueprint, render_template, redirect, url_for
from .common import *
from .cache import cache
from containerLink import PLUGINS

import pluginControl

view_page = Blueprint('view_page',
                      __name__,
                      url_prefix='')

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
    folder_name = doc_name.rstrip('/')
    block_id = timdb.folders.getFolderId(folder_name)

    if block_id is None:
        abort(404)

    possible_groups = timdb.users.getUserGroupsPrintable(getCurrentUserId())
    return render_template('index.html',
                           docID=block_id,
                           userName=getCurrentUserName(),
                           userId=getCurrentUserId(),
                           userGroups=possible_groups,
                           is_owner=hasOwnership(block_id),
                           docName=folder_name)


@cache.memoize(3600)
def get_document(document_id):
    return getTimDb().documents.getDocumentAsHtmlBlocksSanitized(document_id)


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

    version = {'hash': timdb.documents.getNewestVersionHash(doc_id)}
    xs = get_document(DocIdentifier(doc_id, version['hash']))
    doc = timdb.documents.getDocument(doc_id)
    start_index = 0
    if view_range is not None:
        start_index = max(view_range[0], 0)
        end_index = min(view_range[1], len(xs))
        xs = xs[start_index:end_index + 1]

    user = getCurrentUserId()
    if teacher:
        task_ids = pluginControl.find_task_ids(xs, doc_id)
        users = timdb.answers.getUsersForTasks(task_ids)
        if len(users) > 0:
            user = users[0]['id']
    else:
        users = []
    current_user = timdb.users.getUser(user)
    texts, jsPaths, cssPaths, modules = pluginControl.pluginify(xs,
                                                                current_user['name'],
                                                                timdb.answers,
                                                                doc_id,
                                                                current_user['id'],
                                                                sanitize=False)
    if hide_names_in_teacher(doc_id):
        pass
        if not timdb.users.userIsOwner(current_user['id'], doc_id)\
           and current_user['id'] != getCurrentUserId():
            current_user['name'] = '-'
            current_user['real_name'] = 'Undisclosed student'
        for user in users:
            if not timdb.users.userIsOwner(user['id'], doc_id)\
               and user['id'] != getCurrentUserId():
                user['name'] = '-'
                user['real_name'] = 'Undisclosed student %d' % user['id']

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
                           is_owner=hasOwnership(doc_id),
                           rights={'editable': hasEditAccess(doc_id),
                                   'can_mark_as_read': hasReadMarkingRight(doc_id),
                                   'can_comment': hasCommentRight(doc_id),
                                   'browse_own_answers': loggedIn()
                                   },
                           plugins=PLUGINS)
