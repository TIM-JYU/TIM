"""Common functions for use with routes."""
import json
import os
import re
from collections import defaultdict
from urllib.parse import urlparse, urljoin

from flask import current_app, session, abort, g, Response, request, redirect, url_for, flash

import documentmodel.document
import pluginControl
from documentmodel.docparagraphencoder import DocParagraphEncoder
from documentmodel.document import Document
from theme import Theme
from tim_app import db
from timdb.timdb2 import TimDb
from utils import generate_theme_scss, get_combined_css_filename, ThemeNotFoundException


def is_safe_url(url):
    host_url = urlparse(request.host_url)
    test_url = urlparse(urljoin(request.host_url, url))
    return test_url.scheme in ['http', 'https'] and \
           host_url.netloc == test_url.netloc


def safe_redirect(url, **values):
    if is_safe_url(url):
        return redirect(url, **values)
    return redirect(url_for('indexPage'))


def get_current_user():
    return getTimDb().users.get_user(getCurrentUserId())


def getCurrentUserId():
    uid = session.get('user_id')
    return uid if uid is not None else 0


def getCurrentUserName():
    name = session.get('user_name')
    return name if name is not None else 'Anonymous'


def getCurrentUserGroup():
    timdb = getTimDb()
    return timdb.users.get_personal_usergroup(get_current_user())


def getTimDb():
    """

    :rtype : TimDb
    """
    if not hasattr(g, 'timdb'):
        g.timdb = TimDb(db_path=current_app.config['DATABASE'],
                        files_root_path=current_app.config['FILES_PATH'],
                        session=db.session,
                        current_user_name=getCurrentUserName())
    return g.timdb


def verify_admin():
    timdb = getTimDb()
    if not timdb.users.has_admin_access(getCurrentUserId()):
        abort(403, 'This action requires administrative rights.')


def verify_doc_exists(doc_id, message="Sorry, the document does not exist."):
    timdb = getTimDb()
    if not timdb.documents.exists(doc_id):
        abort(404, message)


def verify_edit_access(block_id, message="Sorry, you don't have permission to edit this resource."):
    timdb = getTimDb()
    if not timdb.users.has_edit_access(getCurrentUserId(), block_id):
        abort(403, message)


def verify_manage_access(block_id, message="Sorry, you don't have permission to manage this resource."):
    timdb = getTimDb()
    if not timdb.users.has_manage_access(getCurrentUserId(), block_id):
        abort(403, message)


def has_edit_access(block_id):
    timdb = getTimDb()
    return timdb.users.has_edit_access(getCurrentUserId(), block_id)


def verify_doc_existence(doc_id):
    timdb = getTimDb()
    if not timdb.documents.exists(doc_id):
        abort(404)


def verify_view_access(block_id, require=True, message=None):
    return verify_access(getTimDb().users.has_view_access(getCurrentUserId(), block_id), require, message)


def verify_teacher_access(block_id, require=True, message=None):
    return verify_access(getTimDb().users.has_teacher_access(getCurrentUserId(), block_id), require, message)


def verify_seeanswers_access(block_id, require=True, message=None):
    return verify_access(getTimDb().users.has_seeanswers_access(getCurrentUserId(), block_id), require, message)


def verify_access(has_access, require=True, message=None):
    if has_access:
        return True
    if require:
        abort(403, message or "Sorry, you don't have permission to view this resource.")
    return False


def has_view_access(block_id):
    timdb = getTimDb()
    return timdb.users.has_view_access(getCurrentUserId(), block_id)


def has_comment_right(doc_id):
    return has_view_access(doc_id) and logged_in()


def verify_comment_right(doc_id):
    if not has_comment_right(doc_id):
        abort(403)


def has_read_marking_right(doc_id):
    return has_view_access(doc_id) and logged_in()


def verify_read_marking_right(doc_id):
    if not has_read_marking_right(doc_id):
        abort(403)


def has_teacher_access(doc_id):
    timdb = getTimDb()
    return timdb.users.has_teacher_access(getCurrentUserId(), doc_id)


def has_manage_access(doc_id):
    timdb = getTimDb()
    return timdb.users.has_manage_access(getCurrentUserId(), doc_id)


def has_seeanswers_access(doc_id):
    timdb = getTimDb()
    return timdb.users.has_seeanswers_access(getCurrentUserId(), doc_id)


def get_rights(doc_id):
    return {'editable': has_edit_access(doc_id),
            'can_mark_as_read': has_read_marking_right(doc_id),
            'can_comment': has_comment_right(doc_id),
            'browse_own_answers': logged_in(),
            'teacher': has_teacher_access(doc_id),
            'see_answers': has_seeanswers_access(doc_id),
            'manage': has_manage_access(doc_id),
            'owner': has_ownership(doc_id)
            }


def verifyLoggedIn():
    if not logged_in():
        abort(403, "You have to be logged in to perform this action.")


def has_ownership(block_id):
    timdb = getTimDb()
    return timdb.users.user_is_owner(getCurrentUserId(), block_id)


def verify_ownership(block_id):
    timdb = getTimDb()
    if not timdb.users.user_is_owner(getCurrentUserId(), block_id):
        abort(403, "Sorry, you don't have permission to view this resource.")


def logged_in():
    return getCurrentUserId() != 0


def can_write_to_folder(folderName):
    timdb = getTimDb()
    userFolder = "users/" + getCurrentUserName()
    folder = folderName
    while folder != '':
        if folder == userFolder:
            return True

        folderId = timdb.folders.get_folder_id(folder)
        if folderId is not None:
            return has_edit_access(folderId)

        folder, _ = timdb.folders.split_location(folder)

    return timdb.users.has_admin_access(getCurrentUserId())


def jsonResponse(jsondata, status_code=200):
    response = Response(json.dumps(jsondata,
                                   separators=(',', ':'),
                                   cls=DocParagraphEncoder), mimetype='application/json')
    response.status_code = status_code
    return response


def okJsonResponse():
    return jsonResponse({'status': 'ok'})


def get_newest_document(doc_id):
    """
    Returns the newest Document object with the specified numeric id.

    :rtype: Document
    :type doc_id: int
    :param doc_id: The numeric id.
    :return: The Document object.
    """

    return Document(doc_id, modifier_group_id=getCurrentUserGroup())

def verify_document_version(doc_id, version):
    timdb = getTimDb()
    latest = Document(doc_id).get_version()
    if version != latest:
        abort(400, 'The document version you edited is no longer the latest version. '
                   'Please refresh the page and try again.')


def verify_json_params(*args, require=True, default=None):
    """

    :type args: list[str]
    :rtype: tuple[str]
    """
    result = ()
    json_params = request.get_json() or []
    for arg in args:
        if arg in json_params:
            val = json_params[arg]
        elif not require:
            val = default
        else:
            abort(400, 'Missing required parameter in request: {}'.format(arg))
            return ()

        result += (val,)
    return result


def unpack_args(*args, types):
    result = ()
    json_params = request.args
    for idx, arg in enumerate(args):
        if arg not in json_params:
            abort(400, 'Missing required parameter in request: {}'.format(arg))
        result = result + (types[idx](json_params[arg]),)
    return result


def hide_names_in_teacher(doc_id):
    return False


def post_process_pars(doc, pars, user, sanitize=True, do_lazy=False, edit_window=False, load_plugin_states=True):
    timdb = getTimDb()
    html_pars, js_paths, css_paths, modules = pluginControl.pluginify(doc,
                                                                      pars,
                                                                      user,
                                                                      timdb.answers,
                                                                      sanitize=sanitize,
                                                                      do_lazy=do_lazy,
                                                                      edit_window=edit_window,
                                                                      load_states=load_plugin_states)
    #req_json = request.get_json()

    #if req_json is not None and 'ref-id' in req_json and req_json['ref-id'] != '':
    #    ref_doc_id = req_json.get('ref-doc-id')
    #    ref_id = req_json.get('ref-id')
    #    html_pars = [par for par in html_pars if par['doc_id'] == ref_doc_id and par['id'] == ref_id]

    if edit_window:
        # Skip readings and notes
        return html_pars, js_paths, css_paths, modules

    # There can be several references of the same paragraph in the document, which is why we need a dict of lists
    pars_dict = defaultdict(list)
    for htmlpar in html_pars:
        if htmlpar.get('ref_id') and htmlpar.get('ref_doc_id'):
            key = htmlpar.get('ref_id'), htmlpar.get('ref_doc_id')
            pars_dict[key].append(htmlpar)

        key = htmlpar['id'], htmlpar['doc_id']
        pars_dict[key].append(htmlpar)

    for p in html_pars:
        p['status'] = ''
        p['notes'] = []

    group = timdb.users.get_personal_usergroup(user) if user is not None else timdb.users.get_anon_group_id()
    if user is not None:
        readings = timdb.readings.get_readings(group, doc)
        for r in readings:
            key = (r['par_id'], r['doc_id'])
            pars = pars_dict.get(key)
            if pars:
                for p in pars:
                    if r['par_hash'] == p['t'] or r['par_hash'] == p.get('ref_t'):
                        p['status'] = 'read'
                    elif p.get('status') != 'read':
                        # elif is here so not to overwrite an existing 'read' marking
                        p['status'] = 'modified'

    notes = timdb.notes.getNotes(group, doc)

    for n in notes:
        key = (n['par_id'], n['doc_id'])
        pars = pars_dict.get(key)
        if pars:
            n['editable'] = n['UserGroup_id'] == group or timdb.users.user_is_owner(getCurrentUserId(), doc.doc_id)
            n.pop('UserGroup_id', None)
            n['private'] = n['access'] == 'justme'
            for p in pars:
                if 'notes' not in p:
                    p['notes'] = []
                p['notes'].append(n)
    return html_pars, js_paths, css_paths, modules


def get_referenced_pars_from_req(par):
    if par.is_reference():
        return [ref_par for ref_par in par.get_referenced_pars(set_html=False, tr_get_one=False)]
    else:
        return [par]


def validate_item(item_name, item_type):
    if not logged_in():
        abort(403, 'You have to be logged in to perform this action.'.format(item_type))

    if item_name is None:
        abort(400, 'item_name was None')

    if not all(part for part in item_name.split('/')):
        abort(400, 'The {} name cannot have empty parts.'.format(item_type))

    if re.match('^(\d)*$', item_name) is not None:
        abort(400, 'The {} name can not be a number to avoid confusion with document id.'.format(item_type))
    timdb = getTimDb()
    username = getCurrentUserName()
    if timdb.documents.get_document_id(item_name) is not None or timdb.folders.get_folder_id(item_name) is not None:
        abort(403, 'Item with a same name already exists.')

    if not can_write_to_folder(item_name):
        abort(403, 'You cannot create {}s in this folder. Try users/{} instead.'.format(item_type, username))


def validate_item_and_create(item_name, item_type, owner_group_id):
    timdb = getTimDb()
    validate_item(item_name, item_type)
    item_path, _ = timdb.folders.split_location(item_name)
    timdb.folders.create(item_path, owner_group_id)


def get_user_settings():
    return session.get('settings', {})


def get_preferences():
    """
    Gets the preferences of the current user.

    :return: A dictionary of the user preferences.
    """
    prefs = {}
    if logged_in():
        prefs = getTimDb().users.get_preferences(getCurrentUserId())
        prefs = json.loads(prefs) if prefs is not None else {}
    if not prefs:
        prefs['css_files'] = {}
        prefs['custom_css'] = ''
    css_file_list = [css for css, v in prefs['css_files'].items() if v]
    css_file_list.sort()
    theme_list = [Theme(f) for f in css_file_list]
    try:
        generate_theme_scss(theme_list, os.path.join('static', current_app.config['SASS_GEN_PATH']))
    except ThemeNotFoundException as e:
        flash('TIM was updated and some theme files (such as {}) are no longer available. '
              'See the settings page for the available themes.'.format(e))
        update_preferences(prefs)
        return get_preferences()
    prefs['css_combined'] = get_combined_css_filename(theme_list)
    return prefs


def update_preferences(prefs):
    timdb = getTimDb()
    css_files = prefs.get('css_files', {})
    existing_css_files = {}
    for k, v in css_files.items():
        t = Theme(k)
        if t.exists() and v:
            existing_css_files[t.filename] = True
    prefs['css_files'] = existing_css_files
    timdb.users.set_preferences(getCurrentUserId(), json.dumps(prefs))


def verify_task_access(doc_id, task_id_name):
    # If the user doesn't have access to the document, we need to check if the plugin was referenced
    # from another document
    if not verify_view_access(doc_id, require=False):
        orig_doc = request.get_json().get('ref_from', {}).get('docId', doc_id)
        verify_view_access(orig_doc)
        par_id = request.get_json().get('ref_from', {}).get('par', doc_id)
        par = Document(orig_doc).get_paragraph(par_id)
        if not par.is_reference():
            abort(403)
        pars = documentmodel.document.dereference_pars([par])
        if not any(p.get_attr('taskId') == task_id_name for p in pars):
            abort(403)