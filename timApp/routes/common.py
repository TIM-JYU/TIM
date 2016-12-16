"""Common functions for use with routes."""
import json
import os
import re
from collections import defaultdict
from datetime import datetime
from typing import List, Dict
from urllib.parse import urlparse, urljoin

import dateutil.parser
import magic
import pytz
from bs4 import UnicodeDammit
from flask import current_app, session, abort, Response, request, redirect, url_for, flash

import pluginControl
from documentmodel.docparagraph import DocParagraph
from documentmodel.document import Document
from documentmodel.timjsonencoder import TimJsonEncoder
from markdownconverter import expand_macros, create_environment
from routes.accesshelper import has_ownership, can_write_to_folder, has_edit_access
from routes.dbaccess import get_timdb
from routes.sessioninfo import get_session_usergroup_ids, get_current_user_id, get_current_user_name, \
    get_current_user_group, logged_in
from theme import Theme
from timdb.models.docentry import DocEntry
from timdb.models.folder import Folder
from timdb.models.user import User
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


def verify_doc_exists(doc_id, message="Sorry, the document does not exist."):
    timdb = get_timdb()
    if not timdb.documents.exists(doc_id):
        abort(404, message)


def jsonResponse(jsondata, status_code=200):
    response = Response(to_json_str(jsondata), mimetype='application/json')
    response.status_code = status_code
    return response


def to_json_str(jsondata):
    return json.dumps(jsondata,
                      separators=(',', ':'),
                      cls=TimJsonEncoder)


def set_no_cache_headers(response: Response) -> Response:
    """
    Sets headers for the response that should prevent any caching of the result.
    :param response: Response to be modified.
    :return: We also return the modified object for convenience.
    """
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


def okJsonResponse():
    return jsonResponse({'status': 'ok'})


def get_document_as_current_user(doc_id: int) -> Document:
    """
    Returns the Document object having the current user group as the modifier group ip.

    :param doc_id: The numeric id.
    :return: The Document object.
    """

    return Document(doc_id, modifier_group_id=get_current_user_group())

def verify_document_version(doc_id, version):
    timdb = get_timdb()
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


def post_process_pars(doc: Document, pars, user: User, sanitize=True, do_lazy=False, edit_window=False, load_plugin_states=True):
    timdb = get_timdb()
    html_pars, js_paths, css_paths, modules = pluginControl.pluginify(doc,
                                                                      pars,
                                                                      user,
                                                                      timdb,
                                                                      sanitize=sanitize,
                                                                      do_lazy=do_lazy,
                                                                      edit_window=edit_window,
                                                                      load_states=load_plugin_states)
    #req_json = request.get_json()

    #if req_json is not None and 'ref-id' in req_json and req_json['ref-id'] != '':
    #    ref_doc_id = req_json.get('ref-doc-id')
    #    ref_id = req_json.get('ref-id')
    #    html_pars = [par for par in html_pars if par['doc_id'] == ref_doc_id and par['id'] == ref_id]

    settings = doc.get_settings()
    user_macros = settings.get_user_specific_macros(user)
    delimiter = settings.get_macro_delimiter()
    # Process user-specific macros.
    # We define the environment here because it stays the same for each paragraph. This improves performance.
    env = create_environment(delimiter)
    for htmlpar in html_pars:
        htmlpar['html'] = expand_macros(htmlpar['html'], user_macros, delimiter, env=env, ignore_errors=True)

    if edit_window:
        # Skip readings and notes
        return process_areas(html_pars), js_paths, css_paths, modules

    # There can be several references of the same paragraph in the document, which is why we need a dict of lists
    pars_dict = defaultdict(list)

    if not has_edit_access(doc.doc_id):
        for htmlpar in html_pars:
            if htmlpar.get('is_question'):
                htmlpar['html'] = ' '
                htmlpar['cls'] += ' hidden'

    for htmlpar in html_pars:
        if htmlpar.get('ref_id') and htmlpar.get('ref_doc_id'):
            key = htmlpar.get('ref_id'), htmlpar.get('ref_doc_id')
            pars_dict[key].append(htmlpar)

        key = htmlpar['id'], htmlpar['doc_id']
        pars_dict[key].append(htmlpar)

    for p in html_pars:
        p['status'] = set()
        p['notes'] = []

    group = user.get_personal_group().id if user is not None else timdb.users.get_anon_group_id()
    if user is not None:
        readings = timdb.readings.get_common_readings(get_session_usergroup_ids(), doc)
        for r in readings:
            key = (r.par_id, r.doc_id)
            pars = pars_dict.get(key)
            if pars:
                for p in pars:
                    if r.par_hash == p['t'] or r.par_hash == p.get('ref_t'):
                        p['status'].add(r.type.class_str())
                    elif r.type.class_str() not in p.get('status'):
                        # elif is here so not to overwrite an existing 'read' marking
                        p['status'].add(r.type.class_str() + '-modified')

    notes = timdb.notes.get_notes(group, doc)
    is_owner = timdb.users.user_is_owner(get_current_user_id(), doc.doc_id)
    # Close database here because we won't need it for a while
    timdb.close()

    for n in notes:
        key = (n['par_id'], n['doc_id'])
        pars = pars_dict.get(key)
        if pars:
            n['editable'] = n['usergroup_id'] == group or is_owner
            n.pop('usergroup_id', None)
            n['private'] = n['access'] == 'justme'
            for p in pars:
                if 'notes' not in p:
                    p['notes'] = []
                p['notes'].append(n)

    return process_areas(html_pars), js_paths, css_paths, modules


def getdatetime(s: str, default_val = None):
    try:
        dt = dateutil.parser.parse(s, dayfirst=True)
        return dt if dt.tzinfo is not None else pytz.utc.localize(dt)

    except (ValueError, TypeError):
        return default_val


def process_areas(html_pars: List[Dict]) -> List[Dict]:
    class Area:
        def __init__(self, index, attrs):
            self.index = index
            self.attrs = attrs

    now = pytz.utc.localize(datetime.now())
    min_time = pytz.utc.localize(datetime.min)
    max_time = pytz.utc.localize(datetime.max)

    current_areas = {}
    current_collapsed = []
    new_pars = []
    free_indexes = {0: False, 1: True, 2: True, 3: True}

    def get_free_index():
        for i in range(1, 4):
            if free_indexes[i]:
                free_indexes[i] = False
                return i
        return 0

    for html_par in html_pars:
        new_areas = current_areas.copy()
        area_start = None
        area_end = None
        cur_area = None

        for attrs in ['ref_attrs', 'attrs']:
            if attrs in html_par:
                area_start = html_par[attrs].get('area')
                area_end = html_par[attrs].get('area_end')
                if area_start is not None:
                    cur_area = Area(get_free_index(), html_par[attrs])
                    new_areas[area_start] = cur_area
                if area_end is not None:
                    free_indexes[new_areas[area_end].index] = True
                    new_areas.pop(area_end)
                break

        html_par['areas'] = new_areas
        html_par['other_areas'] = current_areas

        if new_areas != current_areas:
            # This paragraph changes the open areas
            if len(current_areas) > 0:
                # Insert a closing paragraph for current areas
                if area_end is not None:
                    new_pars.append(html_par)
                    if area_end in current_collapsed:
                        current_collapsed.remove(area_end)
                new_pars.append({'id': html_par['id'], 'md': '', 'html': '',
                                 'end_areas': {a: current_areas[a].index for a in current_areas}})

            if len(new_areas) > 0:
                # Insert an opening paragraph for new areas
                collapse = cur_area.attrs.get('collapse') if cur_area else None
                if collapse is not None:
                    html_par['collapse_area'] = area_start
                    collapse_classes = ['areaexpand' if collapse else 'areacollapse']
                    collapse_classes.extend(['areawidget_' + area for area in new_areas if area != area_start])
                    collapse_classes.extend(['par'])

                    if len(current_collapsed) > 0:
                        collapse_classes.append('collapsed')
                    if collapse:
                        current_collapsed.append(area_start)

                    html_par['collapse_class'] = ' '.join(collapse_classes)
                    new_pars.append(html_par)

                new_pars.append({'id': html_par['id'], 'md': '', 'html': '',
                                 'start_areas': {a: new_areas[a].index for a in new_areas},
                                 'collapsed': 'collapsed ' if len(current_collapsed) > 0 else ''})

                if collapse is None and area_end is None:
                    new_pars.append(html_par)

                if cur_area is not None and (cur_area.attrs.get('starttime') or cur_area.attrs.get('endtime')):
                    starttime = getdatetime(cur_area.attrs.get('starttime'), default_val=min_time)
                    endtime = getdatetime(cur_area.attrs.get('endtime'), default_val=max_time)
                    if not starttime <= now < endtime:
                        alttext = cur_area.attrs.get('alttext')
                        if alttext is None:
                            alttext = "This area can only be viewed from <STARTTIME> to <ENDTIME>"
                        alttext = alttext.replace('<STARTTIME>', str(starttime)).replace('<ENDTIME>', str(endtime))
                        new_pars.append(DocParagraph.create(doc=Document(html_par['doc_id']), par_id=html_par['id'],
                                                            md=alttext).html_dict())

        else:
            # Just a normal paragraph
            access = True

            if any([a.attrs.get('starttime') or a.attrs.get('endtime') for a in current_areas.values()]):
                # Timed paragraph
                for a in current_areas.values():
                    starttime = getdatetime(a.attrs.get('starttime'), default_val=min_time)
                    endtime = getdatetime(a.attrs.get('endtime'), default_val=max_time)
                    access &= starttime <= now < endtime
                    if not access and a.attrs.get('alttext') is not None:
                        alttext = a.attrs.get('alttext')

            if access:
                new_pars.append(html_par)

        current_areas = new_areas

    return new_pars

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
    timdb = get_timdb()
    username = get_current_user_name()
    if DocEntry.find_by_path(item_name, try_translation=True) is not None or timdb.folders.get_folder_id(item_name) is not None:
        abort(403, 'Item with a same name already exists.')

    if not can_write_to_folder(item_name):
        abort(403, 'You cannot create {}s in this folder. Try users/{} instead.'.format(item_type, username))


def validate_item_and_create(item_name, item_type, owner_group_id):
    timdb = get_timdb()
    validate_item(item_name, item_type)
    item_path, _ = timdb.folders.split_location(item_name)
    Folder.create(item_path, owner_group_id, apply_default_rights=True)


def get_user_settings():
    return session.get('settings', {})


def get_preferences():
    """
    Gets the preferences of the current user.

    :return: A dictionary of the user preferences.
    """
    prefs = {}
    if logged_in():
        timdb = get_timdb()
        prefs = timdb.users.get_preferences(get_current_user_id())
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
    timdb = get_timdb()
    css_files = prefs.get('css_files', {})
    existing_css_files = {}
    for k, v in css_files.items():
        t = Theme(k)
        if t.exists() and v:
            existing_css_files[t.filename] = True
    prefs['css_files'] = existing_css_files
    timdb.users.set_preferences(get_current_user_id(), json.dumps(prefs))


def save_last_page():
    session['last_doc'] = request.path


def is_considered_unpublished(doc_id):
    timdb = get_timdb()
    owner = timdb.users.get_owner_group(doc_id)
    return has_ownership(doc_id) and not owner.is_large() and len(timdb.users.get_rights_holders(doc_id)) == 0


def validate_uploaded_document_content(file_content):
    raw = file_content.read()
    mime = magic.Magic(mime=True)
    mimetype = mime.from_buffer(raw)
    if mimetype not in current_app.config['ALLOWED_DOCUMENT_UPLOAD_MIMETYPES']:
        abort(400, 'Only markdown files are allowed. This file appears to be {}.'.format(mimetype))

    # UnicodeDammit gives incorrect results if the encoding is UTF-8 without BOM,
    # so try the built-in function first.
    try:
        content = raw.decode('utf-8')
    except UnicodeDecodeError:
        content = UnicodeDammit(raw).unicode_markup
    return content
