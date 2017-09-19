"""Common functions for use with routes."""
from collections import defaultdict
from datetime import datetime
from typing import List, Dict

import pytz
from flask import session, abort, request, flash

from timApp.accesshelper import has_ownership, has_edit_access
from timApp.dbaccess import get_timdb
from timApp.documentmodel.docparagraph import DocParagraph
from timApp.documentmodel.document import Document
from timApp.markdownconverter import expand_macros, create_environment
from timApp.pluginControl import pluginify
from timApp.sessioninfo import get_session_usergroup_ids
from timApp.timdb.models.user import User
from timApp.timdb.userutils import get_anon_group_id
from timApp.timtiming import taketime
from timApp.utils import getdatetime
from timApp.requesthelper import get_boolean


def verify_doc_exists(doc_id, message="Sorry, the document does not exist."):
    timdb = get_timdb()
    if not timdb.documents.exists(doc_id):
        abort(404, message)


# noinspection PyUnusedLocal
def hide_names_in_teacher(doc_id):
    return False


# TODO: post_process_pars is called twice in one save??? Or even 4 times, 2 after editor is closed??
def post_process_pars(doc: Document, pars, user: User, sanitize=True, do_lazy=False, edit_window=False,
                      load_plugin_states=True):
    timdb = get_timdb()
    taketime("start pluginfy")
    html_pars, js_paths, css_paths, modules = pluginify(doc,
                                                        pars,
                                                        user,
                                                        timdb,
                                                        sanitize=sanitize,
                                                        do_lazy=do_lazy,
                                                        edit_window=edit_window,
                                                        load_states=load_plugin_states)
    taketime("end pluginfy")
    macroinfo = doc.get_settings().get_macroinfo()
    user_macros = macroinfo.get_user_specific_macros(user)
    macros = macroinfo.get_macros_with_user_specific(user)
    delimiter = macroinfo.get_macro_delimiter()
    # Process user-specific macros.
    # We define the environment here because it stays the same for each paragraph. This improves performance.
    env = create_environment(delimiter)
    for htmlpar in html_pars: # update only user specific, because others are done in a cache pahes
        if not htmlpar['is_plugin']:  # TODO: Think if plugins still needs to expand macros?
            # htmlpar.insert_rnds(0)
            if not htmlpar['attrs'].get('nomacros', False):
                htmlpar['html'] = expand_macros(htmlpar['html'], user_macros, delimiter, env=env, ignore_errors=True)

    # taketime("macros done")

    if edit_window:
        # Skip readings and notes
        return process_areas(html_pars, macros, delimiter, env), js_paths, css_paths, modules

    # There can be several references of the same paragraph in the document, which is why we need a dict of lists
    pars_dict = defaultdict(list)

    if not has_edit_access(doc.doc_id):
        for htmlpar in html_pars:
            if htmlpar.get('is_question'):
                htmlpar['html'] = ' '
                htmlpar['cls'] = 'hidden'

    for htmlpar in html_pars:
        if htmlpar.get('ref_id') and htmlpar.get('ref_doc_id'):
            key = htmlpar.get('ref_id'), htmlpar.get('ref_doc_id')
            pars_dict[key].append(htmlpar)

        key = htmlpar['id'], htmlpar['doc_id']
        pars_dict[key].append(htmlpar)

    for p in html_pars:
        p['status'] = set()
        p['notes'] = []
    # taketime("pars done")

    group = user.get_personal_group().id if user is not None else get_anon_group_id()
    if user is not None:
        # taketime("readings begin")
        readings = timdb.readings.get_common_readings(get_session_usergroup_ids(), doc)
        taketime("readings end")
        for r in readings:  # TODO: this takes more than one sec???
            key = (r.par_id, r.doc_id)
            pars = pars_dict.get(key)
            if pars:
                for p in pars:
                    if r.par_hash == p['t'] or r.par_hash == p.get('ref_t'):
                        p['status'].add(r.type.class_str())
                    elif r.type.class_str() not in p.get('status'):
                        # elif is here so not to overwrite an existing 'read' marking
                        p['status'].add(r.type.class_str() + '-modified')

    taketime("read mixed")
    notes = timdb.notes.get_notes(group, doc)
    is_owner = has_ownership(doc.doc_id)
    # Close database here because we won't need it for a while
    timdb.close()
    # taketime("notes picked")

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
    # taketime("notes mixed")

    return process_areas(html_pars, macros, delimiter, env), js_paths, css_paths, modules


def process_areas(html_pars: List[Dict], macros, delimiter, env) -> List[Dict]:
    class Area:

        def __init__(self, index, area_attrs):
            self.index = index
            self.attrs = area_attrs

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
                    try:
                        free_indexes[new_areas[area_end].index] = True
                    except KeyError:
                        flash(
                            f'area_end found for "{area_end}" without corresponding start. Fix this to get rid of this warning.')
                    new_areas.pop(area_end, None)
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
                                 'cls': ' '.join(html_par.get('attrs', {} ).get('classes', [])),
                                 'start_areas': {a: new_areas[a].index for a in new_areas},
                                 'collapsed': 'collapsed ' if len(current_collapsed) > 0 else ''})

                if collapse is None and area_end is None:
                    new_pars.append(html_par)

                if cur_area is not None:
                    vis = cur_area.attrs.get('visible')
                    if vis is None:
                        vis = True
                    else:
                        if str(vis).find(delimiter) >= 0:
                            vis = expand_macros(vis, macros, delimiter, env=env, ignore_errors=True)
                        vis = get_boolean(vis, True)
                        cur_area.attrs['visible'] = vis
                    if vis:
                        st = cur_area.attrs.get('starttime')
                        et = cur_area.attrs.get('endtime')
                        if st or et:
                            starttime = getdatetime(st, default_val=min_time)
                            endtime = getdatetime(et, default_val=max_time)
                            if not starttime <= now < endtime:
                                alttext = cur_area.attrs.get('alttext')
                                if alttext is None:
                                    alttext = "This area can only be viewed from <STARTTIME> to <ENDTIME>"
                                alttext = alttext.replace('<STARTTIME>', str(starttime)).replace('<ENDTIME>', str(endtime))
                                new_pars.append(DocParagraph.create(doc=Document(html_par['doc_id']), par_id=html_par['id'],
                                                                    md=alttext).html_dict())

        else:
            # new_pars.append(html_par)
            # continue
            # Just a normal paragraph
            access = True
            attrs = html_par.get('attrs')
            vis = attrs.get('visible')  # check if there is visible attribute in par itself
            if vis is None:
                vis = True
            else:
                if str(vis).find(delimiter) >= 0:
                    vis = expand_macros(vis, macros, delimiter, env=env, ignore_errors=True)
                vis = get_boolean(vis, True)
                if not vis:
                    access = False  # TODO: this should be added as some kind of small par that is visible in edit-mode

                # if any([a.attrs.get('starttime') or a.attrs.get('endtime') for a in current_areas.values()]):
                # Timed paragraph
            if access:  # par itself is visible, is it in some area that is not visible
                for a in current_areas.values():
                    vis = a.attrs.get('visible')
                    if vis is not None:
                        vis = get_boolean(vis, True)
                        if not vis:
                            access = False
                    if access: # is there time limitation in area where par is included
                        st = a.attrs.get('starttime')
                        et = a.attrs.get('endtime')
                        if st or et:
                            starttime = getdatetime(st, default_val=min_time)
                            endtime = getdatetime(et, default_val=max_time)
                            access &= starttime <= now < endtime
                            # if not access and a.attrs.get('alttext') is not None:
                            #     TODO:  what??? here was just todo?  alttext is allready show in area?
                            #     noinspection PyUnusedLocal
                            #     alttext = a.attrs.get('alttext')

            if access:
                new_pars.append(html_par)

        current_areas = new_areas

    return new_pars


def has_special_chars(item_path):
    return set(item_path.lower()) - set('abcdefghijklmnopqrstuvwxyz0123456789/-_')


def get_user_settings():
    return session.get('settings', {})


def save_last_page():
    session['last_doc'] = request.path


def is_considered_unpublished(doc_id):
    timdb = get_timdb()
    owner = timdb.users.get_owner_group(doc_id)
    return has_ownership(doc_id) and (not owner or not owner.is_large()) and \
           len(timdb.users.get_rights_holders(doc_id)) <= 1
