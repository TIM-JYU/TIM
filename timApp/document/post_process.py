"""Common functions for use with routes."""
from collections import defaultdict
from datetime import datetime
from typing import List, Dict, DefaultDict, Tuple

import pytz
from flask import flash

from timApp.auth.accesshelper import has_ownership, has_edit_access, has_teacher_access
from timApp.document.docentry import DocEntry
from timApp.document.hide_names import hide_names_in_teacher
from timApp.note.notes import get_notes, UserNoteAndUser
from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document
from timApp.readmark.readmarkcollection import ReadMarkCollection
from timApp.markdown.markdownconverter import expand_macros, create_environment
from timApp.plugin.pluginControl import pluginify
from timApp.auth.sessioninfo import get_session_usergroup_ids, get_current_user_object
from timApp.user.user import User, check_rights
from timApp.readmark.readings import get_common_readings, get_read_expiry_condition, has_anything_read
from timApp.readmark.readparagraph import ReadParagraph
from timApp.util.timtiming import taketime
from timApp.util.utils import getdatetime, get_boolean


# TODO: post_process_pars is called twice in one save??? Or even 4 times, 2 after editor is closed??
def post_process_pars(doc: Document, pars, user: User, sanitize=True, do_lazy=False, edit_window=False,
                      load_plugin_states=True) -> Tuple[List[dict], List[str], List[str], bool]:
    taketime("start pluginify")
    final_pars, js_paths, css_paths, _ = pluginify(
        doc,
        pars,
        user,
        sanitize=sanitize,
        do_lazy=do_lazy,
        edit_window=edit_window,
        load_states=load_plugin_states,
    )
    taketime("end pluginify")
    should_mark_all_read = False
    settings = doc.get_settings()
    macroinfo = settings.get_macroinfo()
    user_macros = macroinfo.get_user_specific_macros(user)
    macros = macroinfo.get_macros_with_user_specific(user)
    delimiter = macroinfo.get_macro_delimiter()
    doc_nomacros = settings.nomacros()
    # Process user-specific macros.
    # We define the environment here because it stays the same for each paragraph. This improves performance.
    env = create_environment(delimiter)
    for p in final_pars:  # update only user specific, because others are done in a cache pahes
        if not p.is_plugin() and not p.is_setting():  # TODO: Think if plugins still needs to expand macros?
            # p.insert_rnds(0)
            no_macros = DocParagraph.is_no_macros(p.get_attrs(), doc_nomacros)
            if not no_macros:
                f_dict = p.get_final_dict()
                f_dict['html'] = expand_macros(f_dict['html'], user_macros, settings, delimiter, env=env,
                                               ignore_errors=True)

    # taketime("macros done")

    if edit_window:
        # Skip readings and notes
        return process_areas(settings, final_pars, macros, delimiter, env), js_paths, css_paths, should_mark_all_read

    if settings.show_authors():
        authors = doc.get_changelog(-1).get_authorinfo(pars)
        for p in final_pars:
            f_dict = p.get_final_dict()
            f_dict['authorinfo'] = authors.get(f_dict['id'])
    # There can be several references of the same paragraph in the document, which is why we need a dict of lists
    pars_dict: DefaultDict[Tuple[str, int], List[dict]] = defaultdict(list)

    docinfo = doc.get_docinfo()
    if not has_edit_access(docinfo):
        for p in final_pars:
            if p.is_question():
                d = p.get_final_dict()
                d['html'] = ' '
                d['cls'] = 'hidden'
            if p.is_setting():
                d = p.get_final_dict()
                d['html'] = ' '
    else:
        ids = doc.get_par_ids()
        first_par = doc.get_paragraph(ids[0]) if ids else None
        last_par = doc.get_paragraph(ids[-1]) if ids else None
        show_settings_yaml = last_par.is_setting() and first_par.is_setting() if last_par and last_par else True
        if not show_settings_yaml:
            for p in final_pars:
                if p.is_setting():
                    d = p.get_final_dict()
                    d['html'] = ' '

    for p in final_pars:
        d = p.get_final_dict()
        if p.original and not p.original.is_translation():
            key = d.get('ref_id'), d.get('ref_doc_id')
            pars_dict[key].append(d)

        key = d['id'], d['doc_id']
        pars_dict[key].append(d)

    for p in final_pars:
        d = p.get_final_dict()
        d['status'] = ReadMarkCollection()
        d['notes'] = []
    # taketime("pars done")

    curr_user = get_current_user_object()
    group = curr_user.get_personal_group().id
    if curr_user.logged_in:
        # taketime("readings begin")

        usergroup_ids = get_session_usergroup_ids()

        # If we're in exam mode and we're visiting the page for the first time, mark everything read
        if check_rights(doc.get_settings().exam_mode(), doc.docinfo.rights) and \
                not has_anything_read(usergroup_ids, doc):
            should_mark_all_read = True
            readings = []
        else:
            readings = get_common_readings(usergroup_ids,
                                        doc,
                                        get_read_expiry_condition(settings.read_expiry()))
        taketime("readings end")
        for r in readings:  # type: ReadParagraph
            key = (r.par_id, r.doc_id)
            pars = pars_dict.get(key)
            if pars:
                for p in pars:
                    if r.par_hash == p['t'] or r.par_hash == p.get('ref_t'):
                        p['status'].add(r)
                    else:
                        p['status'].add(r, modified=True)

    taketime("read mixed")
    notes = get_notes(group, doc)
    # taketime("notes picked")

    should_hide_names = hide_names_in_teacher(docinfo)
    comment_docs = {docinfo.id: docinfo}
    teacher_access_cache = {}
    for n, u in notes:
        key = (n.par_id, n.doc_id)
        pars = pars_dict.get(key)
        if pars:
            if n.doc_id not in comment_docs:
                comment_docs[n.doc_id] = DocEntry.find_by_id(n.doc_id)
            has_teacher = teacher_access_cache.get(n.doc_id)
            if has_teacher is None:
                has_teacher = bool(has_teacher_access(comment_docs[n.doc_id]))
                teacher_access_cache[n.doc_id] = has_teacher
            editable = n.usergroup_id == group or has_teacher
            private = n.access == 'justme'
            for p in pars:
                if 'notes' not in p:
                    p['notes'] = []
                if should_hide_names and u.id != curr_user.id:
                    u.hide_name = True
                p['notes'].append(UserNoteAndUser(user=u, note=n, editable=editable, private=private))
    # taketime("notes mixed")

    return process_areas(settings, final_pars, macros, delimiter, env), js_paths, css_paths, should_mark_all_read


def process_areas(settings, pars: List[DocParagraph], macros, delimiter, env) -> List[Dict]:
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

    for p in pars:
        html_par = p.get_final_dict()
        new_areas = current_areas.copy()
        cur_area = None
        area_start = p.get_attr('area')
        area_end = p.get_attr('area_end')
        if area_start is not None:
            cur_area = Area(get_free_index(), p.get_attrs())
            new_areas[area_start] = cur_area
        if area_end is not None:
            try:
                free_indexes[new_areas[area_end].index] = True
            except KeyError:
                flash(
                    f'area_end found for "{area_end}" without corresponding start. Fix this to get rid of this warning.')
            new_areas.pop(area_end, None)

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
                            vis = expand_macros(vis, macros, settings, delimiter, env=env, ignore_errors=True)
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
                                                                    md=alttext).get_final_dict())

        else:
            # Just a normal paragraph
            access = True
            vis = p.get_attr('visible')  # check if there is visible attribute in par itself
            if vis is None:
                pass
            else:
                if str(vis).find(delimiter) >= 0:
                    vis = expand_macros(vis, macros, settings, delimiter, env=env, ignore_errors=True)
                vis = get_boolean(vis, True)
                if not vis:  #  TODO: if in preview, put this allways True
                    access = False  # TODO: this should be added as some kind of small par that is visible in edit-mode
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

            if access:
                new_pars.append(html_par)

        current_areas = new_areas

    return new_pars
