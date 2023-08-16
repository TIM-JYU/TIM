"""Common functions for use with routes."""
from collections import defaultdict
from dataclasses import dataclass
from datetime import datetime
from typing import DefaultDict

import pytz

from timApp.auth.get_user_rights_for_item import get_user_rights_for_item
from timApp.document.areainfo import AreaStart, AreaEnd
from timApp.document.docentry import DocEntry
from timApp.document.docparagraph import DocParagraph
from timApp.document.docsettings import DocSettings
from timApp.document.document import Document, dereference_pars
from timApp.document.editing.globalparid import GlobalParId
from timApp.document.hide_names import force_hide_names
from timApp.document.macroinfo import get_user_specific_macros
from timApp.document.par_basic_data import ParBasicData
from timApp.document.prepared_par import PreparedPar
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewContext
from timApp.markdown.autocounters import TimSandboxedEnvironment
from timApp.markdown.markdownconverter import expand_macros
from timApp.note.notes import get_notes, UserNoteAndUser
from timApp.plugin.plugin import Plugin
from timApp.plugin.pluginControl import pluginify
from timApp.readmark.readings import (
    get_common_readings,
    get_read_expiry_condition,
    has_anything_read,
)
from timApp.readmark.readmarkcollection import ReadMarkCollection
from timApp.readmark.readparagraph import ReadParagraph
from timApp.user.user import User, has_no_higher_right
from timApp.util.flask.responsehelper import flash_if_visible
from timApp.util.timtiming import taketime
from timApp.util.utils import getdatetime, get_boolean


@dataclass
class PostProcessResult:
    texts: list[PreparedPar]
    js_paths: list[str]
    css_paths: list[str]
    should_mark_all_read: bool
    plugins: list[Plugin]
    has_plugin_errors: bool


# TODO: post_process_pars is called twice in one save??? Or even 4 times, 2 after editor is closed??
def post_process_pars(
    doc: Document,
    pars: list[DocParagraph],
    user_ctx: UserContext,
    view_ctx: ViewContext,
    sanitize: bool = True,
    do_lazy: bool = False,
    load_plugin_states: bool = True,
    filter_return: GlobalParId | None = None,
) -> PostProcessResult:
    taketime("start pluginify")

    pars_deref = dereference_pars(pars, context_doc=doc, view_ctx=view_ctx)
    if filter_return:
        pars_deref = [
            p
            for p in pars_deref
            if p.get_doc_id() == filter_return.doc_id
            and p.get_id() == filter_return.par_id
        ]
    presult = pluginify(
        doc,
        pars_deref,
        user_ctx,
        view_ctx,
        sanitize=sanitize,
        do_lazy=do_lazy,
        load_states=load_plugin_states,
    )
    final_pars = presult.pars
    taketime("end pluginify")
    should_mark_all_read = False
    settings = doc.get_settings()
    macroinfo = settings.get_macroinfo(view_ctx, user_ctx)
    user_macros = get_user_specific_macros(user_ctx)
    macros = macroinfo.get_macros()
    delimiter = macroinfo.get_macro_delimiter()
    doc_nomacros = settings.nomacros()

    # Process user-specific macros.
    env = macroinfo.jinja_env
    for (
        p
    ) in (
        final_pars
    ):  # update only user specific, because others are done in a cache pahes
        if (
            not p.is_plugin() and not p.is_setting()
        ):  # TODO: Think if plugins still needs to expand macros?
            # p.insert_rnds(0)
            no_macros = DocParagraph.is_no_macros(p.get_attrs(), doc_nomacros)
            if not no_macros:
                ppar = p.prepare(view_ctx)
                ppar.output = expand_macros(
                    ppar.output,
                    user_macros,
                    settings,
                    env=env,
                    ignore_errors=True,
                )

    # taketime("macros done")

    if view_ctx.preview:
        # Skip readings and notes
        return PostProcessResult(
            texts=process_areas(settings, final_pars, macros, delimiter, env, view_ctx),
            js_paths=presult.js_paths,
            css_paths=presult.css_paths,
            should_mark_all_read=should_mark_all_read,
            plugins=presult.all_plugins,
            has_plugin_errors=presult.has_errors,
        )

    if settings.show_authors():
        hide_authors = view_ctx.hide_names_requested
        authors = doc.get_changelog(-1).get_authorinfo(pars)
        if hide_authors:
            for ainfo in authors.values():
                for a in ainfo.authors:
                    if isinstance(a, User):
                        a.hide_name = True
        for p in final_pars:
            ppar = p.prepare(view_ctx)
            ppar.authorinfo = authors.get(ppar.id)
    # There can be several references of the same paragraph in the document, which is why we need a dict of lists
    pars_dict: DefaultDict[tuple[str, int], list[PreparedPar]] = defaultdict(list)

    docinfo = doc.get_docinfo()
    curr_user = user_ctx.logged_user
    if not curr_user.has_edit_access(docinfo):
        for p in final_pars:
            if p.is_question():
                d = p.prepare(view_ctx)
                d.output = " "
                d.html_class = "hidden"
            if p.is_setting():
                d = p.prepare(view_ctx)
                d.output = " "
    else:
        ids = doc.get_par_ids()
        first_par = doc.get_paragraph(ids[0]) if ids else None
        last_par = doc.get_paragraph(ids[-1]) if ids else None
        show_settings_yaml = (
            last_par.is_setting() and first_par.is_setting()
            if last_par and last_par
            else True
        )
        if not show_settings_yaml:
            for p in final_pars:
                if p.is_setting():
                    d = p.prepare(view_ctx)
                    d.output = " "

    for p in final_pars:
        d = p.prepare(view_ctx)
        if p.original and not p.original.is_translation():
            target = d.target
            key = target.id, target.doc_id
            pars_dict[key].append(d)

        key = d.data.id, d.data.doc_id
        pars_dict[key].append(d)

    for p in final_pars:
        d = p.prepare(view_ctx)
        d.status = ReadMarkCollection()
        d.notes = []
    # taketime("pars done")

    group = curr_user.get_personal_group().id
    if not should_hide_readmarks(curr_user, settings):
        # taketime("readings begin")

        # TODO: UserContext should support multiple users like in group login.
        usergroup_ids = [user_ctx.logged_user.get_personal_group().id]

        # If we're in exam mode and we're visiting the page for the first time, mark everything read
        if should_auto_read(
            doc, usergroup_ids, user_ctx.logged_user, view_ctx.for_cache
        ):
            should_mark_all_read = True
            readings = []
        else:
            readings = get_common_readings(
                usergroup_ids, doc, get_read_expiry_condition(settings.read_expiry())
            )
        taketime("readings end")
        for r in readings:  # type: ReadParagraph
            key = (r.par_id, r.doc_id)
            pars = pars_dict.get(key)
            if pars:
                for p in pars:
                    if r.par_hash == p.data.hash or (
                        p.target and r.par_hash == p.target.hash
                    ):
                        p.status.add(r)
                    else:
                        p.status.add(r, modified=True)

    taketime("read mixed")
    notes = get_notes(group, doc)
    # db.session.close()
    # taketime("notes picked")

    should_hide_names = view_ctx.hide_names_requested or force_hide_names(
        curr_user, docinfo
    )
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
                has_teacher = bool(curr_user.has_teacher_access(comment_docs[n.doc_id]))
                teacher_access_cache[n.doc_id] = has_teacher
            editable = n.usergroup_id == group or has_teacher
            private = n.access == "justme"
            for p in pars:
                if p.notes is None:
                    p.notes = []
                if should_hide_names and u.id != curr_user.id:
                    u.hide_name = True
                p.notes.append(
                    UserNoteAndUser(user=u, note=n, editable=editable, private=private)
                )
    # taketime("notes mixed")

    return PostProcessResult(
        texts=process_areas(settings, final_pars, macros, delimiter, env, view_ctx),
        js_paths=presult.js_paths,
        css_paths=presult.css_paths,
        should_mark_all_read=should_mark_all_read,
        plugins=presult.all_plugins,
        has_plugin_errors=presult.has_errors,
    )


@dataclass
class Area:
    name: str
    attrs: dict
    visible: bool | None = None


# TODO: It would be better to return a tree-like structure of the document instead of a flat list.
def process_areas(
    settings: DocSettings,
    pars: list[DocParagraph],
    macros,
    delimiter,
    env: TimSandboxedEnvironment,
    view_ctx: ViewContext,
    use_md: bool = False,
    cache: bool = True,
) -> list[PreparedPar]:
    # If we're only dealing with a single paragraph (happens e.g. when posting a comment),
    # we don't want to include area start/end markers in the final output
    # because the HTML would be broken.
    is_single = len(pars) == 1

    now = pytz.utc.localize(datetime.now())
    min_time = pytz.utc.localize(datetime.min)
    max_time = pytz.utc.localize(datetime.max)

    # Currently open areas. Should be empty after the loop unless there are missing area_ends.
    current_areas: list[Area] = []

    # All non-reference areas that we've seen. Only insert here, never remove.
    encountered_areas: dict[str, Area] = {}

    new_pars: list[PreparedPar] = []
    fix = "Fix this to get rid of this warning."
    for p in pars:
        html_par = p.prepare(view_ctx, use_md, cache)
        cur_area = None
        area_start = p.get_attr("area")
        area_end = p.get_attr("area_end")
        if len(current_areas) > 0:
            if area_end != current_areas[-1].name:
                assert current_areas[-1].visible is not None
                if not current_areas[-1].visible:
                    continue
                # Timed paragraph, is there time limitation in area where par is included
                st = current_areas[-1].attrs.get("starttime")
                et = current_areas[-1].attrs.get("endtime")
                if st or et:
                    starttime = getdatetime(st, default_val=min_time)
                    endtime = getdatetime(et, default_val=max_time)
                    if starttime > now or endtime <= now:
                        continue
        if area_start is not None:
            cur_area = Area(area_start, p.get_attrs())
            current_areas.append(cur_area)
            if not p.ref_chain:
                if area_start in encountered_areas:
                    flash_if_visible(
                        f"Area {area_start} appears more than once in this document. {fix}",
                        view_ctx,
                    )
                encountered_areas[area_start] = cur_area
        if area_end is not None:
            if area_start is not None:
                flash_if_visible(
                    f"The paragraph {p.get_id()} has both area and area_end. {fix}",
                    view_ctx,
                )
            if current_areas:
                # Insert a closing paragraph for the current area.
                # We do this regardless of whether the area_end name matches because it's reasonable and we
                # cannot guess what the user is trying to do.
                if not is_single:
                    html_par.areainfo = AreaEnd(area_end)
                new_pars.append(html_par)
            try:
                latest_area = current_areas.pop()
            except IndexError:
                flash_if_visible(
                    f'area_end found for "{area_end}" without corresponding start. {fix}',
                    view_ctx,
                )
            else:
                if latest_area.name != area_end:
                    flash_if_visible(
                        f'area_end found for "{area_end}" without corresponding start. {fix}',
                        view_ctx,
                    )

        if area_start is not None or area_end is not None:
            if area_start is not None:
                # Insert an opening paragraph for new areas

                if not is_single:
                    collapse = cur_area.attrs.get("collapse")
                    html_par.areainfo = AreaStart(
                        area_start,
                        collapse not in ("false", "") if collapse is not None else None,
                    )
                new_pars.append(html_par)

                vis = cur_area.visible
                if vis is None:
                    vis = cur_area.attrs.get("visible")
                if vis is None:
                    vis = True
                elif isinstance(vis, str):
                    if vis.find(delimiter) >= 0:
                        vis = expand_macros(
                            vis, macros, settings, env=env, ignore_errors=True
                        )
                    vis = get_boolean(vis, True)
                cur_area.visible = vis
                if vis:
                    st = cur_area.attrs.get("starttime")
                    et = cur_area.attrs.get("endtime")
                    if st or et:
                        starttime = getdatetime(st, default_val=min_time)
                        endtime = getdatetime(et, default_val=max_time)
                        if not starttime <= now < endtime:
                            alttext = cur_area.attrs.get("alttext")
                            if alttext is None:
                                alttext = "This area can only be viewed from <STARTTIME> to <ENDTIME>"
                            alttext = alttext.replace(
                                "<STARTTIME>", str(starttime)
                            ).replace("<ENDTIME>", str(endtime))
                            new_pars.append(
                                DocParagraph.create(
                                    doc=Document(html_par.doc_id),
                                    par_id=html_par.id,
                                    md=alttext,
                                ).prepare(view_ctx, use_md, cache)
                            )
                else:
                    # Hide output of the area paragraph if it's there (e.g. collapse title)
                    html_par.areainfo.is_collapsed = None
                    html_par.output = ""

        else:
            # Just a normal paragraph
            access = True
            vis = p.get_attr(
                "visible"
            )  # check if there is visible attribute in par itself
            if vis is None:
                pass
            else:
                if str(vis).find(delimiter) >= 0:
                    vis = expand_macros(
                        vis, macros, settings, env=env, ignore_errors=True
                    )
                vis = get_boolean(vis, True)
                if not vis:  #  TODO: if in preview, put this always True
                    access = False  # TODO: this should be added as some kind of small par that is visible in edit-mode
            if access:
                new_pars.append(html_par)

    # Complete unbalanced areas.
    if current_areas and not is_single:
        flash_if_visible(
            f"{len(current_areas)} areas are missing area_end: {current_areas}",
            view_ctx,
        )
        for _ in current_areas:
            new_pars.append(
                PreparedPar(
                    data=ParBasicData(attrs={}, doc_id=-1, hash="", id="", md=""),
                    output="",
                    from_preamble=None,
                    target=None,
                    areainfo=AreaEnd(name=""),
                    html_class="",
                )
            )
    return new_pars


def should_auto_read(
    doc: Document, usergroup_ids: list[int], user: User, for_cache: bool = False
) -> bool:
    return user.get_prefs().auto_mark_all_read or (
        has_no_higher_right(
            doc.get_settings().exam_mode(),
            get_user_rights_for_item(doc.docinfo, user, allow_duration=for_cache),
        )
        and not has_anything_read(usergroup_ids, doc)
    )


def should_hide_readmarks(curr_user: User, settings: DocSettings) -> bool:
    return not curr_user.logged_in or settings.hide_readmarks()
