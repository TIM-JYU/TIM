"""Routes for document view."""
import dataclasses
import html
import time
from difflib import context_diff
from typing import Union, Any, ValuesView, Generator

import attr
import filelock
import sass
from flask import current_app
from flask import flash
from flask import redirect
from flask import render_template, make_response, Response, stream_with_context
from flask import request
from flask import session
from markupsafe import Markup
from marshmallow import EXCLUDE
from sqlalchemy.orm import joinedload, defaultload

from timApp.answer.answers import add_missing_users_from_groups, get_points_by_rule
from timApp.auth.accesshelper import (
    verify_view_access,
    verify_teacher_access,
    get_doc_or_abort,
    verify_manage_access,
    AccessDenied,
    ItemLockedException,
    verify_edit_access,
    verify_route_access,
)
from timApp.auth.auth_models import BlockAccess
from timApp.auth.get_user_rights_for_item import get_user_rights_for_item
from timApp.auth.sessioninfo import get_current_user_object, logged_in, save_last_page
from timApp.document.caching import check_doc_cache, set_doc_cache, refresh_doc_expire
from timApp.document.create_item import (
    create_or_copy_item,
    create_citation_doc,
    get_templates_for_folder,
)
from timApp.document.docentry import DocEntry, get_documents
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.docrenderresult import DocRenderResult
from timApp.document.docsettings import DocSettings, get_minimal_visibility_settings
from timApp.document.document import (
    get_index_from_html_list,
    dereference_pars,
    Document,
)
from timApp.document.docviewparams import DocViewParams, ViewModelSchema
from timApp.document.hide_names import is_hide_names, force_hide_names
from timApp.document.post_process import (
    post_process_pars,
    should_auto_read,
    should_hide_readmarks,
)
from timApp.document.preloadoption import PreloadOption
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import (
    default_view_ctx,
    ViewRoute,
    ViewContext,
    viewmode_templates,
    DEFAULT_VIEWMODE_TEMPLATE,
)
from timApp.document.viewparams import ViewParams, ViewParamsSchema
from timApp.folder.folder import Folder
from timApp.folder.folder_view import try_return_folder
from timApp.item.block import BlockType, Block
from timApp.item.blockrelevance import BlockRelevance
from timApp.item.item import Item
from timApp.item.partitioning import (
    get_piece_size_from_cookie,
    decide_view_range,
    get_doc_version_hash,
    load_index,
    INCLUDE_IN_PARTS_CLASS_NAME,
    save_index,
    partition_texts,
    get_index_with_header_id,
    get_document_areas,
    RequestedViewRange,
    IndexedViewRange,
    get_area_range,
)
from timApp.item.scoreboard import get_score_infos_if_enabled
from timApp.item.tag import GROUP_TAG_PREFIX
from timApp.item.validation import has_special_chars
from timApp.messaging.messagelist.messagelist_utils import (
    MESSAGE_LIST_DOC_PREFIX,
    MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX,
)
from timApp.peerreview.util.groups import generate_review_groups, PeerReviewException
from timApp.peerreview.util.peerreview_utils import (
    get_reviews_where_user_is_reviewer,
    check_review_grouping,
    is_peerreview_enabled,
)
from timApp.plugin.plugin import find_task_ids
from timApp.plugin.pluginControl import get_all_reqs
from timApp.readmark.readings import mark_all_read
from timApp.tim_app import app
from timApp.timdb.exceptions import PreambleException
from timApp.timdb.sqa import db
from timApp.user.groups import verify_group_view_access
from timApp.user.settings.style_utils import resolve_themes
from timApp.user.settings.styles import generate_style
from timApp.user.user import User, has_no_higher_right
from timApp.user.usergroup import (
    UserGroup,
    get_usergroup_eager_query,
    UserGroupWithSisuInfo,
)
from timApp.user.users import get_rights_holders_all
from timApp.user.userutils import DeletedUserException
from timApp.util.flask.requesthelper import (
    view_ctx_with_urlmacros,
    RouteException,
    NotExist,
)
from timApp.util.flask.responsehelper import add_no_cache_headers
from timApp.util.flask.responsehelper import (
    json_response,
    ok_response,
    get_grid_modules,
)
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.timtiming import taketime
from timApp.util.utils import get_error_message, cache_folder_path
from timApp.util.utils import remove_path_special_chars, seq_to_str
from timApp.velp.velpgroups import set_default_velp_group_selected_and_visible
from tim_common.html_sanitize import sanitize_css

DEFAULT_RELEVANCE = 10

view_page = TypedBlueprint(
    "view_page",
    __name__,
    url_prefix="",
)

DocumentSlice = tuple[list[DocParagraph], IndexedViewRange]
ViewRange = Union[RequestedViewRange, IndexedViewRange]
FlaskViewResult = Union[Response, tuple[Any, int]]


def get_partial_document(doc: Document, view_range: ViewRange) -> DocumentSlice:
    all_pars = doc.get_paragraphs()

    # Handle common unrestricted case first.
    if view_range.is_full:
        return all_pars, IndexedViewRange(b=0, e=len(all_pars), par_count=len(all_pars))

    si = view_range.start_index
    ei = view_range.end_index
    if isinstance(view_range, RequestedViewRange):
        spid = view_range.start_par_id
        epid = view_range.end_par_id
    else:
        spid = None
        epid = None
    actual_start_index = None
    actual_end_index = None
    if si is not None and ei is not None:
        actual_start_index = si
        actual_end_index = ei
    elif si is not None:
        actual_start_index = si
    elif ei is not None:
        actual_end_index = ei
    else:
        for i, par in enumerate(all_pars):
            if par.get_id() == epid:
                actual_end_index = i
            if par.get_id() == spid:
                actual_start_index = i
            if actual_start_index is not None and actual_end_index is not None:
                break
    if actual_start_index is None:
        actual_start_index = 0
    if actual_end_index is None:
        actual_end_index = (
            len(all_pars)
            if view_range.size is None
            else actual_start_index + view_range.size
        )
    pars = all_pars[actual_start_index:actual_end_index]
    return pars, IndexedViewRange(
        b=actual_start_index, e=actual_end_index, par_count=len(all_pars)
    )


def get_document(doc_info: DocInfo, view_range: ViewRange) -> DocumentSlice:
    doc = doc_info.document
    doc.preload_option = PreloadOption.all
    return get_partial_document(doc, view_range)


@view_page.get("/show_slide/<path:doc_path>")
def show_slide(doc_path):
    return view(doc_path, ViewRoute.ShowSlide)


@view_page.get("/view/<path:doc_path>")
def view_document(doc_path):
    taketime("route view begin")
    ret = view(doc_path, ViewRoute.View)
    taketime("route view end")
    return ret


@view_page.get("/teacher/<path:doc_path>")
def teacher_view(doc_path):
    return view(doc_path, ViewRoute.Teacher)


@view_page.get("/velp/<path:doc_path>")
def velp_view(doc_path):
    return view(doc_path, ViewRoute.Velp)


@view_page.get("/answers/<path:doc_path>")
def see_answers_view(doc_path):
    return view(doc_path, ViewRoute.Answers)


@view_page.get("/lecture/<path:doc_path>")
def lecture_view(doc_path):
    return view(doc_path, ViewRoute.Lecture)


@view_page.get("/review/<path:doc_name>")
def review_view(doc_name):
    return view(doc_name, ViewRoute.Review)


@view_page.get("/slide/<path:doc_path>")
def slide_view(doc_path):
    return view(doc_path, ViewRoute.Slide, render_doc=False)


@view_page.get("/par_info/<int:doc_id>/<par_id>")
def par_info(doc_id, par_id):
    doc = get_doc_or_abort(doc_id)
    verify_view_access(doc)
    for o in doc.owners:
        o.load_personal_user()
    par_name = doc.document.get_closest_paragraph_title(par_id)
    return json_response(
        {
            "item": doc,
            "par_name": par_name,
        }
    )


@view_page.get("/docViewInfo/<path:doc_name>")
def doc_access_info(doc_name):
    doc_info = DocEntry.find_by_path(doc_name, fallback_to_id=True)
    if not doc_info:
        raise NotExist()

    can_access = False
    try:
        view_access = verify_view_access(doc_info, require=False, check_duration=True)
        can_access = view_access is not None
    except ItemLockedException as ile:
        view_access = ile.access

    return json_response(
        {"can_access": can_access, "right": view_access}, date_conversion=True
    )


@attr.s(auto_attribs=True)
class ItemWithRights:
    i: Item
    rights: list[BlockAccess]

    def to_json(self):
        return {
            **self.i.to_json(),
            "grouprights": self.rights,
        }


@view_page.get("/getItems")
def items_route(
    folder: str | None = None,
    folder_id: int | None = None,
    recursive: bool = False,
    include_rights: bool = False,
):
    if folder is not None:
        f = Folder.find_by_path(folder)
    elif folder_id is not None:
        f = Folder.get_by_id(folder_id)
    else:
        raise RouteException()
    if not f:
        raise NotExist("Folder not found.")
    if not f.is_root():
        verify_view_access(f)

    items = get_items(f.path, recurse=recursive)
    if include_rights:
        u = get_current_user_object()
        rights = get_rights_holders_all(
            [i.id for i in items if u.has_manage_access(i)], order_by=BlockAccess.type
        )
        items = [ItemWithRights(i, rights[i.id]) for i in items]
    return json_response(items)


@view_page.get("/view")
def index_page():
    save_last_page()
    return render_template("index.jinja2", items=get_items(""), item=Folder.get_root())


@view_page.get("/generateCache/<path:doc_path>")
def gen_cache(
    doc_path: str,
    same_for_all: bool = False,
    force: bool = False,
    print_diffs: bool = False,
    group: str | None = None,
):
    """Pre-generates document cache for the users with non-expired rights.

    Useful for exam documents to reduce server load at the beginning of the exam.

    :param group: The usergroup for which to generate the cache. If omitted, the users are computed from the
      currently active (or upcoming) rights.
    :param print_diffs: Whether to output diff information about cache content. Each cache entry is compared with
     the first cache entry.
    :param doc_path: Path of the document for which to generate the cache.
    :param same_for_all: Whether to use same cache for all users.
     This speeds up cache generation significantly.
    :param force: Whether to force cache generation even if the existing cache seems up-to-date.
    """

    doc_info = DocEntry.find_by_path(doc_path, fallback_to_id=True)
    if not doc_info:
        raise NotExist("Document not found")
    verify_manage_access(doc_info)
    s = doc_info.document.get_settings()
    if not s.is_cached():
        raise RouteException("Document does not have caching enabled.")
    if group:
        ug = UserGroup.get_by_name(group)
        if not ug:
            raise RouteException("usergroup not found")
        groups_that_need_access_check = {ug}
        user_set = set(ug.users)
    else:
        # Compute users from the current rights.
        accesses: ValuesView[BlockAccess] = doc_info.block.accesses.values()
        group_ids = {a.usergroup_id for a in accesses if not a.expired}
        users: list[tuple[User, UserGroup]] = (
            User.query.join(UserGroup, User.groups)
            .filter(UserGroup.id.in_(group_ids))
            .with_entities(User, UserGroup)
            .all()
        )
        groups_that_need_access_check = {
            g for u, g in users if u.get_personal_group() != g
        }
        user_set = {u for u, _ in users}
    for g in groups_that_need_access_check:
        verify_group_view_access(g)
    view_ctx = default_view_ctx
    m = DocViewParams()
    vp = ViewParams()
    users_uniq = list(sorted(user_set, key=lambda u: u.name))
    total = len(users_uniq)
    digits = len(str(total))

    # Make sure tags attribute is always loaded.
    # Otherwise the "translations" variable (in doc_head.jinja2) will first not have "tags" and
    # after encountering someone with manage access, it is loaded and all subsequent users will get it too.
    # This wouldn't cause any user-facing bugs, but it makes it easier to compare cached HTMLs.
    _ = doc_info.block.tags

    def generate() -> Generator[tuple[str, DocRenderResult | None], None, None]:
        first_cache = None
        for i, u in enumerate(users_uniq):
            start = f"{i + 1:>{digits}}/{total} {u.name}: "
            cr = check_doc_cache(doc_info, u, view_ctx, m, vp.nocache)
            view_ctx_cached = dataclasses.replace(view_ctx, for_cache=True)
            if cr.doc and not force:
                yield f"{start}already cached\n", cr.doc
            else:
                if first_cache is None or not same_for_all:
                    dr = render_doc_view(doc_info, m, view_ctx_cached, u, False)
                    first_cache = dr
                else:
                    dr = first_cache
                if dr.allowed_to_cache:
                    set_doc_cache(cr.key, dr)
                    yield f"{start}ok\n", dr
                else:
                    yield f"{start}not allowed to cache (one or more plugins had errors)\n", None

    def generate_with_lock():
        try:
            results = []
            with filelock.FileLock(f"/tmp/generateCache_{doc_info.id}", timeout=0):
                for r, cache_result in generate():
                    if cache_result:
                        results.append(cache_result)
                    yield r

            if not print_diffs:
                return
            if not results:
                return
            yield "\n"

            yield "---Start of diffs---\n"
            # For checking cache correctness, print cache differences compared to the first cached result.
            compare_head = results[0].head_html.splitlines(keepends=True)
            compare_content = results[0].content_html.splitlines(keepends=True)
            for r in results[1:]:
                diff = context_diff(
                    compare_head, r.head_html.splitlines(keepends=True), n=0
                )
                yield "".join(diff)
                diff = context_diff(
                    compare_content, r.content_html.splitlines(keepends=True), n=0
                )
                yield "".join(diff)
                yield "-----------------\n"
            yield "---End of diffs---\n"
        except filelock.Timeout:
            yield "Cache generation for this document is already in progress.\n"

    return Response(stream_with_context(generate_with_lock()), mimetype="text/plain")


debug_time = time.time()


def show_time(s):
    global debug_time
    now = time.time()
    print(s, now - debug_time)
    debug_time = now


def get_module_ids(js_paths: list[str]):
    for jsfile in js_paths:
        yield jsfile.lstrip("/").rstrip(".js")


def goto_view(item_path, model: ViewParams) -> FlaskViewResult:
    return make_response(
        render_template(
            "goto_view.jinja2",
            item_path=item_path,
            display_text=model.goto,
            wait_max=model.wait_max,
            direct_link_timer=model.direct_link_timer,
        )
    )


def view(item_path: str, route: ViewRoute, render_doc: bool = True) -> FlaskViewResult:
    taketime("view begin", zero=True)
    m: DocViewParams = ViewModelSchema.load(request.args, unknown=EXCLUDE)
    vp: ViewParams = ViewParamsSchema.load(request.args, unknown=EXCLUDE)

    if vp.goto:
        return goto_view(item_path, vp)

    if has_special_chars(item_path):
        qs = request.query_string.decode("utf8")
        return redirect(
            remove_path_special_chars(request.path) + (f"?{qs}" if qs else "")
        )

    save_last_page()

    doc_info = DocEntry.find_by_path(
        item_path,
        fallback_to_id=True,
        docentry_load_opts=(
            defaultload(DocEntry._block)
            .defaultload(Block.accesses)
            .joinedload(BlockAccess.usergroup),
            joinedload(DocEntry.trs)
            # TODO: These joinedloads are for some reason very inefficient at least for certain documents.
            #  See https://github.com/TIM-JYU/TIM/issues/2201. Needs more investigation.
            # .joinedload(Translation.docentry),
            # joinedload(DocEntry.trs).joinedload(Translation._block)
        ),
    )
    if doc_info is None:
        return try_return_folder(item_path)

    if m.hide_names is not None:
        session["hide_names"] = m.hide_names

    access = verify_route_access(doc_info, route, require=False)
    if not access:
        if route != ViewRoute.View:
            try:
                verify_view_access(doc_info, check_duration=True)
            except ItemLockedException:
                pass  # Prevent opening the unlock page; instead force redirect to /view first
            return redirect(f"/view/{item_path}")
        if not logged_in():
            return render_login(doc_info.document)
        adm = doc_info.document.get_settings().access_denied_message()
        raise AccessDenied(*((adm,) if adm else ()))

    if vp.login and not logged_in():
        return render_login(doc_info.document)

    should_hide_names = False
    if route == ViewRoute.Review and not verify_teacher_access(doc_info, require=False):
        should_hide_names = True

    current_user = get_current_user_object()
    ug = current_user.get_personal_group()

    if current_user.is_deleted:
        raise DeletedUserException()

    view_ctx = view_ctx_with_urlmacros(
        route, hide_names_requested=should_hide_names or is_hide_names()
    )

    if render_doc:
        cr = check_doc_cache(doc_info, current_user, view_ctx, m, vp.nocache)

        if not cr.doc:
            result = render_doc_view(doc_info, m, view_ctx, current_user, vp.nocache)
            if result.allowed_to_cache:
                set_doc_cache(cr.key, result)
        else:
            # Document reading was skipped during caching, mark it now
            if not cr.doc.hide_readmarks and should_auto_read(
                doc_info.document, [ug.id], current_user
            ):
                mark_all_read(ug.id, doc_info.document)
                db.session.commit()
            refresh_doc_expire(cr.key)
            result = cr.doc
    else:
        result = None

    # This is only used for optimizing database access so that we can close the db session
    # as early as possible.
    preload_personal_folder_and_breadcrumbs(current_user, doc_info)
    # TODO: Closing session here breaks is_attribute_loaded function.
    #  According to https://docs.sqlalchemy.org/en/13/errors.html#error-bhk3, it may not be good practice to close
    #  the session manually in the first place.
    # db.session.close()

    final_html = render_template(
        viewmode_templates.get(route, DEFAULT_VIEWMODE_TEMPLATE) + ".jinja2",
        access=access,
        doc_content=result.content_html if result else "",
        doc_head=result.head_html if result else "",
        item=doc_info,
        route=view_ctx.route.value,
        override_theme=result.override_theme if result else None,
    )
    r = make_response(final_html)
    add_no_cache_headers(r)
    # db.session.commit()
    return r


def preload_personal_folder_and_breadcrumbs(current_user: User, doc_info: DocInfo):
    if current_user.logged_in:
        current_user.get_personal_folder()
    _ = doc_info.parents_to_root_eager


def get_additional_angular_modules(doc_info: DocInfo) -> set[str]:
    result = set()
    doc_settings = doc_info.document.get_settings()
    if doc_info.path.startswith(MESSAGE_LIST_DOC_PREFIX):
        result.add("timMessageListManagement")
    if doc_info.path.startswith(MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX):
        result.add("timArchive")
    if doc_settings.is_style_document():
        result.add("stylePreview")
    result |= set(doc_settings.additional_angular_modules())
    return result


def render_doc_view(
    doc_info: DocInfo,
    m: DocViewParams,
    view_ctx: ViewContext,
    current_user: User,
    clear_cache: bool,
) -> DocRenderResult:
    # Check for incorrect group tags.
    linked_groups = []
    if current_user.has_manage_access(doc_info):
        linked_groups, group_tags = get_linked_groups(doc_info)
        if group_tags:
            names = {ug.ug.name for ug in linked_groups}
            missing = set(group_tags) - names
            if missing:
                flash(f"Document has incorrect group tags: {seq_to_str(list(missing))}")

    piece_size = get_piece_size_from_cookie(request)
    area = None
    areas = None
    r_view_range = None
    if piece_size:
        areas = get_document_areas(doc_info)
    if m.area:
        area = get_area_range(doc_info, m.area)
        if area is not None:
            # RequestedViewRange e returns paragraph e-1 as last paragraph, add +1 to render full area
            r_view_range = RequestedViewRange(b=area[0], e=area[1] + 1, size=None)
        else:
            flash(f"Area {m.area} not found")
    if r_view_range is None:
        r_view_range = RequestedViewRange(b=m.b, e=m.e, size=m.size)
    load_preamble = m.preamble
    view_range = None
    if piece_size and r_view_range.is_full:
        view_range = decide_view_range(doc_info, piece_size, areas=areas)
        load_preamble = True  # If partitioning without URL-param, true is default.
    index_cache_folder = cache_folder_path / "indexcache" / str(doc_info.id)
    contents_have_changed = False
    doc_hash = get_doc_version_hash(doc_info)
    # db.session.close()
    index = load_index(index_cache_folder / f"{doc_hash}.json")
    if index is not None:
        # If cached header is up to date, partition document here.
        xs, view_range = get_document(doc_info, view_range or r_view_range)
    else:
        # Otherwise the partitioning is done after forming the index.
        contents_have_changed = True
        xs, _ = get_document(doc_info, RequestedViewRange(b=None, e=None, size=None))
        _, view_range = get_document(doc_info, view_range or r_view_range)

    doc = doc_info.document

    hide_answers = m.noanswers

    doc_settings = doc.get_settings()

    # Used later to get partitioning with preambles included correct.
    # Includes either only special class preambles, or all of them if b=0.
    preamble_count = 0
    preamble_pars = None
    if (
        load_preamble
        or view_range.starts_from_beginning
        and not doc.preamble_included  # e.g. document is being cached in which case the preamble is preloaded
    ):
        try:
            preamble_pars = doc.insert_preamble_pars(
                [INCLUDE_IN_PARTS_CLASS_NAME]
                if not view_range.starts_from_beginning
                else None
            )
        except PreambleException as e:
            flash(e)
        else:
            xs = preamble_pars + xs
            preamble_count = len(preamble_pars)

    # Preload htmls here to make dereferencing faster
    DocParagraph.preload_htmls(xs, doc_settings, view_ctx, clear_cache)
    src_doc = doc.get_source_document()
    if src_doc is not None:
        DocParagraph.preload_htmls(
            src_doc.get_paragraphs(), src_doc.get_settings(), view_ctx, clear_cache
        )

    rights = get_user_rights_for_item(doc_info, current_user)
    word_list = (
        doc_info.document.get_word_list()
        if rights["editable"] and current_user.get_prefs().use_document_word_list
        else []
    )
    # We need to deference paragraphs at this point already to get the correct task ids
    xs = dereference_pars(xs, context_doc=doc, view_ctx=view_ctx)
    total_points = None
    tasks_done = None
    task_groups = None
    show_task_info = False
    breaklines = False
    user_list = []
    teacher_or_see_answers = view_ctx.route.teacher_or_see_answers
    task_ids, plugin_count, no_accesses = find_task_ids(
        xs,
        view_ctx,
        UserContext.from_one_user(current_user),
        check_access=teacher_or_see_answers,
    )
    if teacher_or_see_answers and no_accesses:
        flash(
            "You do not have full access to the following tasks: "
            + ", ".join([t.doc_task for t in no_accesses])
        )
    points_sum_rule = doc_settings.point_sum_rule()
    if points_sum_rule and not points_sum_rule.count_all:
        total_tasks = len(points_sum_rule.groups)
    else:
        total_tasks = len(task_ids)
    if points_sum_rule and points_sum_rule.scoreboard_error:
        flash(f"Error in point_sum_rule scoreboard: {points_sum_rule.scoreboard_error}")
    usergroups = m.group if m.group is not None else None
    if m.groups is not None:
        usergroups = usergroups + m.groups if usergroups is not None else m.groups
    if usergroups == [""]:
        usergroups = []
    peer_review_start = doc_settings.peer_review_start()
    peer_review_stop = doc_settings.peer_review_stop()
    show_valid_only = (
        m.valid_answers_only
        if m.valid_answers_only is not None
        else doc_settings.show_valid_answers_only()
    )
    if teacher_or_see_answers:
        user_list = None
        ugs = None
        if usergroups is None:
            try:
                usergroups = doc_settings.groups()
            except ValueError as e:
                flash(str(e))
        ugs_without_access = []
        if usergroups is not None:
            ugs = UserGroup.query.filter(UserGroup.name.in_(usergroups)).all()
            if len(ugs) != len(usergroups):
                not_found_ugs = set(usergroups) - set(ug.name for ug in ugs)
                flash(f"Following groups were not found: {not_found_ugs}")
            else:
                for ug in ugs:
                    if not verify_group_view_access(
                        ug, require=False, user=current_user
                    ):
                        flash(f"You don't have access to group '{ug.name}'.")
                        ugs_without_access.append(ug)
            # We allow empty `groups` option to hide all answers by default.
            # In that case, users can use the `groups` URL parameter to show answers.
            if ugs is not None:
                user_list = [u.id for ug in ugs for u in ug.users]
        user_list = get_points_by_rule(
            points_sum_rule, task_ids, user_list, show_valid_only=show_valid_only
        )
        if ugs is not None:
            user_list = add_missing_users_from_groups(
                user_list, list(set(ugs) - set(ugs_without_access))
            )
    elif doc_settings.show_task_summary() and current_user.logged_in:
        info = get_points_by_rule(
            points_sum_rule,
            task_ids,
            [current_user.id],
            force_user=current_user,
            show_valid_only=show_valid_only,
        )
        if info:
            total_points = info[0]["total_points"]
            tasks_done = info[0]["task_count"]
            task_groups = info[0].get("groups")
            breaklines = False
            show_task_info = tasks_done > 0 or total_points != 0
            if points_sum_rule:
                breaklines = points_sum_rule.breaklines
                show_task_info = show_task_info or points_sum_rule.force

    no_question_auto_numbering = None

    if view_ctx.route == ViewRoute.Lecture and current_user.has_edit_access(doc_info):
        no_question_auto_numbering = doc_settings.auto_number_questions()

    current_list_user: User | None = None
    # teacher view sorts user by real name and selects the lowest - ensure first loaded answer matches the user
    if user_list:
        current_list_user = min(
            user_list, key=lambda u: (u["user"].real_name or "").lower()
        )["user"]

    raw_css = doc_settings.css()
    if raw_css:
        try:
            compiled_sass = sass.compile(string=raw_css, output_style="compact")
        except sass.CompileError as e:
            doc_css = None
            flash(
                Markup(
                    f"Document stylesheet has errors: <pre>{html.escape(str(e))}</pre>"
                )
            )
        else:
            # Document styles are visible to anyone but they cannot be moderated easily, we allow only minimal css
            doc_css = sanitize_css(compiled_sass)
    else:
        doc_css = None

    # Custom backgrounds for slides
    slide_background_url = None
    slide_background_color = None

    is_slide = view_ctx.route == ViewRoute.ShowSlide
    if is_slide:
        slide_background_url = doc_settings.get_slide_background_url()
        slide_background_color = doc_settings.get_slide_background_color()
        do_lazy = False
    else:
        do_lazy = (
            m.lazy
            if m.lazy is not None
            else doc_settings.lazy(
                default=plugin_count >= current_app.config["PLUGIN_COUNT_LAZY_LIMIT"]
            )
        )

    user_ctx = UserContext(
        user=current_list_user or current_user,
        logged_user=current_user,
    )
    post_process_result = post_process_pars(
        doc,
        xs,
        user_ctx,
        view_ctx,
        sanitize=False,
        do_lazy=do_lazy,
        load_plugin_states=not hide_answers,
    )

    if view_ctx.route.is_review and is_peerreview_enabled(doc_info):
        user_list = []
        if m.area:
            if area is None:
                raise RouteException(f"Area {m.area} not found")
        elif m.b and m.size == 1:
            if not areas:
                areas = get_document_areas(doc_info)
                for a in areas:
                    if a[0] <= view_range.b and a[1] >= view_range.e:
                        # For now just raise error if someone tries to open a single task inside an area, otherwise
                        # it might cause errors when we generate pairings across the entire area
                        # TODO: Redirect to area view or handle area task list generation here
                        raise RouteException("Requested block is inside an area")
        else:
            raise RouteException(
                "A single block or an area are required for review view"
            )
        tids = []
        # post_process_result.plugins may contain plugins not in the requested range, so find the correct
        # plugins that match the requested b or are inside the requested area
        if m.b and m.size == 1:
            for task in post_process_result.plugins:
                if task.par.id == m.b and task.task_id:
                    tids.append(task.task_id)
                    break
        else:
            area_started = False
            pars_in_area = []
            for t in post_process_result.texts:
                if not area_started and t.attrs.get("area") == m.area:
                    area_started = True
                    continue
                if area_started:
                    if t.attrs.get("area_end") == m.area:
                        break
                    else:
                        pars_in_area.append(t.id)
            for task in post_process_result.plugins:
                if task.par.id in pars_in_area and task.task_id:
                    tids.append(task.task_id)
        if len(tids) < 1:
            raise RouteException("No tasks to review in requested area or block")
        if not check_review_grouping(doc_info, tids):
            try:
                generate_review_groups(doc_info, tids)
                set_default_velp_group_selected_and_visible(doc_info)
            except PeerReviewException as e:
                flash(str(e))
        reviews = get_reviews_where_user_is_reviewer(doc_info, current_user)
        if len(reviews) == 0:
            # TODO: Check for late answers / missing answers / missing group, return proper messages
            flash(
                "No reviewable targets found, review was possibly started before your answer or you were not a member of the peer review group"
            )
        for review in reviews:
            user_list.append(review.reviewable_id)
        user_list = get_points_by_rule(
            points_sum_rule,
            task_ids,
            user_list,
            show_valid_only=show_valid_only,
        )

    if index is None:
        index = get_index_from_html_list(t.output for t in post_process_result.texts)
        doc_hash = get_doc_version_hash(doc_info)
        save_index(index, index_cache_folder / f"{doc_hash}.json")

    # If index was in cache, partitioning will be done earlier.
    if view_range.is_restricted and contents_have_changed:
        post_process_result.texts = partition_texts(
            post_process_result.texts, view_range, preamble_count
        )

    if force_hide_names(current_user, doc_info) or view_ctx.hide_names_requested:
        model_u = User.get_model_answer_user()
        model_u_id = model_u.id if model_u else None
        for entry in user_list:
            eid = entry["user"].id
            if eid != current_user.id and eid != model_u_id:
                entry["user"].hide_name = True

    show_unpublished_bg = doc_info.block.is_unpublished() and not app.config["TESTING"]
    taketime("view to render")

    score_infos = get_score_infos_if_enabled(doc_info, doc_settings, user_ctx)

    reqs = get_all_reqs()  # This is cached so only first time after restart takes time
    taketime("reqs done")
    doctemps = doc_settings.get("editor_templates")
    if doctemps:
        reqs["usertemps"] = doctemps
    if is_slide:
        post_process_result.js_paths.append("tim/document/slide")
    angular_module_names = []
    if teacher_or_see_answers or view_ctx.route.is_review:
        post_process_result.js_paths.append("angular-ui-grid")
        angular_module_names += get_grid_modules()
    post_process_result.js_paths += get_additional_angular_modules(doc_info)

    taketime("before render")
    nav_ranges = []
    if view_range.is_restricted:
        piece_size = get_piece_size_from_cookie(request) or 20
        first_range = decide_view_range(
            doc_info, preferred_set_size=piece_size, index=0, forwards=True, areas=areas
        )
        previous_range = decide_view_range(
            doc_info,
            preferred_set_size=piece_size,
            index=view_range.start_index,
            forwards=False,
            areas=areas,
        )

        next_range = decide_view_range(
            doc_info,
            preferred_set_size=piece_size,
            index=view_range.end_index,
            forwards=True,
            areas=areas,
        )
        last_range = decide_view_range(
            doc_info,
            preferred_set_size=piece_size,
            index=len(doc_info.document.get_paragraphs()),
            forwards=False,
            areas=areas,
        )
        # TODO: Find out if it's better to raise an error when any of these is None.
        if first_range and previous_range and next_range and last_range:
            nav_ranges = [
                first_range.to_json("First"),
                previous_range.to_json("Previous"),
                next_range.to_json("Next"),
                last_range.to_json("Last"),
            ]

    if post_process_result.should_mark_all_read and not view_ctx.for_cache:
        # TODO: Support multiple logged in users without using globals.
        #  On the other hand, should_mark_all_read is used only in exam mode,
        #  so we know there's only one user.
        for group_id in [current_user.get_personal_group().id]:
            mark_all_read(group_id, doc)
        db.session.commit()

    exam_mode = is_exam_mode(doc_settings, rights)

    document_themes = doc_settings.themes()
    if exam_mode:
        document_themes = list(
            dict.fromkeys(doc_settings.exam_mode_themes() + document_themes)
        )
    override_theme = None
    document_themes_final = []
    for theme in document_themes:
        parts = theme.split(":", 1)
        if len(parts) == 2:
            view_route, theme = parts
            if not theme:
                continue
            if view_route and view_ctx.route.value != view_route:
                continue
        document_themes_final.append(theme)

    if document_themes_final:
        document_theme_docs = resolve_themes(document_themes_final)
        # If the user themes are not overridden, they are merged with document themes
        user_themes = current_user.get_prefs().theme_docs()
        if user_themes and not doc_settings.override_user_themes():
            document_theme_docs = list(
                (
                    {d.id: d for d in document_theme_docs}
                    | {d.id: d for d in user_themes}
                ).values()
            )
        theme_style, theme_hash = generate_style(document_theme_docs)
        override_theme = f"{theme_style}?{theme_hash}"

    hide_readmarks = should_hide_readmarks(current_user, doc_settings)

    templates_to_render = (
        ["slide_head.jinja2", "slide_content.jinja2"]
        if is_slide
        else ["doc_head.jinja2", "doc_content.jinja2"]
    )
    tmpl_params = dict(
        hide_links=should_hide_links(doc_settings, rights),
        hide_top_buttons=should_hide_top_buttons(doc_settings, rights),
        pars_only=m.pars_only or should_hide_paragraphs(doc_settings, rights),
        hide_sidemenu=should_hide_sidemenu(doc_settings, rights),
        show_unpublished_bg=show_unpublished_bg,
        exam_mode=exam_mode,
        rights=rights,
        route=view_ctx.route.value,
        edit_mode=(m.edit if current_user.has_edit_access(doc_info) else None),
        item=doc_info,
        text=post_process_result.texts,
        headers=index,
        plugin_users=user_list,
        version=doc.get_version(),
        js=post_process_result.js_paths,
        cssFiles=post_process_result.css_paths,
        jsMods=angular_module_names,
        doc_css=doc_css,
        start_index=view_range.start_index,
        groups=usergroups,
        translations=[
            tr.to_json(curr_user=current_user) for tr in doc_info.translations
        ],
        reqs=reqs,
        no_browser=hide_answers,
        no_question_auto_numbering=no_question_auto_numbering,
        live_updates=doc_settings.live_updates(),
        slide_background_url=slide_background_url,
        slide_background_color=slide_background_color,
        score_infos=score_infos,
        # TODO: Unify "task summary" and "scoreboard" features somehow.
        task_info={
            "total_points": total_points,
            "tasks_done": tasks_done,
            "total_tasks": total_tasks,
            "show": show_task_info,
            "groups": task_groups,
            "breaklines": breaklines,
        },
        peer_review_start=peer_review_start,
        peer_review_stop=peer_review_stop,
        doc_settings=doc_settings,
        word_list=word_list,
        memo_minutes=doc_settings.memo_minutes(),
        linked_groups=linked_groups,
        current_view_range=view_range,
        nav_ranges=nav_ranges,
        should_mark_all_read=post_process_result.should_mark_all_read,
        hide_readmarks=hide_readmarks,
        override_theme=override_theme,
        current_list_user=current_list_user,
        show_valid_answers_only=show_valid_only,
        hide_names_requested=view_ctx.hide_names_requested,
    )
    # db.session.close()
    head, content = (
        render_template("partials/" + tmpl, **tmpl_params)
        for tmpl in templates_to_render
    )
    # db.session.close()
    return DocRenderResult(
        head_html=head,
        content_html=content,
        allowed_to_cache=doc_settings.is_cached()
        and not post_process_result.has_plugin_errors,
        override_theme=override_theme,
        hide_readmarks=hide_readmarks,
    )


def render_login(item: Document | None) -> FlaskViewResult:
    view_settings = get_minimal_visibility_settings(item)
    session["came_from"] = request.url
    session["anchor"] = request.args.get("anchor", "")
    return (
        render_template(
            "loginpage.jinja2",
            came_from=request.full_path,
            anchor=session["anchor"],
            view_settings=view_settings,
        ),
        403,
    )


def get_items(folder: str, recurse=False):
    u = get_current_user_object()
    docs = get_documents(
        search_recursively=recurse, filter_folder=folder, filter_user=u
    )
    docs.sort(key=lambda d: d.title.lower())
    folders = Folder.get_all_in_path(root_path=folder, recurse=recurse)
    folders.sort(key=lambda d: d.title.lower())
    return [f for f in folders if u.has_view_access(f)] + docs


def get_linked_groups(i: Item) -> tuple[list[UserGroupWithSisuInfo], list[str]]:
    group_tags = [
        t.get_group_name() for t in i.block.tags if t.name.startswith(GROUP_TAG_PREFIX)
    ]
    if group_tags:
        return (
            list(
                map(
                    UserGroupWithSisuInfo,
                    get_usergroup_eager_query()
                    .filter(UserGroup.name.in_(group_tags))
                    .all(),
                )
            ),
            group_tags,
        )
    return [], group_tags


@view_page.get("/items/linkedGroups/<int:item_id>")
def get_linked_groups_route(item_id: int):
    d = get_doc_or_abort(item_id)
    verify_teacher_access(d)
    return json_response(get_linked_groups(d)[0])


def should_hide_links(settings: DocSettings, rights: dict):
    return has_no_higher_right(settings.hide_links(), rights)


def should_hide_top_buttons(settings: DocSettings, rights: dict):
    return has_no_higher_right(settings.hide_top_buttons(), rights)


def should_hide_paragraphs(settings: DocSettings, rights: dict):
    return has_no_higher_right(settings.pars_only(), rights)


def should_hide_sidemenu(settings: DocSettings, rights: dict):
    return has_no_higher_right(settings.hide_sidemenu(), rights)


def is_exam_mode(settings: DocSettings, rights: dict):
    return has_no_higher_right(settings.exam_mode(), rights)


@view_page.get("/getParDiff/<int:doc_id>/<int:major>/<int:minor>")
def check_updated_pars(doc_id, major, minor):
    # return json_response({'diff': None,
    #                       'version': None})
    # taketime("before verify")
    doc = get_doc_or_abort(doc_id)
    verify_view_access(doc)
    # taketime("before liveupdates")
    d = doc.document
    settings = d.get_settings()
    live_updates = settings.live_updates(0)  # this cost 1-3 ms.
    global_live_updates = 2  # TODO: take this from somewhere that it is possible to admin to change it by a route

    if 0 < live_updates < global_live_updates:
        live_updates = global_live_updates
    if global_live_updates == 0:  # To stop all live updates
        live_updates = 0
    # taketime("after liveupdates")
    view_ctx = default_view_ctx
    diffs = list(
        d.get_doc_version((major, minor)).parwise_diff(d, view_ctx)
    )  # TODO cache this, about <5 ms
    # taketime("after diffs")
    curr_user = get_current_user_object()
    rights = get_user_rights_for_item(
        doc, curr_user
    )  # about 30-40 ms # TODO: this is the slowest part
    # taketime("after rights")
    for diff in diffs:  # about < 1 ms
        if diff.get("content"):
            post_process_result = post_process_pars(
                d,
                diff["content"],
                UserContext.from_one_user(curr_user),
                view_ctx,
            )
            diff["content"] = {
                "texts": render_template(
                    "partials/paragraphs.jinja2",
                    text=post_process_result.texts,
                    rights=rights,
                    preview=False,
                    hide_readmarks=should_hide_readmarks(curr_user, settings),
                ),
                "js": post_process_result.js_paths,
                "css": post_process_result.css_paths,
            }
    # taketime("after for diffs")
    return json_response(
        {"diff": diffs, "version": d.get_version(), "live": live_updates}
    )


@view_page.get("/manage")
@view_page.get("/slide")
@view_page.get("/teacher")
@view_page.get("/answers")
@view_page.get("/review")
@view_page.get("/lecture")
def index_redirect():
    return redirect("/view")


@view_page.post("/createItem")
def create_item_route(
    item_path: str,
    item_type: str,
    item_title: str,
    cite: int | None = None,
    copy: int | None = None,
    template: str | None = None,
    use_template: bool = True,
):
    if (
        not app.config["ALLOW_CREATE_DOCUMENTS"]
        and not get_current_user_object().is_admin
    ):
        raise AccessDenied("Creating items is disabled.")
    return json_response(
        create_item_direct(
            item_path,
            item_type,
            item_title,
            cite,
            copy,
            template,
            use_template,
        )
    )


@view_page.get("/itemInfo/<item_path>")
def get_doc_basic_info(item_path: str) -> Response:
    item: Item | None = DocEntry.find_by_path(
        item_path, fallback_to_id=True
    ) or Folder.find_by_path(item_path, fallback_to_id=True)
    if item is None:
        raise NotExist("Document does not exist")
    verify_view_access(item)

    res = {
        "id": item.id,
        "type": "folder" if isinstance(item, Folder) else "document",
        "title": item.title,
        "location": item.location,
        "short_name": item.short_name,
    }
    if isinstance(item, DocEntry):
        res |= {
            "lang_id": item.lang_id,
        }
    return json_response(res)


def create_item_direct(
    item_path: str,
    item_type: str,
    item_title: str,
    cite: int | None = None,
    copy: int | None = None,
    template: str | None = None,
    use_template: bool = True,
):
    cite_id, copy_id, template_name = cite, copy, template

    if use_template is None:
        use_template = True

    if cite_id:
        item = create_citation_doc(cite_id, item_path, item_title)
    else:
        item = create_or_copy_item(
            item_path,
            BlockType.Document if item_type == "document" else BlockType.Folder,
            item_title,
            copy_id,
            template_name,
            use_template,
        )
    db.session.commit()
    return item


@view_page.get("/items/<int:item_id>")
def get_item(item_id: int):
    i = Item.find_by_id(item_id)
    if not i:
        raise NotExist("Item not found")
    verify_view_access(i)
    return json_response(i)


@view_page.post("/items/relevance/set/<int:item_id>")
def set_blockrelevance(item_id: int, value: int):
    """
    Add block relevance or edit if it already exists for the block.

    :param value: The relevance value.
    :param item_id: Item id.
    :return: Ok response.
    """

    i = Item.find_by_id(item_id)
    if not i:
        raise NotExist("Item not found")
    verify_manage_access(i)

    relevance_value = value
    # If block has existing relevance, delete it before adding the new one.
    blockrelevance = i.relevance
    if blockrelevance:
        try:
            db.session.delete(blockrelevance)
        except Exception as e:
            db.session.rollback()
            raise RouteException(
                f"Changing block relevance failed: {get_error_message(e)}"
            )
    blockrelevance = BlockRelevance(relevance=relevance_value)

    try:
        i.block.relevance = blockrelevance
        db.session.commit()
    except Exception as e:
        db.session.rollback()
        raise RouteException(
            f"Setting block relevance failed: {get_error_message(e)}: {str(e)}"
        )
    return ok_response()


@view_page.get("/items/relevance/reset/<int:item_id>")
def reset_blockrelevance(item_id: int):
    """
    Reset (delete) block relevance.

    :param item_id: Item id.
    :return: Ok response.
    """

    i = Item.find_by_id(item_id)
    if not i:
        raise NotExist("Item not found")
    verify_manage_access(i)
    blockrelevance = i.relevance
    if blockrelevance:
        try:
            db.session.delete(blockrelevance)
            db.session.commit()
        except Exception as e:
            db.session.rollback()
            raise RouteException(
                f"Resetting block relevance failed: {get_error_message(e)}"
            )
    return ok_response()


@view_page.get("/items/relevance/get/<int:item_id>")
def get_relevance_route(item_id: int):
    """
    Returns item relevance or first non-null parent relevance. If no relevance was found until root,
    return default relevance.

    :param item_id: Item id.
    :return: Relevance object and whether it was inherited or not set (default).
    """
    i = Item.find_by_id(item_id)
    if not i:
        raise NotExist("Item not found")
    verify_view_access(i)

    default = False
    inherited = False

    # If block has set relevance, return it.
    if i.relevance:
        return json_response(
            {"relevance": i.relevance, "default": default, "inherited": inherited}
        )

    # Check parents for relevance in case target block didn't have one.
    parents = i.parents_to_root(include_root=False)
    for parent in parents:
        if parent.relevance:
            inherited = True
            # Return relevance with parent's id.
            return json_response(
                {
                    "relevance": parent.relevance,
                    "default": default,
                    "inherited": inherited,
                }
            )

    # If parents don't have relevance either, return default relevance.
    default = True
    return json_response(
        {
            "relevance": {"block_id": item_id, "relevance": DEFAULT_RELEVANCE},
            "default": default,
            "inherited": inherited,
        }
    )


def get_document_relevance(i: DocInfo) -> int:
    """
    Returns document relevance value or first non-null parent relevance value.
    If no relevance was found until root, return default relevance value.

    :param i: Document.
    :return: Relevance value.
    """

    # If block has set relevance, return it.
    if i.relevance:
        return i.relevance.relevance

    # Check parents for relevance in case target document didn't have one.
    parents = i.parents_to_root(include_root=False)
    for parent in parents:
        if parent.relevance:
            # Return parent relevance.
            return parent.relevance.relevance

    # If parents don't have relevance either, return default value as relevance.
    return DEFAULT_RELEVANCE


@view_page.get("/viewrange/unset/piecesize")
def unset_piece_size():
    resp = make_response()
    resp.set_cookie(
        key="r",
        value="-1",
        expires=0,
        samesite="None",
        secure=app.config["SESSION_COOKIE_SECURE"],
    )
    return resp


@view_page.post("/viewrange/set/piecesize")
def set_piece_size(pieceSize: int):
    """
    Add cookie for user defined view range (if isn't set, doc won't be partitioned).
    """
    piece_size = pieceSize
    if not piece_size or piece_size < 1:
        raise RouteException("Invalid piece size")
    resp = make_response()
    resp.set_cookie(
        key="r",
        value=str(piece_size),
        samesite="None",
        secure=app.config["SESSION_COOKIE_SECURE"],
    )
    return resp


@view_page.get("/viewrange/get/<int:doc_id>/<int:index>/<int:forwards>")
def get_viewrange(doc_id: int, index: int, forwards: int):
    taketime("route view begin")
    current_set_size = get_piece_size_from_cookie(request)
    if not current_set_size:
        raise RouteException("Piece size not found!")
    doc_info = get_doc_or_abort(doc_id)
    verify_view_access(doc_info)
    view_range = decide_view_range(
        doc_info, current_set_size, index, forwards=forwards > 0
    )
    return json_response(view_range)


@view_page.get("/viewrange/getWithHeaderId/<int:doc_id>/<string:header_id>")
def get_viewrange_with_header_id(doc_id: int, header_id: str):
    """
    Route for getting suitable view range for index links.

    :param doc_id: Document id.
    :param header_id: Header id (HTML-attribute id, not the paragraph id).
    :return: View range starting from the header paragraph.
    """
    current_set_size = get_piece_size_from_cookie(request)
    if not current_set_size:
        raise NotExist("Partitioning piece size not found")
    doc_info = get_doc_or_abort(doc_id)
    verify_view_access(doc_info)
    index = get_index_with_header_id(doc_info, header_id)
    if index is None:
        raise NotExist(
            f"Header '{header_id}' not found in the document '{doc_info.short_name}'!"
        )
    view_range = decide_view_range(doc_info, current_set_size, index, forwards=True)
    return json_response(view_range)


@view_page.get("/getTemplates/<path:item_path>")
def get_templates(item_path: str) -> Response:
    d = DocEntry.find_by_path(item_path)
    if not d:
        raise NotExist()
    verify_edit_access(d)
    templates = get_templates_for_folder(d.parent)
    return json_response(templates, date_conversion=True)
