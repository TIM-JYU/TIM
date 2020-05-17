"""Routes for document view."""
import html
import time
import traceback
from typing import Tuple, Optional, List, Union

import attr
from dataclasses import dataclass

import sass
from flask import Blueprint, render_template, make_response, abort
from flask import current_app
from flask import flash
from flask import g
from flask import redirect
from flask import request
from flask import session
from markupsafe import Markup
from marshmallow import ValidationError

from marshmallow_dataclass import class_schema
from timApp.answer.answers import add_missing_users_from_group, get_points_by_rule
from timApp.auth.accesshelper import verify_view_access, verify_teacher_access, verify_seeanswers_access, \
    get_rights, has_edit_access, get_doc_or_abort, verify_manage_access, AccessDenied, ItemLockedException
from timApp.auth.auth_models import BlockAccess
from timApp.auth.sessioninfo import get_current_user_object, logged_in, current_user_in_lecture, \
    save_last_page
from timApp.document.create_item import create_or_copy_item, create_citation_doc
from timApp.document.docentry import DocEntry, get_documents
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.docsettings import DocSettings
from timApp.document.document import get_index_from_html_list, dereference_pars, Document, viewmode_routes
from timApp.document.post_process import post_process_pars
from timApp.document.hide_names import hide_names_in_teacher
from timApp.document.preloadoption import PreloadOption
from timApp.folder.folder import Folder
from timApp.folder.folder_view import try_return_folder
from timApp.item.block import BlockType
from timApp.item.blockrelevance import BlockRelevance
from timApp.item.item import Item
from timApp.item.partitioning import get_piece_size_from_cookie, decide_view_range, get_doc_version_hash, load_index, \
    INCLUDE_IN_PARTS_CLASS_NAME, save_index, partition_texts, get_index_with_header_id, get_document_areas, \
    RequestedViewRange, IndexedViewRange
from timApp.item.tag import GROUP_TAG_PREFIX
from timApp.item.validation import has_special_chars
from timApp.markdown.htmlSanitize import sanitize_html
from timApp.markdown.markdownconverter import create_environment
from timApp.plugin.plugin import find_task_ids
from timApp.plugin.pluginControl import get_all_reqs
from timApp.tim_app import app
from timApp.timdb.exceptions import TimDbException, PreambleException
from timApp.timdb.sqa import db
from timApp.user.groups import verify_group_view_access
from timApp.user.user import User, check_rights
from timApp.user.usergroup import UserGroup, get_usergroup_eager_query, UserGroupWithSisuInfo
from timApp.user.users import get_rights_holders_all
from timApp.user.userutils import DeletedUserException
from timApp.util.flask.requesthelper import verify_json_params, use_model
from timApp.util.flask.responsehelper import json_response, ok_response, get_grid_modules
from timApp.util.logger import log_error
from timApp.util.timtiming import taketime
from timApp.util.utils import get_error_message, cache_folder_path
from timApp.util.utils import remove_path_special_chars, seq_to_str
from timApp.readmark.readings import mark_all_read
from timApp.auth.sessioninfo import get_session_usergroup_ids
from timApp.user.settings.theme import Theme, theme_exists
from timApp.user.settings.theme_css import generate_theme, get_default_scss_gen_dir

DEFAULT_RELEVANCE = 10

view_page = Blueprint('view_page',
                      __name__,
                      url_prefix='')

DocumentSlice = Tuple[List[DocParagraph], IndexedViewRange]
ViewRange = Union[RequestedViewRange, IndexedViewRange]


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
        actual_end_index = len(all_pars) if view_range.size is None else actual_start_index + view_range.size
    pars = all_pars[actual_start_index:actual_end_index]
    return pars, IndexedViewRange(b=actual_start_index, e=actual_end_index, par_count=len(all_pars))


def get_document(doc_info: DocInfo, view_range: ViewRange) -> DocumentSlice:
    doc = doc_info.document
    doc.preload_option = PreloadOption.all
    return get_partial_document(doc, view_range)


@dataclass
class ViewModel:
    b: Union[int, str, None] = None
    e: Union[int, str, None] = None
    size: Union[int, None] = None
    edit: Optional[str] = None
    group: Optional[str] = None
    hide_names: Optional[bool] = None
    lazy: Optional[bool] = None
    login: bool = False
    noanswers: bool = False
    nocache: bool = False
    pars_only: bool = False
    preamble: bool = False
    goto: Optional[str] = None
    wait_max: int = 0
    direct_link_timer: int = 15


    def __post_init__(self):
        if self.b and self.e:
            if type(self.b) != type(self.e):
                raise ValidationError('b and e must be of same type (int or string).')
        if self.e is not None and self.size is not None:
            raise ValidationError('Cannot provide e and size parameters at the same time.')


ViewModelSchema = class_schema(ViewModel)()


@view_page.route("/show_slide/<path:doc_name>")
def show_slide(doc_name):
    html = view(doc_name, 'show_slide.html')
    return html


@view_page.route("/view/<path:doc_name>")
def view_document(doc_name):
    taketime("route view begin")
    ret = view(doc_name, 'view_html.html')
    taketime("route view end")
    return ret


@view_page.route("/teacher/<path:doc_name>")
def teacher_view(doc_name):
    return view(doc_name, 'view_html.html', route="teacher")


@view_page.route("/velp/<path:doc_name>")
def velp_view(doc_name):
    return view(doc_name, 'view_html.html', route="velp")


@view_page.route("/answers/<path:doc_name>")
def see_answers_view(doc_name):
    return view(doc_name, 'view_html.html', route="answers")


@view_page.route("/lecture/<path:doc_name>")
def lecture_view(doc_name):
    return view(doc_name, 'view_html.html', route="lecture")


@view_page.route("/slide/<path:doc_name>")
def slide_document(doc_name):
    html = view(doc_name, 'view_html.html', route="slide")
    return html


@view_page.route("/par_info/<int:doc_id>/<par_id>")
def par_info(doc_id, par_id):
    doc = get_doc_or_abort(doc_id)
    verify_view_access(doc)
    for o in doc.owners:
        o.load_personal_user()
    par_name = doc.document.get_closest_paragraph_title(par_id)
    return json_response({
        'item': doc,
        'par_name': par_name,
    })


@view_page.route("/doc_view_info/<path:doc_name>")
def doc_access_info(doc_name):
    doc_info = DocEntry.find_by_path(doc_name, fallback_to_id=True)
    if not doc_info:
        return abort(404)

    can_access = False
    try:
        view_access = verify_view_access(doc_info, require=False, check_duration=True)
        can_access = view_access is not None
    except ItemLockedException as ile:
        view_access = ile.access

    return json_response({
        'can_access': can_access,
        'right': view_access
    })


@dataclass
class GetItemsModel:
    folder: Optional[str] = None
    folder_id: Optional[int] = None
    recursive: bool = False
    include_rights: bool = False


@attr.s(auto_attribs=True)
class ItemWithRights:
    i: Item
    rights: List[BlockAccess]

    def to_json(self):
        return {
            **self.i.to_json(),
            'grouprights': self.rights,
        }


@view_page.route("/getItems")
@use_model(GetItemsModel)
def items_route(args: GetItemsModel):
    if args.folder is not None:
        f = Folder.find_by_path(args.folder)
    elif args.folder_id is not None:
        f = Folder.get_by_id(args.folder_id)
    else:
        return abort(400)
    if not f:
        return abort(404, 'Folder not found.')
    if not f.is_root():
        verify_view_access(f)

    items = get_items(f.path, recurse=args.recursive)
    if args.include_rights:
        u = get_current_user_object()
        rights = get_rights_holders_all([i.id for i in items if u.has_manage_access(i)], order_by=BlockAccess.type)
        items = [ItemWithRights(i, rights[i.id]) for i in items]
    return json_response(items)


@view_page.route("/view")
def index_page():
    save_last_page()
    in_lecture = current_user_in_lecture()
    return render_template('index.html',
                           items=get_items(''),
                           in_lecture=in_lecture,
                           item=Folder.get_root())


debug_time = time.time()


def show_time(s):
    global debug_time
    now = time.time()
    print(s, now - debug_time)
    debug_time = now


def get_module_ids(js_paths: List[str]):
    for jsfile in js_paths:
        yield jsfile.lstrip('/').rstrip('.js')


def goto_view(item_path, model: ViewModel):
    return render_template('goto_view.html',
                           item_path=item_path,
                           display_text=model.goto,
                           wait_max=model.wait_max,
                           direct_link_timer=model.direct_link_timer)


def view(item_path, template_name, route="view"):
    taketime("view begin", zero=True)
    m: ViewModel = ViewModelSchema.load(request.args, unknown='EXCLUDE')

    if m.goto:
        return goto_view(item_path, m)

    usergroup = m.group
    g.viewmode = route in viewmode_routes
    g.route = route

    if has_special_chars(item_path):
        return redirect(remove_path_special_chars(request.path) + '?' + request.query_string.decode('utf8'))

    save_last_page()

    doc_info = DocEntry.find_by_path(item_path, fallback_to_id=True)

    if doc_info is None:
        return try_return_folder(item_path)

    doc_info.request = request

    edit_mode = m.edit if has_edit_access(doc_info) else None
    create_environment("%%")  # TODO get macroinf

    if m.hide_names is not None:
        session['hide_names'] = m.hide_names

    should_hide_names = False

    if route == 'teacher':
        if not verify_teacher_access(doc_info, require=False, check_duration=True):
            if verify_view_access(doc_info):
                flash("Did someone give you a wrong link? Showing normal view instead of teacher view.")
                return redirect(f'/view/{item_path}')

    if route == 'answers':
        if not verify_seeanswers_access(doc_info, require=False, check_duration=True):
            if verify_view_access(doc_info):
                flash("Did someone give you a wrong link? Showing normal view instead of see answers view.")
                return redirect(f'/view/{item_path}')
        if not verify_teacher_access(doc_info, require=False, check_duration=True):
            should_hide_names = True

    access = verify_view_access(doc_info, require=False, check_duration=True)
    if not access:
        if not logged_in():
            return redirect_to_login()
        else:
            abort(403)

    # Check for incorrect group tags.
    linked_groups = []
    if verify_manage_access(doc_info, require=False):
        linked_groups, group_tags = get_linked_groups(doc_info)
        if group_tags:
            names = set(ug.ug.name for ug in linked_groups)
            missing = set(group_tags) - names
            if missing:
                flash(f'Document has incorrect group tags: {seq_to_str(list(missing))}')

    if m.login and not logged_in():
        return redirect_to_login()

    piece_size = get_piece_size_from_cookie(request)
    areas = None
    if piece_size:
        areas = get_document_areas(doc_info)
    r_view_range = RequestedViewRange(b=m.b, e=m.e, size=m.size)
    load_preamble = m.preamble
    view_range = None
    if piece_size and r_view_range.is_full:
        view_range = decide_view_range(doc_info, piece_size, areas=areas)
        load_preamble = True  # If partitioning without URL-param, true is default.
    index_cache_folder = cache_folder_path / "indexcache" / str(doc_info.id)
    contents_have_changed = False
    doc_hash = get_doc_version_hash(doc_info)
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
    g.doc = doc

    doc.route = route

    clear_cache = m.nocache
    hide_answers = m.noanswers

    teacher_or_see_answers = route in ('teacher', 'answers')
    current_user = get_current_user_object() if logged_in() else None

    if current_user and current_user.is_deleted:
        raise DeletedUserException()

    doc_settings = doc.get_settings(current_user)

    # Used later to get partitioning with preambles included correct.
    # Includes either only special class preambles, or all of them if b=0.
    preamble_count = 0
    if load_preamble or view_range.starts_from_beginning:
        try:
            preamble_pars = doc.insert_preamble_pars(
                [INCLUDE_IN_PARTS_CLASS_NAME] if not view_range.starts_from_beginning else None)
        except PreambleException as e:
            flash(e)
        else:
            xs = preamble_pars + xs
            preamble_count = len(preamble_pars)

    # Preload htmls here to make dereferencing faster
    try:
        DocParagraph.preload_htmls(xs, doc_settings, clear_cache)
    except TimDbException as e:
        log_error(f'Document {doc_info.id} exception:\n{traceback.format_exc(chain=False)}')
        abort(500, str(e))
    src_doc = doc.get_source_document()
    if src_doc is not None:
        DocParagraph.preload_htmls(src_doc.get_paragraphs(), src_doc.get_settings(), clear_cache)

    rights = doc_info.rights
    word_list = (doc_info.document.get_word_list()
                 if rights['editable'] and current_user and current_user.get_prefs().use_document_word_list
                 else [])
    # We need to deference paragraphs at this point already to get the correct task ids
    xs = dereference_pars(xs, context_doc=doc)
    total_points = None
    tasks_done = None
    task_groups = None
    show_task_info = False
    user_list = []
    task_ids, plugin_count, no_accesses = find_task_ids(xs, check_access=teacher_or_see_answers)
    if teacher_or_see_answers and no_accesses:
        flash('You do not have full access to the following tasks: ' + ', '.join([t.doc_task for t in no_accesses]))
    points_sum_rule = doc_settings.point_sum_rule()
    if points_sum_rule and not points_sum_rule.count_all:
        total_tasks = len(points_sum_rule.groups)
    else:
        total_tasks = len(task_ids)
    if teacher_or_see_answers:
        user_list = None
        ug = None
        if usergroup is None:
            try:
                usergroup = doc_settings.group()
            except ValueError:
                flash("The setting 'group' must be a string.")
        if usergroup is not None:
            ug = UserGroup.get_by_name(usergroup)
            if not ug:
                flash(f'User group {usergroup} not found')
            else:
                if not verify_group_view_access(ug, require=False):
                    flash(f"You don't have access to group '{ug.name}'.")
                    ug = None
                else:
                    user_list = [u.id for u in ug.users]
        user_list = get_points_by_rule(points_sum_rule, task_ids, user_list, flatten=True)
        if ug:
            user_list = add_missing_users_from_group(user_list, ug)
    elif doc_settings.show_task_summary() and logged_in():
        info = get_points_by_rule(points_sum_rule, task_ids, [current_user.id], flatten=True)
        if info:
            total_points = info[0]['total_points']
            tasks_done = info[0]['task_count']
            task_groups = info[0].get('groups')
            show_task_info = tasks_done > 0 or total_points != 0

    no_question_auto_numbering = None

    if route == 'lecture' and has_edit_access(doc_info):
        no_question_auto_numbering = doc_settings.auto_number_questions()

    is_in_lecture = current_user_in_lecture()

    current_list_user: Optional[User] = user_list[0]['user'] if user_list else None

    raw_css = doc_settings.css()
    if raw_css:
        try:
            compiled_sass = sass.compile(string=raw_css, output_style='compact')
        except sass.CompileError as e:
            doc_css = None
            flash(Markup(f'Document stylesheet has errors: <pre>{html.escape(str(e))}</pre>'))
        else:
            doc_css = sanitize_html('<style type="text/css">' + compiled_sass + '</style>')
    else:
        doc_css = None

    # Custom backgrounds for slides
    slide_background_url = None
    slide_background_color = None

    is_slide = template_name == 'show_slide.html'
    if is_slide:
        slide_background_url = doc_settings.get_slide_background_url()
        slide_background_color = doc_settings.get_slide_background_color()
        do_lazy = False
    else:
        do_lazy = m.lazy if m.lazy is not None else doc_settings.lazy(
            default=plugin_count >= current_app.config['PLUGIN_COUNT_LAZY_LIMIT'])

    post_process_result = post_process_pars(
        doc,
        xs,
        current_list_user or current_user,
        sanitize=False,
        do_lazy=do_lazy,
        load_plugin_states=not hide_answers,
    )
    if index is None:
        index = get_index_from_html_list(t['html'] for t in post_process_result.texts)
        doc_hash = get_doc_version_hash(doc_info)
        save_index(index, index_cache_folder / f"{doc_hash}.json")

    # If index was in cache, partitioning will be done earlier.
    if view_range.is_restricted and contents_have_changed:
        post_process_result.texts = partition_texts(post_process_result.texts, view_range, preamble_count)

    if hide_names_in_teacher(doc_info) or should_hide_names:
        for entry in user_list:
            if not current_user or entry['user'].id != current_user.id:
                entry['user'].hide_name = True

    show_unpublished_bg = doc_info.block.is_unpublished() and not app.config['TESTING']
    taketime("view to render")

    reqs = get_all_reqs()  # This is cached so only first time after restart takes time
    taketime("reqs done")
    doctemps = doc_settings.get('editor_templates')
    if doctemps:
        reqs["usertemps"] = doctemps
    if is_slide:
        post_process_result.js_paths.append('tim/document/slide')
    angular_module_names = []
    if teacher_or_see_answers:
        post_process_result.js_paths.append('angular-ui-grid')
        angular_module_names += get_grid_modules()
    taketime("before render")
    nav_ranges = []
    if view_range.is_restricted:
        piece_size = get_piece_size_from_cookie(request) or 20
        first_range = decide_view_range(
            doc_info,
            preferred_set_size=piece_size,
            index=0,
            forwards=True,
            areas=areas
        )
        previous_range = decide_view_range(
            doc_info,
            preferred_set_size=piece_size,
            index=view_range.start_index,
            forwards=False,
            areas=areas
        )

        next_range = decide_view_range(
            doc_info,
            preferred_set_size=piece_size,
            index=view_range.end_index,
            forwards=True,
            areas=areas
        )
        last_range = decide_view_range(
            doc_info,
            preferred_set_size=piece_size,
            index=len(doc_info.document.get_paragraphs()),
            forwards=False,
            areas=areas
        )
        # TODO: Find out if it's better to raise an error when any of these is None.
        if first_range and previous_range and next_range and last_range:
            nav_ranges = [
                first_range.to_json('First'),
                previous_range.to_json('Previous'),
                next_range.to_json('Next'),
                last_range.to_json('Last'),
            ]

    if post_process_result.should_mark_all_read:
        for group_id in get_session_usergroup_ids():
            mark_all_read(group_id, doc)
        db.session.commit()

    document_themes = [Theme(f) for f in doc_settings.themes() if theme_exists(f)]
    override_theme = None
    if document_themes:
        # If the user themes are not overridden, they are merged with document themes
        user_themes = current_user.get_prefs().themes if current_user else []
        if user_themes and not doc_settings.override_user_themes():
            document_themes = list(set().union(document_themes, user_themes))
        override_theme = generate_theme(document_themes, get_default_scss_gen_dir())

    return render_template(
        template_name,
        access=access,
        hide_links=should_hide_links(doc_settings, rights),
        hide_top_buttons=should_hide_top_buttons(doc_settings, rights),
        pars_only=m.pars_only or should_hide_paragraphs(doc_settings, rights),
        show_unpublished_bg=show_unpublished_bg,
        exam_mode=is_exam_mode(doc_settings, rights),
        route=route,
        edit_mode=edit_mode,
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
        in_lecture=is_in_lecture,
        group=usergroup,
        translations=doc_info.translations,
        reqs=reqs,
        no_browser=hide_answers,
        no_question_auto_numbering=no_question_auto_numbering,
        live_updates=doc_settings.live_updates(),
        slide_background_url=slide_background_url,
        slide_background_color=slide_background_color,
        task_info={'total_points': total_points,
                   'tasks_done': tasks_done,
                   'total_tasks': total_tasks,
                   'show': show_task_info,
                   'groups': task_groups},
        doc_settings=doc_settings,
        word_list=word_list,
        memo_minutes=doc_settings.memo_minutes(),
        linked_groups=linked_groups,
        current_view_range=view_range,
        nav_ranges=nav_ranges,
        should_mark_all_read=post_process_result.should_mark_all_read,
        override_theme=override_theme,
    )


def redirect_to_login():
    session['came_from'] = request.url
    session['anchor'] = request.args.get('anchor', '')
    return render_template('loginpage.html',
                           came_from=request.full_path,
                           anchor=session['anchor']), 403


def get_items(folder: str, recurse=False):
    u = get_current_user_object()
    docs = get_documents(search_recursively=recurse,
                         filter_folder=folder,
                         filter_user=u)
    docs.sort(key=lambda d: d.title.lower())
    folders = Folder.get_all_in_path(root_path=folder, recurse=recurse)
    folders.sort(key=lambda d: d.title.lower())
    return [f for f in folders if u.has_view_access(f)] + docs


def get_linked_groups(i: Item) -> Tuple[List[UserGroupWithSisuInfo], List[str]]:
    group_tags = [t.get_group_name() for t in i.block.tags if t.name.startswith(GROUP_TAG_PREFIX)]
    if group_tags:
        return list(map(UserGroupWithSisuInfo, get_usergroup_eager_query().filter(UserGroup.name.in_(group_tags)).all()
                        )), group_tags
    return [], group_tags


@view_page.route('/items/linkedGroups/<int:item_id>')
def get_linked_groups_route(item_id: int):
    d = get_doc_or_abort(item_id)
    verify_teacher_access(d)
    return json_response(get_linked_groups(d)[0])


def should_hide_links(settings: DocSettings, rights: dict):
    return check_rights(settings.hide_links(), rights)


def should_hide_top_buttons(settings: DocSettings, rights: dict):
    return check_rights(settings.hide_top_buttons(), rights)


def should_hide_paragraphs(settings: DocSettings, rights: dict):
    return check_rights(settings.pars_only(), rights)


def is_exam_mode(settings: DocSettings, rights: dict):
    return check_rights(settings.exam_mode(), rights)


@view_page.route('/getParDiff/<int:doc_id>/<int:major>/<int:minor>')
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
    diffs = list(d.get_doc_version((major, minor)).parwise_diff(d, check_html=True))  # TODO cache this, about <5 ms
    # taketime("after diffs")
    rights = get_rights(doc)  # about 30-40 ms # TODO: this is the slowest part
    # taketime("after rights")
    for diff in diffs:  # about < 1 ms
        if diff.get('content'):
            post_process_result = post_process_pars(
                d,
                diff['content'],
                get_current_user_object(),
                edit_window=False,
            )
            diff['content'] = {
                'texts': render_template('partials/paragraphs.html',
                                         text=post_process_result.texts,
                                         item={'rights': rights},
                                         preview=False),
                'js': post_process_result.js_paths,
                'css': post_process_result.css_paths,
            }
    # taketime("after for diffs")
    return json_response({'diff': diffs,
                          'version': d.get_version(),
                          'live': live_updates})


@view_page.route("/manage")
@view_page.route("/slide")
@view_page.route("/teacher")
@view_page.route("/answers")
@view_page.route("/lecture")
def index_redirect():
    return redirect('/view')


@dataclass
class CreateItemModel:
    item_path: str
    item_type: str
    item_title: str
    cite: Optional[int] = None
    copy: Optional[int] = None
    template: Optional[str] = None
    use_template: bool = True


@view_page.route("/createItem", methods=["POST"])
@use_model(CreateItemModel)
def create_item_route(m: CreateItemModel):
    if not app.config['ALLOW_CREATE_DOCUMENTS'] and not get_current_user_object().is_admin:
        raise AccessDenied('Creating items is disabled.')
    return json_response(create_item_direct(m))


def create_item_direct(m: CreateItemModel):
    item_path, item_type, item_title = m.item_path, m.item_type, m.item_title
    cite_id, copy_id, template_name, use_template = m.cite, m.copy, m.template, m.use_template

    if use_template is None:
        use_template = True

    if cite_id:
        item = create_citation_doc(cite_id, item_path, item_title)
    else:
        item = create_or_copy_item(item_path, BlockType.Document if item_type == 'document' else BlockType.Folder,
                                   item_title, copy_id, template_name, use_template)
    db.session.commit()
    return item


@view_page.route("/items/<int:item_id>")
def get_item(item_id: int):
    i = Item.find_by_id(item_id)
    if not i:
        abort(404, 'Item not found')
    verify_view_access(i)
    return json_response(i)


@view_page.route('/items/relevance/set/<int:item_id>', methods=["POST"])
def set_blockrelevance(item_id: int):
    """
    Add block relevance or edit if it already exists for the block.
    :param item_id: Item id.
    :return: Ok response.
    """
    # TODO: Using the route with just an URL string (requires browser plugin).

    i = Item.find_by_id(item_id)
    if not i:
        abort(404, 'Item not found')
    verify_manage_access(i)

    # TODO: Use dataclass.
    relevance_value, = verify_json_params('value')
    # If block has existing relevance, delete it before adding the new one.
    blockrelevance = i.relevance
    if blockrelevance:
        try:
            db.session.delete(blockrelevance)
        except Exception as e:
            db.session.rollback()
            abort(400, f"Changing block relevance failed: {get_error_message(e)}")
    blockrelevance = BlockRelevance(relevance=relevance_value)

    try:
        i.block.relevance = blockrelevance
        db.session.commit()
    except Exception as e:
        db.session.rollback()
        abort(400, f"Setting block relevance failed: {get_error_message(e)}: {str(e)}")
    return ok_response()


@view_page.route('/items/relevance/reset/<int:item_id>')
def reset_blockrelevance(item_id: int):
    """
    Reset (delete) block relevance.
    :param item_id: Item id.
    :return: Ok response.
    """

    i = Item.find_by_id(item_id)
    if not i:
        abort(404, 'Item not found')
    verify_manage_access(i)
    blockrelevance = i.relevance
    if blockrelevance:
        try:
            db.session.delete(blockrelevance)
            db.session.commit()
        except Exception as e:
            db.session.rollback()
            abort(400, f"Resetting block relevance failed: {get_error_message(e)}")
    return ok_response()


@view_page.route('/items/relevance/get/<int:item_id>')
def get_relevance_route(item_id: int):
    """
    Returns item relevance or first non-null parent relevance. If no relevance was found until root,
    return default relevance.
    :param item_id: Item id.
    :return: Relevance object and whether it was inherited or not set (default).
    """
    i = Item.find_by_id(item_id)
    if not i:
        abort(404, 'Item not found')
    verify_view_access(i)

    default = False
    inherited = False

    # If block has set relevance, return it.
    if i.relevance:
        return json_response({
            "relevance": i.relevance,
            "default": default,
            "inherited": inherited})

    # Check parents for relevance in case target block didn't have one.
    parents = i.parents_to_root(include_root=False)
    for parent in parents:
        if parent.relevance:
            inherited = True
            # Return relevance with parent's id.
            return json_response({
                "relevance": parent.relevance,
                "default": default,
                "inherited": inherited})

    # If parents don't have relevance either, return default relevance.
    default = True
    return json_response({
        "relevance": {
            "block_id": item_id,
            "relevance": DEFAULT_RELEVANCE
        },
        "default": default,
        "inherited": inherited})


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


@view_page.route('/viewrange/unset/piecesize')
def unset_piece_size():
    resp = make_response()
    resp.set_cookie(
        key="r",
        value="-1",
        expires=0,
        samesite='None',
        secure=app.config['SESSION_COOKIE_SECURE'],
    )
    return resp


@dataclass
class SetViewRangeModel:
    pieceSize: int


@view_page.route('/viewrange/set/piecesize', methods=["POST"])
@use_model(SetViewRangeModel)
def set_piece_size(args: SetViewRangeModel):
    """
    Add cookie for user defined view range (if isn't set, doc won't be partitioned).
    :return: Response.
    """
    piece_size = args.pieceSize
    if not piece_size or piece_size < 1:
        return abort(400, "Invalid piece size")
    resp = make_response()
    resp.set_cookie(
        key="r",
        value=str(piece_size),
        samesite='None',
        secure=app.config['SESSION_COOKIE_SECURE'],
    )
    return resp


@view_page.route('/viewrange/get/<int:doc_id>/<int:index>/<int:forwards>')
def get_viewrange(doc_id: int, index: int, forwards: int):
    taketime("route view begin")
    current_set_size = get_piece_size_from_cookie(request)
    if not current_set_size:
        return abort(400, "Piece size not found!")
    doc_info = get_doc_or_abort(doc_id)
    verify_view_access(doc_info)
    view_range = decide_view_range(doc_info, current_set_size, index, forwards=forwards > 0)
    return json_response(view_range)


@view_page.route('/viewrange/getWithHeaderId/<int:doc_id>/<string:header_id>')
def get_viewrange_with_header_id(doc_id: int, header_id: str):
    """
    Route for getting suitable view range for index links.
    :param doc_id: Document id.
    :param header_id: Header id (HTML-attribute id, not the paragraph id).
    :return: View range starting from the header paragraph.
    """
    current_set_size = get_piece_size_from_cookie(request)
    if not current_set_size:
        return abort(404, "Partitioning piece size not found")
    doc_info = get_doc_or_abort(doc_id)
    verify_view_access(doc_info)
    index = get_index_with_header_id(doc_info, header_id)
    if index is None:
        return abort(404, f"Header '{header_id}' not found in the document '{doc_info.short_name}'!")
    view_range = decide_view_range(doc_info, current_set_size, index, forwards=True)
    return json_response(view_range)
