"""Routes for document view."""
import time
import traceback
from typing import Tuple, Union, Optional

from flask import Blueprint, render_template

import routes.lecture
from documentmodel.docsettings import DocSettings
from documentmodel.document import get_index_from_html_list, dereference_pars
from htmlSanitize import sanitize_html
from options import get_option
from routes.accesshelper import verify_view_access, verify_teacher_access, verify_seeanswers_access, \
    get_rights, get_viewable_blocks_or_none_if_admin
from routes.logger import log_error
from routes.sessioninfo import get_current_user_object
from timdb.timdbexception import TimDbException
from utils import remove_path_special_chars
from .common import *

Range = Tuple[int, int]

view_page = Blueprint('view_page',
                      __name__,
                      url_prefix='')


def get_whole_document(doc):
    pars = [par for par in doc]
    return pars


def get_partial_document(doc: Document, view_range: Range) -> List[DocParagraph]:
    i = 0
    pars = []
    for par in doc:
        if i >= view_range[1]:
            break
        if i >= view_range[0]:
            pars.append(par)
        i += 1
    return pars


def get_document(doc_id: int, view_range: Optional[Range] = None) -> Tuple[Document, List[DocParagraph]]:
    # Separated into 2 functions for optimization
    # (don't cache partial documents and don't check ranges in the loop for whole ones)
    doc = Document(doc_id).get_latest_version()
    doc.load_pars()
    return doc, (get_whole_document(doc) if view_range is None else get_partial_document(doc, view_range))


@view_page.route("/show_slide/<path:doc_name>")
def show_slide(doc_name):
    html = view(doc_name, 'show_slide.html')
    return html


@view_page.route("/view_content/<path:doc_name>")
def view_document_content(doc_name):
    return view(doc_name, 'view_content.html')


@view_page.route("/view/<path:doc_name>")
@view_page.route("/view_html/<path:doc_name>")
@view_page.route("/doc/<path:doc_name>")
def view_document(doc_name):
    return view(doc_name, 'view_html.html')


@view_page.route("/teacher/<path:doc_name>")
def teacher_view(doc_name):
    usergroup = request.args.get('group')
    return view(doc_name, 'view_html.html', usergroup=usergroup, route="teacher")


@view_page.route("/velp/<path:doc_name>")
def velp_view(doc_name):
    usergroup = request.args.get('group')
    return view(doc_name, 'view_html.html', usergroup=usergroup, route="velp")


@view_page.route("/answers/<path:doc_name>")
def see_answers_view(doc_name):
    usergroup = request.args.get('group')
    return view(doc_name, 'view_html.html', usergroup=usergroup, route="answers")


@view_page.route("/lecture/<path:doc_name>")
def lecture_view(doc_name):
    return view(doc_name, 'view_html.html', route="lecture")


@view_page.route("/slide/<path:doc_name>")
def slide_document(doc_name):
    html = view(doc_name, 'view_html.html', route="slide")
    return html


@view_page.route("/par_info/<int:doc_id>/<par_id>")
def par_info(doc_id, par_id):
    timdb = get_timdb()
    info = {}

    def just_name(fullpath):
        return ('/' + fullpath).rsplit('/', 1)[1]

    doc = DocEntry.find_by_id(doc_id, try_translation=True)
    doc_name = doc.path
    info['doc_name'] = just_name(doc_name) if doc_name is not None else 'Document #{}'.format(doc_id)

    group = timdb.users.get_owner_group(doc_id)
    users = timdb.users.get_users_in_group(group.id, limit=2)
    if len(users) == 1:
        info['doc_author'] = '{} ({})'.format(users[0]['name'], group.name)
    else:
        info['doc_author'] = group.name

    par_name = Document(doc_id).get_closest_paragraph_title(par_id)
    if par_name is not None:
        info['par_name'] = par_name

    return jsonResponse(info)


@view_page.route("/getItems")
def items_route():
    return jsonResponse(get_items(request.args.get('folder', '')))


@view_page.route("/view/")
def index_page():
    save_last_page()
    in_lecture = routes.lecture.user_in_lecture()
    return render_template('index.html',
                           items=get_items(''),
                           in_lecture=in_lecture,
                           item=Folder.get_root())


def parse_range(start_index: Union[int, str, None], end_index: Union[int, str, None]) -> Optional[Range]:
    if start_index is None and end_index is None:
        return None
    return int(start_index), int(end_index)


def try_return_folder(doc_name):
    timdb = get_timdb()
    user = get_current_user_id()
    is_in_lecture, lecture_id, = timdb.lectures.check_if_in_any_lecture(user)
    if is_in_lecture:
        is_in_lecture = routes.lecture.check_if_lecture_is_running(lecture_id)

    settings = get_user_settings()

    item_name = doc_name.rstrip('/')
    block_id = timdb.folders.get_folder_id(item_name)

    if block_id is None:
        while block_id is None:
            item_name, _ = timdb.folders.split_location(item_name)
            block_id = timdb.folders.get_folder_id(item_name)

        found_item = None
        if verify_view_access(block_id, require=False):
            found_item = item_name

        can_create_new = can_write_to_folder(item_name) and timdb.folders.get_folder_id(item_name) is not None

        return render_template('create_new.html',
                               can_create_new=can_create_new,
                               in_lecture=is_in_lecture,
                               settings=settings,
                               newItem=doc_name,
                               foundItem=found_item), 404
    verify_view_access(block_id)
    folder = Folder.get_by_id(block_id)
    return render_template('index.html',
                           item=folder,
                           items=get_items(item_name),
                           in_lecture=is_in_lecture,
                           settings=settings)


debug_time = time.time()


def show_time(s):
    global debug_time
    now = time.time()
    print(s, now - debug_time)
    debug_time = now


def view(item_path, template_name, usergroup=None, route="view"):
    if has_special_chars(item_path):
        return redirect(remove_path_special_chars(request.path) + '?' + request.query_string.decode('utf8'))

    save_last_page()

    doc_info = DocEntry.find_by_path(item_path, fallback_to_id=True, try_translation=True)

    if doc_info is None:
        return try_return_folder(item_path)

    doc_id = doc_info.id
    edit_mode = request.args.get('edit', None) if has_edit_access(doc_id) else None

    if route == 'teacher':
        if not verify_teacher_access(doc_id, require=False, check_duration=True):
            if verify_view_access(doc_id):
                flash("Did someone give you a wrong link? Showing normal view instead of teacher view.")
                return redirect('/view/' + item_path)

    if route == 'answers':
        if not verify_seeanswers_access(doc_id, require=False, check_duration=True):
            if verify_view_access(doc_id):
                flash("Did someone give you a wrong link? Showing normal view instead of see answers view.")
                return redirect('/view/' + item_path)

    access = verify_view_access(doc_id, require=False, check_duration=True)
    if not access:
        if not logged_in():
            return redirect_to_login()
        else:
            abort(403)

    timdb = get_timdb()

    if get_option(request, 'login', False) and not logged_in():
        return redirect_to_login()

    try:
        view_range = parse_range(request.args.get('b'), request.args.get('e'))
    except (ValueError, TypeError):
        view_range = None
    start_index = max(view_range[0], 0) if view_range else 0

    doc, xs = get_document(doc_id, view_range)

    clear_cache = get_option(request, "nocache", False)
    hide_answers = get_option(request, 'noanswers', False)

    teacher_or_see_answers = route in ('teacher', 'answers')
    doc_settings = doc.get_settings()

    # Preload htmls here to make dereferencing faster
    try:
        DocParagraph.preload_htmls(xs, doc_settings, clear_cache)
    except TimDbException as e:
        log_error('Document {} exception:\n{}'.format(doc_id, traceback.format_exc(chain=False)))
        abort(500, str(e))
    if doc_settings:
        src_doc_id = doc_settings.get_source_document()
        if src_doc_id is not None:
            src_doc = Document(src_doc_id)
            DocParagraph.preload_htmls(src_doc.get_paragraphs(), src_doc.get_settings(), clear_cache)

    # We need to deference paragraphs at this point already to get the correct task ids
    xs = dereference_pars(xs, source_doc=doc.get_original_document())
    total_points = None
    tasks_done = None
    task_groups = None
    user_list = []
    current_user = get_current_user_object() if logged_in() else None
    task_ids, plugin_count = pluginControl.find_task_ids(xs)
    points_sum_rule = doc_settings.point_sum_rule(default={})
    try:
        total_tasks = len(points_sum_rule['groups'])
    except:
        total_tasks = len(task_ids)
        points_sum_rule = None
    if teacher_or_see_answers:
        user_list = None
        if usergroup is not None:
            user_list = [user['id'] for user in timdb.users.get_users_for_group(usergroup)]
        user_list = timdb.answers.get_points_by_rule(points_sum_rule, task_ids, user_list, flatten=True)
    elif doc_settings.show_task_summary() and logged_in():
        info = timdb.answers.get_points_by_rule(points_sum_rule, task_ids, [current_user.id], flatten=True)
        if info:
            total_points = info[0]['total_points']
            tasks_done = info[0]['task_count']
            task_groups = info[0].get('groups')

    no_question_auto_numbering = None

    if route == 'lecture' and has_edit_access(doc_id):
        no_question_auto_numbering = doc_settings.auto_number_questions()

    is_in_lecture = False
    if logged_in():
        is_in_lecture, lecture_id, = timdb.lectures.check_if_in_any_lecture(current_user.id)
        if is_in_lecture:
            is_in_lecture = routes.lecture.check_if_lecture_is_running(lecture_id)

    # Close database here because we won't need it for a while
    timdb.close()

    current_list_user = User.query.get(user_list[0]['id']) if user_list else None

    raw_css = doc_settings.css() if doc_settings else None
    doc_css = sanitize_html('<style type="text/css">' + raw_css + '</style>') if raw_css else None

    # Custom backgrounds for slides
    slide_background_url = None
    slide_background_color = None

    if template_name == 'show_slide.html':
        slide_background_url = doc_settings.get_slide_background_url()
        slide_background_color = doc_settings.get_slide_background_color()
        do_lazy = False
    else:
        do_lazy = get_option(request, "lazy", doc_settings.lazy(default=plugin_count >= current_app.config['PLUGIN_COUNT_LAZY_LIMIT']))

    texts, jsPaths, cssPaths, modules = post_process_pars(doc,
                                                          xs,
                                                          current_list_user or current_user,
                                                          sanitize=False,
                                                          do_lazy=do_lazy,
                                                          load_plugin_states=not hide_answers)

    index = get_index_from_html_list(t['html'] for t in texts)

    if hide_names_in_teacher(doc_id):
        for user in user_list:
            if not timdb.users.user_is_owner(user['id'], doc_id)\
               and user['id'] != get_current_user_id():
                user['name'] = '-'
                user['real_name'] = 'Someone {}'.format(user['id'])
                user['email'] = 'someone_{}@example.com'.format(user['id'])

    settings = get_user_settings()

    show_unpublished_bg = is_considered_unpublished(doc_id)

    return render_template(template_name,
                           access=access,
                           hide_links=should_hide_links(doc_settings, doc_info.rights),
                           show_unpublished_bg=show_unpublished_bg,
                           route=route,
                           edit_mode=edit_mode,
                           item=doc_info,
                           text=texts,
                           headers=index,
                           plugin_users=user_list,
                           version=doc.get_version(),
                           js=jsPaths,
                           cssFiles=cssPaths,
                           jsMods=modules,
                           doc_css=doc_css,
                           start_index=start_index,
                           in_lecture=is_in_lecture,
                           group=usergroup,
                           translations=doc_info.translations,
                           reqs=pluginControl.get_all_reqs(),
                           settings=settings,
                           no_browser=hide_answers,
                           no_question_auto_numbering=no_question_auto_numbering,
                           live_updates=doc_settings.live_updates(),
                           slide_background_url=slide_background_url,
                           slide_background_color=slide_background_color,
                           task_info={'total_points': total_points,
                                      'tasks_done': tasks_done,
                                      'total_tasks': total_tasks,
                                      'groups': task_groups})


def redirect_to_login():
    session['came_from'] = request.url
    session['anchor'] = request.args.get('anchor', '')
    return render_template('loginpage.html',
                           came_from=request.url,
                           anchor=session['anchor']), 403


def get_items(folder: str):
    timdb = get_timdb()
    docs = timdb.documents.get_documents(filter_ids=get_viewable_blocks_or_none_if_admin(),
                                         search_recursively=False,
                                         filter_folder=folder)
    docs.sort(key=lambda d: d.short_name.lower())
    folders = timdb.folders.get_folders(root_path=folder,
                                        filter_ids=get_viewable_blocks_or_none_if_admin())
    folders.sort(key=lambda d: d.name.lower())
    items = folders + docs
    return items


def should_hide_links(settings: DocSettings, rights: dict):
    hide_type = settings.hide_links()
    return {'view': not rights['editable'] and not rights['see_answers'],
            'edit': not rights['see_answers'],
            'see_answers': not rights['teacher'],
            'teacher': not rights['manage']}.get(hide_type, False)


@view_page.route('/getParDiff/<int:doc_id>/<int:major>/<int:minor>')
def check_updated_pars(doc_id, major, minor):
    verify_view_access(doc_id)
    d = Document(doc_id)
    diffs = list(d.get_doc_version((major, minor)).parwise_diff(d, check_html=True))  # TODO cache this
    rights = get_rights(d.doc_id)
    for diff in diffs:
        if diff.get('content'):
            pars, js_paths, css_paths, modules = post_process_pars(d,
                                                                   diff['content'],
                                                                   get_current_user_object(),
                                                                   edit_window=False)
            diff['content'] = {'texts': render_template('paragraphs.html',
                                                        text=pars,
                                                        item={'rights': rights},
                                                        preview=False),
                               'js': js_paths,
                               'css': css_paths,
                               'angularModule': modules}
    return jsonResponse({'diff': diffs,
                         'version': d.get_version()})
