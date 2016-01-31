"""Routes for document view."""

from contracts import contract, new_contract

from documentmodel.document import DocParagraph, get_index_from_html_list
from htmlSanitize import sanitize_html

new_contract('range', 'tuple(int, int)')

from flask import Blueprint, render_template, url_for
from .common import *
import pluginControl
from options import *
import time
import tim

view_page = Blueprint('view_page',
                      __name__,
                      url_prefix='')


def get_whole_document(doc):
    pars = [par for par in doc]
    return pars


@contract
def get_partial_document(doc: 'Document', view_range: 'range') -> 'list(DocParagraph)':
    i = 0
    pars = []
    for par in doc:
        if i >= view_range[1]:
            break
        if i >= view_range[0]:
            pars.append(par)
        i += 1
    return pars


@contract
def get_document(doc_id: 'int', view_range: 'range|None' = None) -> 'tuple(Document,list(DocParagraph))':
    # Separated into 2 functions for optimization
    # (don't cache partial documents and don't check ranges in the loop for whole ones)
    doc = Document(doc_id).get_latest_version()
    doc.load_pars()
    return doc, (get_whole_document(doc) if view_range is None else get_partial_document(doc, view_range))


@view_page.route("/show_slide/<path:doc_name>")
def show_slide(doc_name):
    return view(doc_name, 'show_slide.html')


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


@view_page.route("/answers/<path:doc_name>")
def see_answers_view(doc_name):
    usergroup = request.args.get('group')
    return view(doc_name, 'view_html.html', usergroup=usergroup, route="answers")


@view_page.route("/lecture/<path:doc_name>")
def lecture_view(doc_name):
    return view(doc_name, 'view_html.html', route="lecture")


@view_page.route("/slide/<path:doc_name>")
def slide_document(doc_name):
    return view(doc_name, 'view_html.html', route="slide")

@view_page.route("/par_info/<int:doc_id>/<par_id>")
def par_info(doc_id, par_id):
    timdb = getTimDb()
    info = {}

    def just_name(fullpath):
        return ('/' + fullpath).rsplit('/', 1)[1]

    doc_name = timdb.documents.get_first_document_name(doc_id)
    info['doc_name'] = just_name(doc_name) if doc_name is not None else 'Document #{}'.format(doc_id)

    group = timdb.users.getOwnerGroup(doc_id)
    users = timdb.users.get_users_in_group(group['id'], limit=2)
    if len(users) == 1:
        info['doc_author'] = '{} ({})'.format(users[0]['name'], group['name'])
    else:
        info['doc_author'] = group['name']

    par_name = Document(doc_id).get_closest_paragraph_title(par_id)
    if par_name is not None:
        info['par_name'] = par_name

    return jsonResponse(info)

@contract
def parse_range(start_index: 'int|str|None', end_index: 'int|str|None') -> 'range|None':
    if start_index is None and end_index is None:
        return None
    return( int(start_index), int(end_index) )


def try_return_folder(doc_name):
    timdb = getTimDb()
    folder_name = doc_name.rstrip('/')
    block_id = timdb.folders.get_folder_id(folder_name)

    if block_id is None:
        abort(404)
    doc = timdb.folders.get(block_id)
    user = getCurrentUserId()
    is_in_lecture, lecture_id, = timdb.lectures.check_if_in_any_lecture(user)
    if is_in_lecture:
        is_in_lecture = tim.check_if_lecture_is_running(lecture_id)

    possible_groups = timdb.users.getUserGroupsPrintable(getCurrentUserId())
    settings = tim.get_user_settings()
    return render_template('index.html',
                           doc=doc,
                           userGroups=possible_groups,
                           rights=get_rights(block_id),
                           folder=True,
                           in_lecture=is_in_lecture,
                           settings=settings)


debug_time = time.time()


def show_time(s):
    global debug_time
    nyt = time.time()
    print(s, nyt - debug_time)
    debug_time = nyt


def view(doc_path, template_name, usergroup=None, route="view"):

    session['last_doc'] = request.path
    timdb = getTimDb()
    doc_info = timdb.documents.resolve_doc_id_name(doc_path)

    if doc_info is None:
        return try_return_folder(doc_path)

    doc_id = doc_info['id']
    if route == "teacher":
        verify_teacher_access(doc_id)

    if route == 'see_answers':
        verify_seeanswers_access(doc_id)

    if not has_view_access(doc_id):
        if not logged_in():
            session['came_from'] = request.url
            session['anchor'] = request.args.get('anchor', '')
            return render_template('loginpage.html',
                                   came_from=request.url,
                                   anchor=session['anchor']), 403
        else:
            abort(403)

    try:
        view_range = parse_range(request.args.get('b'), request.args.get('e'))
    except (ValueError, TypeError):
        view_range = None
    start_index = max(view_range[0], 0) if view_range else 0

    doc, xs = get_document(doc_id, view_range)

    user = getCurrentUserId()

    teacher_or_see_answers = route in ('teacher', 'answers')
    if teacher_or_see_answers:
        task_ids = pluginControl.find_task_ids(xs, doc_id)
        user_list = None
        if usergroup is not None:
            user_list = [user['id'] for user in timdb.users.get_users_for_group(usergroup)]
        users = timdb.answers.getUsersForTasks(task_ids, user_list)
        if len(users) > 0:
            user = users[0]['id']
    else:
        users = []
    current_user = timdb.users.getUser(user)

    clear_cache = get_option(request, "nocache", False)
    doc_settings = doc.get_settings()
    raw_css = doc_settings.css() if doc_settings else None
    doc_css = sanitize_html('<style type="text/css">' + raw_css + '</style>') if raw_css else None
    DocParagraph.preload_htmls(xs, doc_settings, clear_cache)

    if doc_settings:
        src_doc_id = doc_settings.get_source_document()
        if src_doc_id is not None:
            src_doc = Document(src_doc_id)
            DocParagraph.preload_htmls(src_doc.get_paragraphs(), src_doc.get_settings(), clear_cache)

    texts, jsPaths, cssPaths, modules = post_process_pars(doc,
                                                          xs,
                                                          current_user if teacher_or_see_answers or logged_in() else None,
                                                          sanitize=False,
                                                          do_lazy=get_option(request, "lazy", True))

    index = get_index_from_html_list(t['html'] for t in texts)

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

    prefs = timdb.users.getPrefs(getCurrentUserId())
    custom_css_files = json.loads(prefs).get('css_files', {}) if prefs is not None else {}
    if custom_css_files:
        custom_css_files = {key: value for key, value in custom_css_files.items() if value}
    custom_css = json.loads(prefs).get('custom_css', '') if prefs is not None else ''

    settings = tim.get_user_settings()

    is_in_lecture, lecture_id, = timdb.lectures.check_if_in_any_lecture(user)
    if is_in_lecture:
        is_in_lecture = tim.check_if_lecture_is_running(lecture_id)

    result = render_template(template_name,
                             route=route,
                             doc=doc_info,
                             text=texts,
                             headers=index,
                             plugin_users=users,
                             current_user=current_user,
                             version=doc.get_version(),
                             js=jsPaths,
                             cssFiles=cssPaths,
                             jsMods=modules,
                             custom_css_files=custom_css_files,
                             custom_css=custom_css,
                             doc_css=doc_css,
                             start_index=start_index,
                             in_lecture=is_in_lecture,
                             group=usergroup,
                             rights=get_rights(doc_id),
                             translations=timdb.documents.get_translations(doc_id),
                             reqs=pluginControl.get_all_reqs(),
                             settings=settings)
    return result
