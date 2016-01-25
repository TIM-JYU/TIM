"""Routes for document slide."""

from flask import jsonify

from routes.view import *
import pluginControl

slide_page = Blueprint('slide_page',
                       __name__,
                       url_prefix='')


@view_page.route("/show_slide/<path:doc_name>")
def slide_iframe(doc_name):
    try:
        view_range = parse_range(request.args.get('b'), request.args.get('e'))
    except (ValueError, TypeError):
        return abort(400, "Invalid start or end index specified.")

    return slide(doc_name, 'show_slide.html', view_range)


def slide(doc_name, template_name, view_range=None, usergroup=None, teacher=False):
    timdb = getTimDb()
    doc_id = timdb.documents.get_document_id(doc_name)
    if doc_id is None or not timdb.documents.exists(doc_id):
        # Backwards compatibility: try to use as document id
        try:
            doc_id = int(doc_name)
            if not timdb.documents.exists(doc_id):
                return try_return_folder(doc_name)
            doc_name = timdb.documents.get_first_document_name(doc_id)
        except ValueError:
            return try_return_folder(doc_name)

    if teacher:
        verify_teacher_access(doc_id)

    if not has_view_access(doc_id):
        if not logged_in():
            session['came_from'] = request.url
            session['anchor'] = request.args.get('anchor', '')
            return render_template('loginpage.html',
                                   came_from=request.url,
                                   anchor=session['anchor'])
        else:
            abort(403)

    start_index = max(view_range[0], 0) if view_range else 0
    doc, xs = get_document(doc_id, view_range)
    user = getCurrentUserId()

    if teacher:
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
    texts, js_paths, css_paths, modules = pluginControl.pluginify(doc,
                                                                  xs,
                                                                  current_user if teacher or logged_in() else None,
                                                                  timdb.answers,
                                                                  sanitize=False)

    modules.append("ngSanitize")
    modules.append("ngFileUpload")
    prefs = timdb.users.getPrefs(getCurrentUserId())
    custom_css_files = json.loads(prefs).get('css_files', {}) if prefs is not None else {}
    if custom_css_files:
        custom_css_files = {key: value for key, value in custom_css_files.items() if value}
    custom_css = json.loads(prefs).get('custom_css', '') if prefs is not None else ''
    settings = tim.get_user_settings()
    return render_template(template_name,
                           doc={'id': doc_id, 'name': doc_name},
                           text=texts,
                           plugin_users=users,
                           current_user=current_user,
                           version=doc.get_version(),
                           js=js_paths,
                           cssFiles=css_paths,
                           jsMods=modules,
                           custom_css_files=custom_css_files,
                           custom_css=custom_css,
                           start_index=start_index,
                           teacher_mode=teacher,
                           is_owner=has_ownership(doc_id),
                           group=usergroup,
                           settings=settings,
                           rights={'editable': has_edit_access(doc_id),
                                   'can_mark_as_read': has_read_marking_right(doc_id),
                                   'can_comment': has_comment_right(doc_id),
                                   'browse_own_answers': logged_in()
                                   })
