"""Answer-related routes."""
from datetime import datetime

from flask import Blueprint

import documentmodel.document
from .common import *
from plugin import Plugin, PluginException
import pluginControl
import containerLink


answers = Blueprint('answers',
                    __name__,
                    url_prefix='')


def is_answer_valid(plugin, old_answers, tim_info):
    """Determines whether the currently posted answer should be considered valid.

    :param plugin: The plugin object to which the answer was posted.
    :param old_answers: The old answers for this task for the current user.
    :param tim_info: The tim_info structure returned by the plugin or None.
    :return: True if the answer should be considered valid, False otherwise.
    """
    answer_limit = plugin.answer_limit()
    if answer_limit is not None and (answer_limit <= len(old_answers)):
        return False, 'You have exceeded the answering limit.'
    if plugin.starttime(default=datetime(1970, 1, 1)) > datetime.now():
        return False, 'You cannot submit answers yet.'
    if plugin.deadline(default=datetime.max) < datetime.now():
        return False, 'The deadline for submitting answers has passed.'
    return True, 'ok'


@answers.route("/<plugintype>/<task_id>/answer/", methods=['PUT'])
def save_answer(plugintype, task_id):
    """
    Saves the answer submitted by user for a plugin in the database.

    :type task_id: str
    :type plugintype: str
    :param plugintype: The type of the plugin, e.g. csPlugin.
    :param task_id: The task id of the form "22.palidrome".
    :return: JSON
    """
    timdb = getTimDb()
    doc_id, task_id_name = Plugin.parse_task_id(task_id)
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
    if 'input' not in request.get_json():
        return jsonResponse({'error': 'The key "input" was not found from the request.'}, 400)
    answerdata = request.get_json()['input']

    answer_browser_data = request.get_json().get('abData', {})
    is_teacher = answer_browser_data.get('teacher', False)
    if is_teacher:
        verify_seeanswers_access(doc_id)
    try:
        plugin = Plugin.from_task_id(task_id)
    except PluginException as e:
        return abort(400, str(e))

    if plugin.type != plugintype:
        abort(400, 'Plugin type mismatch: {} != {}'.format(plugin.type, plugintype))

    # Load old answers
    old_answers = timdb.answers.get_answers(getCurrentUserId(), task_id, get_collaborators=False)

    # Get the newest answer (state). Only for logged in users.
    state = pluginControl.try_load_json(old_answers[0]['content']) if logged_in() and len(old_answers) > 0 else None

    answer_call_data = {'markup': plugin.values, 'state': state, 'input': answerdata, 'taskID': task_id}

    plugin_response = containerLink.call_plugin_answer(plugintype, answer_call_data)

    try:
        jsonresp = json.loads(plugin_response)
    except ValueError:
        return jsonResponse({'error': 'The plugin response was not a valid JSON string. The response was: ' +
                                      plugin_response}, 400)

    if 'web' not in jsonresp:
        return jsonResponse({'error': 'The key "web" is missing in plugin response.'}, 400)
    result = {'web': jsonresp['web']}
    if 'save' in jsonresp:
        save_object = jsonresp['save']
        tags = []
        tim_info = jsonresp.get('tim_info', {})
        points = tim_info.get('points', None)

        # Save the new state
        try:
            tags = save_object['tags']
        except (TypeError, KeyError):
            pass
        if not is_teacher:
            is_valid, explanation = is_answer_valid(plugin, old_answers, tim_info)
            if answer_browser_data.get('giveCustomPoints'):
                custom_points = answer_browser_data.get('points')
                try:
                    custom_points_float = float(custom_points)
                except ValueError:
                    result['error'] = 'Wrong format for custom points.'
                else:
                    # Small hack: we differentiate custom points from automatic points by appending a space character
                    # at the end of the custom points
                    custom_points += ' '
                    points_min = plugin.user_min_points()
                    points_max = plugin.user_max_points()
                    if points_min is None or points_max is None:
                        result['error'] = 'You cannot give yourself custom points in this task.'
                    elif not (points_min <= custom_points_float <= points_max):
                        result['error'] = 'Points must be in range [{},{}]'.format(points_min, points_max)
                    else:
                        points = custom_points
            result['savedNew'] = timdb.answers.saveAnswer([getCurrentUserId()], task_id, json.dumps(save_object), points, tags, is_valid)
            if not is_valid:
                result['error'] = explanation
        else:
            if answer_browser_data.get('saveTeacher', False):
                verify_teacher_access(doc_id)
                answer_id = answer_browser_data.get('answer_id', None)
                if answer_id is None:
                    return jsonResponse({'error': 'Missing answer_id key'}, 400)
                expected_task_id = timdb.answers.get_task_id(answer_id)
                if expected_task_id != task_id:
                    return jsonResponse({'error': 'Task ids did not match'}, 400)
                users = timdb.answers.get_users(answer_id)
                if len(users) == 0:
                    return jsonResponse({'error': 'No users found for the specified answer'}, 400)
                if not getCurrentUserId() in users:
                    users.append(getCurrentUserId())
                points = answer_browser_data.get('points', points)
                if points == "":
                    points = None
                result['savedNew'] = timdb.answers.saveAnswer(users, task_id, json.dumps(save_object), points, tags, valid=True)

    return jsonResponse(result)


def get_hidden_name(user_id):
    return 'Undisclosed student %d' % user_id


def should_hide_name(doc_id, user_id):
    return not getTimDb().users.has_teacher_access(user_id, doc_id) and user_id != getCurrentUserId()


@answers.route("/taskinfo/<task_id>")
def get_task_info(task_id):
    plugin = Plugin.from_task_id(task_id)
    tim_vars = {'maxPoints': plugin.max_points(),
                'userMin': plugin.user_min_points(),
                'userMax': plugin.user_max_points(),
                'deadline': plugin.deadline(),
                'starttime': plugin.starttime(),
                'answerLimit': plugin.answer_limit()}
    return jsonResponse(tim_vars)


@answers.route("/answers/<task_id>/<user_id>")
def get_answers(task_id, user_id):
    try:
        user_id = int(user_id)
    except ValueError:
        abort(404, 'Not a valid user id')
    verifyLoggedIn()
    timdb = getTimDb()
    doc_id, task_id_name = Plugin.parse_task_id(task_id)
    if not timdb.documents.exists(doc_id):
        abort(404, 'No such document')
    user = timdb.users.getUser(user_id)
    if user_id != getCurrentUserId():
        verify_seeanswers_access(doc_id)
    if user is None:
        abort(400, 'Non-existent user')
    user_answers = timdb.answers.get_answers(user_id, task_id)
    if hide_names_in_teacher(doc_id):
        for answer in user_answers:
            for c in answer['collaborators']:
                if should_hide_name(doc_id, c['user_id']):
                    c['real_name'] = get_hidden_name(c['user_id'])
    return jsonResponse(user_answers)


@answers.route("/allAnswers/<task_id>")
def get_all_answers(task_id):
    verifyLoggedIn()
    timdb = getTimDb()
    doc_id, task_id_name = Plugin.parse_task_id(task_id)
    usergroup = request.args.get('group')
    if not usergroup: usergroup = 0
    if not timdb.documents.exists(doc_id):
        abort(404, 'No such document')

    # Require full teacher rights for getting all answers
    verify_teacher_access(doc_id)
    all_answers = timdb.answers.get_all_answers(task_id, usergroup, hide_names_in_teacher(doc_id))
    return jsonResponse(all_answers)


@answers.route("/getState")
def get_state():
    timdb = getTimDb()
    doc_id, par_id, user_id, answer_id = unpack_args('doc_id',
                                                     'par_id',
                                                     'user_id',
                                                     'answer_id', types=[int, str, int, int])
    if not timdb.documents.exists(doc_id):
        abort(404, 'No such document')
    user = timdb.users.getUser(user_id)
    is_teacher = False
    if user_id != getCurrentUserId():
        verify_seeanswers_access(doc_id)
        is_teacher = True
    if user is None:
        abort(400, 'Non-existent user')
    if not timdb.documents.exists(doc_id):
        abort(404, 'No such document')
    if not has_view_access(doc_id):
        abort(403, 'Permission denied')

    answer = timdb.answers.get_answer(answer_id)
    if answer is None:
        abort(400, 'Non-existent answer')
    access = False
    if any(a['user_id'] == user_id for a in answer['collaborators']):
        access = True
    task_doc_id, task_name = Plugin.parse_task_id(answer['task_id'])
    if is_teacher and task_doc_id == doc_id:
        access = True
    if not access:
        abort(403, "You don't have access to this answer.")

    # version = request.headers['Version']
    doc = Document(doc_id)
    block = doc.get_paragraph(par_id)

    texts, js_paths, css_paths, modules = pluginControl.pluginify(doc,
                                                                  [block],
                                                                  user,
                                                                  timdb.answers,
                                                                  custom_state=answer['content'])
    return jsonResponse({'html': texts[0]['html']})


@answers.route("/getTaskUsers/<task_id>")
def get_task_users(task_id):
    doc_id, task_id_name = Plugin.parse_task_id(task_id)
    verify_seeanswers_access(doc_id)
    usergroup = request.args.get('group')
    users = getTimDb().answers.get_users_by_taskid(task_id)
    if usergroup is not None:
        timdb = getTimDb()
        users = [user for user in users if timdb.users.isUserIdInGroup(user['id'], usergroup)]
    if hide_names_in_teacher(doc_id):
        for user in users:
            if should_hide_name(doc_id, user['id']):
                user['name'] = '-'
                user['real_name'] = get_hidden_name(user['id'])
    return jsonResponse(users)
