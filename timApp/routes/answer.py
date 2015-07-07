"""Answer-related routes."""

from flask import Blueprint

from .common import *
import pluginControl
import containerLink


answers = Blueprint('answers',
                    __name__,
                    url_prefix='')


def get_plugin_markup(doc_id, plugintype, task_id):
    timdb = getTimDb()
    doc_markdown = timdb.documents.getDocumentAsHtmlBlocks(getNewest(doc_id))
    for block in doc_markdown:
        if 'plugin="{}"'.format(plugintype) in block and "<pre" in block and 'id="{}"'.format(task_id) in block:
            markup = pluginControl.get_block_yaml(block)
            return markup
    return None


def parse_task_id(task_id):
    # Assuming task_id is of the form "22.palindrome"
    pieces = task_id.split('.')
    if len(pieces) != 2:
        abort(400, 'The format of task_id is invalid. Expected exactly one dot character.')
    doc_id = int(pieces[0])
    task_id_name = pieces[1]
    return doc_id, task_id_name


@answers.route("/<plugintype>/<task_id>/answer/", methods=['PUT'])
def saveAnswer(plugintype, task_id):
    timdb = getTimDb()

    doc_id, task_id_name = parse_task_id(task_id)
    verifyViewAccess(doc_id)
    if 'input' not in request.get_json():
        return jsonResponse({'error' : 'The key "input" was not found from the request.'}, 400)
    answerdata = request.get_json()['input']

    answer_browser_data = request.get_json().get('abData', {})
    is_teacher = answer_browser_data.get('teacher', False)
    if is_teacher:
        verifyOwnership(doc_id)

    # Load old answers
    old_answers = timdb.answers.getAnswers(getCurrentUserId(), task_id)

    # Get the newest answer (state). Only for logged in users.
    state = pluginControl.try_load_json(old_answers[0]['content']) if loggedIn() and len(old_answers) > 0 else None

    markup = get_plugin_markup(doc_id, plugintype, task_id_name)
    if markup is None:
        return jsonResponse({'error': 'The task was not found in the document. '
                                      + str(doc_id)
                                      + ' ' + task_id_name}, 404)
    if markup == "YAMLERROR: Malformed string":
        return jsonResponse({'error': 'Plugin markup YAML is malformed.'}, 400)

    answer_call_data = {'markup': markup, 'state': state, 'input': answerdata, 'taskID': task_id}

    plugin_response = containerLink.call_plugin_answer(plugintype, answer_call_data)

    try:
        jsonresp = json.loads(plugin_response)
    except ValueError:
        return jsonResponse({'error': 'The plugin response was not a valid JSON string. The response was: '
                                      + plugin_response}, 400)

    if 'web' not in jsonresp:
        return jsonResponse({'error': 'The key "web" is missing in plugin response.'}, 400)

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
            timdb.answers.saveAnswer([getCurrentUserId()], task_id, json.dumps(save_object), points, tags)
        else:
            if answer_browser_data.get('saveTeacher', False):
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
                timdb.answers.saveAnswer(users, task_id, json.dumps(save_object), points, tags)

    return jsonResponse({'web': jsonresp['web']})


def get_hidden_name(user_id):
    return 'Undisclosed student %d' % user_id


def should_hide_name(doc_id, user_id):
    return not getTimDb().users.userIsOwner(user_id, doc_id) and user_id != getCurrentUserId()


@answers.route("/answers/<task_id>/<int:user_id>")
def get_answers(task_id, user_id):
    verifyLoggedIn()
    timdb = getTimDb()
    doc_id, task_id_name = parse_task_id(task_id)
    if not timdb.documents.documentExists(doc_id):
        abort(404, 'No such document')
    user = timdb.users.getUser(user_id)
    if user_id != getCurrentUserId():
        verifyOwnership(doc_id)
    if user is None:
        abort(400, 'Non-existent user')
    user_answers = timdb.answers.getAnswers(user_id, task_id)
    if hide_names_in_teacher(doc_id):
        for answer in user_answers:
            for c in answer['collaborators']:
                if should_hide_name(doc_id, c['user_id']):
                    c['real_name'] = get_hidden_name(c['user_id'])
    return jsonResponse(user_answers)


@answers.route("/getState")
def get_state():
    timdb = getTimDb()
    doc_id, par_id, user_id, state = unpack_args('doc_id', 'par_id', 'user_id', 'state', types=[int, int, int, str])
    if not timdb.documents.documentExists(doc_id):
        abort(404, 'No such document')
    user = timdb.users.getUser(user_id)
    if user_id != getCurrentUserId():
        verifyOwnership(doc_id)
    if user is None:
        abort(400, 'Non-existent user')
    if not timdb.documents.documentExists(doc_id):
        abort(404, 'No such document')
    if not hasViewAccess(doc_id):
        abort(403, 'Permission denied')

    version = request.headers['Version']
    block = timdb.documents.getBlockAsHtml(DocIdentifier(doc_id, version), par_id)

    texts, js_paths, css_paths, modules = pluginControl.pluginify([block],
                                                                  user['name'],
                                                                  timdb.answers,
                                                                  doc_id,
                                                                  user_id,
                                                                  custom_state=state)
    return jsonResponse(texts[0])


@answers.route("/getTaskUsers/<task_id>")
def get_task_users(task_id):
    doc_id, task_id_name = parse_task_id(task_id)
    verifyOwnership(doc_id)
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
