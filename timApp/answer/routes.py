"""Answer-related routes."""
import json
import re
import time
from collections import defaultdict
from datetime import timezone, timedelta, datetime
from typing import Union, List, Tuple, Dict, Match, Iterable

import attr
import dateutil.parser
import dateutil.relativedelta
from flask import Blueprint
from flask import Response
from flask import abort
from flask import request
from marshmallow import Schema, fields, post_load, validates_schema, ValidationError, pre_load
from marshmallow.utils import _Missing, missing
from sqlalchemy import func, tuple_
from sqlalchemy.orm import defaultload, joinedload
from webargs.flaskparser import use_args

from pluginserver_flask import GenericMarkupSchema
from timApp.answer.answer import Answer
from timApp.answer.answer_models import AnswerUpload
from timApp.answer.answers import get_latest_answers_query, get_common_answers, save_answer, get_all_answers
from timApp.auth.accesshelper import verify_logged_in, get_doc_or_abort, verify_manage_access
from timApp.auth.accesshelper import verify_task_access, verify_teacher_access, verify_seeanswers_access, \
    has_teacher_access, \
    verify_view_access, get_plugin_from_request
from timApp.auth.accesstype import AccessType
from timApp.auth.sessioninfo import get_current_user_id, logged_in
from timApp.auth.sessioninfo import get_current_user_object, get_session_users, get_current_user_group
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.document import Document
from timApp.document.post_process import hide_names_in_teacher
from timApp.item.block import Block, BlockType
from timApp.markdown.dumboclient import call_dumbo
from timApp.notification.notify import multi_send_email
from timApp.plugin.containerLink import call_plugin_answer
from timApp.plugin.plugin import Plugin, PluginWrap, NEVERLAZY, TaskNotFoundException
from timApp.plugin.plugin import PluginType
from timApp.plugin.plugin import find_plugin_from_document
from timApp.plugin.pluginControl import find_task_ids, pluginify
from timApp.plugin.pluginControl import task_ids_to_strlist
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.taskid import TaskId, TaskIdAccess
from timApp.timdb.dbaccess import get_timdb
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.user.groups import verify_group_view_access
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import verify_json_params, get_option, get_consent_opt
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.utils import try_load_json, get_current_time

answers = Blueprint('answers',
                    __name__,
                    url_prefix='')


@answers.route("/savePoints/<int:user_id>/<int:answer_id>", methods=['PUT'])
def save_points(answer_id, user_id):
    answer, _ = verify_answer_access(
        answer_id,
        user_id,
        require_teacher_if_not_own=True,
    )
    tid = TaskId.parse(answer.task_id)
    d = get_doc_or_abort(tid.doc_id)
    points, = verify_json_params('points')
    try:
        plugin = Plugin.from_task_id(answer.task_id, user=get_current_user_object())
    except PluginException as e:
        return abort(400, str(e))
    a = Answer.query.get(answer_id)
    try:
        points = points_to_float(points)
    except ValueError:
        abort(400, 'Invalid points format.')
    try:
        a.points = plugin.validate_points(points) if not has_teacher_access(d) else points
    except PluginException as e:
        abort(400, str(e))
    a.last_points_modifier = get_current_user_group()
    db.session.commit()
    return ok_response()


def points_to_float(points: Union[str, float]):
    if points:
        points = float(points)
    else:
        points = None
    return points


@answers.route("/iframehtml/<plugintype>/<task_id_ext>/<int:user_id>/<int:anr>")
def get_iframehtml(plugintype: str, task_id_ext: str, user_id: int, anr: int):
    """
    Gets the HTML to be used in iframe.

    :param plugintype: plugin type
    :param task_id_ext: task id
    :param user_id: the user whose information to get
    :param anr: answer number from answer browser, 0 = newest
    :return: HTML to be used in iframe
    """
    timdb = get_timdb()
    try:
        tid = TaskId.parse(task_id_ext)
    except PluginException as e:
        return abort(400, f'Task id error: {e}')
    d = get_doc_or_abort(tid.doc_id)
    d.document.insert_preamble_pars()

    try:
        tid.block_id_hint = None  # TODO: this should only be done in preview?
        plugin = verify_task_access(d, tid, AccessType.view, TaskIdAccess.ReadWrite)
    except (PluginException, TimDbException) as e:
        return abort(400, str(e))

    if plugin.type != plugintype:
        abort(400, f'Plugin type mismatch: {plugin.type} != {plugintype}')

    users = [User.query.get(user_id)]

    old_answers = get_common_answers(users, tid)

    info = plugin.get_info(users, len(old_answers), look_answer=False and not False, valid=True)

    if anr < 0:
        anr = 0
    # Get the newest answer (state). Only for logged in users.
    state = try_load_json(old_answers[anr].content) if logged_in() and len(old_answers) > 0 else None

    answer_call_data = {'markup': plugin.values,
                        'state': state,
                        'taskID': tid.doc_task,
                        'info': info,
                        'iframehtml': True}

    plugin_response = call_plugin_answer(plugintype, answer_call_data)
    try:
        jsonresp = json.loads(plugin_response)
    except ValueError:
        return json_response({'error': 'The plugin response was not a valid JSON string. The response was: ' +
                                       plugin_response}, 400)
    except PluginException:
        return json_response({'error': 'The plugin response took too long'}, 400)

    if 'iframehtml' not in jsonresp:
        return json_response({'error': 'The key "iframehtml" is missing in plugin response.'}, 400)
    result = jsonresp['iframehtml']
    db.session.commit()
    return result


def chunks(l: List, n: int):
    for i in range(0, len(l), n):
        yield l[i:i + n]


@answers.route("/multiplugin2", methods=['POST'])
def get_answers_for_tasks2():
    """
    Queries all answers for given list of tasks by the given user
    TODO: experimental, delete?
    :return: {answers:[Answer], user: user_id}
    """
    tasks, user_id = verify_json_params('tasks', 'user')
    try:
        user_id = int(user_id)
    except ValueError:
        abort(404, 'Not a valid user id')
    user = User.get_by_id(user_id)
    verify_logged_in()
    try:
        doc_map = {}
        fieldlist = {k: [] for k in tasks}
        for task_id in tasks:
            tid = TaskId.parse(task_id)
            if tid.doc_id not in doc_map:
                dib = get_doc_or_abort(tid.doc_id, f'Document {tid.doc_id} not found')
                verify_seeanswers_access(dib)
                doc_map[tid.doc_id] = dib.document
        answs = user.answers.options(joinedload(Answer.users_all))\
            .order_by(Answer.id.desc()).filter(Answer.valid.is_(True))\
            .filter(Answer.task_id.in_(tasks)).all()
        for a in answs:
            fieldlist[a.task_id].append(a)
        return json_response({"answers": fieldlist, "userId": user_id})
    except Exception as e:
        return abort(400, str(e))


def get_useranswers_for_task(user, task_ids, answer_map):
    """
    Performs a query for latest valid answers by given user for given task
    Similar to pluginControl.get_answers but without counting
    :param user: user
    :param task_ids: tasks to be queried
    :param answer_map: a dict where to add each taskID: Answer
    :return: {taskID: Answer}
    """
    col = func.max(Answer.id).label('col')
    sub = (user
           .answers
           .filter(Answer.task_id.in_(task_ids_to_strlist(task_ids)) & Answer.valid == True)
           .add_columns(col)
           .with_entities(col)
           .group_by(Answer.task_id).subquery())
    answs: List[Answer] = Answer.query.join(sub, Answer.id == sub.c.col) .all()
    for answer in answs:
        if len(answer.users_all) > 1:
            answer_map[answer.task_id] = answer
        else:
            # answer_map[answer.task_id] = answer
            asd = answer.to_json()
            asd.pop('users')
            answer_map[answer.task_id] = asd
    return answs


@answers.route("/userAnswersForTasks", methods=['POST'])
def get_answers_for_tasks():
    """
    Route for getting latest valid answers for given user and list of tasks
    :return: {"answers": {taskID: Answer}, "userId": user_id}
    """
    tasks, user_id = verify_json_params('tasks', 'user')
    try:
        user_id = int(user_id)
    except ValueError:
        abort(404, 'Not a valid user id')
    user = User.get_by_id(user_id)
    verify_logged_in()
    try:
        doc_map = {}
        fieldlist = {}
        tids = []
        for task_id in tasks:
            tid = TaskId.parse(task_id)
            if tid.doc_id not in doc_map:
                dib = get_doc_or_abort(tid.doc_id, f'Document {tid.doc_id} not found')
                verify_seeanswers_access(dib)
                doc_map[tid.doc_id] = dib.document
            tids.append(tid)
        answer_map = {}
        # pluginControl.get_answers(user, tids, answer_map)
        get_useranswers_for_task(user, tids, answer_map)
        return json_response({"answers": answer_map, "userId": user_id})
    except Exception as e:
        return abort(400, str(e))


TASK_PROG = re.compile('([\w\.]*)\((\d*),(\d*)\)(.*)') # see https://regex101.com/r/ZZuizF/2
TASK_NAME_PROG = re.compile("(\d+.)?([\w\d]+)[.\[]?.*")  # see https://regex101.com/r/OjnTAn/4


def widen_fields(fields: List[str]):
    """
    if there is syntax d(1,3) in fileds, it is made d1,d2
    from d(1,3)=t  would come d1=t1, d2=t2
    :param fields: list of fields
    :return: array fields widened
    """
    fields1 = []
    for field in fields:
        parts = field.split(";")
        fields1.extend(parts)

    rfields = []
    for field in fields1:
        try:
            t, a, *rest = field.split("=")
        except ValueError:
            t, a, rest = field, "", None
        t = t.strip()
        a = a.strip()
        match = re.search(TASK_PROG, t)
        if not match:
            rfields.append(field)
            continue

        tb = match.group(1)
        n1 = int(match.group(2))
        n2 = int(match.group(3))
        te = match.group(4)

        for i in range(n1, n2):
            tn = tb + str(i) + te
            if not tb:
                tn = ""
            if a:
                tn += "=" + a + str(i)
            rfields.append(tn)

    return rfields


def get_alias(name):
    """
    Get name part form string like 534.d1.points
    :param name: full name of field
    :return: just name part of field, like d1
    """
    t = name.strip()
    match = re.search(TASK_NAME_PROG, t)
    if not match:
        return name
    return match.group(2)


def get_fields_and_users(u_fields: List[str], groups: List[UserGroup],
                         d: DocInfo, current_user: User, autoalias: bool = False,
                         add_missing_fields: bool = False, allow_non_teacher: bool = False):
    """
    Return fielddata, aliases, field_names
    :param u_fields: list of fields to be used
    :param groups: user groups to be used
    :param d: default document
    :param current_user: current users, check his rights to fields
    :param autoalias: if true, give automatically from d1 same as would be from d1 = d1
    :param add_missing_fields: return estimated field even if it wasn't given previously
    :param allow_non_teacher: can be used also for non techers if othre rights matches
    :return: fielddata, aliases, field_names
    """
    needs_group_access_check = UserGroup.get_teachers_group() not in current_user.groups
    ugroups = []
    for group in groups:
        if needs_group_access_check and group.name != current_user.name:
            if not verify_group_view_access(group, current_user, require=False):
                # return abort(403, f'Missing view access for group {group.name}')
                continue # TODO: study how to give just warning from missing access, extra return string?
        ugroups.append(group)

    if not ugroups:  # if no access, give at least own group
       for group in current_user.groups:
           if group.name == current_user.name:
               print(group.name + " added just user group")
               ugroups.append(group)

    groups = ugroups

    task_ids = []
    task_id_map = defaultdict(list)
    alias_map = {}
    jsrunner_alias_map = {}
    doc_map = {}
    num_prog = re.compile('^\d+\..+/')

    u_fields = widen_fields(u_fields)
    tasks_without_fields = []
    for field in u_fields:
        try:
            t, a, *rest = field.split("=")
        except ValueError:
            a = None
            t, rest = field, None
            if autoalias:
                a = get_alias(t)
        t = t.strip()
        if a:
            a = a.strip()
        if rest:
            return abort(400, f'Invalid alias: {field}')
        if a == '':
            return abort(400, f'Alias cannot be empty: {field}')
        try:
            task_id = TaskId.parse(t, False, False, add_missing_fields)
        except PluginException as e:
            return abort(400, str(e))
        if task_id.field is None:
            tasks_without_fields.append(task_id)
        task_ids.append(task_id)
        if not task_id.doc_id:
            task_id.doc_id = d.id
        task_id_map[task_id.doc_task].append(task_id)
        if a:
            alias = a
            if alias in jsrunner_alias_map:
                abort(400, f'Duplicate alias {alias} in fields attribute')
            alias_map[task_id.extended_or_doc_task] = alias
            jsrunner_alias_map[alias] = task_id.extended_or_doc_task
        if task_id.doc_id in doc_map:
            continue
        dib = get_doc_or_abort(task_id.doc_id, f'Document {task_id.doc_id} not found')

        if not (current_user.has_teacher_access(dib) or allow_non_teacher):
            abort(403, f'Missing teacher access for document {dib.id}')
        doc_map[task_id.doc_id] = dib.document

    if add_missing_fields:
        for task in tasks_without_fields:
            try:
                plug = find_plugin_from_document(doc_map[task.doc_id], task, current_user)
                task.field = plug.get_content_field_name()
            except TaskNotFoundException:
                task.field = "c"
            try:
                alias_map[task.doc_task_with_field] = alias_map[task.doc_task]
                jsrunner_alias_map[alias_map[task.doc_task]] = task.doc_task_with_field
                del alias_map[task.doc_task]
            except KeyError:
                pass

    res = []
    group_filter = UserGroup.id.in_([ug.id for ug in groups])
    sub = []

    # For some reason, with 7 or more fields, executing the following query is very slow.
    # That's why we split the list of task ids in chunks of size 6 and merge the results.
    for task_chunk in chunks(task_ids, 6):
        sub += (
            Answer.query.filter(Answer.task_id.in_(task_ids_to_strlist(task_chunk)))
                .join(User, Answer.users)
                .join(UserGroup, User.groups)
                .filter(group_filter)
                .group_by(Answer.task_id, User.id)
                .with_entities(func.max(Answer.id), User.id)
                .all()
        )
    aid_uid_map = {}
    for aid, uid in sub:
        aid_uid_map[aid] = uid
    users = (
        UserGroup.query.filter(group_filter)
            .join(User, UserGroup.users)
            .options(defaultload(UserGroup.users).lazyload(User.groups))
            .with_entities(User)
            .order_by(User.id)
            .all()
    )
    user_map = {}
    for u in users:
        user_map[u.id] = u
    answs = Answer.query.filter(Answer.id.in_(aid for aid, _ in sub)).all()
    answers_with_users = []
    for a in answs:
        answers_with_users.append((aid_uid_map[a.id], a))
    missing_users = set(u.id for u in users) - set(uid for uid, _ in answers_with_users)
    for mu in missing_users:
        answers_with_users.append((mu, None))
    answers_with_users.sort(key=lambda x: x[0])
    last_user = None
    user_tasks = None
    user_index = -1
    user = None
    for uid, a in answers_with_users:
        if last_user != uid:
            user_index += 1
            user_tasks = {}
            user = users[user_index]
            res.append({'user': user, 'fields': user_tasks})
            last_user = uid
            if not a:
                continue
        for task in task_id_map[a.task_id]:
            if not a:
                value = None
            elif task.field == "points":
                value = a.points
            elif task.field == "datetime":
                value = time.mktime(a.answered_on.timetuple())
            else:
                json_str = a.content
                p = json.loads(json_str)
                if task.field:
                    value = p.get(task.field)
                else:
                    if len(p) > 1:
                        plug = find_plugin_from_document(doc_map[task.doc_id], task, user)
                        content_field = plug.get_content_field_name()
                        value = p.get(content_field)
                    else:
                        values_p = list(p.values())
                        value = values_p[0]
            user_tasks[alias_map.get(task.extended_or_doc_task, task.extended_or_doc_task)] = value
    return res, jsrunner_alias_map, [alias_map.get(ts.extended_or_doc_task, ts.extended_or_doc_task) for ts in task_ids]


class JsRunnerSchema(GenericMarkupSchema):
    creditField = fields.Str()
    gradeField = fields.Str()
    gradingScale = fields.Dict()
    defaultPoints = fields.Float()
    failGrade = fields.Str()
    fieldhelper = fields.Bool()
    group = fields.Str()
    groups = fields.List(fields.Str())
    program = fields.Str()
    preprogram = fields.Str()
    postprogram = fields.Str()
    timeout = fields.Int()
    updateFields = fields.List(fields.Str())
    paramFields = fields.List(fields.Str())
    fields = fields.List(fields.Str(), required=True)

    @validates_schema(skip_on_field_errors=True)
    def validate_schema(self, data):
        if data.get('group') is None and data.get('groups') is None:
            raise ValidationError("Either group or groups must be given.")


@answers.route("/<plugintype>/<task_id_ext>/multiSendEmail/", methods=['POST'])
def multisendemail(plugintype: str, task_id_ext: str):
    try:
        tid = TaskId.parse(task_id_ext)
    except PluginException as e:
        return abort(400, f'Task id error: {e}')
    d = get_doc_or_abort(tid.doc_id)
    verify_teacher_access(d)
    mail_from = get_current_user_object().email
    bcc = ""
    bccme = request.json.get('bccme', False)
    if bccme:
        bcc = mail_from
    multi_send_email(
        rcpt=request.json.get('rcpt'),
        subject=request.json.get('subject'),
        msg=request.json.get('msg'),
        mail_from=mail_from,
        reply_to=mail_from,
        bcc=bcc
    )
    return ok_response()


@answers.route("/<plugintype>/<task_id_ext>/answer/", methods=['PUT'])
def post_answer(plugintype: str, task_id_ext: str):
    """Saves the answer submitted by user for a plugin in the database.

    :param plugintype: The type of the plugin, e.g. csPlugin.
    :param task_id_ext: The extended task id of the form "22.palidrome.par_id".
    :return: JSON

    """

    try:
        tid = TaskId.parse(task_id_ext)
    except PluginException as e:
        return abort(400, f'Task id error: {e}')
    d = get_doc_or_abort(tid.doc_id)
    d.document.insert_preamble_pars()

    curr_user = get_current_user_object()
    ptype = PluginType(plugintype)
    answerdata, = verify_json_params('input')
    answer_browser_data, answer_options = verify_json_params('abData', 'options', require=False, default={})
    force_answer = answer_options.get('forceSave', False)
    is_teacher = answer_browser_data.get('teacher', False)
    save_teacher = answer_browser_data.get('saveTeacher', False)
    should_save_answer = answer_browser_data.get('saveAnswer', True) and tid.task_name

    if tid.is_points_ref:
        verify_teacher_access(d)
        given_points = answerdata.get(ptype.get_content_field_name())
        if given_points is not None:
            try:
                given_points = float(given_points)
            except ValueError:
                return abort(400, 'Points must be a number.')
        a = curr_user.answers.filter_by(task_id=tid.doc_task).order_by(Answer.id.desc()).first()
        if a:
            a.points = given_points
            s = None
        else:
            a = Answer(
                content=json.dumps({ptype.get_content_field_name(): ''}),
                points=given_points,
                task_id=tid.doc_task,
                users_all=[curr_user],
                valid=True,
            )
            db.session.add(a)
            db.session.flush()
            s = a.id
        db.session.commit()
        return json_response({'savedNew': s, 'web': {'result': 'points saved'}})

    if save_teacher:
        verify_teacher_access(d)
    users = None

    try:
        get_task = answerdata and answerdata.get("getTask", None) and ptype.can_give_task()
    except:
        get_task = False

    if not (should_save_answer or get_task) or is_teacher:
        verify_seeanswers_access(d)
    ctx_user = None
    if is_teacher:
        answer_id = answer_browser_data.get('answer_id', None)
        user_id = answer_browser_data.get('userId', None)

        if answer_id is not None:
            answer = Answer.query.get(answer_id)
            if not answer:
                return abort(404, f'Answer not found: {answer_id}')
            expected_task_id = answer.task_id
            if expected_task_id != tid.doc_task:
                return abort(400, 'Task ids did not match')

            # Later on, we may call users.append, but we don't want to modify the users of the existing
            # answer. Therefore, we make a copy of the user list so that SQLAlchemy no longer associates
            # the user list with the answer.
            users = list(answer.users_all)
            if not users:
                return abort(400, 'No users found for the specified answer')
            if user_id not in (u.id for u in users):
                return abort(400, 'userId is not associated with answer_id')
        elif user_id and user_id != curr_user.id and False: # TODO: Vesa's hack to no need for belong teachers group
            teacher_group = UserGroup.get_teachers_group()
            if curr_user not in teacher_group.users:
                abort(403, 'Permission denied: you are not in teachers group.')
        if user_id:
            ctx_user = User.query.get(user_id)
            if not ctx_user:
                abort(404, f'User {user_id} not found')
            users = [ctx_user]  # TODO: Vesa's hack to save answer to student
    try:
        plugin = verify_task_access(d, tid, AccessType.view, TaskIdAccess.ReadWrite, context_user=ctx_user)
    except (PluginException, TimDbException) as e:
        return abort(400, str(e))

    if plugin.type != plugintype:
        abort(400, f'Plugin type mismatch: {plugin.type} != {plugintype}')

    upload = None

    if ( plugin.values.get("useCurrentUser", False) ):  # For plugins that is saved only for current user
        users = [curr_user];

    if isinstance(answerdata, dict):
        file = answerdata.get('uploadedFile', '')
        trimmed_file = file.replace('/uploads/', '')
        type = answerdata.get('type', '')
        if trimmed_file and type == 'upload':
            # The initial upload entry was created in /pluginUpload route, so we need to check that the owner matches
            # what the browser is saying. Additionally, we'll associate the answer with the uploaded file later
            # in this route.
            block = Block.query.filter((Block.description == trimmed_file) &
                                       (Block.type_id == BlockType.Upload.value)).first()
            if block is None:
                abort(400, f'Non-existent upload: {trimmed_file}')
            verify_view_access(block, message="You don't have permission to touch this file.")
            upload = AnswerUpload.query.filter(AnswerUpload.upload_block_id == block.id).first()
            # if upload.answer_id is not None:
            #    abort(400, f'File was already uploaded: {file}')

    # Load old answers

    if users is None:
        users = [User.query.get(u['id']) for u in get_session_users()]

    old_answers = get_common_answers(users, tid)
    try:
        valid, _ = plugin.is_answer_valid(len(old_answers), {})
    except PluginException as e:
        return abort(400, str(e))
    info = plugin.get_info(users, len(old_answers), look_answer=is_teacher and not save_teacher, valid=valid)

    # Get the newest answer (state). Only for logged in users.
    state = try_load_json(old_answers[0].content) if logged_in() and len(old_answers) > 0 else None

    if plugin.type == 'jsrunner':
        s = JsRunnerSchema()
        try:
            s.load(plugin.values)
        except ValidationError as e:
            return abort(400, str(e))
        groupnames = plugin.values.get('groups', [plugin.values.get('group')])
        g = UserGroup.query.filter(UserGroup.name.in_(groupnames))
        found_groups = g.all()
        not_found_groups = sorted(list(set(groupnames) - set(g.name for g in found_groups)))
        if not_found_groups:
            abort(404, f'The following groups were not found: {", ".join(not_found_groups)}')

        try:
            pcomps = verify_json_params('paramComps')
            if pcomps: # TODO: lisää rajapintaan valmiiksi tuo paramComps, niin ei tarvii lähdekoodia manipuloida
                preprg = plugin.values.get("preprogram", "")
                # TODO:  miksi pcomps on taulukko???
                plugin.values["preprogram"] = "gtools.params = " + json.dumps(pcomps[0]) +";\n" + preprg
        except:
            pass

        siw = plugin.values.get("showInView", False)

        answerdata['data'], answerdata['aliases'], _ = get_fields_and_users(
            plugin.values['fields'],
            found_groups,
            d,
            get_current_user_object(), allow_non_teacher=siw,
        )
        if plugin.values.get('program') is None:
            abort(400, "Attribute 'program' is required.")

    answer_call_data = {'markup': plugin.values,
                        'state': state,
                        'input': answerdata,
                        'taskID': tid.doc_task,
                        'info': info}

    plugin_response = call_plugin_answer(plugintype, answer_call_data)
    try:
        jsonresp = json.loads(plugin_response)
    except ValueError:
        return json_response({'error': 'The plugin response was not a valid JSON string. The response was: ' +
                                       plugin_response}, 400)
    except PluginException:
        return json_response({'error': 'The plugin response took too long'}, 400)

    if 'web' not in jsonresp:
        # jsonresp["web"] = { 'error': plugin_response }
        # return json_response({'error': 'The key "web" is missing in plugin response.'}, 400)
        return json_response({'error': plugin_response, 'web': ''}, 400)
    result = {'web': jsonresp['web']}

    # if plugin.type == 'jsrunner' or plugin.type == 'tableForm' or plugin.type == 'importData':
    if 'savedata' in jsonresp:
        siw = answer_call_data.get("markup",{}).get("showInView", False)
        handle_jsrunner_response(jsonresp, result, d, allow_non_teacher = siw)
        db.session.commit()
        if 'save' not in jsonresp: # plugin may also hope some things to be saved
            return json_response(result)

    def add_reply(obj, key, run_markdown=False):
        if key not in plugin.values:
            return
        text_to_add = plugin.values[key]
        if run_markdown:
            dumbo_result = call_dumbo([text_to_add])
            text_to_add = dumbo_result[0]
        obj[key] = text_to_add

    if not get_task:
        add_reply(result['web'], '-replyImage')
        add_reply(result['web'], '-replyMD', True)
        add_reply(result['web'], '-replyHTML')
    if 'save' in jsonresp and not get_task:
        # TODO: RND_SEED: save used rnd_seed for this answer if answer is saved, found from par.get_rnd_seed()
        save_object = jsonresp['save']
        tags = []
        tim_info = jsonresp.get('tim_info', {})
        points = tim_info.get('points', None)
        multiplier = plugin.points_multiplier()
        if multiplier and points is not None:
            points *= plugin.points_multiplier()
        elif not multiplier:
            points = None
        # Save the new state
        try:
            tags = save_object['tags']
        except (TypeError, KeyError):
            pass
        if (not is_teacher and should_save_answer) or ( 'savedata' in jsonresp):
            is_valid, explanation = plugin.is_answer_valid(len(old_answers), tim_info)
            points_given_by = None
            if answer_browser_data.get('giveCustomPoints'):
                try:
                    points = plugin.validate_points(answer_browser_data.get('points'))
                except PluginException as e:
                    result['error'] = str(e)
                else:
                    points_given_by = get_current_user_group()
            if points or save_object is not None or tags:
                result['savedNew'] = save_answer(users,
                                                 tid,
                                                 json.dumps(save_object),
                                                 points,
                                                 tags,
                                                 is_valid,
                                                 points_given_by,
                                                 force_answer)
            else:
                result['savedNew'] = None
            if not is_valid:
                result['error'] = explanation
        elif save_teacher:
            points = answer_browser_data.get('points', points)
            points = points_to_float(points)
            result['savedNew'] = save_answer(users,
                                             tid,
                                             json.dumps(save_object),
                                             points,
                                             tags,
                                             valid=True,
                                             points_given_by=get_current_user_group(),
                                             saver=curr_user)
        else:
            result['savedNew'] = None
        if result['savedNew'] is not None and upload is not None:
            # Associate this answer with the upload entry
            upload.answer_id = result['savedNew']

    db.session.commit()
    return json_response(result)


def handle_jsrunner_response(jsonresp, result, current_doc: DocInfo = None, allow_non_teacher = False):
    # TODO: Might need to rewrite this function for optimization
    save_obj = jsonresp.get('savedata')
    if not save_obj:
        return
    tasks = set()
    doc_map: Dict[int, DocInfo] = {}
    for item in save_obj:
        task_u = item['fields']
        for tid in task_u.keys():
            tasks.add(tid)
            try:
                id_num = TaskId.parse(tid, False, False, True)
            except PluginException:
                return abort(400, f'Invalid task name: {tid.split(".")[1]}')
            if not id_num.doc_id:
                return abort(400, f'Doc id missing: {tid}')
            if id_num.doc_id not in doc_map:
                doc_map[id_num.doc_id] = get_doc_or_abort(id_num.doc_id)
    task_content_name_map = {}
    curr_user = get_current_user_object()
    for task in tasks:
        t_id = TaskId.parse(task, False, False, True)
        dib = doc_map[t_id.doc_id]
        if not (curr_user.has_teacher_access(dib) or allow_non_teacher):
            return abort(403, f'Missing teacher access for document {dib.id}')
        try:
            plugin = verify_task_access(dib, t_id, AccessType.view, TaskIdAccess.ReadWrite)  # , context_user=ctx_user)
        except (PluginException, TimDbException) as e:
            return abort(400, str(e))

        if t_id.task_name == "grade" or t_id.task_name == "credit":
            task_content_name_map[task] = 'c'
            continue
        try:
            if t_id.field  and t_id.field != "points":
                task_content_name_map[task] = t_id.field
            else:
                plug = find_plugin_from_document(dib.document, t_id, curr_user)
                content_field = plug.get_content_field_name()
                task_content_name_map[task] = content_field
        except TaskNotFoundException as e:
            #task_display = t_id.doc_task if t_id.doc_id != current_doc.id else t_id.task_name
            #if not result['web'].get('error', None):
            #    result['web']['error'] = 'Errors:\n'
            #result['web']['error'] += f"Task not found: {task_display}\n"
            #task_content_name_map[task] = 'ignore'
            #continue
            task_content_name_map[task] = "c"

    for user in save_obj:
        u_id = user['user']
        u = User.get_by_id(u_id)
        user_fields = user['fields']
        task_map = {}
        for key, value in user_fields.items():
            task_id = TaskId.parse(key, False, False, True)
            field = task_id.field
            if field is None:
                field = task_content_name_map[task_id.doc_task]
            try:
                task_map[task_id.doc_task][field] = value
            except KeyError:
                task_map[task_id.doc_task] = {}
                task_map[task_id.doc_task][field] = value
        for taskid, contents in task_map.items():
            task_id = TaskId.parse(taskid, False, False)
            an: Answer = get_latest_answers_query(task_id, [u]).first()
            points = None
            content = {}
            new_answer = True
            if an:
                points = an.points
                content = json.loads(an.content)
                new_answer = False
            for field, value in contents.items():
                if field == 'points':
                    if value == "":
                        value = None
                    else:
                        try:
                            value = float(value)
                        except ValueError:
                            if not result['web'].get('error', None):
                                result['web']['error'] = 'Errors:\n'
                            result['web'][
                                'error'] += f"Value {value} is not valid point value for task {task_id.task_name}\n"
                            continue
                    if points != value:
                        new_answer = True
                    points = value
                else:
                    if an and content.get(field, "") != value:
                        new_answer = True
                    content[field] = value
            if not new_answer:
                continue
            if content == {}:
                # TODO: can this be reached without points field?
                content[task_content_name_map[task_id.doc_task + ".points"]] = None
            content = json.dumps(content)
            ans = Answer(
                content=content,
                points=points,
                task_id=task_id.doc_task,
                users=[u],
                valid=True,
                saver=curr_user,
            )
            db.session.add(ans)
        # for key, value in user_fields.items():
        #     content_field = task_content_name_map[key]
        #     if content_field == 'ignore':
        #         continue
        #     task_id = TaskId.parse(key, False, False, True)
        #     an: Answer = get_latest_answers_query(task_id, [u]).first()
        #     points = None
        #     content = json.dumps({content_field: None})
        #     if task_id.field == 'points':
        #         if value == "":
        #             value = None
        #         else:
        #             try:
        #                 value = float(value)
        #             except ValueError:
        #                 if not result['web'].get('error', None):
        #                     result['web']['error'] = 'Errors:\n'
        #                 result['web']['error'] += f"Value {value} is not valid point value for task {task_id.task_name}\n"
        #                 continue
        #         points = value
        #     else:
        #         content = json.dumps({content_field: value})
        #     if an:
        #         an_content = json.loads(an.content)
        #         if task_id.field == 'points':
        #             if an.points == points:
        #                 continue
        #         else:
        #             if an_content.get(content_field) == value:
        #                 continue
        #             an_content[content_field] = value
        #             points = an.points
        #         content = json.dumps(an_content)
        #     ans = Answer(
        #         content=content,
        #         points=points,
        #         task_id=task_id.doc_task,
        #         users=[u],
        #         valid=True,
        #         saver=curr_user,
        #     )
        #     db.session.add(ans)


def get_hidden_name(user_id):
    return 'Student %d' % user_id


def should_hide_name(d: DocInfo, user: User):
    return True
    # return not user.has_teacher_access(d) and user.id != get_current_user_id()


def maybe_hide_name(d: DocInfo, u: User):
    if should_hide_name(d, u):
        u.hide_name = True

@answers.route("/infosForTasks", methods=['POST'])
def get_task_infos():
    """
    Returns task infos for given list of tasks
    :return: {[task_id]: taskInfo}
    """
    tasks, = verify_json_params('tasks')
    doc_map = {}
    user = get_current_user_object()
    infolist = {}
    for task_id in tasks:
        try:
            tid = TaskId.parse(task_id)
            if tid.doc_id not in doc_map:
                dib = get_doc_or_abort(tid.doc_id, f'Document {tid.doc_id} not found')
                doc_map[tid.doc_id] = dib.document
            plugin = find_plugin_from_document(doc_map[tid.doc_id], tid, user)
            tim_vars = {'maxPoints': plugin.max_points(),
                        'userMin': plugin.user_min_points(),
                        'userMax': plugin.user_max_points(),
                        'deadline': plugin.deadline(),
                        'starttime': plugin.starttime(),
                        'answerLimit': plugin.answer_limit(),
                        'triesText': plugin.values.get('triesText', 'Tries left:'),
                        'pointsText': plugin.values.get('pointsText', 'Points:')
                        }
            infolist[task_id] = tim_vars
        except (TaskNotFoundException, PluginException) as e:
            return abort(400, str(e))
    return json_response(infolist)

@answers.route("/taskinfo/<task_id>")
def get_task_info(task_id):
    try:
        plugin = Plugin.from_task_id(task_id, user=get_current_user_object())
        tim_vars = {'maxPoints': plugin.max_points(),
                    'userMin': plugin.user_min_points(),
                    'userMax': plugin.user_max_points(),
                    'deadline': plugin.deadline(),
                    'starttime': plugin.starttime(),
                    'answerLimit': plugin.answer_limit(),
                    'triesText': plugin.values.get('triesText', 'Tries left:'),
                    'pointsText': plugin.values.get('pointsText', 'Points:')
                    }
    except PluginException as e:
        return abort(400, str(e))
    return json_response(tim_vars)


@answers.route("/answers/<task_id>/<user_id>")
def get_answers(task_id, user_id):
    try:
        user_id = int(user_id)
    except ValueError:
        abort(404, 'Not a valid user id')
    verify_logged_in()
    try:
        tid = TaskId.parse(task_id)
    except PluginException as e:
        return abort(400, str(e))
    d = get_doc_or_abort(tid.doc_id)
    user = User.get_by_id(user_id)
    if user_id != get_current_user_id():
        verify_seeanswers_access(d)
    if user is None:
        abort(400, 'Non-existent user')
    try:
        user_answers: List[Answer] = user.get_answers_for_task(tid.doc_task).all()
        if hide_names_in_teacher():
            for answer in user_answers:
                for u in answer.users_all:
                    maybe_hide_name(d, u)
        return json_response(user_answers)
    except Exception as e:
        return abort(400, str(e))


@answers.route("/allDocumentAnswersPlain/<path:doc_path>")
def get_document_answers(doc_path):
    d = DocEntry.find_by_path(doc_path, fallback_to_id=True)
    pars = d.document.get_dereferenced_paragraphs()
    task_ids, _, _ = find_task_ids(pars)
    return get_all_answers_list_plain(task_ids)


@answers.route("/allAnswersPlain/<task_id>")
def get_all_answers_plain(task_id):
    return get_all_answers_list_plain([TaskId.parse(task_id)])


def get_all_answers_list_plain(task_ids: List[TaskId]):
    all_answers = get_all_answers_as_list(task_ids)
    jointext = "\n"
    print_opt = get_option(request, 'print', 'all')
    print_answers = print_opt == "all" or print_opt == "answers"
    if print_answers:
        jointext = "\n\n----------------------------------------------------------------------------------\n"
    text = jointext.join(all_answers)
    return Response(text, mimetype='text/plain')


def get_all_answers_as_list(task_ids: List[TaskId]):
    verify_logged_in()
    if not task_ids:
        return []
    doc_ids = set()
    for tid in task_ids:
        doc_ids.add(tid.doc_id)
        d = get_doc_or_abort(tid.doc_id)
        # Require full teacher rights for getting all answers
        verify_teacher_access(d)

    usergroup = get_option(request, 'group', None)
    age = get_option(request, 'age', 'max')
    valid = get_option(request, 'valid', '1')
    name_opt = get_option(request, 'name', 'both')
    sort_opt = get_option(request, 'sort', 'task')
    print_opt = get_option(request, 'print', 'all')
    period_opt = get_option(request, 'period', 'whenever')
    consent = get_consent_opt()
    printname = name_opt == 'both'

    period_from, period_to = period_handling(task_ids, doc_ids, period_opt)

    if not usergroup:
        usergroup = None

    hide_names = name_opt == 'anonymous'
    hide_names = hide_names or hide_names_in_teacher()
    all_answers = get_all_answers(task_ids,
                                  usergroup,
                                  hide_names,
                                  age,
                                  valid,
                                  printname,
                                  sort_opt,
                                  print_opt,
                                  period_from,
                                  period_to,
                                  consent=consent)
    return all_answers


@answers.route("/allAnswers/<task_id>")
def get_all_answers_route(task_id):
    all_answers = get_all_answers_as_list(task_id)
    return json_response(all_answers)


class GetStateSchema(Schema):
    answer_id = fields.Int(required=True)
    par_id = fields.Str()
    doc_id = fields.Str()
    user_id = fields.Int(required=True)
    review = fields.Bool(missing=False)


    @post_load
    def make_obj(self, data):
        return GetStateModel(**data)


class GetMultiStatesSchema(Schema):
    answer_ids = fields.List(fields.Int())
    user_id = fields.Int()
    doc_id = fields.Int()

    # @pre_load()
    # def debug(self, data):
    #     print("debug me")

    @post_load
    def make_obj(self, data):
        return GetMultiStatesModel(**data)


@attr.s(auto_attribs=True)
class GetMultiStatesModel:
    answer_ids: List[int]
    user_id: int
    doc_id: int


@attr.s(auto_attribs=True)
class GetStateModel:
    answer_id: int
    user_id: int
    review: bool
    par_id: Union[str, _Missing] = missing
    doc_id: Union[str, _Missing] = missing


@answers.route("/getMultiStates")
@use_args(GetMultiStatesSchema())
def get_multi_states(args: GetMultiStatesModel):
    """
    WIP
    Queries plugin states for multiple answers
    :param args: {answer_ids: list of answers, user_id, doc_id}
    :return: {answerID: {'html': html, 'reviewHtml': None}}
    """
    answer_ids, user_id, doc_id = args.answer_ids, args.user_id, args.doc_id
    print(args)
    docentry = get_doc_or_abort(doc_id)
    verify_seeanswers_access(docentry)
    doc = Document(doc_id)
    user = User.query.get(user_id)
    if user is None:
        abort(400, 'Non-existent user')
    doc.insert_preamble_pars()
    answs = Answer.query.filter(Answer.id.in_(answer_ids)).all()
    response = {}
    # blocks = []
    for ans in answs:
        ###
        tid = TaskId.parse(ans.task_id)
        #if parid, maybe_set_hint?
        try:
            doc, plug = get_plugin_from_request(doc, task_id=tid, u=user)
            #plug = find_plugin_from_document(doc, tid, user)
        except PluginException as e:
            return abort(400, str(e))
        block = plug.par
        a, b, c, plug = pluginify(
            doc,
            [block],
            user,
            custom_answer=ans,
            pluginwrap=PluginWrap.Nothing,
            do_lazy=NEVERLAZY,
            debugging_multistate=True,
        )
        html = plug.get_final_output()
        response[ans.id] = {'html': html, 'reviewHtml': None}
        ####
    #     tid = TaskId.parse(ans.task_id)
    #     # if parid, maybe_set_hint?
    #     try:
    #         doc, plug = get_plugin_from_request(doc, task_id=tid, u=user)
    #         #plug = find_plugin_from_document(doc, tid, user)
    #     except PluginException as e:
    #         return abort(400, str(e))
    #     block = plug.par
    #     blocks.append(block)
    # a, b, c, plug = pluginify(
    #     doc,
    #     blocks,
    #     user,
    #     # custom_answer=ans,
    #     pluginwrap=PluginWrap.Nothing,
    #     do_lazy=NEVERLAZY,
    # )
    # print("hmm")
    return json_response(response)

@answers.route("/getState")
@use_args(GetStateSchema())
def get_state(args: GetStateModel):
    par_id, user_id, answer_id, review = args.par_id, args.user_id, args.answer_id, args.review

    try:
        answer, doc_id = verify_answer_access(answer_id, user_id)
        if args.doc_id:
            doc_id = args.doc_id
    except PluginException as e:
        return abort(400, str(e))
    doc = Document(doc_id)
    # if doc_id != d_id and doc_id not in doc.get_referenced_document_ids():
    #     abort(400, 'Bad document id')

    tid = TaskId.parse(answer.task_id)
    if par_id:
        tid.maybe_set_hint(par_id)
    user = User.query.get(user_id)
    if user is None:
        abort(400, 'Non-existent user')
    doc.insert_preamble_pars()
    try:
        doc, plug = get_plugin_from_request(doc, task_id=tid, u=user)
    except PluginException as e:
        return abort(400, str(e))
    block = plug.par

    _, _, _, plug = pluginify(
        doc,
        [block],
        user,
        custom_answer=answer,
        pluginwrap=PluginWrap.Nothing,
        do_lazy=NEVERLAZY,
    )
    html = plug.get_final_output()
    if review:
        block.final_dict = None
        _, _, _, rplug = pluginify(
            doc,
            [block],
            user,
            custom_answer=answer,
            review=review,
            pluginwrap=PluginWrap.Nothing,
            do_lazy=NEVERLAZY,
        )
        rhtml = rplug.get_final_output()
        return json_response({'html': html, 'reviewHtml': rhtml})
    else:
        return json_response({'html': html, 'reviewHtml': None})


def verify_answer_access(
        answer_id: int,
        user_id: int,
        require_teacher_if_not_own=False,
        required_task_access_level: TaskIdAccess = TaskIdAccess.ReadOnly,
) -> Tuple[Answer, int]:
    answer: Answer = Answer.query.get(answer_id)
    if answer is None:
        abort(400, 'Non-existent answer')
    tid = TaskId.parse(answer.task_id)
    d = get_doc_or_abort(tid.doc_id)
    d.document.insert_preamble_pars()

    if verify_teacher_access(d, require=False):  # TODO: tarkista onko oikein tämä!!! Muuten tuli virhe toisten vastauksia hakiessa.
        return answer, tid.doc_id

    if user_id != get_current_user_id() or not logged_in():
        if require_teacher_if_not_own:
            verify_task_access(d, tid, AccessType.teacher, required_task_access_level)
        else:
            verify_task_access(d, tid, AccessType.see_answers, required_task_access_level)
    else:
        verify_task_access(d, tid, AccessType.view, required_task_access_level)
        if not any(a.id == user_id for a in answer.users_all):
            abort(403, "You don't have access to this answer.")
    return answer, tid.doc_id


@answers.route("/getTaskUsers/<task_id>")
def get_task_users(task_id):
    tid = TaskId.parse(task_id)
    d = get_doc_or_abort(tid.doc_id)
    verify_seeanswers_access(d)
    usergroup = request.args.get('group')
    q = User.query.join(Answer, User.answers).filter_by(task_id=task_id).join(UserGroup, User.groups).order_by(
        User.real_name.asc())
    if usergroup is not None:
        q = q.filter(UserGroup.name.in_([usergroup]))
    users = q.all()
    if hide_names_in_teacher():
        for user in users:
            maybe_hide_name(d, user)
    return json_response(users)


@answers.route('/renameAnswers/<old_name>/<new_name>/<path:doc_path>')
def rename_answers(old_name: str, new_name: str, doc_path: str):
    d = DocEntry.find_by_path(doc_path, fallback_to_id=True)
    if not d:
        abort(404)
    verify_manage_access(d)
    force = get_option(request, 'force', False)
    for n in (old_name, new_name):
        if not re.fullmatch('[a-zA-Z0-9_-]+', n):
            abort(400, f'Invalid task name: {n}')
    conflicts = Answer.query.filter_by(task_id=f'{d.id}.{new_name}').count()
    if conflicts > 0 and not force:
        abort(400, f'The new name conflicts with {conflicts} other answers with the same task name.')
    answers_to_rename = Answer.query.filter_by(task_id=f'{d.id}.{old_name}').all()
    for a in answers_to_rename:
        a.task_id = f'{d.id}.{new_name}'
    db.session.commit()
    return json_response({'modified': len(answers_to_rename), 'conflicts': conflicts})


def period_handling(task_ids, doc_ids, period):
    """
    Returns start and end of an period for answer results.
    :param task_ids: Task ids containing the answers.
    :param doc_ids: Documents containing the answers.
    :param period: Period options: whenever, sincelast, day, week, month, other.
    :return: Return "from"-period and "to"-period.
    """
    period_from = datetime.min.replace(tzinfo=timezone.utc)
    period_to = get_current_time()

    since_last_key = task_ids[0].doc_task if task_ids else None
    if len(task_ids) > 1:
        since_last_key = str(next(d for d in doc_ids))
        if len(doc_ids) > 1:
            since_last_key = None

        # Period from which to take results.
    if period == 'whenever':
        pass
    elif period == 'sincelast':
        u = get_current_user_object()
        prefs = u.get_prefs()
        last_answer_fetch = prefs.last_answer_fetch
        period_from = last_answer_fetch.get(since_last_key, datetime.min.replace(tzinfo=timezone.utc))
        last_answer_fetch[since_last_key] = get_current_time()
        prefs.last_answer_fetch = last_answer_fetch
        u.set_prefs(prefs)
        db.session.commit()
    elif period == 'day':
        period_from = period_to - timedelta(days=1)
    elif period == 'week':
        period_from = period_to - timedelta(weeks=1)
    elif period == 'month':
        period_from = period_to - dateutil.relativedelta.relativedelta(months=1)
    elif period == 'other':
        period_from_str = get_option(request, 'periodFrom', period_from.isoformat())
        period_to_str = get_option(request, 'periodTo', period_to.isoformat())
        try:
            period_from = dateutil.parser.parse(period_from_str)
        except (ValueError, OverflowError):
            pass
        try:
            period_to = dateutil.parser.parse(period_to_str)
        except (ValueError, OverflowError):
            pass

    return period_from, period_to
