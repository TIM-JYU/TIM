"""Answer-related routes."""
import json
import re
from collections import defaultdict

import requests
from dataclasses import dataclass, field
from datetime import datetime
from requests.exceptions import MissingSchema, InvalidURL
from typing import Union, List, Tuple, Dict, Optional, Any, Callable, TypedDict, DefaultDict

from flask import Blueprint, session, current_app
from flask import Response
from flask import abort
from flask import request
from marshmallow import validates_schema, ValidationError
from marshmallow.utils import missing
from sqlalchemy import func
from sqlalchemy.orm import lazyload
from webargs.flaskparser import use_args

from markupmodels import GenericMarkupModel
from timApp.answer.answer import Answer
from timApp.answer.answer_models import AnswerUpload
from timApp.answer.answers import get_existing_answers_info, save_answer, get_all_answers, \
    valid_answers_query, valid_taskid_filter
from timApp.auth.accesshelper import verify_logged_in, get_doc_or_abort, verify_manage_access, AccessDenied, \
    verify_admin
from timApp.auth.accesshelper import verify_task_access, verify_teacher_access, verify_seeanswers_access, \
    has_teacher_access, \
    verify_view_access, get_plugin_from_request
from timApp.auth.accesstype import AccessType
from timApp.auth.login import create_or_update_user
from timApp.auth.sessioninfo import get_current_user_id, logged_in, user_context_with_logged_in
from timApp.auth.sessioninfo import get_current_user_object, get_session_users, get_current_user_group
from timApp.document.caching import clear_doc_cache
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.document import Document
from timApp.document.hide_names import hide_names_in_teacher
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewRoute, ViewContext, default_view_ctx
from timApp.item.block import Block, BlockType
from timApp.markdown.dumboclient import call_dumbo
from timApp.modules.py.marshmallow_dataclass import class_schema
from timApp.notification.notify import multi_send_email
from timApp.plugin.containerLink import call_plugin_answer
from timApp.plugin.importdata.importData import MissingUser
from timApp.plugin.jsrunner import jsrunner_run, JsRunnerParams, JsRunnerError
from timApp.plugin.plugin import Plugin, PluginWrap, NEVERLAZY, TaskNotFoundException, find_task_ids, \
    CachedPluginFinder
from timApp.plugin.plugin import find_plugin_from_document
from timApp.plugin.pluginControl import pluginify
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.plugintype import PluginType
from timApp.plugin.taskid import TaskId, TaskIdAccess
from timApp.tim_app import get_home_organization_group
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.user.groups import do_create_group, verify_group_edit_access
from timApp.user.user import User, UserInfo
from timApp.user.user import maxdate
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember
from timApp.util.answerutil import period_handling
from timApp.util.flask.requesthelper import verify_json_params, get_option, get_consent_opt, RouteException, use_model, \
    get_urlmacros_from_request
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.get_fields import get_fields_and_users, MembershipFilter, UserFields, RequestedGroups, \
    ALL_ANSWERED_WILDCARD, GetFieldsAccess
from timApp.util.logger import log_info
from timApp.util.utils import get_current_time, approximate_real_name
from timApp.util.utils import local_timezone
from timApp.util.utils import try_load_json, seq_to_str, is_valid_email
from utils import Missing

answers = Blueprint('answers',
                    __name__,
                    url_prefix='')


@answers.route("/savePoints/<int:user_id>/<int:answer_id>", methods=['PUT'])
def save_points(answer_id, user_id):
    answer, _ = verify_answer_access(
        answer_id,
        user_id,
        default_view_ctx,
        require_teacher_if_not_own=True,
    )
    tid = TaskId.parse(answer.task_id)
    d = get_doc_or_abort(tid.doc_id)
    points, = verify_json_params('points')
    try:
        plugin, _ = Plugin.from_task_id(
            answer.task_id,
            user_ctx=user_context_with_logged_in(None),
            view_ctx=default_view_ctx,
        )
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


@dataclass
class AnswerIdModel:
    answer_id: int


@dataclass
class ValidityModel(AnswerIdModel):
    valid: bool


@dataclass
class DeleteCollabModel(AnswerIdModel):
    user_id: int


@answers.route("/answer/saveValidity", methods=['PUT'])
@use_model(ValidityModel)
def save_validity(m: ValidityModel):
    a, doc_id = verify_answer_access(
        m.answer_id,
        get_current_user_object().id,
        default_view_ctx,
        require_teacher_if_not_own=True,
    )
    verify_teacher_access(get_doc_or_abort(doc_id))
    a.valid = m.valid
    db.session.commit()
    return ok_response()


@answers.route("/answer/delete", methods=['POST'])
@use_model(AnswerIdModel)
def delete_answer(m: AnswerIdModel):
    """Deletes an answer.

    This does not completely delete the answer but only removes user associations from it,
    so it is no longer visible in TIM.
    """
    a, doc_id = verify_answer_access(
        m.answer_id,
        get_current_user_object().id,
        default_view_ctx,
        require_teacher_if_not_own=True,
    )
    verify_teacher_access(get_doc_or_abort(doc_id))
    verify_admin()
    unames = [u.name for u in a.users_all]
    a.users_all = []
    db.session.commit()
    u = get_current_user_object()
    log_info(f'{u.name} deleted answer {a.id} (of {seq_to_str(unames)}) in task {a.task_id}')
    return ok_response()


@answers.route("/answer/deleteCollaborator", methods=['POST'])
@use_model(DeleteCollabModel)
def delete_answer_collab(m: DeleteCollabModel):
    """Deletes an answer collaborator.
    """
    a, doc_id = verify_answer_access(
        m.answer_id,
        get_current_user_object().id,
        default_view_ctx,
        require_teacher_if_not_own=True,
    )
    verify_teacher_access(get_doc_or_abort(doc_id))
    verify_admin()
    collab_to_remove = User.get_by_id(m.user_id)
    if not collab_to_remove:
        raise RouteException(f'Answer {m.answer_id} does not have collaborator {m.user_id}')
    a.users_all.remove(collab_to_remove)
    db.session.commit()
    u = get_current_user_object()
    log_info(f'{u.name} deleted collaborator {collab_to_remove.name} from answer {a.id} in task {a.task_id}')
    return ok_response()


def points_to_float(points: Union[str, float]):
    if isinstance(points, float):
        return points
    if points == '':
        return None
    if points is None:
        return None
    return float(points)


@answers.route("/iframehtml/<plugintype>/<task_id_ext>/<int:user_id>")
@answers.route("/iframehtml/<plugintype>/<task_id_ext>/<int:user_id>/<int:answer_id>")
def get_iframehtml(plugintype: str, task_id_ext: str, user_id: int, answer_id: Optional[int] = None):
    """
    Gets the HTML to be used in iframe.

    :param plugintype: plugin type
    :param task_id_ext: task id
    :param user_id: the user whose information to get
    :param answer_id: answer id from answer browser
    :return: HTML to be used in iframe
    """
    try:
        tid = TaskId.parse(task_id_ext)
    except PluginException as e:
        return abort(400, f'Task id error: {e}')
    d = get_doc_or_abort(tid.doc_id)
    d.document.insert_preamble_pars()

    ctx_user = User.get_by_id(user_id)
    if not ctx_user:
        raise RouteException('User not found')
    vr = verify_task_access(
        d,
        tid,
        AccessType.view,
        TaskIdAccess.ReadWrite,
        context_user=user_context_with_logged_in(ctx_user),
        view_ctx=default_view_ctx,
    )
    plugin = vr.plugin
    answer = None
    if answer_id is not None:
        answer, doc_id = verify_answer_access(
            answer_id,
            ctx_user.id,
            default_view_ctx,
            require_teacher_if_not_own=True,
        )

    if plugin.type != plugintype:
        abort(400, f'Plugin type mismatch: {plugin.type} != {plugintype}')

    users = [ctx_user]

    answerinfo = get_existing_answers_info(users, tid)

    info = plugin.get_info(users, answerinfo.count)

    state = try_load_json(answer.content) if answer else None

    answer_call_data = {'markup': plugin.values,
                        'state': state,
                        'taskID': tid.doc_task,
                        'info': info,
                        'iframehtml': True}

    vals = get_plug_vals(d, tid, user_context_with_logged_in(users[0]), default_view_ctx)
    if vals:
        answer_call_data['markup']['fielddata'] = vals.to_json()

    jsonresp = call_plugin_answer_and_parse(answer_call_data, plugintype)

    if 'iframehtml' not in jsonresp:
        return json_response({'error': 'The key "iframehtml" is missing in plugin response.'}, 400)
    result = jsonresp['iframehtml']
    return result


def call_plugin_answer_and_parse(answer_call_data, plugintype):
    plugin_response = call_plugin_answer(plugintype, answer_call_data)
    try:
        jsonresp = json.loads(plugin_response)
    except ValueError as e:
        raise PluginException(
            'The plugin response was not a valid JSON string. The response was: ' + plugin_response) from e
    return jsonresp


def get_useranswers_for_task(user: User, task_ids: List[TaskId], answer_map):
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
           .filter(valid_taskid_filter(task_ids))
           .add_columns(col)
           .with_entities(col)
           .group_by(Answer.task_id).subquery())
    answs: List[Answer] = Answer.query.join(sub, Answer.id == sub.c.col).all()
    for answer in answs:
        if len(answer.users_all) > 1:
            answer_map[answer.task_id] = answer
        else:
            asd = answer.to_json()
            asd.pop('users')
            answer_map[answer.task_id] = asd
    return answs


def get_globals_for_tasks(task_ids: List[TaskId], answer_map):
    col = func.max(Answer.id).label('col')
    cnt = func.count(Answer.id).label('cnt')
    sub = (valid_answers_query(task_ids)
           .add_columns(col, cnt)
           .with_entities(col, cnt)
           .group_by(Answer.task_id).subquery()
           )
    answers: List[Tuple[Answer, int]] = (
        Answer.query.join(sub, Answer.id == sub.c.col)
            .with_entities(Answer, sub.c.cnt)
            .all()
    )
    for answer in answers:
        asd = answer.Answer.to_json()
        answer_map[answer.Answer.task_id] = asd
    return cnt, answers


@dataclass
class UserAnswersForTasksModel:
    tasks: List[str]
    user: int


UserAnswersForTasksSchema = class_schema(UserAnswersForTasksModel)


@answers.route("/userAnswersForTasks", methods=['POST'])
@use_args(UserAnswersForTasksSchema())
def get_answers_for_tasks(args: UserAnswersForTasksModel):
    """
    Route for getting latest valid answers for given user and list of tasks
    :return: {"answers": {taskID: Answer}, "userId": user_id}
    """
    tasks, user_id = args.tasks, args.user
    user = User.get_by_id(user_id)
    if user is None:
        abort(400, 'Non-existent user')
    verify_logged_in()
    try:
        doc_map = {}
        tids = []
        gtids = []
        for task_id in tasks:
            tid = TaskId.parse(task_id)
            if tid.doc_id not in doc_map:
                dib = get_doc_or_abort(tid.doc_id, f'Document {tid.doc_id} not found')
                verify_seeanswers_access(dib)
                doc_map[tid.doc_id] = dib.document
            if tid.is_global:
                gtids.append(tid)
            else:
                tids.append(tid)
        answer_map = {}
        if tids:
            get_useranswers_for_task(user, tids, answer_map)
        if gtids:
            get_globals_for_tasks(gtids, answer_map)
        return json_response({"answers": answer_map, "userId": user_id})
    except Exception as e:
        return abort(400, str(e))


@dataclass
class JsRunnerMarkupModel(GenericMarkupModel):
    fields: Union[
        List[str], Missing] = missing  # This is actually required, but we cannot use non-default arguments here...
    autoadd: Union[bool, Missing] = missing
    autoUpdateTables: Union[bool, Missing] = True
    creditField: Union[str, Missing] = missing
    defaultPoints: Union[float, Missing] = missing
    failGrade: Union[str, Missing] = missing
    fieldhelper: Union[bool, Missing] = missing
    gradeField: Union[str, Missing] = missing
    gradingScale: Union[Dict[Any, Any], Missing] = missing
    group: Union[str, Missing] = missing
    groups: Union[List[str], Missing] = missing
    includeUsers: Union[MembershipFilter, Missing] = field(default=MembershipFilter.Current,
                                                           metadata={'by_value': True})
    selectIncludeUsers: bool = False
    paramFields: Union[List[str], Missing] = missing
    postprogram: Union[str, Missing] = missing
    preprogram: Union[str, Missing] = missing
    program: Union[str, Missing] = missing
    overrideGrade: bool = False
    showInView: bool = False
    confirmText: Union[str, Missing] = missing
    timeout: Union[int, Missing] = missing
    updateFields: Union[List[str], Missing] = missing
    nextRunner: Union[str, Missing] = missing
    timeZoneDiff: Union[int, Missing] = missing

    @validates_schema(skip_on_field_errors=True)
    def validate_schema(self, data, **_):
        if data.get('fields') is None:
            raise ValidationError('Missing data for required field.', field_name='fields')
        if data.get('group') is None and data.get('groups') is None:
            raise ValidationError("Either group or groups must be given.")


JsRunnerMarkupSchema = class_schema(JsRunnerMarkupModel)


@dataclass
class JsRunnerInputModel:
    nosave: Union[bool, Missing] = missing
    userNames: Union[List[str], Missing] = missing
    paramComps: Union[Dict[str, str], Missing] = missing
    includeUsers: MembershipFilter = field(default=MembershipFilter.Current, metadata={'by_value': True})


@dataclass
class RefFrom:
    docId: int
    par: str


AnswerData = Dict[str, Any]


@dataclass
class JsRunnerAnswerModel:
    input: JsRunnerInputModel
    ref_from: Optional[RefFrom] = None
    abData: Union[AnswerData, Missing] = missing


JsRunnerAnswerSchema = class_schema(JsRunnerAnswerModel)


@dataclass
class SendEmailModel:
    rcpts: str
    msg: str
    subject: str
    bccme: Union[bool, Missing, None] = missing


SendEmailSchema = class_schema(SendEmailModel)


@answers.route('/sendemail/', methods=['post'])
@use_args(SendEmailSchema())
def send_email(args: SendEmailModel):
    """
    Route for sending email
    TODO: combine with multisendemail
    :return:
    """
    rcpts, msg, subject, bccme = args.rcpts, args.msg, args.subject, args.bccme
    curr_user = get_current_user_object()
    if curr_user not in UserGroup.get_teachers_group().users and curr_user not in get_home_organization_group().users:
        abort(403, "Sorry, you don't have permission to use this resource.")
    curr_user = get_current_user_object()
    if bccme:
        bcc = curr_user.email
    multi_send_email(
        rcpt=rcpts,
        subject=subject,
        msg=msg,
        mail_from=curr_user.email,
        reply_to=curr_user.email,
        bcc=bcc
    )
    return ok_response()


@answers.route("/multiSendEmail/<task_id_ext>", methods=['POST'])
def multisendemail(task_id_ext: str):
    tid = TaskId.parse(task_id_ext)
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


@answers.route("/<plugintype>/<task_id_ext>/answer/", methods=['put'])
@answers.route("/<plugintype>/<task_id_ext>/answer", methods=['put'])
def post_answer(plugintype: str, task_id_ext: str):
    """Saves the answer submitted by user for a plugin in the database.

    :param plugintype: The type of the plugin, e.g. csPlugin.
    :param task_id_ext: The extended task id of the form "22.palidrome.par_id".
    :return: JSON

    """

    receive_time = get_current_time()
    tid = TaskId.parse(task_id_ext)
    d = get_doc_or_abort(tid.doc_id)
    d.document.insert_preamble_pars()

    curr_user = get_current_user_object()

    # It is rare but possible that the current user has been deleted (for example as the result of merging 2 accounts).
    # We assume it's the case here, so we clear the session and ask to log in again.
    if curr_user.is_deleted:
        session.clear()
        raise AccessDenied('Please refresh the page and log in again.')

    ptype = PluginType(plugintype)
    answerdata, = verify_json_params('input')
    answer_browser_data, answer_options = verify_json_params('abData', 'options', require=False, default={})
    force_answer = answer_options.get('forceSave', False)  # Only used in feedback plugin.
    is_teacher = answer_browser_data.get('teacher', False)
    save_teacher = answer_browser_data.get('saveTeacher', False)
    should_save_answer = answer_browser_data.get('saveAnswer', True) and tid.task_name

    if tid.is_points_ref:
        return handle_points_ref(answerdata, curr_user, d, ptype, tid)

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
                raise PluginException(f'Answer not found: {answer_id}')
            expected_task_id = answer.task_id
            if expected_task_id != tid.doc_task:
                raise PluginException('Task ids did not match')

            # Later on, we may call users.append, but we don't want to modify the users of the existing
            # answer. Therefore, we make a copy of the user list so that SQLAlchemy no longer associates
            # the user list with the answer.
            users = list(answer.users_all)
            if not users:
                raise PluginException('No users found for the specified answer')
            # For now global fields use current user in browser
            # We set answerer user to be current user later so we ignore user mismatch in global case
            if user_id not in (u.id for u in users) and not tid.is_global:
                raise PluginException('userId is not associated with answer_id')
        elif user_id and user_id != curr_user.id and False:  # TODO: Vesa's hack to no need for belong teachers group
            teacher_group = UserGroup.get_teachers_group()
            if curr_user not in teacher_group.users:
                raise PluginException('Permission denied: you are not in teachers group.')
        if user_id:
            ctx_user = User.query.get(user_id)
            if not ctx_user:
                raise PluginException(f'User {user_id} not found')
            users = [ctx_user]  # TODO: Vesa's hack to save answer to student
    try:
        vr = verify_task_access(
            d,
            tid,
            AccessType.view,
            TaskIdAccess.ReadWrite,
            context_user=user_context_with_logged_in(ctx_user),
            view_ctx=ViewContext(ViewRoute.View, False, urlmacros=get_urlmacros_from_request()),
            allow_grace_period=True,
        )
        plugin = vr.plugin
    except (PluginException, TimDbException) as e:
        raise PluginException(str(e))

    if plugin.type != plugintype:
        raise PluginException(f'Plugin type mismatch: {plugin.type} != {plugintype}')

    uploads = []

    if not logged_in() and not plugin.known.anonymous:
        raise RouteException('You must be logged in to answer this task.')
    if plugin.known.useCurrentUser or plugin.task_id.is_global:  # For plugins that is saved only for current user
        users = [curr_user]

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
                raise PluginException(f'Non-existent upload: {trimmed_file}')
            verify_view_access(block, message="You don't have permission to touch this file.")
            uploads = [AnswerUpload.query.filter(AnswerUpload.upload_block_id == block.id).first()]
            # if upload.answer_id is not None:
            #    raise PluginException(f'File was already uploaded: {file}')

        files: List[int] = answerdata.get('uploadedFiles', None)
        if files is not None:
            for file in files:
                trimmed_file = file["path"].replace('/uploads/', '')
                block = Block.query.filter((Block.description == trimmed_file) &
                                           (Block.type_id == BlockType.Upload.value)).first()
                if block is None:
                    raise PluginException(f'Non-existent upload: {trimmed_file}')
                verify_view_access(block, message="You don't have permission to touch this file.")
                uploads.append(AnswerUpload.query.filter(AnswerUpload.upload_block_id == block.id).first())

    # Load old answers

    if users is None:
        users = [User.query.get(u['id']) for u in get_session_users()]

    answerinfo = get_existing_answers_info(users, tid)
    valid, _ = plugin.is_answer_valid(answerinfo.count, {})
    info = plugin.get_info(users, answerinfo.count, look_answer=is_teacher and not save_teacher, valid=valid)

    # Get the newest answer (state). Only for logged in users.
    state = try_load_json(answerinfo.latest_answer.content) if logged_in() and answerinfo.latest_answer else None

    preprocessor = answer_call_preprocessors.get(plugin.type)
    if preprocessor:
        preprocessor(answerdata, curr_user, d, plugin)

    answer_call_data = {'markup': plugin.values,
                        'state': state,
                        'input': answerdata,
                        'taskID': tid.doc_task,
                        'info': info}

    jsonresp = call_plugin_answer_and_parse(answer_call_data, plugintype)

    web = jsonresp.get('web')
    if web is None:
        raise PluginException(f'Got malformed response from plugin: {jsonresp}')
    result = {'web': web}

    if 'savedata' in jsonresp:
        siw = answer_call_data.get("markup", {}).get("showInView", False)
        add_group = None
        if plugin.type == 'importData':
            add_group = plugin.values.get('addUsersToGroup')
        saveresult = save_fields(
            jsonresp,
            d,
            allow_non_teacher=siw,
            add_users_to_group=add_group,
        )

        # TODO: Could report the result to other plugins too.
        if plugin.type == 'importData':
            web['fieldresult'] = saveresult

    def add_reply(obj, key, run_markdown=False):
        if key not in plugin.values:
            return
        text_to_add = plugin.values[key]
        if run_markdown:
            dumbo_result = call_dumbo([text_to_add])
            text_to_add = dumbo_result[0]
        obj[key] = text_to_add

    noupdate = False  #  if true do not send new id

    if not get_task:
        add_reply(result['web'], '-replyImage')
        add_reply(result['web'], '-replyMD', True)
        add_reply(result['web'], '-replyHTML')
    if 'save' in jsonresp and not get_task:
        # TODO: RND_SEED: save used rnd_seed for this answer if answer is saved, found from par.get_rnd_seed()
        save_object = jsonresp['save']
        tags = []
        tim_info = jsonresp.get('tim_info', {})
        if tim_info.get("noupdate", False):
            noupdate = True
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

        def get_name_and_val(name1, name2=""):
            """
            Try with name1, -name1 amnd name2
            return working name and value or "", None
            """
            name = name1
            val = plugin.values.get(name, None)
            if val:
                return name, val

            name = "-" + name1
            val = plugin.values.get(name, None)
            if val:
                return name, val

            if name2:
                name = name2
                val = plugin.values.get(name, None)  # old name
            if val:
                return name, val

            name = ""
            return name, val

        postprogram_name, postprogram = \
            get_name_and_val("postprogram", "postProgram")

        postlibraries_name, postlibraries = get_name_and_val("postlibraries")

        postoutput = plugin.values.get("postoutput", 'feedback')

        def get_from_url(url):
            try:
                r = requests.get(url)
            except Exception as ex:
                raise RouteException(str(ex) + " " + url)
            return r.content.decode("utf-8")

        if postprogram and postlibraries:
            libs = ""
            for lib in postlibraries:
                try:
                    content = get_from_url(lib)
                    if content.startswith('{"error"'):
                        web["error"] += lib + "\n" + content
                        postprogram = ""
                        break
                    libs += content
                except Exception as ex:
                    web["error"] += lib + "\n" + str(ex)
                    postprogram = ""
            if postprogram:
                postprogram = libs + \
                              "\n//=== END LIBRARIES ===\n" + \
                              postprogram

        def set_postoutput(result, output, postoutput):
            if not postoutput or not output:
                return
            parts = postoutput.split(".")
            r = result
            lastkey = parts[-1]
            for p in parts[:-1]:
                if not p in r:
                    r[p] = {}
                r = r[p]
            r[lastkey] = r.get(lastkey, '') + str(output)

        def add_value(result, key, data):
            value = data.get(key, None)
            if value is None:
                return
            if value.startswith('md:'):
                value = call_dumbo([value[3:]])[0]
            result[key] = result.get(key, "") + value

        def postprogram_result(data, output):
            result["web"] = data.get("web", web)
            add_value(result, "error", data)
            add_value(result, "feedback", data)
            add_value(result, "topfeedback", data)
            if output.startswith('md:'):
                output = call_dumbo([output[3:]])[0]
            set_postoutput(result, output, postoutput)

        if (not is_teacher and should_save_answer) or ('savedata' in jsonresp):
            is_valid, explanation = plugin.is_answer_valid(answerinfo.count, tim_info)
            if vr.is_expired:
                fixed_time = receive_time - d.document.get_settings().answer_submit_time_tolerance()
                if fixed_time < (vr.access.accessible_to or maxdate):
                    is_valid = True
                else:
                    is_valid = False
                    explanation = 'Your view access to this document has expired, so this answer was saved but marked as invalid.'
            points_given_by = None
            if answer_browser_data.get('giveCustomPoints'):
                try:
                    points = plugin.validate_points(answer_browser_data.get('points'))
                except PluginException as e:
                    result['error'] = str(e)
                else:
                    points_given_by = get_current_user_group()

            if postprogram:
                data = {'points': points,
                        'save_object': save_object,
                        'tags': tags,
                        'is_valid': is_valid,
                        'force_answer': force_answer,
                        'error': '',
                        'web': web,
                        }
                try:
                    params = JsRunnerParams(code=postprogram, data=data)
                    data, output = jsrunner_run(params)
                    points = data.get("points", points)
                    save_object = data.get("save_object", save_object)
                    is_valid = data.get("is_valid", is_valid)
                    force_answer = data.get("force_answer", force_answer)
                    postprogram_result(data, output)
                except JsRunnerError as e:
                    return json_response({'web': {'error': 'Error in JavaScript: ' + e.args[0]}})

            if points or save_object is not None or tags:
                result['savedNew'] = save_answer(
                    users,
                    tid,
                    save_object,
                    points,
                    tags,
                    is_valid,
                    points_given_by,
                    force_answer,
                    plugintype=ptype,
                    max_content_len=current_app.config['MAX_ANSWER_CONTENT_SIZE'],
                )
            else:
                result['savedNew'] = None
            if noupdate:
                result['savedNew'] = None

            if not is_valid:
                result['error'] = explanation
        elif save_teacher:
            points = answer_browser_data.get('points', points)
            points = points_to_float(points)
            result['savedNew'] = save_answer(
                users,
                tid,
                save_object,
                points,
                tags,
                valid=True,
                points_given_by=get_current_user_group(),
                saver=curr_user,
                plugintype=ptype,
                max_content_len=current_app.config['MAX_ANSWER_CONTENT_SIZE'],
            )
        else:
            result['savedNew'] = None
            if postprogram:
                data = {'points': points,
                        'save_object': save_object,
                        'tags': tags,
                        'is_valid': True,
                        'force_answer': force_answer,
                        'error': '',
                        'web': web,
                        }
                try:
                    params = JsRunnerParams(code=postprogram, data=data)
                    data, output = jsrunner_run(params)
                    points = data.get("points", points)
                    output += "\nPoints: " + str(points)
                    postprogram_result(data, output)
                except JsRunnerError as e:
                    return json_response({'web': {'error': 'Error in JavaScript: ' + e.args[0]}})
        if result['savedNew'] is not None and uploads:
            # Associate this answer with the upload entries
            for upload in uploads:
                upload.answer_id = result['savedNew']

    db.session.commit()

    for u in users:
        clear_doc_cache(d, u)

    try:
        if postprogram_name:
            result['web']['markup'].pop(postprogram_name)  # TODO: stdy why someone puts markup here
    except:
        pass

    return json_response(result)


def preprocess_jsrunner_answer(answerdata: AnswerData, curr_user: User, d: DocInfo, plugin: Plugin):
    """Executed before the actual jsrunner answer route is called.
    This is required to fetch the requested data from the database."""

    s = JsRunnerMarkupSchema()
    # noinspection PyTypeChecker
    runnermarkup: JsRunnerMarkupModel = s.load(plugin.values)
    # noinspection PyTypeChecker
    runner_req: JsRunnerAnswerModel = JsRunnerAnswerSchema().load(request.get_json())
    groupnames = runnermarkup.groups
    if groupnames is missing:
        groupnames = [runnermarkup.group]
    requested_groups = RequestedGroups.from_name_list(groupnames)
    not_found_groups = sorted(list(set(groupnames) - set(g.name for g in requested_groups.groups)
                                   - {ALL_ANSWERED_WILDCARD}))  # Ensure the wildcard is removed
    if not_found_groups:
        raise PluginException(f'The following groups were not found: {", ".join(not_found_groups)}')
    if runner_req.input.paramComps:  # TODO: add paramComps to the interface, so no need to manipulate source code
        preprg = runnermarkup.preprogram or ''
        plugin.values["preprogram"] = f"gtools.params = {json.dumps(runner_req.input.paramComps)};\n{preprg}"
    siw = runnermarkup.showInView
    if not runnermarkup.selectIncludeUsers and runnermarkup.includeUsers != runner_req.input.includeUsers:
        raise AccessDenied('Not allowed to select includeUsers option.')

    ensure_grade_and_credit(runnermarkup.program, runnermarkup.fields)

    answerdata['data'], answerdata['aliases'], _, _ = get_fields_and_users(
        runnermarkup.fields,
        requested_groups,
        d,
        get_current_user_object(),
        default_view_ctx,
        access_option=GetFieldsAccess.from_bool(siw),
        member_filter_type=runner_req.input.includeUsers,
        user_filter=User.name.in_(runner_req.input.userNames) if runner_req.input.userNames else None
    )
    answerdata.pop('paramComps', None)  # This isn't needed by jsrunner server, so don't send it.
    # plugin.values['timeZoneDiff'] = 3
    tzd = plugin.values.get('timeZoneDiff', None)
    if tzd is None:
        localtz = local_timezone
        localoffset = localtz.utcoffset(datetime.now())
        tzd = localoffset.total_seconds() / 3600
        plugin.values['timeZoneDiff'] = tzd
    if runnermarkup.program is missing:
        raise PluginException("Attribute 'program' is required.")


def ensure_grade_and_credit(prg, flds):
    if not prg:
        return
    if prg.find('grade') >= 0 or prg.find('Grade'):  # add grade to fields if missing
        grade_found = False
        credit_found = False
        for fld in flds:
            if fld.startswith('grade'):
                grade_found = True
            if fld.startswith('credit'):
                credit_found = True
            if grade_found and credit_found:
                break
        if not grade_found:
            flds.append('grade')
        if not credit_found:
            flds.append('credit')


answer_call_preprocessors: Dict[str, Callable[[AnswerData, User, DocInfo, Plugin], None]] = {
    'jsrunner': preprocess_jsrunner_answer,
}


def handle_points_ref(answerdata: AnswerData, curr_user: User, d: DocInfo, ptype: PluginType, tid: TaskId):
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


class JsrunnerGroups(TypedDict, total=False):
    set: Dict[str, List[int]]
    add: Dict[str, List[int]]
    remove: Dict[str, List[int]]


MAX_GROUPS_PER_CALL = 10


def handle_jsrunner_groups(groupdata: Optional[JsrunnerGroups]):
    if not groupdata:
        return
    curr_user = get_current_user_object()
    groups_created = 0
    for op, group_set in groupdata.items():
        for name, uids in group_set.items():
            ug = UserGroup.get_by_name(name)
            if not ug:
                if op == 'set':
                    if groups_created >= MAX_GROUPS_PER_CALL:
                        raise RouteException(
                            f'Maximum of {MAX_GROUPS_PER_CALL} groups can be created per one jsrunner run.',
                        )
                    ug, _ = do_create_group(name)
                    groups_created += 1
                else:
                    raise RouteException(f'Group does not exist: {name}')
            else:
                verify_group_edit_access(ug)
            users: List[User] = User.query.filter(User.id.in_(uids)).all()
            found_user_ids = set(u.id for u in users)
            missing_ids = set(uids) - found_user_ids
            if missing_ids:
                raise RouteException(f'Users not found: {missing_ids}')
            if op == 'set':
                ug.memberships_sel = [UserGroupMember(user=u, adder=curr_user) for u in users]
            elif op == 'add':
                for u in users:
                    u.add_to_group(ug, added_by=curr_user)
            elif op == 'remove':
                ug.memberships_sel = [ugm for ugm in ug.memberships_sel if ugm.user_id not in found_user_ids]
            else:
                raise RouteException(f'Unexpected group operation: {op}')


class UserFieldEntry(TypedDict):
    user: int
    fields: Dict[str, str]


def create_missing_users(users: List[MissingUser]) -> Tuple[List[UserFieldEntry], List[User]]:
    created_users = []
    for mu in users:
        ui = mu.user
        if ui.email is not None and not is_valid_email(ui.email):
            raise RouteException(f'Invalid email: "{ui.email}"')
        if ui.username is None:
            ui.username = ui.email
        if ui.full_name is None and ui.email is not None:
            # Approximate real name with the help of email.
            # This won't be fully accurate, but we can't do better.
            ui.full_name = approximate_real_name(ui.email)
        u = create_or_update_user(ui, update_username=False)
        created_users.append(u)
    db.session.flush()
    fields = []
    for u, missing_u in zip(created_users, users):
        fields.append({'user': u.id, 'fields': missing_u.fields})
    return fields, created_users


MissingUserSchema = class_schema(MissingUser)


@dataclass
class FieldSaveResult:
    users_created: List[User] = field(default_factory=list)
    users_missing: List[UserInfo] = field(default_factory=list)
    fields_changed: int = 0
    fields_unchanged: int = 0
    fields_ignored: int = 0


class FieldSaveUserEntry(TypedDict):
    user: int
    fields: Dict[str, str]


class FieldSaveRequest(TypedDict, total=False):
    savedata: Optional[List[FieldSaveUserEntry]]
    ignoreMissing: Optional[bool]
    allowMissing: Optional[bool]
    createMissingUsers: Optional[bool]
    missingUsers: Optional[Any]
    groups: Optional[JsrunnerGroups]


def verify_user_create_right():
    u = get_current_user_object()
    if u.is_admin:
        return
    user_creators = UserGroup.get_user_creator_group()
    if user_creators not in u.groups:
        raise AccessDenied('You do not have permission to create users.')


def save_fields(
        jsonresp: FieldSaveRequest,
        current_doc: Optional[DocInfo] = None,
        allow_non_teacher: bool = False,
        add_users_to_group: Optional[str] = None,
) -> FieldSaveResult:
    save_obj = jsonresp.get('savedata')
    ignore_missing = jsonresp.get('ignoreMissing', False)
    allow_missing = jsonresp.get('allowMissing', False)
    ignore_fields = {}
    handle_jsrunner_groups(jsonresp.get('groups'))
    missing_users = jsonresp.get('missingUsers')
    saveresult = FieldSaveResult()
    curr_user = get_current_user_object()
    if save_obj is None:
        save_obj = []
    if missing_users:
        m_users: List[MissingUser] = MissingUserSchema().load(missing_users, many=True)
        if jsonresp.get('createMissingUsers'):
            verify_user_create_right()
            new_fields, users = create_missing_users(m_users)
            save_obj += new_fields
            saveresult.users_created = users
        else:
            saveresult.users_missing = [mu.user for mu in m_users]
    if not save_obj:
        return saveresult
    tasks = set()
    doc_map: Dict[int, DocInfo] = {}
    user_map: Dict[int, User] = {u.id: u for u in User.query.filter(User.id.in_(x['user'] for x in save_obj)).all()}

    # We need this separate "add_users_to_group" parameter because the plugin may have reported missing users.
    # They are created above, so the plugin cannot report them with "groups" in jsonresp because the user IDs are not
    # known until now.
    if add_users_to_group:
        handle_jsrunner_groups({'add': {add_users_to_group: [k for k in user_map.keys()]}})

    for item in save_obj:
        task_u = item['fields']
        for tid in task_u.keys():
            tasks.add(tid)
            try:
                id_num = TaskId.parse(tid, require_doc_id=False, allow_block_hint=False, allow_custom_field=True)
            except PluginException:
                return abort(400, f'Invalid task name: {tid.split(".")[1]}')
            if not id_num.doc_id:
                return abort(400, f'Doc id missing: {tid}')
            if id_num.doc_id not in doc_map:
                doc_map[id_num.doc_id] = get_doc_or_abort(id_num.doc_id)
    task_content_name_map = {}
    for task in tasks:
        t_id = TaskId.parse(task, require_doc_id=True, allow_block_hint=False, allow_custom_field=True)
        if ignore_fields.get(t_id.doc_task, False):
            continue
        dib = doc_map[t_id.doc_id]
        # TODO: Return case-specific abort messages
        if not (curr_user.has_teacher_access(dib) or (allow_non_teacher and t_id.doc_id == current_doc.id) or (
                curr_user.has_view_access(dib) and dib.document.get_own_settings().get("allow_external_jsrunner",
                                                                                       False))):
            return abort(403, f'Missing teacher access for document {dib.id}')
        try:
            vr = verify_task_access(
                dib,
                t_id,
                AccessType.view,
                TaskIdAccess.ReadWrite,
                user_context_with_logged_in(None),
                default_view_ctx,
            )
            plugin = vr.plugin
        except TaskNotFoundException as e:
            if not allow_missing:
                if ignore_missing:
                    ignore_fields[t_id.doc_task] = True
                    continue
                return abort(400, str(e))
            plugin = PluginType('textfield')  # assuming textfield type for fields that are not in the document
        except (PluginException, TimDbException) as e:
            return abort(400, str(e))

        # TODO this 'if' seems unnecessary
        if t_id.task_name in ('grade', 'credit', 'completionDate'):
            task_content_name_map[task] = 'c'
            continue

        if t_id.field and t_id.field != "points" and t_id.field != "styles":
            if plugin.type == "numericfield" or plugin.type == "textfield":
                if t_id.field != plugin.get_content_field_name():
                    return abort(400, f'Error saving to {task}: {t_id.field} is not an accepted field.')
            task_content_name_map[task] = t_id.field
        else:
            task_content_name_map[task] = plugin.get_content_field_name()

    parsed_task_ids = {
        key: TaskId.parse(key, require_doc_id=True, allow_block_hint=False, allow_custom_field=True)
        for user in save_obj for key in user['fields'].keys()
    }
    sq = (Answer.query
          .filter(Answer.task_id.in_([tid.doc_task for tid in parsed_task_ids.values() if not tid.is_global]))
          .join(User, Answer.users)
          .filter(User.id.in_(user_map.keys()))
          .group_by(User.id, Answer.task_id)
          .with_entities(func.max(Answer.id).label('aid'), User.id.label('uid'))
          .subquery())
    datas = Answer.query.join(sq, Answer.id == sq.c.aid).with_entities(sq.c.uid, Answer).all()
    global_answers = get_global_answers(parsed_task_ids)
    answer_map = defaultdict(dict)
    for uid, a in datas:
        answer_map[uid][a.task_id] = a
    for uid in user_map.keys():
        for a in global_answers:
            answer_map[uid][a.task_id] = a
    cpf = CachedPluginFinder(
        doc_map=doc_map,
        curr_user=UserContext.from_one_user(curr_user),
        view_ctx=default_view_ctx,
    )
    for user in save_obj:
        u_id = user['user']
        u = user_map.get(u_id)
        if not u:
            return abort(400, f'User id {u_id} not found')
        user_fields = user['fields']
        task_map: DefaultDict[str, Dict[str, Any]] = defaultdict(dict)
        for key, value in user_fields.items():
            task_id = parsed_task_ids[key]
            if ignore_fields.get(task_id.doc_task, False):
                saveresult.fields_ignored += 1
                continue
            field = task_id.field
            if field is None:
                field = task_content_name_map[task_id.doc_task]
            task_map[task_id.doc_task][field] = value
        for taskid, contents in task_map.items():
            task_id = TaskId.parse(taskid, require_doc_id=False, allow_block_hint=False)
            if ignore_fields.get(task_id.doc_task, False):
                continue
            an: Answer = answer_map[u.id].get(task_id.doc_task)
            points = None
            content = {}
            new_answer = False
            if an:
                points = an.points
                content = json.loads(an.content)
            lastfield = "c"
            for field, value in contents.items():
                lastfield = field
                if field == 'points':
                    if value == "":
                        value = None
                    else:
                        try:
                            value = float(value)
                        except ValueError:
                            raise RouteException(f'Value {value} is not valid point value for task {task_id.task_name}')
                    if points != value:
                        new_answer = True
                    points = value
                elif field == "styles":
                    if isinstance(value, str):
                        try:
                            value = json.loads(value or "null")
                        except json.decoder.JSONDecodeError:
                            raise RouteException(
                                f'Value {value} is not valid style syntax for task {task_id.task_name}')
                    plug = cpf.find(task_id)
                    if not plug:
                        continue
                    if plug.allow_styles_field():
                        if not an or content.get(field) != value:
                            new_answer = True
                        if value is None:
                            content.pop(field, None)
                        else:
                            content[field] = value

                        # Ensure there's always a content field even when setting styles to an empty answer.
                        c_field = task_content_name_map[f'{task_id.doc_task}.{field}']
                        if c_field not in content:
                            content[c_field] = None
                elif field == "JSSTRING":  # TODO check if this should be ALL!  No this is for settings using string
                    if not an or json.dumps(content) != value:
                        new_answer = True
                    content = json.loads(value)  # TODO: shoud this be inside if
                else:
                    if not an or content.get(field, "") != value:
                        new_answer = True
                    content[field] = value
            if not new_answer:
                saveresult.fields_unchanged += 1
                continue
            if not content:
                content[task_content_name_map[f'{task_id.doc_task}.{lastfield}']] = None
            content = json.dumps(content)
            ans = Answer(
                content=content,
                points=points,
                task_id=task_id.doc_task,
                users=[u],
                valid=True,
                saver=curr_user,
            )
            saveresult.fields_changed += 1
            # If this was a global task, add it to all users in the answer map so we won't save it multiple times.
            if task_id.is_global:
                for uid in user_map.keys():
                    answer_map[uid][ans.task_id] = ans
            db.session.add(ans)
    return saveresult


def get_global_answers(parsed_task_ids: Dict[str, TaskId]) -> List[Answer]:
    sq2 = (Answer.query
           .filter(Answer.task_id.in_([tid.doc_task for tid in parsed_task_ids.values() if tid.is_global]))
           .group_by(Answer.task_id)
           .with_entities(func.max(Answer.id).label('aid'))
           .subquery())
    global_datas = Answer.query.join(sq2, Answer.id == sq2.c.aid).with_entities(Answer).all()
    return global_datas


def get_hidden_name(user_id):
    return 'Student %d' % user_id


def should_hide_name(d: DocInfo, user: User):
    # return True
    # return not user.has_teacher_access(d) and user.id != get_current_user_id()
    return user.id != get_current_user_id()


def maybe_hide_name(d: DocInfo, u: User):
    if should_hide_name(d, u):
        # NOTE! To anonymize user, do NOT assign to u's real_name, name, etc. attributes here (or anywhere else either)
        # because it is
        #  1) dangerous (the anonymization would be persisted if db.session.commit() was called after the assignment)
        #  2) not necessary because the hiding is done in User.to_json method.
        u.hide_name = True


@answers.route("/taskinfo/<task_id>")
def get_task_info(task_id):
    try:
        user_ctx = user_context_with_logged_in(None)
        plugin, d = Plugin.from_task_id(task_id, user_ctx=user_ctx, view_ctx=default_view_ctx)
        verify_task_access(
            d,
            plugin.task_id,
            AccessType.view,
            TaskIdAccess.ReadOnly,
            allow_grace_period=True,
            context_user=user_ctx,
            view_ctx=default_view_ctx,
        )
        tim_vars = find_tim_vars(plugin)
    except PluginException as e:
        return abort(400, str(e))
    return json_response(tim_vars)


def find_tim_vars(plugin: Plugin):
    tim_vars = {
        'maxPoints': plugin.max_points(),
        'userMin': plugin.user_min_points(),
        'userMax': plugin.user_max_points(),
        'showPoints': plugin.show_points(),
        'deadline': plugin.deadline(),
        'starttime': plugin.starttime(),
        'answerLimit': plugin.answer_limit(),
        'triesText': plugin.known.tries_text(),
        'pointsText': plugin.known.points_text(),
    }
    return tim_vars


def hide_points(a: Answer):
    j = a.to_json()
    j['points'] = None

    c = j.get('content', None)
    if c and c.find('"points": {') >= 0:  # TODO: Hack for csPlugin
        c = json.loads(c)
        c.pop('points')
        c = json.dumps(c)
        j['content'] = c

    if a.points is not None:
        j['points_hidden'] = True
    return j


@answers.route('/exportAnswers/<path:doc_path>')
def export_answers(doc_path: str):
    d = DocEntry.find_by_path(doc_path)
    if not d:
        raise RouteException('Document not found')
    verify_teacher_access(d)
    answer_list: List[Tuple[Answer, str]] = (
        Answer.query
            .filter(Answer.task_id.startswith(f'{d.id}.'))
            .join(User, Answer.users)
            .with_entities(Answer, User.email)
            .all()
    )
    return json_response([{
        'email': email,
        'content': a.content,
        'valid': a.valid,
        'points': a.points,
        'time': a.answered_on,
        'task': a.task_name,
    } for a, email in answer_list])


@dataclass
class ExportedAnswer:
    content: str
    email: str
    points: Union[int, float, None]
    task: str
    time: datetime
    valid: bool


@dataclass
class ImportAnswersModel:
    doc: str
    answers: List[ExportedAnswer]
    allow_missing_users: bool = False


@answers.route('/importAnswers', methods=['post'])
@use_model(ImportAnswersModel)
def import_answers(m: ImportAnswersModel):
    d = DocEntry.find_by_path(m.doc)
    if not d:
        raise RouteException('Document not found')
    verify_teacher_access(d)
    verify_admin()
    existing_answers: List[Tuple[Answer, str]] = (
        Answer.query
            .filter(Answer.task_id.startswith(f'{d.id}.'))
            .join(User, Answer.users)
            .with_entities(Answer, User.email)
            .all()
    )
    existing_set = set((a.task_name, a.answered_on, a.valid, a.points, email) for a, email in existing_answers)
    dupes = 0
    users = {u.email: u for u in User.query.filter(User.email.in_([a.email for a in m.answers])).all()}
    requested_users = set(a.email for a in m.answers)
    missing_users = requested_users - set(users.keys())
    if missing_users and not m.allow_missing_users:
        raise RouteException(f'Email(s) not found: {seq_to_str(list(missing_users))}')
    m.answers.sort(key=lambda a: a.time)
    all_imported = []
    for a in m.answers:
        if (a.task, a.time, a.valid, a.points, a.email) not in existing_set:
            u = users.get(a.email)
            if not u:
                if not m.allow_missing_users:
                    raise Exception('Missing user should have been reported earlier')
                continue
            imported_answer = Answer(
                task_id=f'{d.id}.{a.task}',
                valid=a.valid,
                points=a.points,
                content=a.content,
                answered_on=a.time,
            )
            imported_answer.users_all.append(u)
            db.session.add(imported_answer)
            all_imported.append(imported_answer)
        else:
            dupes += 1
    db.session.flush()

    # Sanity check: Make sure that the ids are in the same order as the timestamps of the answers - we currently rely on
    # the fact that the latest answer has the largest id.
    all_imported.sort(key=lambda a: a.id)
    for a, b in zip(all_imported, all_imported[1:]):
        if a.answered_on > b.answered_on:
            raise Exception('Import bug: Answer ids were in different order than answer timestamps. Imported nothing.')

    db.session.commit()
    return json_response({
        'imported': len(all_imported),
        'skipped_duplicates': dupes,
        'missing_users': list(missing_users),
    })


@answers.route("/getAnswers/<task_id>/<int:user_id>")
def get_answers(task_id: str, user_id: int):
    verify_logged_in()
    try:
        tid = TaskId.parse(task_id)
    except PluginException as e:
        return abort(400, str(e))
    d = get_doc_or_abort(tid.doc_id)
    user = User.get_by_id(user_id)
    if user_id != get_current_user_id():
        verify_seeanswers_access(d)
    elif d.document.get_settings().get('need_view_for_answers', False):
        verify_view_access(d)
    if user is None:
        abort(400, 'Non-existent user')
    try:
        p = find_plugin_from_document(d.document, tid, user_context_with_logged_in(user), default_view_ctx)
    except TaskNotFoundException:
        p = None
    user_answers: List[Answer] = user.get_answers_for_task(tid.doc_task).all()
    if hide_names_in_teacher(d, context_user=user):
        for answer in user_answers:
            for u in answer.users_all:
                maybe_hide_name(d, u)
    if p and not p.known.show_points() and not get_current_user_object().has_teacher_access(d):
        user_answers = list(map(hide_points, user_answers))
    return json_response(user_answers)


@answers.route("/allDocumentAnswersPlain/<path:doc_path>")
def get_document_answers(doc_path):
    d = DocEntry.find_by_path(doc_path, fallback_to_id=True)
    pars = d.document.get_dereferenced_paragraphs(default_view_ctx)
    task_ids, _, _ = find_task_ids(pars, default_view_ctx, user_context_with_logged_in(None))
    return get_all_answers_list_plain(task_ids)


@answers.route("/allAnswersPlain/<task_id>")
def get_all_answers_plain(task_id):
    return get_all_answers_list_plain([TaskId.parse(task_id)])


def get_all_answers_list_plain(task_ids: List[TaskId]):
    all_answers, format_opt = get_all_answers_as_list(task_ids)
    if format_opt == 'json':
        return json_response(all_answers)
    jointext = "\n"
    print_opt = get_option(request, 'print', 'all')
    print_answers = print_opt == "all" or print_opt == "answers"
    if print_answers:
        jointext = "\n\n----------------------------------------------------------------------------------\n"
    text = jointext.join(all_answers)
    return Response(text, mimetype='text/plain')


def get_all_answers_as_list(task_ids: List[TaskId]):
    verify_logged_in()
    format_opt = get_option(request, 'format', 'text')
    if not task_ids:
        return [], format_opt
    doc_ids = set()
    d = None
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
    format_opt = get_option(request, 'format', 'text')
    consent = get_consent_opt()
    printname = name_opt == 'both'

    period_from, period_to = period_handling(task_ids, doc_ids, period_opt)

    if not usergroup:
        usergroup = None

    hide_names = name_opt == 'anonymous'
    if d:
        # Above, we're requiring teacher access to all documents, so it does not matter which DocInfo we pass here.
        hide_names = hide_names or hide_names_in_teacher(d)
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
                                  format_opt,
                                  consent=consent)
    return all_answers, format_opt


class GraphData(TypedDict):
    data: List[Union[str, float, None]]
    labels: List[str]


@dataclass
class FieldInfo:
    data: UserFields
    aliases: Dict[str, str]
    fieldnames: List[str]
    graphdata: GraphData


def get_plug_vals(doc: DocInfo, tid: TaskId, user_ctx: UserContext, view_ctx: ViewContext) -> Optional[FieldInfo]:
    d, plug = get_plugin_from_request(doc.document, tid, user_ctx, view_ctx)
    flds = plug.known.fields
    if not flds:
        return None

    data, aliases, field_names, _ = get_fields_and_users(
        flds,
        RequestedGroups([user_ctx.user.personal_group_prop]),
        doc,
        user_ctx.logged_user,
        view_ctx,
        add_missing_fields=True,
        access_option=GetFieldsAccess.from_bool(True),
    )
    df = data[0]['fields']
    da = []
    for fn in field_names:
        da.append(df.get(fn, 0))
    return FieldInfo(
        data=df,
        aliases=aliases,
        fieldnames=field_names,
        graphdata={'data': da, 'labels': field_names},
    )


@answers.route("/jsframe/userChange/<task_id>/<user_id>")
def get_jsframe_data(task_id, user_id):
    """
        TODO: check proper rights
    """
    tid = TaskId.parse(task_id)
    doc = get_doc_or_abort(tid.doc_id)
    # verify_seeanswers_access(doc)
    user = User.get_by_id(user_id)
    curr_user = get_current_user_object()
    try:
        vals = get_plug_vals(doc, tid, UserContext(user=user, logged_user=curr_user), default_view_ctx)
        return json_response(vals)
    except Exception as e:
        return abort(400, str(e))
        # return json_response({})


@dataclass
class GetStateModel:
    user_id: int
    answer_id: Optional[int] = None
    par_id: Optional[str] = None
    doc_id: Optional[int] = None
    review: bool = False
    task_id: Optional[str] = None


GetStateSchema = class_schema(GetStateModel)


@answers.route("/getState")
@use_args(GetStateSchema())
def get_state(args: GetStateModel):
    par_id = args.par_id
    user_id = args.user_id
    answer_id = args.answer_id
    review = args.review
    task_id = args.task_id
    answer = None
    user = User.get_by_id(user_id)
    if user is None:
        abort(400, 'Non-existent user')
    if answer_id:
        try:
            answer, doc_id = verify_answer_access(answer_id, user_id, default_view_ctx, allow_grace_period=True)
        except PluginException as e:
            return abort(400, str(e))
        doc = Document(doc_id)
        tid = TaskId.parse(answer.task_id)
    elif task_id:
        tid = TaskId.parse(task_id)
        d = get_doc_or_abort(tid.doc_id)
        if get_current_user_id() != user_id:
            verify_seeanswers_access(d)
        else:
            verify_view_access(d)
        doc = d.document
    else:
        return abort(400, "Missing answer ID or task ID")

    doc.insert_preamble_pars()
    if par_id:
        tid.maybe_set_hint(par_id)
    user_ctx = user_context_with_logged_in(user)
    try:
        doc, plug = get_plugin_from_request(doc, task_id=tid, u=user_ctx, view_ctx=default_view_ctx)
    except PluginException as e:
        return abort(400, str(e))
    block = plug.par

    presult = pluginify(doc, [block], user_ctx, default_view_ctx, custom_answer=answer, task_id=task_id, do_lazy=NEVERLAZY,
                        pluginwrap=PluginWrap.Nothing)
    plug = presult.custom_answer_plugin
    html = plug.get_final_output()
    if review:
        block.final_dict = None
        presult2 = pluginify(doc, [block], user_ctx, default_view_ctx, custom_answer=answer, task_id=task_id, do_lazy=NEVERLAZY,
                             review=review, pluginwrap=PluginWrap.Nothing)
        rplug = presult2.custom_answer_plugin
        rhtml = rplug.get_final_output()
        return json_response({'html': html, 'reviewHtml': rhtml})
    else:
        return json_response({'html': html, 'reviewHtml': None})


def verify_answer_access(
        answer_id: int,
        user_id: int,
        view_ctx: ViewContext,
        require_teacher_if_not_own=False,
        required_task_access_level: TaskIdAccess = TaskIdAccess.ReadOnly,
        allow_grace_period: bool = False,
) -> Tuple[Answer, int]:
    answer: Answer = Answer.query.get(answer_id)
    if answer is None:
        abort(400, 'Non-existent answer')
    tid = TaskId.parse(answer.task_id)

    if tid.is_global:
        return answer, tid.doc_id

    d = get_doc_or_abort(tid.doc_id)
    d.document.insert_preamble_pars()

    if verify_teacher_access(d, require=False):
        return answer, tid.doc_id

    user_ctx = user_context_with_logged_in(None)
    if user_id != get_current_user_id() or not logged_in():
        if require_teacher_if_not_own:
            verify_task_access(d, tid, AccessType.teacher, required_task_access_level, user_ctx, view_ctx)
        else:
            verify_task_access(d, tid, AccessType.see_answers, required_task_access_level, user_ctx, view_ctx)
    else:
        verify_task_access(
            d,
            tid,
            AccessType.view,
            required_task_access_level,
            allow_grace_period=allow_grace_period,
            context_user=user_ctx,
            view_ctx=view_ctx,
        )
        if not any(a.id == user_id for a in answer.users_all):
            abort(403, "You don't have access to this answer.")
    return answer, tid.doc_id


@answers.route("/getTaskUsers/<task_id>")
def get_task_users(task_id):
    tid = TaskId.parse(task_id)
    d = get_doc_or_abort(tid.doc_id)
    verify_seeanswers_access(d)

    usergroup = request.args.get('group')
    q = (
        User.query
            .options(lazyload(User.groups))
            .join(Answer, User.answers)
            .filter_by(task_id=task_id)
            .join(UserGroup, User.groups)
            .order_by(User.real_name.asc())
    )
    if usergroup is not None:
        q = q.filter(UserGroup.name.in_([usergroup]))
    users = q.all()
    if hide_names_in_teacher(d):
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
