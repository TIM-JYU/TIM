"""Answer-related routes."""
import json
import re
from typing import Union, List, Tuple, Dict, Optional, Any, Callable

from dataclasses import dataclass, field
from flask import Blueprint
from flask import Response
from flask import abort
from flask import request
from marshmallow import validates_schema, ValidationError
from marshmallow.utils import _Missing as Missing, missing
from sqlalchemy import func
from sqlalchemy.orm import lazyload
from webargs.flaskparser import use_args

from pluginserver_flask import GenericMarkupModel
from timApp.answer.answer import Answer
from timApp.answer.answer_models import AnswerUpload
from timApp.answer.answers import get_latest_answers_query, get_common_answers, save_answer, get_all_answers, \
    valid_answers_query, valid_taskid_filter
from timApp.auth.accesshelper import verify_logged_in, get_doc_or_abort, verify_manage_access, AccessDenied
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
from timApp.modules.py.marshmallow_dataclass import class_schema
from timApp.notification.notify import multi_send_email
from timApp.plugin.containerLink import call_plugin_answer
from timApp.plugin.plugin import Plugin, PluginWrap, NEVERLAZY, TaskNotFoundException, is_global, is_global_id, \
    find_task_ids
from timApp.plugin.plugin import PluginType
from timApp.plugin.plugin import find_plugin_from_document
from timApp.plugin.pluginControl import pluginify
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.taskid import TaskId, TaskIdAccess
from timApp.tim_app import get_home_organization_group
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.answerutil import period_handling
from timApp.util.flask.requesthelper import verify_json_params, get_option, get_consent_opt, RouteException
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.get_fields import get_fields_and_users, MembershipFilter
from timApp.util.utils import try_load_json

# TODO: Remove methods in util/answerutil where many "moved" here to avoid circular imports

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
    if isinstance(points, float):
        return points
    if points == '':
        return None
    if points is None:
        return None
    return float(points)


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
    """
    if is_global_id(tid):
        answer_map = {}
        get_globals_for_tasks([tid], answer_map)
    else:
        old_answers = get_common_answers(users, tid)
    """

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

    vals = get_plug_vals(d, tid, get_current_user_object(), users[0])
    if vals:
        answer_call_data['markup']['fielddata'] = vals.get('fielddata', {})

    jsonresp = call_plugin_answer_and_parse(answer_call_data, plugintype)

    if 'iframehtml' not in jsonresp:
        return json_response({'error': 'The key "iframehtml" is missing in plugin response.'}, 400)
    result = jsonresp['iframehtml']
    db.session.commit()
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
    answs: List[Answer] = Answer.query.join(sub, Answer.id == sub.c.col) .all()
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
    currid = get_current_user_id()
    try:
        doc_map = {}
        tids = []
        gtids = []
        for task_id in tasks:
            tid = TaskId.parse(task_id)
            if tid.doc_id not in doc_map:
                dib = get_doc_or_abort(tid.doc_id, f'Document {tid.doc_id} not found')
                if user_id != currid:
                    verify_seeanswers_access(dib)
                elif dib.document.get_own_settings().get('need_view_for_answers', False):
                    verify_view_access(dib)
                doc_map[tid.doc_id] = dib.document
            if is_global_id(tid):
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
    fields: Union[List[str], Missing] = missing  # This is actually required, but we cannot use non-default arguments here...
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
    includeUsers: Union[MembershipFilter, Missing] = field(default=MembershipFilter.Current, metadata={'by_value': True})
    selectIncludeUsers: bool = False
    paramFields: Union[List[str], Missing] = missing
    postprogram: Union[str, Missing] = missing
    preprogram: Union[str, Missing] = missing
    program: Union[str, Missing] = missing
    overrideGrade: bool = False
    showInView: bool = False
    timeout: Union[int, Missing] = missing
    updateFields: Union[List[str], Missing] = missing

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
    groups: Union[List[str], Missing] = missing
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


@answers.route("/multiSendEmail/<task_id_ext>/", methods=['POST'])
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


@answers.route("/<plugintype>/<task_id_ext>/answer/", methods=['PUT'])
def post_answer(plugintype: str, task_id_ext: str):
    """Saves the answer submitted by user for a plugin in the database.

    :param plugintype: The type of the plugin, e.g. csPlugin.
    :param task_id_ext: The extended task id of the form "22.palidrome.par_id".
    :return: JSON

    """

    tid = TaskId.parse(task_id_ext)
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
            if user_id not in (u.id for u in users):
                raise PluginException('userId is not associated with answer_id')
        elif user_id and user_id != curr_user.id and False: # TODO: Vesa's hack to no need for belong teachers group
            teacher_group = UserGroup.get_teachers_group()
            if curr_user not in teacher_group.users:
                raise PluginException('Permission denied: you are not in teachers group.')
        if user_id:
            ctx_user = User.query.get(user_id)
            if not ctx_user:
                raise PluginException(f'User {user_id} not found')
            users = [ctx_user]  # TODO: Vesa's hack to save answer to student
    try:
        plugin = verify_task_access(d, tid, AccessType.view, TaskIdAccess.ReadWrite, context_user=ctx_user)
    except (PluginException, TimDbException) as e:
        raise PluginException(str(e))

    if plugin.type != plugintype:
        raise PluginException(f'Plugin type mismatch: {plugin.type} != {plugintype}')

    upload = None

    if not logged_in() and not plugin.values.get('anonymous', False):
        raise RouteException('You must be logged in to answer this task.')
    if plugin.values.get("useCurrentUser", False) or is_global(plugin):  # For plugins that is saved only for current user
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
            upload = AnswerUpload.query.filter(AnswerUpload.upload_block_id == block.id).first()
            # if upload.answer_id is not None:
            #    raise PluginException(f'File was already uploaded: {file}')

    # Load old answers

    if users is None:
        users = [User.query.get(u['id']) for u in get_session_users()]

    old_answers = get_common_answers(users, tid)
    valid, _ = plugin.is_answer_valid(len(old_answers), {})
    info = plugin.get_info(users, len(old_answers), look_answer=is_teacher and not save_teacher, valid=valid)

    # Get the newest answer (state). Only for logged in users.
    state = try_load_json(old_answers[0].content) if logged_in() and len(old_answers) > 0 else None

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
        raise PluginException('Missing "web" in plugin response.')
    result = {'web': web}

    # if plugin.type == 'jsrunner' or plugin.type == 'tableForm' or plugin.type == 'importData':
    if 'savedata' in jsonresp:
        siw = answer_call_data.get("markup", {}).get("showInView", False)
        handle_jsrunner_response(jsonresp, d, allow_non_teacher=siw)

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
                                                 save_object,
                                                 points,
                                                 tags,
                                                 is_valid,
                                                 points_given_by,
                                                 force_answer,
                                                 plugintype=ptype)
            else:
                result['savedNew'] = None
            if not is_valid:
                result['error'] = explanation
        elif save_teacher:
            points = answer_browser_data.get('points', points)
            points = points_to_float(points)
            result['savedNew'] = save_answer(users,
                                             tid,
                                             save_object,
                                             points,
                                             tags,
                                             valid=True,
                                             points_given_by=get_current_user_group(),
                                             saver=curr_user,
                                             plugintype=ptype)
        else:
            result['savedNew'] = None
        if result['savedNew'] is not None and upload is not None:
            # Associate this answer with the upload entry
            upload.answer_id = result['savedNew']

    db.session.commit()
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
    g = UserGroup.query.filter(UserGroup.name.in_(groupnames))
    found_groups = g.all()
    req_groups = runner_req.input.groups
    # Check if groups given as request arg can be considered as subset of groups given in markup
    if req_groups:
        all_req_groups = UserGroup.query.filter(UserGroup.name.in_(req_groups)).all()
        if not curr_user.has_edit_access(d):
            user_set = set()
            for g in all_req_groups:
                members = g.users.all()
                user_set.update(members)
            for u in user_set:  # type: User
                u_within_jsrunner_groups = False
                for fg in found_groups:
                    if fg in u.groups:
                        u_within_jsrunner_groups = True
                        break
                if not u_within_jsrunner_groups:
                    raise PluginException(f'Given groups are not associated with the original groups.')
        groupnames = req_groups
        found_groups = all_req_groups
    not_found_groups = sorted(list(set(groupnames) - set(g.name for g in found_groups)))
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
        found_groups,
        d,
        get_current_user_object(),
        allow_non_teacher=siw,
        member_filter_type=runner_req.input.includeUsers,
    )
    answerdata.pop('paramComps', None)  # This isn't needed by jsrunner server, so don't send it.
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


def handle_jsrunner_response(jsonresp, current_doc: DocInfo = None, allow_non_teacher: bool = False):
    save_obj = jsonresp.get('savedata')
    ignore_missing = jsonresp.get('ignoreMissing', False)
    allow_missing = jsonresp.get('allowMissing', False)
    ignore_fields = {}
    if not save_obj:
        return
    tasks = set()
    doc_map: Dict[int, DocInfo] = {}
    user_map: Dict[int, User] = {u.id: u for u in User.query.filter(User.id.in_(x['user'] for x in save_obj)).all()}
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
    curr_user = get_current_user_object()
    for task in tasks:
        t_id = TaskId.parse(task, require_doc_id=False, allow_block_hint=False, allow_custom_field=True)
        if ignore_fields.get(t_id.doc_task, False):
            continue
        dib = doc_map[t_id.doc_id]
        # TODO: Return case-specific abort messages
        if not (curr_user.has_teacher_access(dib) or (allow_non_teacher and t_id.doc_id == current_doc.id) or (curr_user.has_view_access(dib) and dib.document.get_own_settings().get("allow_external_jsrunner", False))):
            return abort(403, f'Missing teacher access for document {dib.id}')
        try:
            plugin = verify_task_access(dib, t_id, AccessType.view, TaskIdAccess.ReadWrite)  # , context_user=ctx_user)
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

    for user in save_obj:
        u_id = user['user']
        u = user_map.get(u_id)
        if not u:
            return abort(400, f'User id {u_id} not found')
        user_fields = user['fields']
        task_map = {}
        for key, value in user_fields.items():
            task_id = TaskId.parse(key, require_doc_id=False, allow_block_hint=False, allow_custom_field=True)
            if ignore_fields.get(task_id.doc_task, False):
                continue
            field = task_id.field
            if field is None:
                field = task_content_name_map[task_id.doc_task]
            try:
                task_map[task_id.doc_task][field] = value
            except KeyError:
                task_map[task_id.doc_task] = {}
                task_map[task_id.doc_task][field] = value
        for taskid, contents in task_map.items():
            task_id = TaskId.parse(taskid, require_doc_id=False, allow_block_hint=False)
            if ignore_fields.get(task_id.doc_task, False):
                continue
            an: Answer = get_latest_answers_query(task_id, [u]).first()
            points = None
            content = {}
            new_answer = True
            if an:
                points = an.points
                content = json.loads(an.content)
                new_answer = False
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
                            return abort(400, f'Value {value} is not valid point value for task {task_id.task_name}')
                    if points != value:
                        new_answer = True
                    points = value
                elif field == "styles":
                    try:
                        if type(value) is str:
                            if value == "":
                                value = None
                            else:
                                try:
                                    value = json.loads(value)
                                except json.decoder.JSONDecodeError:
                                    return abort(400,
                                                 f'Value {value} is not valid style syntax for task {task_id.task_name}')
                        plug = find_plugin_from_document(doc_map[task_id.doc_id].document, task_id, curr_user)
                        allow_styles = plug.allow_styles_field()
                        if allow_styles:
                            if an and content.get(field, None) != value:
                                new_answer = True
                            if value is None:
                                try:
                                    del content[field]
                                except KeyError:
                                    pass
                            else:
                                content[field] = value
                            # Ensure that there's always a content field even when setting styles to empty answer

                            # TODO: this line is definitely wrong because the 'task' variable
                            #  does not exist in this loop.
                            prev_content_value = content.get(task_content_name_map[task], None)
                            if prev_content_value is None:
                                content[task_content_name_map[task]] = None
                        else:
                            continue
                    except TaskNotFoundException as e:
                        continue
                elif field == "JSSTRING":
                    if an and json.dumps(content) != value:
                        new_answer = True
                    content = json.loads(value)
                else:
                    if an and content.get(field, "") != value:
                        new_answer = True
                    content[field] = value
            if not new_answer:
                continue
            if content == {}:
                # Reached if fields were only points or failed styles
                if list(contents) == ['styles']:
                    continue
                content[task_content_name_map[task_id.doc_task + "." + lastfield]] = None
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
            tim_vars = find_tim_vars(plugin)
            infolist[task_id] = tim_vars
        except (TaskNotFoundException, PluginException) as e:
            return abort(400, str(e))
    return json_response(infolist)


@answers.route("/taskinfo/<task_id>")
def get_task_info(task_id):
    try:
        plugin = Plugin.from_task_id(task_id, user=get_current_user_object())
        tim_vars = find_tim_vars(plugin)
    except PluginException as e:
        return abort(400, str(e))
    return json_response(tim_vars)


def find_tim_vars(plugin: Plugin):
    tim_vars = {'maxPoints': plugin.max_points(),
                'userMin': plugin.user_min_points(),
                'userMax': plugin.user_max_points(),
                'deadline': plugin.deadline(),
                'starttime': plugin.starttime(),
                'answerLimit': plugin.answer_limit(),
                'triesText': plugin.values.get('triesText', 'Tries left:'),
                'pointsText': plugin.values.get('pointsText', 'Points:')
                }
    return tim_vars


@answers.route("/getAnswers/<task_id>/<user_id>")
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
    hide_names = False
    if user_id != get_current_user_id():
        verify_seeanswers_access(d)
        if not verify_teacher_access(d, require=False, check_duration=True):
            hide_names = True
    elif d.document.get_own_settings().get('need_view_for_answers', False):
        verify_view_access(d)
    if user is None:
        abort(400, 'Non-existent user')
    try:
        user_answers: List[Answer] = user.get_answers_for_task(tid.doc_task).all()
        if hide_names_in_teacher() or hide_names:
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


def get_plug_vals(doc, tid, curr_user, user):
    plug = get_plugin_from_request(doc.document, tid, curr_user)
    vals = {}
    flds = plug[1].values.get('fields', [])
    if not flds:
        return {}

    data, aliases, field_names, _ = get_fields_and_users(
        flds,
        [user.personal_group_prop],
        doc,
        get_current_user_object(),
        add_missing_fields=True,
        allow_non_teacher=True,
    )
    df = data[0]['fields']
    da = []
    labels = []
    for fn in field_names:
        da.append(df.get(fn, 0))
        labels.append(fn)
    vals['fielddata'] = {'data': df,
                         'aliases': aliases,
                         'fieldnames': field_names,
                         'graphdata': {'data': da, 'labels': labels}}
    return vals


@answers.route("/jsframeUserChange/<task_id>/<user_id>")
def get_jsframe_data(task_id, user_id):
    """
        TODO: Delete (experimental)
        TODO: if leave, think rights carefully
    """
    tid = TaskId.parse(task_id)
    doc = get_doc_or_abort(tid.doc_id)
    # verify_seeanswers_access(doc)
    user = User.get_by_id(user_id)
    curr_user = get_current_user_object()
    try:
        vals = get_plug_vals(doc, tid, curr_user, user)
        """
                vals = {}
                flds = plug[1].values.get('fields', [])
                if not flds:
                    return json_response({})
        
                data, aliases, field_names = get_fields_and_users(
                    flds,
                    [user.personal_group_prop],
                    doc,
                    get_current_user_object(),
                    add_missing_fields=True,
                    allow_non_teacher=True,
                )
                df = data[0]['fields']
                da = []
                labels = []
                for fn in field_names:
                    da.append(df.get(fn, 0))
                    labels.append(fn)
                vals['fielddata'] = {'data': data[0]['fields'],
                                     'aliases': aliases,
                                     'fieldnames': field_names,
                                     'graphdata': {'data': da, 'labels': labels}}
        """
        return json_response(vals)
    except Exception as e:
        return abort(400, str(e))
        # return json_response({})


@dataclass
class GetMultiStatesModel:
    answer_ids: List[int]
    user_id: int


GetMultiStatesSchema = class_schema(GetMultiStatesModel)


@dataclass
class GetStateModel:
    answer_id: int
    user_id: int
    par_id: Optional[str] = None
    doc_id: Optional[int] = None
    review: bool = False


GetStateSchema = class_schema(GetStateModel)


@answers.route("/getMultiStates")
@use_args(GetMultiStatesSchema())
def get_multi_states(args: GetMultiStatesModel):
    """
    WIP
    Queries plugin states for multiple answers
    :param args: {answer_ids: list of answers, user_id, doc_id}
    :return: {answerID: {'html': html, 'reviewHtml': None}}
    """
    answer_ids, user_id = args.answer_ids, args.user_id
    print(args)
    user = User.query.get(user_id)
    if user is None:
        abort(400, 'Non-existent user')
    answs = Answer.query.filter(Answer.id.in_(answer_ids)).all()
    response = {}
    doc_map = {}
    for ans in answs:
        tid = TaskId.parse(ans.task_id)
        if tid.doc_id not in doc_map:
            dib = get_doc_or_abort(tid.doc_id, f'Document {tid.doc_id} not found')
            verify_seeanswers_access(dib)
            doc_map[tid.doc_id] = dib.document
        try:
            doc, plug = get_plugin_from_request(doc_map[tid.doc_id], task_id=tid, u=user)
        except PluginException as e:
            return abort(400, str(e))
        except AssertionError:
            return abort(400, 'answer_id is not associated with doc_id')
        block = plug.par
        _, _, _, plug = pluginify(
            doc,
            [block],
            user,
            custom_answer=ans,
            pluginwrap=PluginWrap.Nothing,
            do_lazy=NEVERLAZY,
        )
        html = plug.get_final_output()
        response[ans.id] = {'html': html, 'reviewHtml': None}
    return json_response(response)


@answers.route("/getState")
@use_args(GetStateSchema())
def get_state(args: GetStateModel):
    par_id, user_id, answer_id, review = args.par_id, args.user_id, args.answer_id, args.review

    try:
        answer, doc_id = verify_answer_access(answer_id, user_id)
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

    if is_global_id(tid):
        return answer, tid.doc_id

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
    hide_names = False
    if not verify_teacher_access(d, require=False, check_duration=True):
        hide_names = True

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
    if hide_names_in_teacher() or hide_names:
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
