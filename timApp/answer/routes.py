"""Answer-related routes."""
import json
import re
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Union, Any, Callable, TypedDict, DefaultDict

from flask import Response
from flask import request
from flask import session, current_app
from marshmallow import validates_schema, ValidationError
from marshmallow.utils import missing
from sqlalchemy import func
from sqlalchemy.orm import lazyload

from timApp.answer.answer import Answer
from timApp.answer.answer_models import AnswerUpload
from timApp.answer.answers import (
    get_existing_answers_info,
    save_answer,
    valid_answers_query,
    valid_taskid_filter,
    ExistingAnswersInfo,
    NameOptions,
    AllAnswersOptions,
    FormatOptions,
    AnswerPrintOptions,
    get_all_answers,
)
from timApp.answer.backup import send_answer_backup_if_enabled
from timApp.answer.exportedanswer import ExportedAnswer
from timApp.auth.accesshelper import (
    verify_logged_in,
    get_doc_or_abort,
    verify_manage_access,
    AccessDenied,
    verify_admin,
    get_origin_from_request,
    verify_ip_ok,
    TaskAccessVerification,
)
from timApp.auth.accesshelper import (
    verify_task_access,
    verify_teacher_access,
    verify_seeanswers_access,
    has_teacher_access,
    verify_view_access,
    get_plugin_from_request,
)
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.auth.get_user_rights_for_item import get_user_rights_for_item
from timApp.auth.login import create_or_update_user
from timApp.auth.sessioninfo import (
    get_current_user_id,
    logged_in,
    user_context_with_logged_in,
    get_other_session_users_objs,
    clear_session,
)
from timApp.auth.sessioninfo import get_current_user_object, get_current_user_group
from timApp.document.caching import clear_doc_cache
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document, dereference_pars
from timApp.document.hide_names import hide_names_in_teacher
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import (
    ViewRoute,
    ViewContext,
    default_view_ctx,
    OriginInfo,
    UrlMacros,
)
from timApp.item.block import Block, BlockType
from timApp.item.taskblock import insert_task_block, TaskBlock
from timApp.markdown.dumboclient import call_dumbo
from timApp.messaging.messagelist.messagelist_utils import (
    UserGroupDiff,
    sync_usergroup_messagelist_members,
)
from timApp.notification.notification import NotificationType
from timApp.notification.notify import notify_doc_watchers
from timApp.notification.send_email import multi_send_email
from timApp.peerreview.peerreview_utils import (
    has_review_access,
    get_reviews_for_user,
    is_peerreview_enabled,
    get_reviews_for_document,
)
from timApp.plugin.containerLink import call_plugin_answer
from timApp.plugin.importdata.importData import MissingUser
from timApp.plugin.jsrunner import jsrunner_run, JsRunnerParams, JsRunnerError
from timApp.plugin.plugin import (
    Plugin,
    PluginWrap,
    NEVERLAZY,
    TaskNotFoundException,
    find_task_ids,
    CachedPluginFinder,
)
from timApp.plugin.plugin import find_plugin_from_document
from timApp.plugin.pluginControl import pluginify
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.plugintype import PluginType, PluginTypeBase
from timApp.plugin.taskid import TaskId, TaskIdAccess
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.user.groups import do_create_group, verify_group_edit_access
from timApp.user.user import User, UserInfo, has_no_higher_right
from timApp.user.user import maxdate
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember
from timApp.user.userutils import grant_access
from timApp.util.answerutil import get_answer_period
from timApp.util.flask.requesthelper import (
    get_option,
    get_consent_opt,
    RouteException,
    get_urlmacros_from_request,
    NotExist,
    get_from_url,
)
from timApp.util.flask.responsehelper import json_response, ok_response, to_dict
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.get_fields import (
    get_fields_and_users,
    MembershipFilter,
    UserFields,
    RequestedGroups,
    ALL_ANSWERED_WILDCARD,
    GetFieldsAccess,
)
from timApp.util.logger import log_info
from timApp.util.utils import get_current_time, approximate_real_name
from timApp.util.utils import local_timezone
from timApp.util.utils import try_load_json, seq_to_str, is_valid_email
from timApp.velp.annotations import get_annotations_with_comments_in_document
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import value_or_default
from tim_common.utils import Missing

PRE_POST_ERROR = """
You must have at least one
  return data;
row in code!
"""

answers = TypedBlueprint("answers", __name__, url_prefix="")

PointsType = Union[
    float,  # Points as float
    str,  # Points as string, convert them to float
    None,  # Clear points, only by teacher
]


@answers.put("/savePoints/<int:user_id>/<int:answer_id>")
def save_points(answer_id: int, user_id: int, points: PointsType = None) -> Response:
    answer, _ = verify_answer_access(
        answer_id,
        user_id,
        default_view_ctx,
        require_teacher_if_not_own=True,
    )
    tid = TaskId.parse(answer.task_id)
    if tid.doc_id is None:
        raise RouteException("Task ID must include document ID")
    d = get_doc_or_abort(tid.doc_id)
    try:
        plugin, _ = Plugin.from_task_id(
            answer.task_id,
            user_ctx=user_context_with_logged_in(None),
            view_ctx=default_view_ctx,
        )
    except PluginException as e:
        raise RouteException(str(e))
    a = Answer.query.get(answer_id)
    try:
        points = points_to_float(points)
    except ValueError:
        raise RouteException("Invalid points format.")
    try:
        a.points = (
            plugin.validate_points(points) if not has_teacher_access(d) else points
        )
    except PluginException as e:
        raise RouteException(str(e))
    a.last_points_modifier = get_current_user_group()
    db.session.commit()
    return ok_response()


@answers.put("/answer/saveValidity")
def save_validity(answer_id: int, valid: bool) -> Response:
    a, doc_id = verify_answer_access(
        answer_id,
        get_current_user_object().id,
        default_view_ctx,
        require_teacher_if_not_own=True,
    )
    verify_teacher_access(get_doc_or_abort(doc_id))
    a.valid = valid
    db.session.commit()
    return ok_response()


@answers.post("/answer/delete")
def delete_answer(answer_id: int) -> Response:
    """Deletes an answer.

    This does not completely delete the answer but only removes user associations from it,
    so it is no longer visible in TIM.
    """
    a, doc_id = verify_answer_access(
        answer_id,
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
    log_info(
        f"{u.name} deleted answer {a.id} (of {seq_to_str(unames)}) in task {a.task_id}"
    )
    return ok_response()


@answers.post("/answer/deleteCollaborator")
def delete_answer_collab(answer_id: int, user_id: int) -> Response:
    """Deletes an answer collaborator."""
    a, doc_id = verify_answer_access(
        answer_id,
        get_current_user_object().id,
        default_view_ctx,
        require_teacher_if_not_own=True,
    )
    verify_teacher_access(get_doc_or_abort(doc_id))
    verify_admin()
    collab_to_remove = User.get_by_id(user_id)
    if not collab_to_remove:
        raise RouteException(f"Answer {answer_id} does not have collaborator {user_id}")
    a.users_all.remove(collab_to_remove)
    db.session.commit()
    u = get_current_user_object()
    log_info(
        f"{u.name} deleted collaborator {collab_to_remove.name} from answer {a.id} in task {a.task_id}"
    )
    return ok_response()


def points_to_float(points: str | float | None) -> float | None:
    if isinstance(points, float):
        return points
    if points == "":
        return None
    if points is None:
        return None
    return float(points)


def get_iframehtml_answer_impl(
    plugintype: str, task_id_ext: str, user_id: int, answer_id: int | None = None
) -> Response:
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
        raise RouteException(f"Task id error: {e}")
    if tid.doc_id is None:
        raise RouteException("Task ID must include document ID")
    d = get_doc_or_abort(tid.doc_id)
    d.document.insert_preamble_pars()

    ctx_user = User.get_by_id(user_id)
    if not ctx_user:
        raise RouteException("User not found")
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
        raise RouteException(f"Plugin type mismatch: {plugin.type} != {plugintype}")

    users = [ctx_user]

    answerinfo = get_existing_answers_info(users, tid)

    info = plugin.get_info(users, answerinfo.count)

    state = try_load_json(answer.content) if answer else None

    answer_call_data = {
        "markup": plugin.values,
        "state": state,
        "taskID": tid.doc_task,
        "info": info,
        "iframehtml": True,
    }

    vals = get_plug_vals(
        d, tid, user_context_with_logged_in(users[0]), default_view_ctx
    )
    if vals:
        answer_call_data["markup"]["fielddata"] = to_dict(vals)

    jsonresp = call_plugin_answer_and_parse(answer_call_data, plugintype)

    if "iframehtml" not in jsonresp:
        return json_response(
            {"error": 'The key "iframehtml" is missing in plugin response.'}, 400
        )
    result = jsonresp["iframehtml"]
    return result


def call_plugin_answer_and_parse(answer_call_data: dict, plugintype: str) -> dict:
    plugin_response = call_plugin_answer(plugintype, answer_call_data)
    try:
        jsonresp = json.loads(plugin_response)
    except ValueError as e:
        raise PluginException(
            "The plugin response was not a valid JSON string. The response was: "
            + plugin_response
        ) from e
    return jsonresp


@answers.get("/iframehtml/<plugintype>/<task_id_ext>/<int:user_id>/<int:answer_id>")
def get_iframehtml_answer(
    plugintype: str, task_id_ext: str, user_id: int, answer_id: int | None = None
) -> Response:
    return get_iframehtml_answer_impl(plugintype, task_id_ext, user_id, answer_id)


@answers.get("/iframehtml/<plugintype>/<task_id_ext>/<int:user_id>")
def get_iframehtml(plugintype: str, task_id_ext: str, user_id: int) -> Response:
    return get_iframehtml_answer_impl(plugintype, task_id_ext, user_id)


def get_useranswers_for_task(
    user: User, task_ids: list[TaskId], answer_map: dict[str, dict]
) -> list[Answer]:
    """
    Performs a query for latest valid answers by given user for given task
    Similar to :func:`timApp.plugin.pluginControl.get_answers` but without counting

    :param user: user
    :param task_ids: tasks to be queried
    :param answer_map: a dict where to add each taskID: Answer
    :return: {taskID: Answer}
    """
    col = func.max(Answer.id).label("col")
    sub = (
        user.answers.filter(valid_taskid_filter(task_ids))
        .add_columns(col)
        .with_entities(col)
        .group_by(Answer.task_id)
        .subquery()
    )
    answs: list[Answer] = Answer.query.join(sub, Answer.id == sub.c.col).all()
    for answer in answs:
        if len(answer.users_all) > 1:
            answer_map[answer.task_id] = answer.to_json()
        else:
            asd = answer.to_json()
            asd.pop("users")
            answer_map[answer.task_id] = asd
    return answs


def get_globals_for_tasks(task_ids: list[TaskId], answer_map: dict[str, dict]) -> None:
    col = func.max(Answer.id).label("col")
    cnt = func.count(Answer.id).label("cnt")
    sub = (
        valid_answers_query(task_ids)
        .add_columns(col, cnt)
        .with_entities(col, cnt)
        .group_by(Answer.task_id)
        .subquery()
    )
    answers_all: list[tuple[Answer, int]] = (
        Answer.query.join(sub, Answer.id == sub.c.col)
        .with_entities(Answer, sub.c.cnt)
        .all()
    )
    for answer, _ in answers_all:
        asd = answer.to_json()
        answer_map[answer.task_id] = asd


@answers.post("/userAnswersForTasks")
def get_answers_for_tasks(tasks: list[str], user_id: int) -> Response:
    """
    Route for getting latest valid answers for given user and list of tasks

    :return: {"answers": {taskID: Answer}, "userId": user_id}
    """
    user = User.get_by_id(user_id)
    if user is None:
        raise RouteException("Non-existent user")
    verify_logged_in()
    try:
        doc_map = {}
        tids = []
        gtids = []
        for task_id in tasks:
            tid = TaskId.parse(task_id)
            if tid.doc_id is None:
                raise RouteException(f"Task ID {task_id} is missing document ID.")
            if tid.doc_id not in doc_map:
                dib = get_doc_or_abort(tid.doc_id, f"Document {tid.doc_id} not found")
                if not dib.document.get_settings().peer_review():
                    verify_seeanswers_access(dib)
                doc_map[tid.doc_id] = dib.document
            if tid.is_global:
                gtids.append(tid)
            else:
                tids.append(tid)
        answer_map: dict[str, dict] = {}
        if tids:
            get_useranswers_for_task(user, tids, answer_map)
        if gtids:
            get_globals_for_tasks(gtids, answer_map)
        return json_response({"answers": answer_map, "userId": user_id})
    except Exception as e:
        raise RouteException(str(e))


@dataclass
class JsRunnerMarkupModel(GenericMarkupModel):
    fields: (
        list[str] | Missing
    ) = missing  # This is actually required, but we cannot use non-default arguments here...
    autoadd: bool | Missing = missing
    autoUpdateTables: bool | Missing = True
    creditField: str | Missing = missing
    defaultPoints: float | Missing = missing
    failGrade: str | Missing = missing
    fieldhelper: bool | Missing = missing
    gradeField: str | Missing = missing
    gradingScale: dict[Any, Any] | Missing = missing
    group: str | Missing = missing
    groups: list[str] | Missing = missing
    includeUsers: MembershipFilter | Missing = field(
        default=MembershipFilter.Current, metadata={"by_value": True}
    )
    selectIncludeUsers: bool = False
    paramFields: list[str] | Missing = missing
    postprogram: str | Missing = missing
    preprogram: str | Missing = missing
    program: str | Missing = missing
    overrideGrade: bool = False
    showInView: bool = False
    canOverwritePoints: bool = False
    confirmText: str | Missing = missing
    timeout: int | Missing = missing
    updateFields: list[str] | Missing = missing
    nextRunner: str | Missing = missing
    timeZoneDiff: int | Missing = missing
    peerReview: bool | Missing = missing

    @validates_schema(skip_on_field_errors=True)
    def validate_schema(self, data: dict, **_: dict) -> None:
        if data.get("fields") is None:
            raise ValidationError(
                "Missing data for required field.", field_name="fields"
            )
        if data.get("group") is None and data.get("groups") is None:
            raise ValidationError("Either group or groups must be given.")


JsRunnerMarkupSchema = class_schema(JsRunnerMarkupModel)


@dataclass
class JsRunnerInputModel:
    nosave: bool | Missing = missing
    userNames: list[str] | Missing = missing
    paramComps: dict[str, str] | Missing = missing
    includeUsers: MembershipFilter | Missing = field(
        default=missing, metadata={"by_value": True}
    )


@dataclass
class RefFrom:
    docId: int
    par: str


AnswerData = dict[str, Any]


@dataclass
class JsRunnerAnswerModel:
    input: JsRunnerInputModel
    ref_from: RefFrom | None = None
    abData: AnswerData | Missing = missing


JsRunnerAnswerSchema = class_schema(JsRunnerAnswerModel)


@answers.post("/multiSendEmail/<doc_id>")
def multisendemail(
    doc_id: int,
    rcpt: str,
    subject: str,
    msg: str,
    bccme: bool = False,
    replyall: bool = False,
) -> Response:
    d = get_doc_or_abort(doc_id)
    verify_teacher_access(d)
    mail_from = get_current_user_object().email
    bcc = ""
    if bccme:
        bcc = mail_from
    multi_send_email(
        rcpt=rcpt,
        subject=subject,
        msg=msg,
        mail_from=mail_from,
        reply_to=mail_from if not replyall else None,
        bcc=bcc,
        reply_all=replyall,
    )
    return ok_response()


# TODO: Fix plugins to generally send only specific answer type
# TODO: Write tests to ensure plugins send correct data type
InputAnswer = Union[AnswerData, list[Any], int, float, str]


# noinspection PyShadowingBuiltins
@answers.put("/<plugintype>/<task_id_ext>/answer")
def post_answer(
    plugintype: str,
    task_id_ext: str,
    input: InputAnswer,
    abData: dict[str, Any] = field(default_factory=dict),
    options: dict[str, Any] = field(default_factory=dict),
) -> Response:
    """Saves the answer submitted by user for a plugin in the database.

    :param plugintype: The type of the plugin, e.g. csPlugin.
     TODO: No longer needed because it is checked from the document block's plugin attribute.
    :param task_id_ext: The extended task id of the form "22.palidrome.par_id".
    :param input: Answer data to save
    :param options: Options to apply for answer saving
    :param abData: Data applied from answer browser
    """
    curr_user = get_current_user_object()
    verify_ip_ok(user=curr_user, msg="Answering is not allowed from this IP address.")
    return json_response(
        post_answer_impl(
            task_id_ext,
            input,
            abData,
            options,
            curr_user,
            get_urlmacros_from_request(),
            get_other_session_users_objs(),
            get_origin_from_request(),
        ).result
    )


@dataclass
class AnswerRouteResult:
    result: dict[str, Any]
    plugin: Plugin


def get_postanswer_plugin_etc(
    d: DocInfo,
    tid: TaskId,
    answer_browser_data: dict,
    curr_user: User,
    ctx_user: User | None,
    urlmacros: UrlMacros,
    users: list[User] | None,
    other_session_users: list[User],
    origin: OriginInfo | None,
    force_answer: bool,
) -> tuple[TaskAccessVerification, ExistingAnswersInfo, list[User], bool, bool, bool]:
    allow_save = True
    ask_new = False

    context_user = UserContext(ctx_user or curr_user, curr_user)
    view_ctx = ViewContext(ViewRoute.View, False, urlmacros=urlmacros, origin=origin)
    doc, found_plugin = get_plugin_from_request(d.document, tid, context_user, view_ctx)
    # newtask = found_plugin.value.get("newtask", False)
    newtask = found_plugin.is_new_task()
    assert found_plugin.task_id is not None
    if (
        found_plugin.known.useCurrentUser or found_plugin.task_id.is_global
    ):  # For plugins that is saved only for current user
        users = [curr_user]
    if users is None:
        users = [curr_user] + other_session_users
    if newtask:  # found_plugin.par.get_attr("seed") == "answernr":
        force_answer = True  # variable tasks are always saved even with same answer

    answerinfo = get_existing_answers_info(users, tid)
    answernr = -1
    answernr_to_user = None

    if newtask:  # only if task is with new random after every answer
        # Next three lines was there originally for stack, but let's see if we manage without them
        # if isinstance(answerdata, dict):
        # answernr = answerdata.get("answernr", -1)
        # ask_new = answerdata.get("askNew", False)
        if answernr < 0:
            answernr = answer_browser_data.get("answernr", -1)
        answernr_to_user = answernr
        if answernr < 0:
            answernr_to_user = answerinfo.count
            answernr = answerinfo.count
        if not ask_new:
            ask_new = answernr == answerinfo.count
            allow_save = ask_new

    try:
        vr = verify_task_access(
            d,
            tid,
            AccessType.view,
            TaskIdAccess.ReadWrite,
            context_user=context_user,
            view_ctx=view_ctx,
            allow_grace_period=True,
            answernr=answernr_to_user,
        )
    except (PluginException, TimDbException) as e:
        raise PluginException(str(e))
    return vr, answerinfo, users, allow_save, ask_new, force_answer


def post_answer_impl(
    task_id_ext: str,
    answerdata: InputAnswer,
    answer_browser_data: dict,
    answer_options: dict,
    curr_user: User,
    urlmacros: UrlMacros,
    other_session_users: list[User],
    origin: OriginInfo | None,
) -> AnswerRouteResult:
    receive_time = get_current_time()
    tid = TaskId.parse(task_id_ext)
    if tid.doc_id is None:
        raise PluginException(f"Task ID is missing document ID: {task_id_ext}")
    d = get_doc_or_abort(tid.doc_id)
    d.document.insert_preamble_pars()

    # It is rare but possible that the current user has been deleted (for example as the result of merging 2 accounts).
    # We assume it's the case here, so we clear the session and ask to log in again.
    if curr_user.is_deleted:
        clear_session()
        raise AccessDenied("Please refresh the page and log in again.")

    rights = get_user_rights_for_item(d, curr_user)
    if has_no_higher_right(d.document.get_settings().disable_answer(), rights):
        raise AccessDenied("Answering is disabled for this document.")

    force_answer = answer_options.get(
        "forceSave", False
    )  # Only used in feedback plugin.
    is_teacher_mode = answer_browser_data.get("teacher", False)
    save_teacher = answer_browser_data.get("saveTeacher", False)
    should_save_answer = answer_browser_data.get("saveAnswer", True) and tid.task_name

    if save_teacher:
        verify_teacher_access(d, user=curr_user)
    users = None

    ctx_user = None

    if is_teacher_mode:
        answer_id = answer_browser_data.get("answer_id", None)
        user_id = answer_browser_data.get("userId", None)

        if answer_id is not None:
            answer = Answer.query.get(answer_id)
            if not answer:
                raise PluginException(f"Answer not found: {answer_id}")
            expected_task_id = answer.task_id
            if expected_task_id != tid.doc_task:
                raise PluginException("Task ids did not match")

            # Later on, we may call users.append, but we don't want to modify the users of the existing
            # answer. Therefore, we make a copy of the user list so that SQLAlchemy no longer associates
            # the user list with the answer.
            users = list(answer.users_all)
            if not users:
                raise PluginException("No users found for the specified answer")
            # For now global fields use current user in browser
            # We set answerer user to be current user later so we ignore user mismatch in global case
            if user_id not in (u.id for u in users) and not tid.is_global:
                raise PluginException("userId is not associated with answer_id")
        elif (
            user_id and user_id != curr_user.id and False
        ):  # TODO: Vesa's hack to no need for belong teachers group
            teacher_group = UserGroup.get_teachers_group()
            if curr_user not in teacher_group.users:
                raise PluginException(
                    "Permission denied: you are not in teachers group."
                )
        if user_id:
            ctx_user = User.query.get(user_id)
            if not ctx_user:
                raise PluginException(f"User {user_id} not found")
            users = [ctx_user]  # TODO: Vesa's hack to save answer to student

    (
        vr,
        answerinfo,
        users,
        allow_save,
        ask_new,
        force_answer,
    ) = get_postanswer_plugin_etc(
        d,
        tid,
        answer_browser_data,
        curr_user,
        ctx_user,
        urlmacros,
        users,
        other_session_users,
        origin,
        force_answer,
    )
    plugin = vr.plugin

    if tid.is_points_ref:
        if not isinstance(answerdata, dict):
            raise PluginException("Invalid answer data format")
        return AnswerRouteResult(
            result=handle_points_ref(answerdata, curr_user, d, plugin.ptype, tid),
            plugin=plugin,
        )

    get_task = (
        isinstance(answerdata, dict)
        and answerdata.get("getTask", False)
        and plugin.ptype.can_give_task()
    )
    if not (should_save_answer or get_task) or is_teacher_mode:
        verify_seeanswers_access(d, user=curr_user)

    uploads = []

    if not curr_user.logged_in and not plugin.known.anonymous:
        raise RouteException("You must be logged in to answer this task.")

    if isinstance(answerdata, dict):
        file = answerdata.get("uploadedFile", "")
        trimmed_file = file.replace("/uploads/", "")
        type = answerdata.get("type", "")
        if trimmed_file and type == "upload":
            uploads = check_answerupload_file_accesses([trimmed_file], curr_user)
        files: list[dict] = answerdata.get("uploadedFiles", None)
        if files is not None:
            trimmed_files = [f["path"].replace("/uploads/", "") for f in files]
            uploads = check_answerupload_file_accesses(trimmed_files, curr_user)

    # Load old answers

    valid, _ = plugin.is_answer_valid(answerinfo.count, {})
    info = plugin.get_info(
        users,
        answerinfo.count,
        look_answer=is_teacher_mode and not save_teacher,
        valid=valid,
    )
    if ask_new:
        info["askNew"] = True

    # Get the newest answer (state). Only for logged in users.
    state = (
        try_load_json(answerinfo.latest_answer.content)
        if curr_user.logged_in and answerinfo.latest_answer
        else None
    )
    # TODO: get state from AB selected answer if new_task == true
    # TODO: Why state is needed for new answers?
    # TODO: Stack gets default for the field there???
    answer_id = answer_browser_data.get("answer_id", None)
    if answer_id is not None and curr_user.logged_in:
        answer = Answer.query.get(answer_id)
        if answer:
            state = try_load_json(answer.content)

    preprocessor = answer_call_preprocessors.get(plugin.type)
    if preprocessor:
        preprocessor(answerdata, curr_user, d, plugin)

    # print(json.dumps(answerdata))  # uncomment this to follow what answers are used in browser tests

    answer_call_data = {
        "markup": plugin.values,
        "state": state,
        "input": answerdata,
        "taskID": tid.doc_task,
        "info": info,
    }

    result = {}
    web = ""

    def set_postoutput(result: dict, output: Any | None, outputname: str) -> None:
        if not outputname or (not output and not preoutput):
            return
        parts = outputname.split(".")
        r = result
        lastkey = parts[-1]
        for p in parts[:-1]:
            if not p in r:
                r[p] = {}
            r = r[p]
        r[lastkey] = r.get(lastkey, "") + str(output)

    def add_value(result: dict, key: str, data: dict) -> None:
        value = data.get(key, None)
        if value is None:
            return
        if value.startswith("md:"):
            value = call_dumbo([value[3:]])[0]
        result[key] = result.get(key, "") + value

    def postprogram_result(data: dict, output: Any | None, outputname: str) -> None:
        result["web"] = data.get("web", web)
        add_value(result, "error", data)
        add_value(result, "feedback", data)
        add_value(result, "topfeedback", data)
        if output.startswith("md:"):
            output = call_dumbo([output[3:]])[0]
        set_postoutput(result, output, outputname)

    preoutput = ""
    preprogram = plugin.values.get("preprogram", None)
    if preprogram and plugin.type != "jsrunner":
        try:
            params = JsRunnerParams(
                code=preprogram,
                data=answer_call_data,
                error_text=PRE_POST_ERROR,
                caller="preprogram:",
            )
            answer_call_data, preoutput = jsrunner_run(params)
        except JsRunnerError as e:
            return AnswerRouteResult(
                result={"web": {"error": "Error in JavaScript: " + e.args[0]}},
                plugin=plugin,
            )

    if preoutput:
        postprogram_result(
            answer_call_data, preoutput, plugin.values.get("preoutput", "feedback")
        )

    jsonresp = call_plugin_answer_and_parse(answer_call_data, plugin.type)

    web = jsonresp.get("web")
    if web is None:
        raise PluginException(f"Got malformed response from plugin: {jsonresp}")
    result["web"] = web

    if "savedata" in jsonresp:
        siw = answer_call_data.get("markup", {}).get("showInView", False)
        overwrite_points = answer_call_data.get("markup", {}).get(
            "canOverwritePoints", False
        )
        add_group = None
        if plugin.type == "importData":
            add_group = plugin.values.get("addUsersToGroup")
        saveresult = save_fields(
            jsonresp,
            curr_user,
            d,
            allow_non_teacher=siw,
            add_users_to_group=add_group,
            overwrite_previous_points=overwrite_points,
        )

        # TODO: Could report the result to other plugins too.
        if plugin.type == "importData":
            web["fieldresult"] = saveresult

    def add_reply(obj: dict, key: str, run_markdown: bool = False) -> None:
        if key not in plugin.values:
            return
        text_to_add = plugin.values[key]
        if run_markdown:
            dumbo_result = call_dumbo([text_to_add])
            text_to_add = dumbo_result[0]
        obj[key] = text_to_add

    noupdate = False  # if true do not send new id

    resultmd = result["web"].get("md", None)
    if resultmd:
        result["web"]["md"] = call_dumbo([resultmd])[0]

    if not get_task:
        add_reply(result["web"], "-replyImage")
        add_reply(result["web"], "-replyMD", True)
        add_reply(result["web"], "-replyHTML")
    if "save" in jsonresp and not get_task:
        # TODO: RND_SEED: save used rnd_seed for this answer if answer is saved, found from par.get_rnd_seed()
        save_object = jsonresp["save"]
        tags = []
        tim_info = jsonresp.get("tim_info", {})
        if tim_info.get("noupdate", False):
            noupdate = True
        points = tim_info.get("points", None)
        multiplier = plugin.points_multiplier()
        if multiplier and points is not None:
            points *= plugin.points_multiplier()
        elif not multiplier:
            points = None
        # Save the new state
        try:
            tags = save_object["tags"]
        except (TypeError, KeyError):
            pass

        def get_name_and_val(name1: str, name2: str = "") -> tuple[str, Any]:
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

        postprogram_name, postprogram = get_name_and_val("postprogram", "postProgram")

        postlibraries_name, postlibraries = get_name_and_val("postlibraries")

        postoutput = plugin.values.get("postoutput", "feedback")

        if postprogram and postlibraries:
            libs = ""
            postlibraries_edit = plugin.values.get("postlibrariesEdit", {})
            libnr = 0
            for lib in postlibraries:
                try:
                    content = get_from_url(lib)
                    if content.startswith('{"error"'):
                        web["error"] += lib + "\n" + content
                        postprogram = ""
                        break
                    libedit = postlibraries_edit.get(libnr, None)
                    if libedit:
                        libdel = libedit.get("deleteAfter", None)
                        if libdel:
                            delpos = content.find(libdel)
                            if delpos >= 0:
                                content = content[0:delpos]
                    libs += content
                except Exception as ex:
                    web["error"] += lib + "\n" + str(ex)
                    postprogram = ""
                libnr += 1
            if postprogram:
                postprogram = libs + "\n//=== END LIBRARIES ===\n" + postprogram

        if (not is_teacher_mode and should_save_answer) or ("savedata" in jsonresp):
            is_valid, explanation = plugin.is_answer_valid(answerinfo.count, tim_info)
            if vr.is_invalid:
                is_valid = False
                explanation = vr.invalidate_reason
            elif vr.is_expired:
                fixed_time = (
                    receive_time
                    - d.document.get_settings().answer_submit_time_tolerance()
                )
                if fixed_time > (vr.access.accessible_to or maxdate):
                    is_valid = False
                    explanation = "Your view access to this document has expired, so this answer was saved but marked as invalid."
            points_given_by = None
            if answer_browser_data.get("giveCustomPoints"):
                try:
                    points = plugin.validate_points(answer_browser_data.get("points"))
                except PluginException as e:
                    result["error"] = str(e)
                else:
                    points_given_by = get_current_user_group()

            if postprogram:
                data = {
                    "users": [u.to_json() for u in users],
                    "answer_call_data": answer_call_data,
                    "points": points,
                    "save_object": save_object,
                    "tags": tags,
                    "is_valid": is_valid,
                    "force_answer": force_answer,
                    "error": "",
                    "web": web,
                }
                _, postprogram_fields = get_name_and_val(
                    "postprogram_fields", "postprogramFields"
                )
                if postprogram_fields and isinstance(postprogram_fields, list):
                    # TODO: Add support for multiple users in the same session
                    field_data, field_aliases, _, _ = get_fields_and_users(
                        postprogram_fields,
                        RequestedGroups(groups=[curr_user.get_personal_group()]),
                        d,
                        curr_user,
                        default_view_ctx,
                        access_option=GetFieldsAccess.from_bool(True),
                    )
                    # We only obtain current user's fields
                    user_fields = field_data[0]["fields"]
                    data["fields"] = {"values": user_fields, "names": field_aliases}
                try:
                    params = JsRunnerParams(
                        code=postprogram,
                        data=data,
                        error_text=PRE_POST_ERROR,
                        caller="postprogram:",
                    )
                    data, output = jsrunner_run(params)
                    points = data.get("points", points)
                    save_object = data.get("save_object", save_object)
                    is_valid = data.get("is_valid", is_valid)
                    force_answer = data.get("force_answer", force_answer)
                    allow_save = data.get("allow_save", allow_save)
                    postprogram_result(data, output, postoutput)
                except JsRunnerError as e:
                    return AnswerRouteResult(
                        result={"web": {"error": "Error in JavaScript: " + e.args[0]}},
                        plugin=plugin,
                    )

            if (points or save_object is not None or tags) and allow_save:
                a = save_answer(
                    users,
                    tid,
                    save_object,
                    points,
                    tags,
                    is_valid,
                    points_given_by,
                    force_answer,
                    plugintype=plugin.ptype,
                    max_content_len=current_app.config["MAX_ANSWER_CONTENT_SIZE"],
                    origin=origin,
                )
                result["savedNew"] = a.id if a else None
                if a:
                    notify_doc_watchers(
                        d,
                        "",
                        NotificationType.AnswerAdded,
                        plugin.par,
                        answer_number=answerinfo.count + 1,
                        curr_user=curr_user,
                    )
                    send_answer_backup_if_enabled(a)
            else:
                result["savedNew"] = None
            if noupdate:
                result["savedNew"] = None

            # Validity info can be different from error (e.g. answer can be valid but error is given by postprogram)
            result["valid"] = is_valid
            if not is_valid:
                result["error"] = explanation
        elif save_teacher:
            # Getting points from teacher ignores points automatically computed by the task
            # For now we accept task points since most of the time that's what a teacher might want
            # TODO: Accept teacher's points or task points depending on context (e.g. button)
            # points = answer_browser_data.get("points", points)
            points = points_to_float(points)
            a = save_answer(
                users,
                tid,
                save_object,
                points,
                tags,
                valid=True,
                points_given_by=get_current_user_group(),
                saver=curr_user,
                plugintype=plugin.ptype,
                max_content_len=current_app.config["MAX_ANSWER_CONTENT_SIZE"],
                origin=origin,
            )
            # TODO: Could call backup here too, but first we'd need to add support for saver in export/import.
            result["savedNew"] = a.id if a else None
        else:
            result["savedNew"] = None
            if postprogram:
                data = {
                    "users": [u.to_json() for u in users],
                    "answer_call_data": answer_call_data,
                    "points": points,
                    "save_object": save_object,
                    "tags": tags,
                    "is_valid": True,
                    "force_answer": force_answer,
                    "error": "",
                    "web": web,
                }
                try:
                    params = JsRunnerParams(
                        code=postprogram,
                        data=data,
                        error_text=PRE_POST_ERROR,
                        caller="postprogram:",
                    )
                    data, output = jsrunner_run(params)
                    points = data.get("points", points)
                    output += "\nPoints: " + str(points)
                    postprogram_result(data, output, postoutput)
                except JsRunnerError as e:
                    return AnswerRouteResult(
                        result={"web": {"error": "Error in JavaScript: " + e.args[0]}},
                        plugin=plugin,
                    )
        if result["savedNew"] is not None and uploads:
            # Associate this answer with the upload entries
            for upload in uploads:
                upload.answer_id = result["savedNew"]

    db.session.commit()

    for u in users:
        clear_doc_cache(d, u)

    try:
        if postprogram_name:
            result["web"]["markup"].pop(
                postprogram_name
            )  # TODO: stdy why someone puts markup here
    except:
        pass

    return AnswerRouteResult(result=result, plugin=plugin)


def check_answerupload_file_accesses(
    filelist: list[str], curr_user: User
) -> list[AnswerUpload]:
    """
    Checks user's access to uploads by checking access to the answers associated with them
    """
    uploads: list[AnswerUpload] = []
    doc_map = {}
    blocks = Block.query.filter(
        Block.description.in_(filelist) & (Block.type_id == BlockType.Upload.value)
    ).all()
    if len(blocks) != len(filelist):
        block_filelist = [b.description for b in blocks]
        for f in filelist:
            if f not in block_filelist:
                raise PluginException(f"Non-existent upload: {f}")
    for block in blocks:
        if not verify_view_access(block, user=curr_user, require=False):
            answerupload = block.answerupload.first()
            if answerupload is None:
                raise RouteException(
                    "Upload has not been associated with any answer; it should be re-uploaded"
                )
            answer = answerupload.answer
            if not answer:
                raise RouteException(
                    "Upload has not been associated with any answer; it should be re-uploaded"
                )
            if curr_user not in answer.users_all:
                did = TaskId.parse(answer.task_id).doc_id
                if did not in doc_map:
                    d = get_doc_or_abort(did)
                    verify_teacher_access(
                        d, message="You don't have permission to touch this file."
                    )
                    doc_map[did] = d
        uploads.append(block.answerupload.first())
    return uploads


def preprocess_jsrunner_answer(
    answerdata: AnswerData, curr_user: User, d: DocInfo, plugin: Plugin
) -> None:
    """Executed before the actual jsrunner answer route is called.
    This is required to fetch the requested data from the database."""

    s = JsRunnerMarkupSchema()
    runnermarkup: JsRunnerMarkupModel = s.load(plugin.values)
    runner_req: JsRunnerAnswerModel = JsRunnerAnswerSchema().load({"input": answerdata})
    groupnames = runnermarkup.groups
    if groupnames is missing:
        groupnames = [runnermarkup.group]
    requested_groups = RequestedGroups.from_name_list(groupnames)
    not_found_groups = sorted(
        list(
            set(groupnames)
            - {g.name for g in requested_groups.groups}
            - {ALL_ANSWERED_WILDCARD}
        )
    )  # Ensure the wildcard is removed
    if not_found_groups:
        raise PluginException(
            f'The following groups were not found: {", ".join(not_found_groups)}'
        )
    if (
        runner_req.input.paramComps
    ):  # TODO: add paramComps to the interface, so no need to manipulate source code
        preprg = runnermarkup.preprogram or ""
        plugin.values[
            "preprogram"
        ] = f"gtools.params = {json.dumps(runner_req.input.paramComps)};\n{preprg}"
    siw = runnermarkup.showInView
    markup_include_opt = value_or_default(
        runnermarkup.includeUsers, MembershipFilter.Current
    )
    if (
        not runnermarkup.selectIncludeUsers
        and isinstance(runner_req.input.includeUsers, MembershipFilter)
        and markup_include_opt != runner_req.input.includeUsers
    ):
        raise AccessDenied("Not allowed to select includeUsers option.")

    ensure_grade_and_credit(runnermarkup.program, runnermarkup.fields)

    answerdata["data"], answerdata["aliases"], _, _ = get_fields_and_users(
        runnermarkup.fields,
        requested_groups,
        d,
        curr_user,
        default_view_ctx,
        access_option=GetFieldsAccess.from_bool(siw),
        member_filter_type=value_or_default(
            runner_req.input.includeUsers, markup_include_opt
        ),
        user_filter=User.name.in_(runner_req.input.userNames)
        if runner_req.input.userNames
        else None,
    )
    if runnermarkup.peerReview:
        if not curr_user.has_teacher_access(d):
            raise AccessDenied("Teacher access required to browse all peer reviews")
        answerdata["peerreviews"] = get_reviews_for_document(d)
    else:
        answerdata["peerreviews"] = []

    answerdata["testvelps"] = get_annotations_with_comments_in_document(
        curr_user, d, False
    )
    answerdata.pop(
        "paramComps", None
    )  # This isn't needed by jsrunner server, so don't send it.
    # plugin.values['timeZoneDiff'] = 3
    tzd = plugin.values.get("timeZoneDiff", None)
    if tzd is None:
        localtz = local_timezone
        localoffset = localtz.utcoffset(datetime.now())
        tzd = localoffset.total_seconds() / 3600
        plugin.values["timeZoneDiff"] = tzd
    if runnermarkup.program is missing:
        raise PluginException("Attribute 'program' is required.")


def ensure_grade_and_credit(prg: str, flds: list[str]) -> None:
    if not prg:
        return
    if prg.find("grade") >= 0 or prg.find("Grade"):  # add grade to fields if missing
        grade_found = False
        credit_found = False
        for fld in flds:
            if fld.startswith("grade"):
                grade_found = True
            if fld.startswith("credit"):
                credit_found = True
            if grade_found and credit_found:
                break
        if not grade_found:
            flds.append("grade")
        if not credit_found:
            flds.append("credit")


answer_call_preprocessors: dict[
    str, Callable[[AnswerData, User, DocInfo, Plugin], None]
] = {
    "jsrunner": preprocess_jsrunner_answer,
}


def handle_points_ref(
    answerdata: AnswerData,
    curr_user: User,
    d: DocInfo,
    ptype: PluginTypeBase,
    tid: TaskId,
) -> dict:
    verify_teacher_access(d, user=curr_user)
    given_points = answerdata.get(ptype.get_content_field_name())
    if given_points is not None:
        try:
            given_points = float(given_points)
        except ValueError:
            raise RouteException("Points must be a number.")
    a = (
        curr_user.answers.filter_by(task_id=tid.doc_task)
        .order_by(Answer.id.desc())
        .first()
    )
    if a:
        a.points = given_points
        s = None
    else:
        a = Answer(
            content=json.dumps({ptype.get_content_field_name(): ""}),
            points=given_points,
            task_id=tid.doc_task,
            users_all=[curr_user],
            valid=True,
        )
        db.session.add(a)
        db.session.flush()
        s = a.id
    db.session.commit()
    return {"savedNew": s, "web": {"result": "points saved"}}


class JsrunnerGroups(TypedDict, total=False):
    set: dict[str, list[int]]
    add: dict[str, list[int]]
    remove: dict[str, list[int]]


MAX_GROUPS_PER_CALL = 10


@dataclass
class UserGroupMembersState:
    before: set[int]
    after: set[int]


def handle_jsrunner_groups(groupdata: JsrunnerGroups | None, curr_user: User) -> None:
    if not groupdata:
        return
    groups_created = 0
    group_members_state = {}
    for op, group_set in groupdata.items():
        for name, uids in group_set.items():
            ug = UserGroup.get_by_name(name)
            if not ug:
                if op == "set":
                    if groups_created >= MAX_GROUPS_PER_CALL:
                        raise RouteException(
                            f"Maximum of {MAX_GROUPS_PER_CALL} groups can be created per one jsrunner run.",
                        )
                    ug, _ = do_create_group(name)
                    groups_created += 1
                else:
                    raise RouteException(f"Group does not exist: {name}")
            else:
                verify_group_edit_access(ug, curr_user)
            if ug not in group_members_state:
                current_state = {um.user_id for um in ug.memberships_sel}
                group_members_state[ug] = UserGroupMembersState(
                    before=current_state, after=set(current_state)
                )
            users: list[User] = User.query.filter(User.id.in_(uids)).all()
            found_user_ids = {u.id for u in users}
            missing_ids = set(uids) - found_user_ids
            if missing_ids:
                raise RouteException(f"Users not found: {missing_ids}")
            if op == "set":
                ug.memberships_sel = [
                    UserGroupMember(user=u, adder=curr_user) for u in users
                ]
                group_members_state[ug].after = {
                    um.user.id for um in ug.memberships_sel
                }
            elif op == "add":
                # Add by hand because memberships_sel is not updated in add_to_group
                after_set = group_members_state[ug].after
                for u in users:
                    u.add_to_group(ug, added_by=curr_user, sync_mailing_lists=False)
                    after_set.add(u.id)
            elif op == "remove":
                ug.memberships_sel = [
                    ugm
                    for ugm in ug.memberships_sel
                    if ugm.user_id not in found_user_ids
                ]
                group_members_state[ug].after = {
                    um.user.id for um in ug.memberships_sel
                }
            else:
                raise RouteException(f"Unexpected group operation: {op}")

    diffs = {
        group.id: UserGroupDiff(
            add_user_ids=list(diff.after - diff.before),
            remove_user_ids=list(diff.before - diff.after),
        )
        for group, diff in group_members_state.items()
    }

    # JSRunner group actions are permanent unlike with user UI
    sync_usergroup_messagelist_members(diffs, permanent_delete=True)


class UserFieldEntry(TypedDict):
    user: int
    fields: dict[str, str]


def create_missing_users(
    users: list[MissingUser],
) -> tuple[list[UserFieldEntry], list[User]]:
    created_users = []
    for mu in users:
        ui = mu.user
        if ui.email is not None:
            # A+ may give users with invalid mails like '6128@localhost'. Just skip over those.
            if ui.email.endswith("@localhost"):
                continue
            if not is_valid_email(ui.email):
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
        fields.append({"user": u.id, "fields": missing_u.fields})
    return fields, created_users


MissingUserSchema = class_schema(MissingUser)


@dataclass
class FieldSaveResult:
    users_created: list[User] = field(default_factory=list)
    users_missing: list[UserInfo] = field(default_factory=list)
    fields_changed: int = 0
    fields_unchanged: int = 0
    fields_ignored: int = 0


class FieldSaveUserEntry(TypedDict):
    user: int
    fields: dict[str, str]


class FieldSaveRequest(TypedDict, total=False):
    savedata: list[FieldSaveUserEntry] | None
    ignoreMissing: bool | None
    allowMissing: bool | None
    createMissingUsers: bool | None
    missingUsers: Any | None
    groups: JsrunnerGroups | None


def verify_user_create_right(curr_user: User) -> None:
    if curr_user.is_admin:
        return
    user_creators = UserGroup.get_user_creator_group()
    if user_creators not in curr_user.groups:
        raise AccessDenied("You do not have permission to create users.")


def save_fields(
    jsonresp: FieldSaveRequest,
    curr_user: User,
    current_doc: DocInfo | None = None,
    allow_non_teacher: bool = False,
    add_users_to_group: str | None = None,
    overwrite_previous_points: bool = False,
) -> FieldSaveResult:
    save_obj = jsonresp.get("savedata")
    ignore_missing = jsonresp.get("ignoreMissing", False)
    allow_missing = jsonresp.get("allowMissing", False)
    ignore_fields = {}
    handle_jsrunner_groups(jsonresp.get("groups"), curr_user)
    missing_users = jsonresp.get("missingUsers")
    saveresult = FieldSaveResult()
    if save_obj is None:
        save_obj = []
    if missing_users:
        m_users: list[MissingUser] = MissingUserSchema().load(missing_users, many=True)
        if jsonresp.get("createMissingUsers"):
            verify_user_create_right(curr_user)
            new_fields, users = create_missing_users(m_users)
            save_obj += new_fields
            saveresult.users_created = users
        else:
            saveresult.users_missing = [mu.user for mu in m_users]
    if not save_obj:
        return saveresult
    tasks = set()
    doc_map: dict[int, DocInfo] = {}
    user_map: dict[int, User] = {
        u.id: u
        for u in User.query.filter(User.id.in_(x["user"] for x in save_obj)).all()
    }

    # We need this separate "add_users_to_group" parameter because the plugin may have reported missing users.
    # They are created above, so the plugin cannot report them with "groups" in jsonresp because the user IDs are not
    # known until now.
    if add_users_to_group:
        handle_jsrunner_groups(
            {"add": {add_users_to_group: [k for k in user_map.keys()]}}, curr_user
        )

    for item in save_obj:
        task_u = item["fields"]
        for tid in task_u.keys():
            tasks.add(tid)
            try:
                id_num = TaskId.parse(
                    tid,
                    require_doc_id=False,
                    allow_block_hint=False,
                    allow_custom_field=True,
                )
            except PluginException:
                raise RouteException(f'Invalid task name: {tid.split(".")[1]}')
            if not id_num.doc_id:
                raise RouteException(f"Doc id missing: {tid}")
            if id_num.doc_id not in doc_map:
                doc_map[id_num.doc_id] = get_doc_or_abort(id_num.doc_id)
    task_content_name_map = {}
    for task in tasks:
        t_id = TaskId.parse(
            task, require_doc_id=True, allow_block_hint=False, allow_custom_field=True
        )
        if ignore_fields.get(t_id.doc_task, False):
            continue
        dib = doc_map[t_id.doc_id]
        # TODO: Return case-specific abort messages
        if not (
            curr_user.has_teacher_access(dib)
            or (allow_non_teacher and t_id.doc_id == current_doc.id)
            or (
                curr_user.has_view_access(dib)
                and dib.document.get_own_settings().get(
                    "allow_external_jsrunner", False
                )
            )
        ):
            raise AccessDenied(f"Missing teacher access for document {dib.id}")
        try:
            vr = verify_task_access(
                dib,
                t_id,
                AccessType.view,
                TaskIdAccess.ReadWrite,
                UserContext.from_one_user(curr_user),
                default_view_ctx,
            )
            plugin = vr.plugin
        except TaskNotFoundException as e:
            if not allow_missing:
                if ignore_missing:
                    ignore_fields[t_id.doc_task] = True
                    continue
                raise RouteException(str(e))
            plugin = PluginType.resolve(
                "textfield"
            )  # assuming textfield type for fields that are not in the document
        except (PluginException, TimDbException) as e:
            raise RouteException(str(e))

        # TODO this 'if' seems unnecessary
        if t_id.task_name in ("grade", "credit", "completionDate"):
            task_content_name_map[task] = "c"
            continue

        if t_id.field and t_id.field != "points" and t_id.field != "styles":
            if plugin.type == "numericfield" or plugin.type == "textfield":
                if t_id.field != plugin.get_content_field_name():
                    raise RouteException(
                        f"Error saving to {task}: {t_id.field} is not an accepted field."
                    )
            task_content_name_map[task] = t_id.field
        else:
            task_content_name_map[task] = plugin.get_content_field_name()

    parsed_task_ids = {
        key: TaskId.parse(
            key, require_doc_id=True, allow_block_hint=False, allow_custom_field=True
        )
        for user in save_obj
        for key in user["fields"].keys()
    }
    sq = (
        Answer.query.filter(
            Answer.task_id.in_(
                [tid.doc_task for tid in parsed_task_ids.values() if not tid.is_global]
            )
        )
        .join(User, Answer.users)
        .filter(User.id.in_(user_map.keys()))
        .group_by(User.id, Answer.task_id)
        .with_entities(func.max(Answer.id).label("aid"), User.id.label("uid"))
        .subquery()
    )
    datas = (
        Answer.query.join(sq, Answer.id == sq.c.aid)
        .with_entities(sq.c.uid, Answer)
        .all()
    )
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
        u_id = user["user"]
        u = user_map.get(u_id)
        if not u:
            raise RouteException(f"User id {u_id} not found")
        user_fields = user["fields"]
        task_map: DefaultDict[str, dict[str, Any]] = defaultdict(dict)
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
            points_changed = False
            if an:
                points = an.points
                content = json.loads(an.content)
            lastfield = "c"
            for field, value in contents.items():
                lastfield = field
                if field == "points":
                    if value == "":
                        value = None
                    else:
                        try:
                            value = float(value)
                        except ValueError:
                            raise RouteException(
                                f"Value {value} is not valid point value for task {task_id.task_name}"
                            )
                    if points != value:
                        points_changed = True
                    points = value
                elif field == "styles":
                    if isinstance(value, str):
                        try:
                            value = json.loads(value or "null")
                        except json.decoder.JSONDecodeError:
                            raise RouteException(
                                f"Value {value} is not valid style syntax for task {task_id.task_name}"
                            )
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
                        c_field = task_content_name_map[f"{task_id.doc_task}.{field}"]
                        if c_field not in content:
                            content[c_field] = None
                elif (
                    field == "JSSTRING"
                ):  # TODO check if this should be ALL!  No this is for settings using string
                    if not an or json.dumps(content) != value:
                        new_answer = True
                    content = json.loads(value)  # TODO: shoud this be inside if
                else:
                    if not an or content.get(field, "") != value:
                        new_answer = True
                    content[field] = value

            if points_changed:
                if an and not new_answer and overwrite_previous_points:
                    an.points = points
                else:
                    new_answer = True

            if not new_answer:
                saveresult.fields_unchanged += 1
                continue
            if not content:
                content[task_content_name_map[f"{task_id.doc_task}.{lastfield}"]] = None
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


def get_global_answers(parsed_task_ids: dict[str, TaskId]) -> list[Answer]:
    sq2 = (
        Answer.query.filter(
            Answer.task_id.in_(
                [tid.doc_task for tid in parsed_task_ids.values() if tid.is_global]
            )
        )
        .group_by(Answer.task_id)
        .with_entities(func.max(Answer.id).label("aid"))
        .subquery()
    )
    global_datas = (
        Answer.query.join(sq2, Answer.id == sq2.c.aid).with_entities(Answer).all()
    )
    return global_datas


def get_hidden_name(user_id: str) -> str:
    return "Student %d" % user_id


def should_hide_name(d: DocInfo, user: User, model_u: User | None) -> bool:
    # return True
    # return not user.has_teacher_access(d) and user.id != get_current_user_id()
    return user.id != get_current_user_id() and user != model_u


def maybe_hide_name(d: DocInfo, u: User, model_u: User | None) -> None:
    if should_hide_name(d, u, model_u):
        # NOTE! To anonymize user, do NOT assign to u's real_name, name, etc. attributes here (or anywhere else either)
        # because it is
        #  1) dangerous (the anonymization would be persisted if db.session.commit() was called after the assignment)
        #  2) not necessary because the hiding is done in User.to_json method.
        u.hide_name = True


@answers.get("/taskinfo/<task_id>")
def get_task_info(task_id) -> Response:
    try:
        user_ctx = user_context_with_logged_in(None)
        plugin, d = Plugin.from_task_id(
            task_id, user_ctx=user_ctx, view_ctx=default_view_ctx
        )
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
        raise RouteException(str(e))
    return json_response(tim_vars)


def find_tim_vars(plugin: Plugin) -> dict:
    tim_vars = {
        "maxPoints": plugin.max_points(),
        "userMin": plugin.user_min_points(),
        "userMax": plugin.user_max_points(),
        "showPoints": plugin.show_points(),
        "deadline": plugin.deadline(),
        "starttime": plugin.starttime(),
        "answerLimit": plugin.answer_limit(),
        "triesText": plugin.known.tries_text(),
        "pointsText": plugin.known.points_text(),
        "buttonNewTask": plugin.values.get("buttonNewTask", None),
    }
    if plugin.is_new_task():
        tim_vars["newtask"] = True
    return tim_vars


def hide_points(a: Answer) -> dict:
    j = a.to_json()
    j["points"] = None

    # TODO: Hack for csPlugin
    c = a.content_as_json
    if isinstance(c, dict):
        c.pop("points", None)
        j["content"] = json.dumps(c)

    if a.points is not None:
        j["points_hidden"] = True
    return j


@answers.get("/exportAnswers/<path:doc_path>")
def export_answers(doc_path: str) -> Response:
    d = DocEntry.find_by_path(doc_path, try_translation=False)
    if not d:
        raise RouteException("Document not found")
    verify_teacher_access(d)
    answer_list: list[tuple[Answer, str]] = (
        Answer.query.filter(Answer.task_id.startswith(f"{d.id}."))
        .join(User, Answer.users)
        .with_entities(Answer, User.email)
        .all()
    )
    return json_response(
        [
            {
                "email": email,
                "content": a.content,
                "valid": a.valid,
                "points": a.points,
                "time": a.answered_on,
                "task": a.task_name,
                "doc": doc_path,
            }
            for a, email in answer_list
        ]
    )


@answers.post("/importAnswers")
def import_answers(
    answers: list[ExportedAnswer],
    allow_missing_users: bool = False,
    doc_map: dict[str, str] = field(default_factory=dict),
) -> Response:
    verify_admin()
    doc_paths = {doc_map.get(a.doc, a.doc) for a in answers}
    docs = DocEntry.query.filter(DocEntry.name.in_(doc_paths)).all()
    doc_path_map = {d.path: d for d in docs}
    missing_docs = doc_paths - set(doc_path_map)
    if missing_docs:
        raise RouteException(f"Some documents not found: {missing_docs}")
    for d in docs:
        verify_teacher_access(d)
    filter_cond = Answer.task_id.startswith(f"{docs[0].id}.")
    for d in docs[1:]:
        filter_cond |= Answer.task_id.startswith(f"{d.id}.")
    existing_answers: list[tuple[Answer, str]] = (
        Answer.query.filter(filter_cond)
        .join(User, Answer.users)
        .with_entities(Answer, User.email)
        .all()
    )
    existing_set = {
        (a.parsed_task_id.doc_id, a.task_name, a.answered_on, a.valid, a.points, email)
        for a, email in existing_answers
    }
    dupes = 0
    users = {
        u.email: u
        for u in User.query.filter(User.email.in_([a.email for a in answers])).all()
    }
    requested_users = {a.email for a in answers}
    missing_users = requested_users - set(users.keys())
    if missing_users and not allow_missing_users:
        raise RouteException(f"Email(s) not found: {seq_to_str(list(missing_users))}")
    answers.sort(key=lambda a: a.time)
    all_imported = []
    for a in answers:
        doc_id = doc_path_map[doc_map.get(a.doc, a.doc)].id
        if (doc_id, a.task, a.time, a.valid, a.points, a.email) not in existing_set:
            u = users.get(a.email)
            if not u:
                if not allow_missing_users:
                    raise Exception("Missing user should have been reported earlier")
                continue
            imported_answer = Answer(
                task_id=f"{doc_id}.{a.task}",
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
            raise Exception(
                "Import bug: Answer ids were in different order than answer timestamps. Imported nothing."
            )

    db.session.commit()
    return json_response(
        {
            "imported": len(all_imported),
            "skipped_duplicates": dupes,
            "missing_users": list(missing_users),
        }
    )


@answers.get("/getAnswers/<task_id>/<int:user_id>")
def get_answers(task_id: str, user_id: int) -> Response:
    verify_logged_in()
    try:
        tid = TaskId.parse(task_id)
    except PluginException as e:
        raise RouteException(str(e))
    d = get_doc_or_abort(tid.doc_id)
    user = User.get_by_id(user_id)
    if user is None:
        raise RouteException("Non-existent user")
    curr_user = get_current_user_object()
    if user_id != get_current_user_id():
        if not verify_seeanswers_access(d, require=False):
            if not is_peerreview_enabled(d):
                raise AccessDenied()
            if not has_review_access(d, curr_user, None, user):
                raise AccessDenied()

    elif d.document.get_settings().get("need_view_for_answers", False):
        verify_view_access(d)
    user_answers: list[Answer] = user.get_answers_for_task(tid.doc_task).all()
    user_context = user_context_with_logged_in(user)
    try:
        p = find_plugin_from_document(d.document, tid, user_context, default_view_ctx)
    except TaskNotFoundException:
        p = None
    if hide_names_in_teacher(d, context_user=user):
        model_u = User.get_model_answer_user()
        for answer in user_answers:
            for u in answer.users_all:
                maybe_hide_name(d, u, model_u)
    if p and not p.known.show_points() and not curr_user.has_teacher_access(d):
        user_answers = list(map(hide_points, user_answers))
    return json_response(user_answers)


@answers.get("/allDocumentAnswersPlain/<path:doc_path>", model=AllAnswersOptions)
def get_document_answers(doc_path: str, options: AllAnswersOptions) -> Response:
    d = DocEntry.find_by_path(doc_path, fallback_to_id=True)
    pars = d.document.get_dereferenced_paragraphs(default_view_ctx)
    task_ids, _, _ = find_task_ids(
        pars, default_view_ctx, user_context_with_logged_in(None)
    )
    return get_all_answers_list_plain(task_ids, options)


@answers.get("/allAnswersPlain/<task_id>", model=AllAnswersOptions)
def get_all_answers_plain(task_id: str, options: AllAnswersOptions) -> Response:
    return get_all_answers_list_plain([TaskId.parse(task_id)], options)


def get_all_answers_list_plain(
    task_ids: list[TaskId], options: AllAnswersOptions
) -> Response:
    all_answers = get_all_answers_as_list(task_ids, options)
    if options.format == FormatOptions.JSON:
        return json_response(all_answers)
    jointext = "\n"
    print_answers = (
        options.print == AnswerPrintOptions.ALL
        or options.print == AnswerPrintOptions.ANSWERS
    )
    if print_answers:
        jointext = "\n\n----------------------------------------------------------------------------------\n"
    text = jointext.join(all_answers)
    return Response(text, mimetype="text/plain")


def get_all_answers_as_list(
    task_ids: list[TaskId], options: AllAnswersOptions
) -> list[str]:
    verify_logged_in()
    if not task_ids:
        return []
    doc_ids = set()
    d = None
    for tid in task_ids:
        doc_ids.add(tid.doc_id)
        d = get_doc_or_abort(tid.doc_id)
        # Require at least seeanswers access to view all answers
        verify_seeanswers_access(d)

    # TODO: Integrate directly into AllAnswerOptions
    options.consent = get_consent_opt()
    options.period_from, options.period_to = get_answer_period(
        task_ids, doc_ids, options
    )

    # Check only for the first document since we require seeanswers for all
    if (
        d
        and (not has_teacher_access(d) or hide_names_in_teacher(d))
        and options.name
        not in (
            NameOptions.ANON,
            NameOptions.PSEUDO,
        )
    ):
        options.name = NameOptions.ANON

    if options.name == NameOptions.PSEUDO:
        if not options.salt:
            raise RouteException("Missing salt for generating pseudonyms")
        if len(options.salt) < 10:
            raise RouteException(
                "For optimal results, use at least 10 characters for the hash"
            )

    return get_all_answers(task_ids, options)


class GraphData(TypedDict):
    data: list[str | float | None]
    labels: list[str]


@dataclass
class FieldInfo:
    data: UserFields
    aliases: dict[str, str]
    fieldnames: list[str]
    graphdata: GraphData


def get_plug_vals(
    doc: DocInfo, tid: TaskId, user_ctx: UserContext, view_ctx: ViewContext
) -> FieldInfo | None:
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
    df = data[0]["fields"]
    da = []
    for fn in field_names:
        da.append(df.get(fn, 0))
    return FieldInfo(
        data=df,
        aliases=aliases,
        fieldnames=field_names,
        graphdata={"data": da, "labels": field_names},
    )


@answers.get("/jsframe/userChange/<task_id>/<user_id>")
def get_jsframe_data(task_id: str, user_id: str) -> Response:
    """
    TODO: check proper rights
    """
    tid = TaskId.parse(task_id)
    doc = get_doc_or_abort(tid.doc_id)
    # verify_seeanswers_access(doc)
    user = User.get_by_id(user_id)
    curr_user = get_current_user_object()
    try:
        vals = get_plug_vals(
            doc, tid, UserContext(user=user, logged_user=curr_user), default_view_ctx
        )
        return json_response(vals)
    except Exception as e:
        raise RouteException(str(e))
        # return json_response({})


@answers.get("/getState")
def get_state(
    user_id: int,
    answer_id: int | None = None,
    par_id: str | None = None,
    doc_id: int | None = None,
    review: bool = False,
    task_id: str | None = None,
    answernr: int | None = None,
    ask_new: bool | None = False,
) -> Response:
    answer = None
    user = User.get_by_id(user_id)
    if user is None:
        raise RouteException("Non-existent user")
    view_ctx = ViewContext(ViewRoute.View, False, origin=get_origin_from_request())
    if answer_id:
        answer = Answer.query.get(answer_id)
        if not answer:
            raise RouteException("Non-existent answer")
        tid = TaskId.parse(answer.task_id)
        d = get_doc_or_abort(tid.doc_id)
        doc_id = d.id
        if not has_review_access(d, get_current_user_object(), None, user):
            try:
                answer, doc_id = verify_answer_access(
                    answer_id,
                    user_id,
                    view_ctx,
                    allow_grace_period=True,
                )
            except PluginException as e:
                raise RouteException(str(e))
        doc = Document(doc_id)
        tid = TaskId.parse(answer.task_id)
    elif task_id:
        tid = TaskId.parse(task_id)
        d = get_doc_or_abort(tid.doc_id)
        if get_current_user_id() != user_id and not has_review_access(
            d, get_current_user_object(), None, user
        ):
            verify_seeanswers_access(d)
        else:
            verify_view_access(d)
        doc = d.document
    else:
        raise RouteException("Missing answer ID or task ID")

    doc.insert_preamble_pars()
    if par_id:
        tid.maybe_set_hint(par_id)

    user_ctx = user_context_with_logged_in(user)
    try:
        doc, plug = get_plugin_from_request(
            doc, task_id=tid, u=user_ctx, view_ctx=view_ctx
        )
    except PluginException as e:
        raise RouteException(str(e))
    plug.par.answer_nr = answernr
    plug.par.ask_new = ask_new
    block = plug.par

    def deref() -> list[DocParagraph]:
        return dereference_pars([block], context_doc=doc, view_ctx=view_ctx)

    presult = pluginify(
        doc,
        deref(),
        user_ctx,
        view_ctx,
        custom_answer=answer,
        task_id=task_id,
        do_lazy=NEVERLAZY,
        pluginwrap=PluginWrap.Nothing,
    )
    plug = presult.custom_answer_plugin
    html = plug.get_final_output()
    if review:
        block.prepared_par = None
        presult2 = pluginify(
            doc,
            deref(),
            user_ctx,
            view_ctx,
            custom_answer=answer,
            task_id=task_id,
            do_lazy=NEVERLAZY,
            review=review,
            pluginwrap=PluginWrap.Nothing,
        )
        rplug = presult2.custom_answer_plugin
        rhtml = rplug.get_final_output()
        return json_response({"html": html, "reviewHtml": rhtml})
    else:
        return json_response({"html": html, "reviewHtml": None})


def verify_answer_access(
    answer_id: int,
    user_id: int,
    view_ctx: ViewContext,
    require_teacher_if_not_own: bool = False,
    required_task_access_level: TaskIdAccess = TaskIdAccess.ReadOnly,
    allow_grace_period: bool = False,
) -> tuple[Answer, int]:
    answer: Answer = Answer.query.get(answer_id)
    if answer is None:
        raise RouteException("Non-existent answer")
    tid = TaskId.parse(answer.task_id)
    assert tid.doc_id is not None

    if tid.is_global:
        return answer, tid.doc_id

    d = get_doc_or_abort(tid.doc_id)
    d.document.insert_preamble_pars()

    if verify_teacher_access(d, require=False):
        return answer, tid.doc_id

    user_ctx = user_context_with_logged_in(None)
    if user_id != get_current_user_id() or not logged_in():
        if require_teacher_if_not_own:
            verify_task_access(
                d,
                tid,
                AccessType.teacher,
                required_task_access_level,
                user_ctx,
                view_ctx,
            )
        else:
            verify_task_access(
                d,
                tid,
                AccessType.see_answers,
                required_task_access_level,
                user_ctx,
                view_ctx,
            )
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
            raise AccessDenied("You don't have access to this answer.")
    return answer, tid.doc_id


@answers.get("/getTaskUsers/<task_id>")
def get_task_users(task_id: str) -> Response:
    tid = TaskId.parse(task_id)
    if tid.doc_id is None:
        raise RouteException("Task is missing document ID")
    d = get_doc_or_abort(tid.doc_id)
    if not verify_seeanswers_access(d, require=False):
        curr_user = get_current_user_object()
        if not is_peerreview_enabled(d):
            raise AccessDenied()
        reviews = get_reviews_for_user(d, curr_user)
        if not reviews:
            raise AccessDenied()
        users = list(r.reviewable for r in reviews if r.task_name == tid.task_name)
    else:
        usergroup = request.args.get("group")
        q = (
            User.query.options(lazyload(User.groups))
            .join(Answer, User.answers)
            .filter_by(task_id=task_id)
            .order_by(User.real_name.asc())
            .distinct()
        )
        if usergroup is not None:
            q = q.join(UserGroup, User.groups).filter(UserGroup.name.in_([usergroup]))
        users = q.all()
    if hide_names_in_teacher(d):
        model_u = User.get_model_answer_user()
        for user in users:
            maybe_hide_name(d, user, model_u)
    return json_response(users)


@answers.get("/renameAnswers/<old_name>/<new_name>/<path:doc_path>")
def rename_answers(old_name: str, new_name: str, doc_path: str) -> Response:
    d = DocEntry.find_by_path(doc_path, fallback_to_id=True)
    if not d:
        raise NotExist()
    verify_manage_access(d)
    force = get_option(request, "force", False)
    for n in (old_name, new_name):
        if not re.fullmatch("[a-zA-Z0-9_-]+", n):
            raise RouteException(f"Invalid task name: {n}")
    conflicts = Answer.query.filter_by(task_id=f"{d.id}.{new_name}").count()
    if conflicts > 0 and not force:
        raise RouteException(
            f"The new name conflicts with {conflicts} other answers with the same task name."
        )
    answers_to_rename = Answer.query.filter_by(task_id=f"{d.id}.{old_name}").all()
    for a in answers_to_rename:
        a.task_id = f"{d.id}.{new_name}"
    db.session.commit()
    return json_response({"modified": len(answers_to_rename), "conflicts": conflicts})


@answers.get("/unlockTask")
def unlock_task(task_id: str) -> Response:
    tid = TaskId.parse(task_id)
    if tid.doc_id is None:
        raise RouteException(f"Task ID is missing document: {task_id}")
    d = get_doc_or_abort(tid.doc_id)
    verify_view_access(d)
    doc = d.document
    current_user = get_current_user_object()
    view_ctx = ViewContext(ViewRoute.View, False, origin=get_origin_from_request())
    user_ctx = user_context_with_logged_in(current_user)
    try:
        doc, plug = get_plugin_from_request(
            doc, task_id=tid, u=user_ctx, view_ctx=view_ctx
        )
    except PluginException as e:
        raise RouteException(str(e))
    access_duration = plug.known.accessDuration
    if not isinstance(access_duration, int):
        raise RouteException("Task is not a timed task.")
    b = TaskBlock.get_by_task(tid.doc_task)
    ba = None
    if not b:
        b = insert_task_block(task_id=tid.doc_task, owner_groups=d.owners)
    else:
        ba = BlockAccess.query.filter_by(
            block_id=b.id,
            type=AccessType.view.value,
            usergroup_id=current_user.get_personal_group().id,
        ).first()
    if not ba:
        time_now = get_current_time()
        expire_time = time_now + timedelta(seconds=access_duration)
        grant_access(
            current_user.get_personal_group(),
            b.block,
            AccessType.view,
            accessible_from=time_now,
            accessible_to=expire_time,
        )
        db.session.commit()
    else:
        expire_time = ba.accessible_to
    return json_response({"end_time": expire_time})
