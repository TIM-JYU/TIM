import ipaddress
import json
from dataclasses import dataclass
from datetime import datetime, timezone, timedelta
from pathlib import Path

from flask import flash, current_app
from flask import request, g
from marshmallow import missing
from sqlalchemy import inspect, select

from timApp.answer.answer import Answer
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.auth.session.util import (
    SessionExpired,
    session_has_access,
)
from timApp.auth.sessioninfo import (
    logged_in,
    get_other_users_as_list,
    get_current_user_group,
    get_current_user_object,
    get_current_user_id,
    user_context_with_logged_in,
)
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document, dereference_pars
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewContext, OriginInfo, ViewRoute
from timApp.folder.folder import Folder
from timApp.item.block import BlockType, Block
from timApp.item.item import Item
from timApp.notification.send_email import send_email
from timApp.peerreview.util.peerreview_utils import is_peerreview_enabled
from timApp.plugin.plugin import (
    Plugin,
    find_plugin_from_document,
    maybe_get_plugin_from_par,
)
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.taskid import TaskId, TaskIdAccess
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db, run_sql
from timApp.user.user import ItemOrBlock, User
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import grant_access
from timApp.util.flask.requesthelper import get_option, RouteException, NotExist
from timApp.util.logger import log_warning
from timApp.util.utils import get_current_time
from tim_common.markupmodels import AccessField, PreviousTaskInfo


def get_doc_or_abort(doc_id: int, msg: str | None = None) -> DocInfo:
    d = DocEntry.find_by_id(doc_id)
    if not d:
        raise NotExist(msg or "Document not found")
    return d


def get_item_or_abort(item_id: int):
    i = Item.find_by_id(item_id)
    if not i:
        raise NotExist("Item not found")
    return i


def get_folder_or_abort(folder_id: int):
    f = Folder.get_by_id(folder_id)
    if not f:
        raise NotExist("Folder not found")
    return f


def verify_admin(require: bool = True, user: User | None = None) -> bool:
    if not check_admin_access(user=user):
        if require:
            raise AccessDenied("This action requires administrative rights.")
        return False
    return True


def verify_admin_no_ret(require=True):
    verify_admin(require)


def verify_edit_access(
    b: ItemOrBlock,
    require=True,
    message=None,
    check_duration=False,
    check_parents=False,
    user=None,
):
    return verify_access(
        b,
        AccessType.edit,
        require=require,
        message=message,
        check_duration=check_duration,
        check_parents=check_parents,
        user=user,
    )


def verify_manage_access(
    b: ItemOrBlock,
    require=True,
    message=None,
    check_duration=False,
    check_parents=False,
):
    return verify_access(
        b,
        AccessType.manage,
        require=require,
        message=message,
        check_duration=check_duration,
        check_parents=check_parents,
    )


def verify_access(
    b: ItemOrBlock,
    access_type: AccessType,
    require: bool = True,
    message: str | None = None,
    check_duration=False,
    check_parents=False,
    grace_period=timedelta(seconds=0),
    user: User | None = None,
):
    u = user or get_current_user_object()
    has_access = u.has_access(b, access_type, grace_period)
    if not has_access and check_parents:
        # Only uploaded files and images have a parent so far.
        for x in (u.has_access(p, access_type, grace_period) for p in b.parents):
            if x:
                has_access = x
                break
    return abort_if_not_access_and_required(
        has_access,
        u,
        b,
        access_type,
        require,
        message,
        check_duration=check_duration,
    )


def check_inherited_right(
    u: User,
    b: ItemOrBlock,
    access_type: AccessType,
    grace_period: timedelta,
) -> BlockAccess | None:
    has_access = None
    is_docinfo = isinstance(b, DocInfo)
    if is_docinfo or (isinstance(b, Block) and b.type_id == BlockType.Document.value):
        doc = b if is_docinfo else DocEntry.find_by_id(b.id)
        if not doc:
            return None
        if doc.path_without_lang in current_app.config["INHERIT_FOLDER_RIGHTS_DOCS"]:
            has_access = u.has_access(b.parent, access_type, grace_period)
    return has_access


def get_inherited_right_blocks(b: ItemOrBlock) -> list[Block]:
    inherited_right_docs = current_app.config["INHERIT_FOLDER_RIGHTS_DOCS"]
    if not inherited_right_docs:
        return []
    is_docinfo = isinstance(b, DocInfo)
    if is_docinfo or (isinstance(b, Block) and b.type_id == BlockType.Document.value):
        doc = b if is_docinfo else DocEntry.find_by_id(b.id)
        if not doc:
            return []
        if doc.path_without_lang in inherited_right_docs:
            return [b.parent.id]
    return []


def verify_view_access(
    b: ItemOrBlock,
    require=True,
    message=None,
    check_duration=False,
    check_parents=False,
    user=None,
):
    return verify_access(
        b,
        AccessType.view,
        require=require,
        message=message,
        check_duration=check_duration,
        check_parents=check_parents,
        user=user,
    )


def verify_teacher_access(
    b: ItemOrBlock,
    require=True,
    message=None,
    check_duration=False,
    check_parents=False,
    user=None,
):
    return verify_access(
        b,
        AccessType.teacher,
        require=require,
        message=message,
        check_duration=check_duration,
        check_parents=check_parents,
        user=user,
    )


def verify_copy_access(
    b: ItemOrBlock,
    require=True,
    message=None,
    check_duration=False,
    check_parents=False,
):
    return verify_access(
        b,
        AccessType.copy,
        require=require,
        message=message,
        check_duration=check_duration,
        check_parents=check_parents,
    )


def verify_seeanswers_access(
    b: ItemOrBlock,
    require=True,
    message=None,
    check_duration=False,
    check_parents=False,
    user=None,
):
    return verify_access(
        b,
        AccessType.see_answers,
        require=require,
        message=message,
        check_duration=check_duration,
        check_parents=check_parents,
        user=user,
    )


class ItemLockedException(Exception):
    """The exception that is raised (in /view route) when a user attempts to access an item for which he has a duration
    access that has not yet begun or the access has expired."""

    def __init__(
        self,
        access: BlockAccess,
        msg: str | None = None,
        next_doc: DocInfo | None = None,
    ):
        self.access = access
        self.msg = msg
        self.next_doc = next_doc


def abort_if_not_access_and_required(
    access_obj: BlockAccess,
    user: User,
    block: ItemOrBlock,
    access_type: AccessType,
    require=True,
    message=None,
    check_duration=False,
):
    if access_obj:
        return access_obj

    if not session_has_access(block, user):
        raise SessionExpired()

    block_ids = [block.id, *get_inherited_right_blocks(block)]

    if check_duration:
        ba = (
            run_sql(
                select(BlockAccess)
                .filter(BlockAccess.block_id.in_(block_ids))
                .filter_by(
                    type=access_type.value,
                    usergroup_id=get_current_user_group(),
                )
                .limit(1)
            )
            .scalars()
            .first()
        )
        if ba is None:
            ba_group: BlockAccess = (
                run_sql(
                    select(BlockAccess)
                    .filter(BlockAccess.block_id.in_(block_ids))
                    .filter_by(type=access_type.value)
                    .filter(
                        BlockAccess.usergroup_id.in_(
                            get_current_user_object()
                            .get_groups(include_expired=False)
                            .with_only_columns(UserGroup.id)
                        )
                    )
                    .limit(1)
                )
                .scalars()
                .first()
            )
            if ba_group is not None:
                ba = BlockAccess(
                    block_id=ba_group.block_id,
                    type=ba_group.type,
                    usergroup_id=get_current_user_group(),
                    accessible_from=ba_group.accessible_from,
                    accessible_to=ba_group.accessible_to,
                    duration=ba_group.duration,
                    duration_from=ba_group.duration_from,
                    duration_to=ba_group.duration_to,
                    require_confirm=ba_group.require_confirm,
                )
        if ba is not None:
            unlock = get_option(request, "unlock", False)
            if unlock and ba.unlockable:
                ba.accessible_from = get_current_time()
                ba.accessible_to = ba.accessible_from + ba.duration_now

                # if this is a group duration, it means we created a personal BlockAccess instance above, so we
                # need to add it
                if inspect(ba).transient:
                    db.session.add(ba)
                db.session.commit()  # TODO ensure nothing else gets committed than the above
                if isinstance(block, Item):
                    targets = current_app.config["DIST_RIGHTS_UNLOCK_TARGETS"]
                    curr_targets = targets.get(block.path)
                    if curr_targets:
                        from timApp.tim_celery import send_unlock_op

                        send_unlock_op.delay(
                            get_current_user_object().email, curr_targets
                        )
                flash("Item was unlocked successfully.")
                if ba.accessible_from < ba.accessible_to:
                    return ba
                else:
                    raise ItemLockedException(ba)
            else:
                # Chaining: If the right to this document has expired, check if there is a document that should
                # get auto-confirmed.
                if ba.expired:
                    msg, next_doc = maybe_auto_confirm(block)
                else:
                    msg, next_doc = None, None
                raise ItemLockedException(ba, msg, next_doc)
    if require:
        raise AccessDenied(
            message or "Sorry, you don't have permission to use this resource."
        )
    return None


def maybe_auto_confirm(block: ItemOrBlock):
    msg = None
    next_doc = None
    if isinstance(block, DocInfo):
        s = block.document.get_settings()
        ac = s.auto_confirm()
        if isinstance(ac, str):
            target = None
            if block.lang_id:
                target = DocEntry.find_by_path(f"{ac}/{block.lang_id}")
            if not target:
                target = DocEntry.find_by_path(ac)
            if not target:
                flash("auto_confirm document does not exist")
            else:
                t_s = target.document.get_settings()
                asc = t_s.allow_self_confirm_from()
                allowed = set()
                if isinstance(asc, str):
                    allowed.add(asc)
                elif isinstance(asc, list):
                    for a in asc:
                        if isinstance(a, str):
                            allowed.add(a)
                aliases = {a.path for a in block.aliases}
                if allowed & aliases:
                    try:
                        acc = get_single_view_access(target, allow_group=True)
                    except AccessDenied as e:
                        flash("Cannot get access to target document: " + str(e))
                    else:
                        next_doc = target
                        msg = s.expire_next_doc_message()
                        acc.do_confirm()
                        db.session.commit()
                else:
                    flash("Document is not authorized to auto-confirm rights")
    return msg, next_doc


def has_view_access(b: ItemOrBlock):
    u = get_current_user_object()
    return check_inherited_right(
        u, b, AccessType.view, grace_period=timedelta(seconds=0)
    ) or u.has_view_access(b)


def has_edit_access(b: ItemOrBlock):
    return get_current_user_object().has_edit_access(b)


def has_comment_right(b: ItemOrBlock):
    return has_view_access(b) if logged_in() else None


def has_read_marking_right(b: ItemOrBlock):
    return has_view_access(b) if logged_in() else None


def has_teacher_access(b: ItemOrBlock):
    return get_current_user_object().has_teacher_access(b)


def has_manage_access(b: ItemOrBlock):
    return get_current_user_object().has_manage_access(b)


def has_seeanswers_access(b: ItemOrBlock):
    return get_current_user_object().has_seeanswers_access(b)


def has_ownership(b: ItemOrBlock):
    return get_current_user_object().has_ownership(b)


def check_admin_access(block_id=None, user=None) -> BlockAccess | None:
    curr_user = user
    if curr_user is None:
        curr_user = get_current_user_object()
    if curr_user.is_admin:
        return BlockAccess(
            block_id=block_id,
            accessible_from=datetime.min.replace(tzinfo=timezone.utc),
            type=AccessType.owner.value,
            usergroup_id=curr_user.get_personal_group().id,
        )
    return None


def verify_logged_in() -> None:
    if not logged_in():
        raise AccessDenied("You have to be logged in to perform this action.")


def verify_ownership(
    b: ItemOrBlock,
    require=True,
    message=None,
    check_duration=False,
    check_parents=False,
):
    return verify_access(
        b,
        AccessType.owner,
        require=require,
        message=message,
        check_duration=check_duration,
        check_parents=check_parents,
    )


def verify_read_marking_right(b: ItemOrBlock):
    if not has_read_marking_right(b):
        raise AccessDenied()


def verify_comment_right(b: ItemOrBlock):
    if not has_comment_right(b):
        raise AccessDenied()


def get_plugin_from_request(
    doc: Document,
    task_id: TaskId,
    u: UserContext,
    view_ctx: ViewContext,
    answernr: int | None = None,
) -> tuple[Document, Plugin]:
    assert doc.doc_id == task_id.doc_id
    orig_info = view_ctx.origin
    orig_doc_id, orig_par_id = (
        (orig_info.doc_id, orig_info.par_id) if orig_info else (None, None)
    )
    plug = find_plugin_from_document(doc, task_id, u, view_ctx)
    par_id = plug.par.get_id()
    if orig_doc_id is None or orig_par_id is None:
        if not doc.has_paragraph(par_id):
            raise RouteException("Plugin not found")
        return doc, plug
    if orig_doc_id != doc.doc_id:
        orig_doc = Document(orig_doc_id)
    else:
        orig_doc = doc
    orig_doc.insert_preamble_pars()
    try:
        orig_par = orig_doc.get_paragraph(orig_par_id)
    except TimDbException:
        raise PluginException(f"Plugin paragraph not found: {orig_par_id}")
    pars = dereference_pars([orig_par], context_doc=orig_doc, view_ctx=view_ctx)
    ctx_doc = (
        orig_doc
        if (
            not orig_doc.get_docinfo().is_original_translation
            and orig_par.is_translation()
        )
        else doc
    )
    for p in pars:
        if p.get_id() == par_id:
            if answernr is not None:
                p.answer_nr = answernr
            return ctx_doc, maybe_get_plugin_from_par(p, task_id, u, view_ctx)
    return doc, plug


def get_origin_from_request() -> OriginInfo | None:
    ref_from = (request.get_json(silent=True) or {}).get("ref_from") or {}
    doc_id = ref_from.get(
        "docId", get_option(request, "ref_from_doc_id", default=None, cast=int)
    )
    par_id = ref_from.get("par", get_option(request, "ref_from_par_id", default=None))
    return (
        OriginInfo(doc_id=doc_id, par_id=par_id)
        if doc_id is not None and par_id is not None
        else None
    )


@dataclass
class TaskAccessVerification:
    plugin: Plugin
    access: BlockAccess
    is_expired: bool  # True if grace period is allowed and the current time is within the grace period.
    is_invalid: bool = (
        False  # user has access, but any possible answers are deemed invalid
    )
    invalidate_reason: str | None = None


def verify_task_access(
    d: DocInfo,
    task_id: TaskId,
    access_type: AccessType,
    required_task_access_level: TaskIdAccess,
    context_user: UserContext,
    view_ctx: ViewContext,
    allow_grace_period: bool = False,
    answernr: int | None = None,
) -> TaskAccessVerification:
    assert d.id == task_id.doc_id
    doc, found_plugin = get_plugin_from_request(
        d.document, task_id, context_user, view_ctx, answernr
    )
    access = verify_access(
        doc.get_docinfo(), access_type, require=False, user=context_user.logged_user
    )
    is_expired = False
    if not access:
        if not allow_grace_period:
            raise AccessDenied(f"No access for task {d.id}.{task_id.task_name}")
        access = verify_access(
            doc.get_docinfo(),
            access_type,
            grace_period=doc.get_settings().answer_grace_period(),
        )
        is_expired = True
    ctx_user_teacher_access = context_user.logged_user.has_teacher_access(
        doc.get_docinfo()
    )

    def is_readonly() -> bool:
        if found_plugin.known.readonly not in (missing, None):
            return found_plugin.known.readonly
        return found_plugin.task_id.access_specifier == TaskIdAccess.ReadOnly

    if (
        is_readonly()
        and required_task_access_level == TaskIdAccess.ReadWrite
        and not ctx_user_teacher_access
    ):
        raise AccessDenied(
            f"This task/field {task_id.task_name} is readonly and thus only writable for teachers."
        )
    is_invalid = False
    invalidate_reason = None
    if (
        found_plugin.is_timed()
        and not ctx_user_teacher_access
        and required_task_access_level == TaskIdAccess.ReadWrite
    ):
        found_plugin.set_access_end_for_user(user=context_user.logged_user)
        if found_plugin.access_end_for_user:
            if found_plugin.access_end_for_user < get_current_time():
                is_invalid = True
                invalidate_reason = "Your access to this task has expired."
        else:
            is_invalid = True
            invalidate_reason = "You haven't started this task yet."
    if (
        not is_invalid
        and (found_plugin.known.modelAnswer and found_plugin.known.modelAnswer.lock)
        and required_task_access_level == TaskIdAccess.ReadWrite
    ):
        found_plugin.set_access_end_for_user(user=context_user.logged_user)
        if found_plugin.access_end_for_user:
            if found_plugin.access_end_for_user < get_current_time():
                is_invalid = True
                invalidate_reason = (
                    found_plugin.known.modelAnswer.lockedAnswerMessage
                    or "You have already downloaded the model answer and can not save new answers anymore."
                )
    if (
        not is_invalid
        and found_plugin.known.accessField
        and required_task_access_level == TaskIdAccess.ReadWrite
    ):
        is_invalid = not check_access_from_field(
            context_user, found_plugin.known.accessField, found_plugin.task_id
        )
        if is_invalid:
            invalidate_reason = (
                found_plugin.known.accessField.error
                or "You have expired your access to this task."
            )
    if (
        not is_invalid
        and found_plugin.known.previousTask
        and not ctx_user_teacher_access
        and required_task_access_level == TaskIdAccess.ReadWrite
    ):
        valid, reason = check_access_from_previous_task(
            context_user, view_ctx, found_plugin.known.previousTask, d.document
        )
        if not valid:
            is_invalid = True
            invalidate_reason = reason

    return TaskAccessVerification(
        plugin=found_plugin,
        access=access,
        is_expired=is_expired,
        is_invalid=is_invalid,
        invalidate_reason=invalidate_reason,
    )


def verify_answer_access(
    answer_id: int,
    user_id: int,
    view_ctx: ViewContext,
    require_teacher_if_not_own: bool = False,
    required_task_access_level: TaskIdAccess = TaskIdAccess.ReadOnly,
    allow_grace_period: bool = False,
) -> tuple[Answer, int]:
    answer: Answer = db.session.get(Answer, answer_id)
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


def verify_route_access(
    doc_info: DocInfo, route: ViewRoute, require: bool = True
) -> BlockAccess | None:
    match route:
        case ViewRoute.Teacher:
            return verify_teacher_access(doc_info, require=require)
        case ViewRoute.Answers:
            return verify_seeanswers_access(doc_info, require=require)
        case ViewRoute.Review:
            if not is_peerreview_enabled(doc_info):
                return None
            return verify_view_access(doc_info, require=require, check_duration=True)
        case ViewRoute.View | ViewRoute.Lecture | ViewRoute.Slide | ViewRoute.ShowSlide | ViewRoute.Velp:
            return verify_view_access(doc_info, require=require, check_duration=True)
        case _:
            raise ValueError(f"Unknown route {route}")


def check_access_from_field(
    context_user: UserContext, access_field: AccessField, task_id: TaskId
):
    current_user = context_user.logged_user
    field_to_check = access_field.field
    tid = TaskId.parse(field_to_check, require_doc_id=False)
    if not tid.doc_id:
        tid = TaskId.parse(str(task_id.doc_id) + "." + field_to_check)
    prev = (
        current_user.answers.filter_by(task_id=tid.doc_task, valid=True)
        .order_by(Answer.id.desc())
        .first()
    )
    if not prev:
        return True
    try:
        value = int(json.loads(prev.content)["c"])
        if value >= access_field.limit:
            return False
    except (json.decoder.JSONDecodeError, ValueError):
        pass
    return True


def check_access_from_previous_task(
    context_user: UserContext,
    view_ctx: ViewContext,
    prerequisite_info: PreviousTaskInfo,
    current_doc: Document,
) -> tuple[bool, str | None]:
    prerequisite_taskid = TaskId.parse(prerequisite_info.taskid, require_doc_id=False)
    prequisite_taskid_doc = current_doc
    if not prerequisite_taskid.doc_id:
        prerequisite_taskid = TaskId.parse(
            str(current_doc.doc_id) + "." + prerequisite_info.taskid
        )
        prequisite_taskid_doc = get_doc_or_abort(prerequisite_taskid.doc_id).document
    if prerequisite_info.requireLock:
        _, prerequisite_plugin = get_plugin_from_request(
            prequisite_taskid_doc, prerequisite_taskid, context_user, view_ctx
        )
        prerequisite_plugin.set_access_end_for_user(user=context_user.logged_user)
        if (
            not prerequisite_plugin.access_end_for_user
            or prerequisite_plugin.access_end_for_user >= get_current_time()
        ):
            return False, "You need to lock the prerequisite task first"
    if prerequisite_info.count:
        current_user = context_user.logged_user
        current_count = current_user.get_answers_for_task(
            prerequisite_taskid.doc_task
        ).count()
        if current_count < prerequisite_info.count:
            return (
                False,
                f"You need to attempt the prerequisite task at least {prerequisite_info.count} times first",
            )
    return True, None


def grant_access_to_session_users(i: ItemOrBlock):
    for u in get_other_users_as_list():
        grant_access(
            User.get_by_id(int(u["id"])).get_personal_group(), i, AccessType.manage
        )


def reset_request_access_cache():
    del_attr_if_exists(g, "manageable")
    del_attr_if_exists(g, "viewable")
    del_attr_if_exists(g, "teachable")
    del_attr_if_exists(g, "see_answers")
    del_attr_if_exists(g, "owned")
    del_attr_if_exists(g, "editable")


def del_attr_if_exists(obj, attr_name: str):
    if hasattr(obj, attr_name):
        delattr(obj, attr_name)


def can_see_par_source(u: User, p: DocParagraph):
    d = p.doc.get_docinfo()
    if u.has_copy_access(d):
        return True
    if not u.has_view_access(d):
        return False
    if not p.is_plugin() and not p.has_plugins():
        return True
    return False


class AccessDenied(Exception):
    pass


def get_single_view_access(i: Item, allow_group: bool = False) -> BlockAccess:
    u = get_current_user_object()
    accs: list[BlockAccess] = (
        u.get_personal_group().accesses.filter_by(block_id=i.id).all()
    )
    if not accs and allow_group:
        lig = UserGroup.get_logged_in_group()
        ugroups = {gid for gid, in u.groups_dyn.with_entities(UserGroup.id).all()}
        for (ugid, act), acc in i.block.accesses.items():
            if (ugid == lig.id or ugid in ugroups) and act == AccessType.view.value:
                new_acc = u.grant_access(
                    i,
                    AccessType.view,
                    accessible_to=acc.accessible_to,
                    duration_from=acc.duration_from,
                    duration_to=acc.duration_to,
                    duration=acc.duration,
                )
                accs.append(new_acc)
    if not accs:
        raise AccessDenied(f"No access found for {i.path}.")
    if len(accs) > 1:
        raise AccessDenied(f"Multiple accesses found for {i.path}.")
    acc = accs[0]
    if acc.access_type != AccessType.view:
        raise AccessDenied(
            f"Access type is {acc.access_type.name} instead of view in {i.path}."
        )
    if acc.expired:
        raise AccessDenied(f"Access is already expired for {i.path}.")
    return acc


def is_allowed_ip() -> bool:
    ip_allowlist = current_app.config["IP_BLOCK_ALLOWLIST"]
    if ip_allowlist is None:
        return True
    return any(
        ipaddress.ip_address(request.remote_addr) in network for network in ip_allowlist
    )


def is_blocked_ip() -> bool:
    ip = request.remote_addr
    fp = get_ipblocklist_path()
    try:
        with fp.open("r") as f:
            ip_blocklist = f.read()
    except FileNotFoundError:
        return False
    ip_lines = ip_blocklist.splitlines()
    return ip in ip_lines


def get_ipblocklist_path() -> Path:
    return Path(current_app.config["FILES_PATH"]) / "ipblocklist"


def verify_ip_ok(user: User | None, msg: str | None = None) -> bool:
    if (not user or not user.is_admin) and not is_allowed_ip():
        username = user.name if user else "Anonymous"
        cfg = current_app.config
        ip_block_log_only = cfg["IP_BLOCK_LOG_ONLY"]
        is_in_blocklist = is_blocked_ip()
        should_block = not ip_block_log_only or is_in_blocklist
        blocked_or_allowed = "blocked" if should_block else "allowed"
        log_warning(
            f"IP {request.remote_addr} outside allowlist ({username}) - {blocked_or_allowed}"
        )
        msg_end = "Request was blocked." if should_block else "Request was allowed."
        reply_tos = [cfg["ERROR_EMAIL"]]
        if user and user.email:
            reply_tos.append(user.email)
        if not is_in_blocklist:
            send_email(
                rcpt=cfg["ERROR_EMAIL"],
                subject=f'{cfg["TIM_HOST"]}: '
                f"IP {request.remote_addr} outside allowlist ({username}) "
                f"- {blocked_or_allowed}",
                mail_from=cfg["WUFF_EMAIL"],
                reply_to=",".join(reply_tos),
                msg=f"""
IP {request.remote_addr} was outside allowlist.

URL: {request.url}

User: {username}

{msg_end}
                """.strip(),
            )
        if should_block:
            raise AccessDenied(msg or cfg["IP_BLOCK_ROUTE_MESSAGE"] or "IPNotAllowed")
        return False
    else:
        return True


def verify_user_create_right(curr_user: User) -> None:
    if curr_user.is_admin:
        return
    user_creators = UserGroup.get_user_creator_group()
    if user_creators not in curr_user.groups:
        raise AccessDenied("You do not have permission to create users.")
