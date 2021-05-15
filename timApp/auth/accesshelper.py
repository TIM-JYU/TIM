import ipaddress
from dataclasses import dataclass
from datetime import datetime, timezone, timedelta
from pathlib import Path
from typing import Optional, Tuple, List

from flask import flash, current_app
from flask import request, g
from sqlalchemy import inspect

from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.auth.sessioninfo import logged_in, get_other_users_as_list, \
    get_current_user_group, get_current_user_object
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document, dereference_pars
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewContext, OriginInfo
from timApp.folder.folder import Folder
from timApp.item.item import Item, ItemBase
from timApp.notification.send_email import send_email
from timApp.plugin.plugin import Plugin, find_plugin_from_document, maybe_get_plugin_from_par
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.taskid import TaskId, TaskIdAccess
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.user.user import ItemOrBlock, User
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import grant_access
from timApp.util.flask.requesthelper import get_option, RouteException, NotExist
from timApp.util.logger import log_warning
from timApp.util.utils import get_current_time


def get_doc_or_abort(doc_id: int, msg: Optional[str] = None) -> DocInfo:
    d = DocEntry.find_by_id(doc_id)
    if not d:
        raise NotExist(msg or 'Document not found')
    return d


def get_item_or_abort(item_id: int):
    i = Item.find_by_id(item_id)
    if not i:
        raise NotExist('Item not found')
    return i


def get_folder_or_abort(folder_id: int):
    f = Folder.get_by_id(folder_id)
    if not f:
        raise NotExist('Folder not found')
    return f


def verify_admin(require: bool=True, user: Optional[User]=None) -> bool:
    if not check_admin_access(user=user):
        if require:
            raise AccessDenied('This action requires administrative rights.')
        return False
    return True


def verify_admin_no_ret(require=True):
    verify_admin(require)


def verify_edit_access(b: ItemOrBlock, require=True, message=None, check_duration=False, check_parents=False):
    return verify_access(b, AccessType.edit, require=require, message=message, check_duration=check_duration, check_parents=check_parents)


def verify_manage_access(b: ItemOrBlock, require=True, message=None, check_duration=False, check_parents=False):
    return verify_access(b, AccessType.manage, require=require, message=message, check_duration=check_duration, check_parents=check_parents)


def verify_access(
        b: ItemOrBlock,
        access_type: AccessType,
        require: bool = True,
        message: Optional[str] = None,
        check_duration=False,
        check_parents=False,
        grace_period=timedelta(seconds=0),
        user: Optional[User] = None,
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
        b,
        access_type,
        require,
        message,
        check_duration=check_duration,
    )


def verify_view_access(b: ItemOrBlock, require=True, message=None, check_duration=False, check_parents=False, user=None):
    return verify_access(b, AccessType.view, require=require, message=message, check_duration=check_duration, check_parents=check_parents, user=user)


def verify_teacher_access(b: ItemOrBlock, require=True, message=None, check_duration=False, check_parents=False, user=None):
    return verify_access(b, AccessType.teacher, require=require, message=message, check_duration=check_duration, check_parents=check_parents, user=user)


def verify_copy_access(b: ItemOrBlock, require=True, message=None, check_duration=False, check_parents=False):
    return verify_access(b, AccessType.copy, require=require, message=message, check_duration=check_duration, check_parents=check_parents)


def verify_seeanswers_access(b: ItemOrBlock, require=True, message=None, check_duration=False, check_parents=False, user=None):
    return verify_access(b, AccessType.see_answers, require=require, message=message, check_duration=check_duration, check_parents=check_parents, user=user)


class ItemLockedException(Exception):
    """The exception that is raised (in /view route) when a user attempts to access an item for which he has a duration
    access that has not yet begun or the access has expired."""

    def __init__(
            self,
            access: BlockAccess,
            msg: Optional[str]=None,
            next_doc: Optional[DocInfo]=None,
    ):
        self.access = access
        self.msg = msg
        self.next_doc = next_doc


def abort_if_not_access_and_required(access_obj: BlockAccess,
                                     block: ItemOrBlock,
                                     access_type: AccessType,
                                     require=True,
                                     message=None,
                                     check_duration=False):
    if access_obj:
        return access_obj
    if check_duration:
        ba = BlockAccess.query.filter_by(block_id=block.id,
                                         type=access_type.value,
                                         usergroup_id=get_current_user_group()).first()
        if ba is None:
            ba_group: BlockAccess = BlockAccess.query.filter_by(block_id=block.id,
                                                                type=access_type.value).filter(
                BlockAccess.usergroup_id.in_(get_current_user_object().get_groups().with_entities(UserGroup.id))
            ).first()
            if ba_group is not None:
                ba = BlockAccess(block_id=ba_group.block_id,
                                 type=ba_group.type,
                                 usergroup_id=get_current_user_group(),
                                 accessible_from=ba_group.accessible_from,
                                 accessible_to=ba_group.accessible_to,
                                 duration=ba_group.duration,
                                 duration_from=ba_group.duration_from,
                                 duration_to=ba_group.duration_to)
        if ba is not None:
            unlock = get_option(request, 'unlock', False)
            if unlock and ba.unlockable:
                ba.accessible_from = get_current_time()
                ba.accessible_to = ba.accessible_from + ba.duration_now

                # if this is a group duration, it means we created a personal BlockAccess instance above, so we
                # need to add it
                if inspect(ba).transient:
                    db.session.add(ba)
                db.session.commit()  # TODO ensure nothing else gets committed than the above
                if isinstance(block, Item):
                    targets = current_app.config['DIST_RIGHTS_UNLOCK_TARGETS']
                    curr_targets = targets.get(block.path)
                    if curr_targets:
                        from timApp.tim_celery import send_unlock_op
                        send_unlock_op.delay(get_current_user_object().email, curr_targets)
                flash('Item was unlocked successfully.')
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
        raise AccessDenied(message or "Sorry, you don't have permission to use this resource.")
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
                target = DocEntry.find_by_path(f'{ac}/{block.lang_id}')
            if not target:
                target = DocEntry.find_by_path(ac)
            if not target:
                flash('auto_confirm document does not exist')
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
                aliases = set(a.path for a in block.aliases)
                if allowed & aliases:
                    try:
                        acc = get_single_view_access(target, allow_group=True)
                    except AccessDenied as e:
                        flash('Cannot get access to target document: ' + str(e))
                    else:
                        next_doc = target
                        msg = s.expire_next_doc_message()
                        acc.do_confirm()
                        db.session.commit()
                else:
                    flash('Document is not authorized to auto-confirm rights')
    return msg, next_doc


def has_view_access(b: ItemOrBlock):
    u = get_current_user_object()
    return u.has_view_access(b)


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


def check_admin_access(block_id=None, user=None):
    curr_user = user
    if curr_user is None:
        curr_user = get_current_user_object()
    if curr_user.is_admin:
        return BlockAccess(block_id=block_id,
                           accessible_from=datetime.min.replace(tzinfo=timezone.utc),
                           type=AccessType.owner.value,
                           usergroup_id=curr_user.get_personal_group().id)
    return None


def get_rights(d: ItemBase):
    u = get_current_user_object()
    return {'editable': bool(u.has_edit_access(d)),
            'can_mark_as_read': bool(logged_in() and u.has_view_access(d)),
            'can_comment': bool(logged_in() and u.has_view_access(d)),
            'copy': bool(logged_in() and u.has_copy_access(d)),
            'browse_own_answers': logged_in(),
            'teacher': bool(u.has_teacher_access(d)),
            'see_answers': bool(u.has_seeanswers_access(d)),
            'manage': bool(u.has_manage_access(d)),
            'owner': bool(u.has_ownership(d))
            }


def verify_logged_in() -> None:
    if not logged_in():
        raise AccessDenied("You have to be logged in to perform this action.")


def verify_ownership(b: ItemOrBlock, require=True, message=None, check_duration=False, check_parents=False):
    return verify_access(b, AccessType.owner, require=require, message=message, check_duration=check_duration, check_parents=check_parents)


def verify_read_marking_right(b: ItemOrBlock):
    if not has_read_marking_right(b):
        raise AccessDenied()


def verify_comment_right(b: ItemOrBlock):
    if not has_comment_right(b):
        raise AccessDenied()


def get_plugin_from_request(doc: Document, task_id: TaskId, u: UserContext, view_ctx: ViewContext) -> Tuple[Document, Plugin]:
    assert doc.doc_id == task_id.doc_id
    orig_info = view_ctx.origin
    orig_doc_id, orig_par_id = (orig_info.doc_id, orig_info.par_id) if orig_info else (None, None)
    plug = find_plugin_from_document(doc, task_id, u, view_ctx)
    par_id = plug.par.get_id()
    if orig_doc_id is None or orig_par_id is None:
        if not doc.has_paragraph(par_id):
            raise RouteException('Plugin not found')
        return doc, plug
    if orig_doc_id != doc.doc_id:
        orig_doc = Document(orig_doc_id)
    else:
        orig_doc = doc
    orig_doc.insert_preamble_pars()
    try:
        orig_par = orig_doc.get_paragraph(orig_par_id)
    except TimDbException:
        raise PluginException(f'Plugin paragraph not found: {orig_par_id}')
    pars = dereference_pars([orig_par], context_doc=orig_doc, view_ctx=view_ctx)
    ctx_doc = orig_doc if (not orig_doc.get_docinfo().is_original_translation and orig_par.is_translation()) else doc
    for p in pars:
        if p.get_id() == par_id:
            return ctx_doc, maybe_get_plugin_from_par(p, task_id, u, view_ctx)
    return doc, plug


def get_origin_from_request() -> Optional[OriginInfo]:
    ref_from = ((request.get_json() or {}).get('ref_from') or {})
    doc_id = ref_from.get('docId', get_option(request, 'ref_from_doc_id',
                                              default=None, cast=int))
    par_id = ref_from.get('par', get_option(request, 'ref_from_par_id',
                                            default=None))
    return OriginInfo(doc_id=doc_id, par_id=par_id) if doc_id is not None and par_id is not None else None


@dataclass
class TaskAccessVerification:
    plugin: Plugin
    access: BlockAccess
    is_expired: bool  # True if grace period is allowed and the current time is within the grace period.


def verify_task_access(
        d: DocInfo,
        task_id: TaskId,
        access_type: AccessType,
        required_task_access_level: TaskIdAccess,
        context_user: UserContext,
        view_ctx: ViewContext,
        allow_grace_period: bool = False,
) -> TaskAccessVerification:
    assert d.id == task_id.doc_id
    doc, found_plugin = get_plugin_from_request(d.document, task_id, context_user, view_ctx)
    access = verify_access(doc.get_docinfo(), access_type, require=False, user=context_user.logged_user)
    is_expired = False
    if not access:
        if not allow_grace_period:
            raise AccessDenied(f'No access for task {d.id}.{task_id.task_name}')
        access = verify_access(doc.get_docinfo(), access_type, grace_period=doc.get_settings().answer_grace_period())
        is_expired = True

    if found_plugin.task_id.access_specifier == TaskIdAccess.ReadOnly and \
            required_task_access_level == TaskIdAccess.ReadWrite and \
            not context_user.logged_user.has_teacher_access(doc.get_docinfo()):
        raise AccessDenied(f'This task/field {task_id.task_name} is readonly and thus only writable for teachers.')
    return TaskAccessVerification(
        plugin=found_plugin,
        access=access,
        is_expired=is_expired,
    )


def grant_access_to_session_users(i: ItemOrBlock):
    for u in get_other_users_as_list():
        grant_access(User.get_by_id(int(u['id'])).get_personal_group(),
                     i,
                     AccessType.manage)


def reset_request_access_cache():
    del_attr_if_exists(g, 'manageable')
    del_attr_if_exists(g, 'viewable')
    del_attr_if_exists(g, 'teachable')
    del_attr_if_exists(g, 'see_answers')
    del_attr_if_exists(g, 'owned')
    del_attr_if_exists(g, 'editable')


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
    accs: List[BlockAccess] = u.get_personal_group().accesses.filter_by(block_id=i.id).all()
    if not accs and allow_group:
        lig = UserGroup.get_logged_in_group()
        ugroups = set(gid for gid, in u.groups_dyn.with_entities(UserGroup.id).all())
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
        raise AccessDenied(f"Access type is {acc.access_type.name} instead of view in {i.path}.")
    if acc.expired:
        raise AccessDenied(f"Access is already expired for {i.path}.")
    return acc


def is_allowed_ip() -> bool:
    ip_allowlist = current_app.config['IP_BLOCK_ALLOWLIST']
    if ip_allowlist is None:
        return True
    return any(ipaddress.ip_address(request.remote_addr) in network for network in ip_allowlist)


def is_blocked_ip() -> bool:
    ip = request.remote_addr
    fp = get_ipblocklist_path()
    try:
        with fp.open('r') as f:
            ip_blocklist = f.read()
    except FileNotFoundError:
        return False
    ip_lines = ip_blocklist.splitlines()
    return ip in ip_lines


def get_ipblocklist_path() -> Path:
    return Path(current_app.config['FILES_PATH']) / 'ipblocklist'


def verify_ip_ok(user: Optional[User], msg: str = 'IPNotAllowed'):
    if (not user or not user.is_admin) and not is_allowed_ip():
        username = user.name if user else 'Anonymous'
        cfg = current_app.config
        ip_block_log_only = cfg['IP_BLOCK_LOG_ONLY']
        is_in_blocklist = is_blocked_ip()
        should_block = not ip_block_log_only or is_in_blocklist
        blocked_or_allowed = "blocked" if should_block else "allowed"
        log_warning(f'IP {request.remote_addr} outside allowlist ({username}) - {blocked_or_allowed}')
        msg_end = 'Request was blocked.' if should_block else 'Request was allowed.'
        reply_tos = [cfg['ERROR_EMAIL']]
        if user:
            reply_tos.append(user.email)
        if not is_in_blocklist:
            send_email(
                rcpt=cfg['ERROR_EMAIL'],
                subject=f'{cfg["TIM_HOST"]}: '
                        f'IP {request.remote_addr} outside allowlist ({username}) '
                        f'- {blocked_or_allowed}',
                mail_from=cfg['WUFF_EMAIL'],
                reply_to=','.join(reply_tos),
                msg=f"""
IP {request.remote_addr} was outside allowlist.

URL: {request.url}

User: {username}

{msg_end}
                """.strip())
        if should_block:
            raise AccessDenied(msg)
