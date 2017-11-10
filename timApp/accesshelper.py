from datetime import datetime, timezone
from typing import Optional, Tuple

from flask import flash
from flask import request, g
from sqlalchemy import inspect
from werkzeug.exceptions import abort

import timApp.timdb
from timApp.documentmodel.docparagraph import DocParagraph
from timApp.documentmodel.document import Document, dereference_pars
from timApp.requesthelper import get_option
from timApp.sessioninfo import get_current_user_id, logged_in, get_other_users_as_list, \
    get_current_user_group, get_current_user_object
from timApp.timdb.accesstype import AccessType
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.usergroup import UserGroup
from timApp.timdb.tim_models import db, BlockAccess
from timApp.timdb.timdb2 import TimDb
from timApp.timdb.timdbexception import TimDbException
from timApp.timdb.userutils import get_access_type_id, grant_access


def get_doc_or_abort(doc_id: int):
    d = DocEntry.find_by_id(doc_id, try_translation=True)
    if not d:
        abort(404, 'Document not found')
    return d


def verify_admin():
    if not check_admin_access():
        abort(403, 'This action requires administrative rights.')


def verify_edit_access(block_id: int, require=True, message=None, check_duration=False):
    return abort_if_not_access_and_required(has_edit_access(block_id), block_id, 'edit', require, message,
                                            check_duration)


def verify_manage_access(block_id: int, require=True, message=None, check_duration=False):
    return abort_if_not_access_and_required(has_manage_access(block_id), block_id, 'manage', require, message,
                                            check_duration)


def has_edit_access(block_id):
    return check_admin_access(block_id) or get_editable_blocks().get(block_id)


def verify_access(block_id: int, access_type: AccessType, require: bool = True, message: Optional[str] = None):
    if access_type == AccessType.view:
        return abort_if_not_access_and_required(has_view_access(block_id), block_id, access_type, require, message)
    elif access_type == AccessType.edit:
        return abort_if_not_access_and_required(has_edit_access(block_id), block_id, access_type, require, message)
    elif access_type == AccessType.see_answers:
        return abort_if_not_access_and_required(has_seeanswers_access(block_id), block_id, access_type, require, message)
    elif access_type == AccessType.teacher:
        return abort_if_not_access_and_required(has_teacher_access(block_id), block_id, access_type, require, message)
    elif access_type == AccessType.manage:
        return abort_if_not_access_and_required(has_manage_access(block_id), block_id, access_type, require, message)
    abort(400, 'Bad request - unknown access type')


def verify_view_access(block_id, require=True, message=None, check_duration=False):
    return abort_if_not_access_and_required(has_view_access(block_id), block_id, 'view', require, message, check_duration)


def verify_teacher_access(block_id, require=True, message=None, check_duration=False):
    return abort_if_not_access_and_required(has_teacher_access(block_id), block_id, 'teacher', require, message, check_duration)


def verify_seeanswers_access(block_id, require=True, message=None, check_duration=False):
    return abort_if_not_access_and_required(has_seeanswers_access(block_id), block_id, 'see answers', require, message, check_duration)


class ItemLockedException(Exception):
    """The exception that is raised (in /view route) when a user attempts to access an item for which he has a duration
    access that has not yet begun."""

    def __init__(self, access: BlockAccess):
        self.access = access


def abort_if_not_access_and_required(access_obj: BlockAccess,
                                     block_id: int,
                                     access_type,
                                     require=True,
                                     message=None,
                                     check_duration=False):
    if access_obj:
        return access_obj
    if check_duration:
        ba = BlockAccess.query.filter_by(block_id=block_id,
                                         type=get_access_type_id(access_type),
                                         usergroup_id=get_current_user_group()).first()
        if ba is None:
            ba_group: BlockAccess = BlockAccess.query.filter_by(block_id=block_id,
                                                                type=get_access_type_id(access_type)).filter(
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
                ba.accessible_from = datetime.now(tz=timezone.utc)
                ba.accessible_to = ba.accessible_from + ba.duration

                # if this is a group duration, it means we created a personal BlockAccess instance above, so we
                # need to add it
                if inspect(ba).transient:
                    db.session.add(ba)
                db.session.commit()  # TODO ensure nothing else gets committed than the above
                flash('Item was unlocked successfully.')
                if ba.accessible_from < ba.accessible_to:
                    return ba
                else:
                    raise ItemLockedException(ba)
            else:
                raise ItemLockedException(ba)
    if require:
        abort(403, message or "Sorry, you don't have permission to view this resource.")
    return None


def has_view_access(block_id):
    return check_admin_access(block_id) or get_viewable_blocks().get(block_id)


def has_comment_right(doc_id):
    return has_view_access(doc_id) if logged_in() else None


def verify_comment_right(doc_id):
    if not has_comment_right(doc_id):
        abort(403)


def has_read_marking_right(doc_id):
    return has_view_access(doc_id) if logged_in() else None


def verify_read_marking_right(doc_id):
    if not has_read_marking_right(doc_id):
        abort(403)


def check_admin_access(block_id=None):
    curr_user = get_current_user_object()
    if curr_user.is_admin:
        return BlockAccess(block_id=block_id,
                           accessible_from=datetime.min.replace(tzinfo=timezone.utc),
                           type=AccessType.owner.value,
                           usergroup_id=curr_user.get_personal_group().id)
    return None


def has_teacher_access(doc_id):
    return check_admin_access(doc_id) or get_teachable_blocks().get(doc_id)


def has_manage_access(doc_id):
    return check_admin_access(doc_id) or get_manageable_blocks().get(doc_id)


def has_seeanswers_access(doc_id):
    return check_admin_access(doc_id) or get_see_answers_blocks().get(doc_id)


def get_rights(doc_id):
    return {'editable': bool(has_edit_access(doc_id)),
            'can_mark_as_read': bool(has_read_marking_right(doc_id)),
            'can_comment': bool(has_comment_right(doc_id)),
            'browse_own_answers': logged_in(),
            'teacher': bool(has_teacher_access(doc_id)),
            'see_answers': bool(has_seeanswers_access(doc_id)),
            'manage': bool(has_manage_access(doc_id)),
            'owner': bool(has_ownership(doc_id))
            }


def verify_logged_in():
    if not logged_in():
        abort(403, "You have to be logged in to perform this action.")


def has_ownership(block_id):
    return check_admin_access(block_id) or get_owned_blocks().get(block_id)


def verify_ownership(block_id):
    if not has_ownership(block_id):
        abort(403, "Sorry, you don't have permission to view this resource.")


def get_par_from_request(doc: Document, par_id=None, task_id_name=None) -> Tuple[Document, DocParagraph]:
    orig_doc_id, orig_par_id = get_orig_doc_and_par_id_from_request()
    if par_id is None:
        try:
            par_id = doc.get_paragraph_by_task(task_id_name).get_id()
        except TimDbException as e:
            abort(400, str(e))
    if orig_doc_id is None or orig_par_id is None:
        try:
            return doc, doc.get_paragraph(par_id)
        except TimDbException as e:
            abort(400, str(e))
    orig_doc = Document(orig_doc_id)
    orig_par = orig_doc.get_paragraph(orig_par_id)
    pars = dereference_pars([orig_par], source_doc=doc)
    ctx_doc = orig_doc if (not orig_doc.get_docinfo().is_original_translation and orig_par.is_translation()) else doc
    for p in pars:
        if p.get_id() == par_id:
            return ctx_doc, p
    abort(404)


def get_orig_doc_and_par_id_from_request():
    ref_from = ((request.get_json() or {}).get('ref_from') or {})
    doc_id = ref_from.get('docId', get_option(request, 'ref_from_doc_id',
                                              default=None, cast=int))
    par_id = ref_from.get('par', get_option(request, 'ref_from_par_id',
                                            default=None))
    return doc_id, par_id


def verify_task_access(doc_id, task_id_name, access_type):
    # If the user doesn't have access to the document, we need to check if the plugin was referenced
    # from another document
    if not verify_access(doc_id, access_type, require=False):
        orig_doc, par_id = get_orig_doc_and_par_id_from_request()
        if orig_doc is None or par_id is None:
            abort(403)
        verify_access(orig_doc, access_type)
        par = Document(orig_doc).get_paragraph(par_id)
        if not par.is_reference():
            abort(403)
        pars = dereference_pars([par])
        found_par = next((p for p in pars if p.get_attr('taskId') == task_id_name and p.doc.doc_id == doc_id), None)
        if found_par is None:
            abort(403)
        return found_par


def grant_access_to_session_users(timdb: TimDb, block_id: int):
    for u in get_other_users_as_list():
        grant_access(timdb.users.get_personal_usergroup_by_id(u['id']),
                                 block_id,
                                 'manage',
                                 commit=False)
    db.session.commit()


def get_owned_blocks():
    if not hasattr(g, 'owned'):
        g.owned = timApp.timdb.userutils.get_owned_blocks(get_current_user_id())
    return g.owned


def get_editable_blocks():
    if not hasattr(g, 'editable'):
        g.editable = timApp.timdb.userutils.get_editable_blocks(get_current_user_id())
    return g.editable


def get_viewable_blocks_or_none_if_admin():
    if get_current_user_object().is_admin:
        return None
    return get_viewable_blocks()


def get_viewable_blocks():
    if not hasattr(g, 'viewable'):
        g.viewable = timApp.timdb.userutils.get_viewable_blocks(get_current_user_id())
    return g.viewable


def get_manageable_blocks():
    if not hasattr(g, 'manageable'):
        g.manageable = timApp.timdb.userutils.get_manageable_blocks(get_current_user_id())
    return g.manageable


def get_teachable_blocks():
    if not hasattr(g, 'teachable'):
        g.teachable = timApp.timdb.userutils.get_teachable_blocks(get_current_user_id())
    return g.teachable


def get_see_answers_blocks():
    if not hasattr(g, 'see_answers'):
        g.see_answers = timApp.timdb.userutils.get_see_answers_blocks(get_current_user_id())
    return g.see_answers


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
