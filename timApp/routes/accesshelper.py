from typing import Optional

from flask import request, g
from werkzeug.exceptions import abort

import documentmodel.document
from documentmodel.document import Document
from options import get_option
from routes.dbaccess import get_timdb
from routes.sessioninfo import get_current_user_id, logged_in, get_current_user_name, get_other_users_as_list
from timdb.accesstype import AccessType
from timdb.tim_models import db
from timdb.timdb2 import TimDb


def verify_admin():
    timdb = get_timdb()
    if not timdb.users.has_admin_access(get_current_user_id()):
        abort(403, 'This action requires administrative rights.')


def verify_edit_access(block_id, message="Sorry, you don't have permission to edit this resource."):
    if not has_edit_access(block_id):
        abort(403, message)


def verify_manage_access(block_id, message="Sorry, you don't have permission to manage this resource."):
    if not has_manage_access(block_id):
        abort(403, message)


def has_edit_access(block_id):
    return block_id in get_editable_blocks()


def verify_access(block_id: int, access_type: AccessType, require: bool = True, message: Optional[str] = None):
    if access_type == AccessType.view:
        return abort_if_not_access_and_required(has_view_access(block_id), require, message)
    elif access_type == AccessType.edit:
        return abort_if_not_access_and_required(has_edit_access(block_id), require, message)
    elif access_type == AccessType.see_answers:
        return abort_if_not_access_and_required(has_seeanswers_access(block_id), require, message)
    elif access_type == AccessType.teacher:
        return abort_if_not_access_and_required(has_teacher_access(block_id), require, message)
    elif access_type == AccessType.manage:
        return abort_if_not_access_and_required(has_manage_access(block_id), require, message)
    abort(400, 'Bad request - unknown access type')


def verify_view_access(block_id, require=True, message=None):
    return abort_if_not_access_and_required(has_view_access(block_id), require, message)


def verify_teacher_access(block_id, require=True, message=None):
    return abort_if_not_access_and_required(has_teacher_access(block_id), require, message)


def verify_seeanswers_access(block_id, require=True, message=None):
    return abort_if_not_access_and_required(has_seeanswers_access(block_id), require, message)


def abort_if_not_access_and_required(has_access, require=True, message=None):
    if has_access:
        return True
    if require:
        abort(403, message or "Sorry, you don't have permission to view this resource.")
    return False


def has_view_access(block_id):
    return block_id in get_viewable_blocks()


def has_comment_right(doc_id):
    return has_view_access(doc_id) and logged_in()


def verify_comment_right(doc_id):
    if not has_comment_right(doc_id):
        abort(403)


def has_read_marking_right(doc_id):
    return has_view_access(doc_id) and logged_in()


def verify_read_marking_right(doc_id):
    if not has_read_marking_right(doc_id):
        abort(403)


def has_teacher_access(doc_id):
    return doc_id in get_teachable_blocks()


def has_manage_access(doc_id):
    return doc_id in get_manageable_blocks()


def has_seeanswers_access(doc_id):
    return doc_id in get_see_answers_blocks()


def get_rights(doc_id):
    return {'editable': has_edit_access(doc_id),
            'can_mark_as_read': has_read_marking_right(doc_id),
            'can_comment': has_comment_right(doc_id),
            'browse_own_answers': logged_in(),
            'teacher': has_teacher_access(doc_id),
            'see_answers': has_seeanswers_access(doc_id),
            'manage': has_manage_access(doc_id),
            'owner': has_ownership(doc_id)
            }


def verify_logged_in():
    if not logged_in():
        abort(403, "You have to be logged in to perform this action.")


def has_ownership(block_id):
    return block_id in get_owned_blocks()


def verify_ownership(block_id):
    if not has_ownership(block_id):
        abort(403, "Sorry, you don't have permission to view this resource.")


def can_write_to_folder(folder_name):
    timdb = get_timdb()
    user_folder = "users/" + get_current_user_name()
    folder = folder_name
    while folder != '':
        if folder == user_folder:
            return True

        folder_id = timdb.folders.get_folder_id(folder)
        if folder_id is not None:
            return has_edit_access(folder_id)

        folder, _ = timdb.folders.split_location(folder)

    return timdb.users.has_admin_access(get_current_user_id())


def verify_task_access(doc_id, task_id_name, access_type):
    # If the user doesn't have access to the document, we need to check if the plugin was referenced
    # from another document
    if not verify_access(doc_id, access_type, require=False):
        orig_doc = (request.get_json() or {}).get('ref_from', {}).get('docId', get_option(request, 'ref_from_doc_id',
                                                                                          default=doc_id))
        verify_access(orig_doc, access_type)
        par_id = (request.get_json() or {}).get('ref_from', {}).get('par', get_option(request, 'ref_from_par_id',
                                                                                      default=None))
        if par_id is None:
            abort(403)
        par = Document(orig_doc).get_paragraph(par_id)
        if not par.is_reference():
            abort(403)
        pars = documentmodel.document.dereference_pars([par])
        found_par = next((p for p in pars if p.get_attr('taskId') == task_id_name and p.doc.doc_id == doc_id), None)
        if found_par is None:
            abort(403)
        return found_par


def grant_access_to_session_users(timdb: TimDb, block_id: int):
    for u in get_other_users_as_list():
        timdb.users.grant_access(timdb.users.get_personal_usergroup_by_id(u['id']),
                                 block_id,
                                 'manage',
                                 commit=False)
    db.session.commit()


def get_owned_blocks():
    if not hasattr(g, 'owned'):
        timdb = get_timdb()
        g.owned = timdb.users.get_owned_blocks(get_current_user_id())
    return g.owned


def get_editable_blocks():
    if not hasattr(g, 'editable'):
        timdb = get_timdb()
        g.editable = timdb.users.get_editable_blocks(get_current_user_id())
    return g.editable


def get_viewable_blocks():
    if not hasattr(g, 'viewable'):
        timdb = get_timdb()
        g.viewable = timdb.users.get_viewable_blocks(get_current_user_id())
    return g.viewable


def get_manageable_blocks():
    if not hasattr(g, 'manageable'):
        timdb = get_timdb()
        g.manageable = timdb.users.get_manageable_blocks(get_current_user_id())
    return g.manageable


def get_teachable_blocks():
    if not hasattr(g, 'teachable'):
        timdb = get_timdb()
        g.teachable = timdb.users.get_teachable_blocks(get_current_user_id())
    return g.teachable


def get_see_answers_blocks():
    if not hasattr(g, 'see_answers'):
        timdb = get_timdb()
        g.see_answers = timdb.users.get_see_answers_blocks(get_current_user_id())
    return g.see_answers
