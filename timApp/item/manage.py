"""Routes for manage view."""
import re
from datetime import timezone, datetime
from typing import Generator, Tuple

import dateutil.parser
from flask import Blueprint, render_template
from flask import abort
from flask import redirect
from flask import request
from isodate import Duration
from isodate import parse_duration

from timApp.auth.accesshelper import verify_manage_access, verify_ownership, verify_view_access, has_ownership, \
    verify_edit_access, get_doc_or_abort, get_item_or_abort, get_folder_or_abort
from timApp.item.block import BlockType
from timApp.timdb.dbaccess import get_timdb
from timApp.document.create_item import copy_document_and_enum_translations
from timApp.util.flask.requesthelper import verify_json_params, get_option
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.auth.sessioninfo import get_current_user_group
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.documents import delete_document
from timApp.item.item import Item, copy_rights
from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder, path_includes
from timApp.user.usergroup import UserGroup
from timApp.timdb.sqa import db
from timApp.auth.auth_models import AccessType
from timApp.user.userutils import grant_access, grant_default_access, get_access_type_id
from timApp.util.utils import remove_path_special_chars, split_location, join_location
from timApp.item.validation import validate_item, validate_item_and_create, has_special_chars

manage_page = Blueprint('manage_page',
                        __name__,
                        url_prefix='')  # TODO: Better URL prefix.


@manage_page.route("/manage/<path:path>")
def manage(path):
    if has_special_chars(path):
        return redirect(remove_path_special_chars(request.path) + '?' + request.query_string.decode('utf8'))
    item = DocEntry.find_by_path(path, fallback_to_id=True)
    if item is None:
        item = Folder.find_by_path(path, fallback_to_id=True)
        if item is None:
            abort(404)

    verify_view_access(item)

    access_types = AccessType.query.all()
    is_folder = isinstance(item, Folder)
    if not is_folder:
        item.serialize_content = True
        item.changelog_length = get_option(request, 'history', 100)

    return render_template('manage_folder.html' if is_folder else 'manage_document.html',
                           route='manage',
                           translations=item.translations if not is_folder else None,
                           item=item,
                           access_types=access_types)


@manage_page.route("/changelog/<int:doc_id>/<int:length>")
def get_changelog(doc_id, length):
    doc = get_doc_or_abort(doc_id)
    verify_manage_access(doc)
    return json_response({'versions': doc.get_changelog_with_names(length)})


@manage_page.route("/permissions/add/<int:item_id>/<group_name>/<perm_type>", methods=["PUT"])
def add_permission(item_id, group_name, perm_type):
    is_owner, group_ids, acc_from, acc_to, dur_from, dur_to, duration = verify_and_get_params(
        item_id, group_name, perm_type)
    i = get_item_or_abort(item_id)
    add_perm(acc_from, acc_to, dur_from, dur_to, duration, group_ids, item_id, perm_type)
    check_ownership_loss(is_owner, i, perm_type)
    db.session.commit()
    return ok_response()


@manage_page.route("/permissions/addRecursive/<group_name>/<perm_type>/<path:folder_path>")
def add_permission_recursive(group_name, perm_type, folder_path):
    folder = Folder.find_by_path(folder_path)
    if not folder:
        abort(404)
    item_id = folder.id
    _, group_ids, acc_from, acc_to, dur_from, dur_to, duration = verify_and_get_params(
        item_id, group_name, perm_type)
    for d in folder.get_all_documents(include_subdirs=True):
        _, is_owner = verify_permission_edit_access(d.id, perm_type)
        add_perm(acc_from, acc_to, dur_from, dur_to, duration, group_ids, d.id, perm_type)
        check_ownership_loss(is_owner, d, perm_type)
    db.session.commit()
    return ok_response()


@manage_page.route("/permissions/removeRecursive/<group_name>/<perm_type>/<path:folder_path>")
def remove_permission_recursive(group_name, perm_type, folder_path):
    folder = Folder.find_by_path(folder_path)
    if not folder:
        abort(404)
    item_id = folder.id
    _, group_ids, _, _, _, _, _ = verify_and_get_params(
        item_id, group_name, perm_type)
    timdb = get_timdb()
    for d in folder.get_all_documents(include_subdirs=True):
        _, is_owner = verify_permission_edit_access(d.id, perm_type)
        for group in group_ids:
            timdb.users.remove_access(group, d.id, perm_type)
        check_ownership_loss(is_owner, d, perm_type)
    db.session.commit()
    return ok_response()


def add_perm(acc_from, acc_to, dur_from, dur_to, duration, group_ids, item_id, perm_type):
    if get_current_user_object().get_personal_folder().id == item_id:
        if perm_type == 'owner':
            abort(403, 'You cannot add owners to your personal folder.')
    try:
        for group_id in group_ids:
            grant_access(group_id,
                         item_id,
                         perm_type,
                         accessible_from=acc_from,
                         accessible_to=acc_to,
                         duration_from=dur_from,
                         duration_to=dur_to,
                         duration=duration,
                         commit=False)
    except KeyError:
        abort(400, 'Invalid permission type.')


@manage_page.route("/permissions/remove/<int:item_id>/<int:group_id>/<perm_type>", methods=["PUT"])
def remove_permission(item_id, group_id, perm_type):
    i, had_ownership = verify_permission_edit_access(item_id, perm_type)
    try:
        t = get_access_type_id(perm_type)
        for a in i.block.accesses:
            if a.usergroup_id == group_id and a.type == t:
                db.session.delete(a)
                break
    except KeyError:
        abort(400, 'Unknown permission type')
    check_ownership_loss(had_ownership, i, perm_type)
    db.session.commit()
    return ok_response()


def check_ownership_loss(had_ownership, item, perm_type):
    db.session.flush()
    db.session.refresh(item)
    if had_ownership and not has_ownership(item):
        abort(403, 'You cannot remove ownership from yourself.')


@manage_page.route("/alias/<int:doc_id>", methods=["GET"])
def get_doc_names(doc_id):
    d = get_doc_or_abort(doc_id)
    verify_manage_access(d)
    return json_response(d.aliases)


@manage_page.route("/alias/<int:doc_id>/<path:new_alias>", methods=["PUT"])
def add_alias(doc_id, new_alias):
    d = get_doc_or_abort(doc_id)
    verify_manage_access(d)
    is_public, = verify_json_params('public', require=False, default=True)

    new_alias = new_alias.strip('/')

    validate_item(new_alias, 'alias')

    parent_folder, _ = split_location(new_alias)
    Folder.create(parent_folder, get_current_user_group())
    d.add_alias(new_alias, is_public)
    db.session.commit()
    return ok_response()


@manage_page.route("/alias/<path:alias>", methods=["POST"])
def change_alias(alias):
    alias = alias.strip('/')
    new_alias, = verify_json_params('new_name')
    new_alias = new_alias.strip('/')
    is_public, = verify_json_params('public', require=False, default=True)

    doc = DocEntry.find_by_path(alias, try_translation=False)
    if doc is None:
        return abort(404, 'The document does not exist!')

    verify_manage_access(doc)

    new_parent, _ = split_location(new_alias)
    f = Folder.find_first_existing(new_alias)
    if alias != new_alias:
        if DocEntry.find_by_path(new_alias) is not None or Folder.find_by_path(new_alias) is not None:
            return abort(403, 'Item with a same name already exists.')
        src_f = Folder.find_first_existing(alias)
        if not get_current_user_object().can_write_to_folder(src_f):
            return abort(403, "You don't have permission to write to the source folder.")

    if not get_current_user_object().can_write_to_folder(f):
        return abort(403, "You don't have permission to write to the destination folder.")

    Folder.create(new_parent, get_current_user_group())
    doc.name = new_alias
    doc.public = is_public
    db.session.commit()
    return ok_response()


@manage_page.route("/alias/<path:alias>", methods=["DELETE"])
def remove_alias(alias):
    alias = alias.strip('/')

    doc = DocEntry.find_by_path(alias, try_translation=False)
    if doc is None:
        return abort(404, 'The document does not exist!')

    verify_manage_access(doc)

    if len(doc.aliases) <= 1:
        return abort(403, "You can't delete the only name the document has.")

    f = Folder.find_first_existing(alias)
    if not get_current_user_object().can_write_to_folder(f):
        return abort(403, "You don't have permission to write to that folder.")

    db.session.delete(doc)
    db.session.commit()
    return ok_response()


@manage_page.route("/rename/<int:item_id>", methods=["PUT"])
def rename_folder(item_id):
    new_name = request.get_json()['new_name'].strip('/')

    d = DocEntry.find_by_id(item_id)
    if d:
        return abort(403, 'Rename route is no longer supported for documents.')

    f = get_folder_or_abort(item_id)
    verify_manage_access(f)

    parent, _ = split_location(new_name)
    parent_f = Folder.find_by_path(parent)

    if parent_f is None:
        # Maybe do a recursive create with permission checks here later?
        return abort(403, "The location does not exist.")

    if parent_f.id == item_id:
        return abort(403, "A folder cannot contain itself.")

    validate_item(new_name, 'folder')

    f.rename_path(new_name)
    db.session.commit()
    return json_response({'new_name': new_name})


@manage_page.route("/permissions/get/<int:item_id>")
def get_permissions(item_id):
    timdb = get_timdb()
    i = get_item_or_abort(item_id)
    verify_manage_access(i)
    grouprights = timdb.users.get_rights_holders(item_id)
    return json_response({'grouprights': grouprights, 'accesstypes': AccessType.query.all()})


@manage_page.route("/defaultPermissions/<object_type>/get/<int:folder_id>")
def get_default_document_permissions(folder_id, object_type):
    timdb = get_timdb()
    f = get_folder_or_abort(folder_id)
    verify_manage_access(f)
    grouprights = timdb.users.get_default_rights_holders(folder_id, BlockType.from_str(object_type))
    return json_response({'grouprights': grouprights})


@manage_page.route("/defaultPermissions/<object_type>/add/<int:folder_id>/<group_name>/<perm_type>", methods=["PUT"])
def add_default_doc_permission(folder_id, group_name, perm_type, object_type):
    _, group_ids, acc_from, acc_to, dur_from, dur_to, duration = verify_and_get_params(folder_id, group_name, perm_type)
    grant_default_access(group_ids,
                         folder_id,
                         perm_type,
                         BlockType.from_str(object_type),
                         accessible_from=acc_from,
                         accessible_to=acc_to,
                         duration_from=dur_from,
                         duration_to=dur_to,
                         duration=duration)
    db.session.commit()
    return ok_response()


@manage_page.route("/defaultPermissions/<object_type>/remove/<int:folder_id>/<int:group_id>/<perm_type>", methods=["PUT"])
def remove_default_doc_permission(folder_id, group_id, perm_type, object_type):
    f = get_folder_or_abort(folder_id)
    verify_manage_access(f)
    timdb = get_timdb()
    timdb.users.remove_default_access(group_id, folder_id, perm_type, BlockType.from_str(object_type))
    db.session.commit()
    return ok_response()


def verify_and_get_params(item_id, group_name, perm_type):
    _, is_owner = verify_permission_edit_access(item_id, perm_type)
    groups = UserGroup.query.filter(UserGroup.name.in_([name.strip() for name in group_name.split(';')])).all()
    if len(groups) == 0:
        abort(404, 'No user group with this name was found.')
    group_ids = [group.id for group in groups]
    req_json = request.get_json()
    if req_json is None:
        req_json = {}
    access_type = req_json.get('type', 'always')

    try:
        accessible_from = dateutil.parser.parse(req_json.get('from')) if access_type == 'range' else None
    except TypeError:
        accessible_from = None
    try:
        accessible_to = dateutil.parser.parse(req_json.get('to')) if access_type == 'range' else None
    except TypeError:
        accessible_to = None

    try:
        duration_accessible_from = dateutil.parser.parse(
            req_json.get('durationFrom')) if access_type == 'duration' else None
    except TypeError:
        duration_accessible_from = None
    try:
        duration_accessible_to = dateutil.parser.parse(
            req_json.get('durationTo')) if access_type == 'duration' else None
    except TypeError:
        duration_accessible_to = None

    duration = parse_duration(req_json.get('duration')) if access_type == 'duration' else None

    if access_type == 'always' or (accessible_from is None and duration is None):
        accessible_from = datetime.now(tz=timezone.utc)

    # SQLAlchemy doesn't know how to adapt Duration instances, so we convert it to timedelta.
    if isinstance(duration, Duration):
        try:
            duration = duration.totimedelta(start=datetime.min)
        except (OverflowError, ValueError):
            abort(400, 'Duration is too long.')
    return is_owner, group_ids, accessible_from, accessible_to, duration_accessible_from, duration_accessible_to, duration


def verify_permission_edit_access(item_id: int, perm_type: str) -> Tuple[Item, bool]:
    """Verifies that the user has right to edit a permission.

    :param item_id: The item id to check for permission.
    :param perm_type: The permission type.
    :return: True if the user has ownership, False if just manage access.

    """
    i = get_item_or_abort(item_id)
    if perm_type == 'owner':
        verify_ownership(i)
        return i, True
    else:
        verify_manage_access(i)
        return i, False


@manage_page.route("/documents/<int:doc_id>", methods=["DELETE"])
def del_document(doc_id):
    d = get_doc_or_abort(doc_id)
    verify_ownership(d)
    abort(403, 'Deleting documents has been disabled until a proper backup mechanism is implemented. '
               'Please contact TIM administrators if you really want to delete this document. '
               'You can hide this document from others by removing all permissions.')
    delete_document(doc_id)
    return ok_response()


@manage_page.route("/folders/<folder_id>", methods=["DELETE"])
def delete_folder(folder_id):
    f = get_folder_or_abort(folder_id)
    verify_ownership(f)
    if not f.is_empty:
        return abort(403, "The folder is not empty. Only empty folders can be deleted.")

    f.delete()
    db.session.commit()
    return ok_response()


@manage_page.route("/changeTitle/<int:item_id>", methods=["PUT"])
def change_title(item_id):
    item = get_item_or_abort(item_id)
    verify_edit_access(item)
    new_title, = verify_json_params('new_title')
    item.title = new_title
    db.session.commit()
    return ok_response()


def get_copy_folder_params(folder_id):
    f = get_folder_or_abort(folder_id)
    verify_manage_access(f)
    dest, exclude = verify_json_params('destination', 'exclude')
    compiled = get_pattern(exclude)
    if path_includes(dest, f.path):
        return abort(403, 'Cannot copy folder inside of itself.')
    return f, dest, compiled


@manage_page.route("/copy/<int:folder_id>", methods=["POST"])
def copy_folder_endpoint(folder_id):
    f, dest, compiled = get_copy_folder_params(folder_id)
    o = get_current_user_group()
    validate_item_and_create(dest, 'folder', o)
    nf = Folder.create(dest, o, apply_default_rights=True)
    ug = get_current_user_object().get_personal_group()
    copy_folder(f, nf, ug, compiled)
    db.session.commit()
    return json_response(nf)


def get_pattern(exclude: str):
    if not exclude:
        exclude = 'a^'
    try:
        return re.compile(exclude)
    except:
        abort(400, f'Wrong pattern format: {exclude}')


@manage_page.route("/copy/<int:folder_id>/preview", methods=["POST"])
def copy_folder_preview(folder_id):
    f, dest, compiled = get_copy_folder_params(folder_id)
    preview_list = []
    for i in enum_items(f, compiled):
        preview_list.append({'to': join_location(dest, i.get_relative_path(f.path)), 'from': i.path})
    return json_response(preview_list)


def enum_items(folder: Folder, exclude_re) -> Generator[Item, None, None]:
    for d in folder.get_all_documents(include_subdirs=False):
        if not exclude_re.search(d.path):
            yield d
    for f in folder.get_all_folders():
        if not exclude_re.search(f.path):
            yield f
            for it in enum_items(f, exclude_re):
                yield it


def copy_folder(f_from: Folder, f_to: Folder, modifier: UserGroup, exclude_re):
    for d in f_from.get_all_documents(include_subdirs=False):
        if exclude_re.search(d.path):
            continue
        nd = DocEntry.create(join_location(f_to.path, d.short_name), title=d.title)
        copy_rights(d, nd)
        nd.document.modifier_group_id = modifier.id
        for tr, new_tr in copy_document_and_enum_translations(d, nd):
            copy_rights(tr, new_tr)
    for f in f_from.get_all_folders():
        if exclude_re.search(f.path):
            continue
        nf = Folder.create(join_location(f_to.path, f.short_name), title=f.title)
        copy_rights(f, nf)
        copy_folder(f, nf, modifier, exclude_re)
