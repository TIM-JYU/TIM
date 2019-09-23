"""Routes for manage view."""
import re
from datetime import datetime
from enum import Enum
from typing import Generator, List, Optional

import dateutil.parser
from dataclasses import dataclass, field
from flask import Blueprint, render_template
from flask import abort
from flask import redirect
from flask import request
from isodate import Duration
from isodate import parse_duration
from webargs.flaskparser import use_args

from marshmallow_dataclass import class_schema
from timApp.auth.accesshelper import verify_manage_access, verify_ownership, verify_view_access, has_ownership, \
    verify_edit_access, get_doc_or_abort, get_item_or_abort, get_folder_or_abort
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import AccessTypeModel
from timApp.auth.sessioninfo import get_current_user_group_object
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import copy_document_and_enum_translations
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import move_document, find_free_name
from timApp.folder.folder import Folder, path_includes
from timApp.item.block import BlockType, Block
from timApp.item.item import Item, copy_rights
from timApp.item.validation import validate_item, validate_item_and_create_intermediate_folders, has_special_chars
from timApp.timdb.sqa import db
from timApp.user.user import User, ItemOrBlock
from timApp.user.usergroup import UserGroup
from timApp.user.users import remove_access, remove_default_access, get_default_rights_holders, get_rights_holders
from timApp.user.userutils import grant_access, grant_default_access, get_access_type_id
from timApp.util.flask.requesthelper import verify_json_params, get_option
from timApp.util.flask.responsehelper import json_response, ok_response, get_grid_modules
from timApp.util.utils import remove_path_special_chars, split_location, join_location, get_current_time, \
    split_by_semicolon

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

    access_types = AccessTypeModel.query.all()
    is_folder = isinstance(item, Folder)
    if not is_folder:
        item.serialize_content = True
        item.changelog_length = get_option(request, 'history', 100)

    return render_template('manage_folder.html' if is_folder else 'manage_document.html',
                           route='manage',
                           translations=item.translations if not is_folder else None,
                           item=item,
                           js=['angular-ui-grid'],
                           jsMods=get_grid_modules(),
                           access_types=access_types)


@manage_page.route("/changelog/<int:doc_id>/<int:length>")
def get_changelog(doc_id, length):
    doc = get_doc_or_abort(doc_id)
    verify_manage_access(doc)
    return json_response({'versions': doc.get_changelog_with_names(length)})


@manage_page.route("/permissions/add/<int:item_id>/<group_name>/<perm_type>", methods=["PUT"])
def add_permission(item_id, group_name, perm_type):
    i = get_item_or_abort(item_id)
    is_owner, groups, acc_from, acc_to, dur_from, dur_to, duration = verify_and_get_params(
        i, group_name, perm_type)
    add_perm(acc_from, acc_to, dur_from, dur_to, duration, groups, i, perm_type)
    check_ownership_loss(is_owner, i)
    db.session.commit()
    return ok_response()


class TimeType(Enum):
    always = 0
    range = 1
    duration = 2


@dataclass
class TimeOpt:
    type: TimeType
    duration: Optional[Duration] = None
    to: Optional[datetime] = None
    ffrom: Optional[datetime] = field(metadata={'data_key': 'from'}, default=None)
    durationTo: Optional[datetime] = None
    durationFrom: Optional[datetime] = None


class EditOption(Enum):
    Add = 0
    Remove = 1


@dataclass
class PermissionEditModel:
    ids: List[int]
    type: AccessType
    time: TimeOpt
    action: EditOption = field(metadata={'by_value': True})
    groups: List[str]


@manage_page.route("/permissions/edit", methods=["put"])
@use_args(class_schema(PermissionEditModel)())
def edit_permissions(args: PermissionEditModel):
    groups = UserGroup.query.filter(UserGroup.name.in_(args.groups)).all()
    nonexistent = set(args.groups) - set(g.name for g in groups)
    if nonexistent:
        return abort(400, f'Non-existent groups: {nonexistent}')
    items = Block.query.filter(Block.id.in_(args.ids)
                               & Block.type_id.in_([BlockType.Document.value, BlockType.Folder.value])).all()
    accessible_from = get_current_time()
    acc_to = None
    dur_from = None
    dur_to = None
    duration = None
    if args.time.type == TimeType.range:
        accessible_from = args.time.ffrom
        acc_to = args.time.to
    if args.time.type == TimeType.duration:
        accessible_from = None
        dur_from = args.time.durationFrom
        dur_to = args.time.durationTo
        duration = args.time.duration
    for i in items:
        is_owner = verify_permission_edit_access(i, args.type.name)
        if args.action == EditOption.Add:
            add_perm(
                accessible_from, acc_to,
                dur_from, dur_to,
                duration, groups, i, args.type.name,
            )
        else:
            for g in groups:
                remove_perm(g.id, i, args.type.value)
        check_ownership_loss(is_owner, i)
    db.session.commit()
    return ok_response()


def add_perm(acc_from, acc_to, dur_from, dur_to, duration, groups, item: Item, perm_type):
    if get_current_user_object().get_personal_folder().id == item.id:
        if perm_type == 'owner':
            abort(403, 'You cannot add owners to your personal folder.')
    try:
        for group in groups:
            grant_access(group,
                         item,
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
    i = get_item_or_abort(item_id)
    had_ownership = verify_permission_edit_access(i, perm_type)
    try:
        t = get_access_type_id(perm_type)
        remove_perm(group_id, i.block, t)
    except KeyError:
        abort(400, 'Unknown permission type')
    check_ownership_loss(had_ownership, i)
    db.session.commit()
    return ok_response()


def remove_perm(group_id: int, b: Block, t: int):
    for a in b.accesses:
        if a.usergroup_id == group_id and a.type == t:
            db.session.delete(a)
            break


def check_ownership_loss(had_ownership, item):
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

    validate_item_and_create_intermediate_folders(new_alias, BlockType.Document, get_current_user_group_object())
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

    if alias != new_alias:
        src_f = Folder.find_first_existing(alias)
        if not get_current_user_object().can_write_to_folder(src_f):
            return abort(403, "You don't have permission to write to the source folder.")
        validate_item_and_create_intermediate_folders(new_alias, BlockType.Document, get_current_user_group_object())

    doc.name = new_alias
    doc.public = is_public
    if all(not a.public for a in doc.aliases):
        abort(400, 'This is the only visible name for this document, so you cannot make it invisible.')
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

    validate_item(new_name, BlockType.Folder)

    f.rename_path(new_name)
    db.session.commit()
    return json_response({'new_name': new_name})


@manage_page.route("/permissions/get/<int:item_id>")
def get_permissions(item_id):
    i = get_item_or_abort(item_id)
    verify_manage_access(i)
    grouprights = get_rights_holders(item_id)
    return json_response({'grouprights': grouprights, 'accesstypes': AccessTypeModel.query.all()})


@manage_page.route("/defaultPermissions/<object_type>/get/<int:folder_id>")
def get_default_document_permissions(folder_id, object_type):
    f = get_folder_or_abort(folder_id)
    verify_manage_access(f)
    grouprights = get_default_rights_holders(folder_id, BlockType.from_str(object_type))
    return json_response({'grouprights': grouprights})


@manage_page.route("/defaultPermissions/<object_type>/add/<int:folder_id>/<group_name>/<perm_type>", methods=["PUT"])
def add_default_doc_permission(folder_id, group_name, perm_type, object_type):
    i = get_folder_or_abort(folder_id)
    _, groups, acc_from, acc_to, dur_from, dur_to, duration = verify_and_get_params(i, group_name, perm_type)
    grant_default_access(groups,
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
    ug = UserGroup.query.get(group_id)
    if not ug:
        abort(404, 'Usergroup not found')
    remove_default_access(ug, folder_id, perm_type, BlockType.from_str(object_type))
    db.session.commit()
    return ok_response()


def verify_and_get_params(item: Item, group_name: str, perm_type: str):
    is_owner = verify_permission_edit_access(item, perm_type)
    groups = UserGroup.query.filter(UserGroup.name.in_(split_by_semicolon(group_name))).all()
    if len(groups) == 0:
        abort(404, 'No user group with this name was found.')
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
        accessible_from = get_current_time()

    # SQLAlchemy doesn't know how to adapt Duration instances, so we convert it to timedelta.
    if isinstance(duration, Duration):
        try:
            duration = duration.totimedelta(start=datetime.min)
        except (OverflowError, ValueError):
            abort(400, 'Duration is too long.')
    return is_owner, groups, accessible_from, accessible_to, duration_accessible_from, duration_accessible_to, duration


def verify_permission_edit_access(i: ItemOrBlock, perm_type: str) -> bool:
    """Verifies that the user has right to edit a permission.

    :param i: The item to check for permission.
    :param perm_type: The permission type.
    :return: True if the user has ownership, False if just manage access.

    """
    if perm_type == 'owner':
        verify_ownership(i)
        return True
    else:
        verify_manage_access(i)
        return False


@manage_page.route("/documents/<int:doc_id>", methods=["DELETE"])
def del_document(doc_id):
    d = get_doc_or_abort(doc_id)
    verify_ownership(d)
    f = get_trash_folder()
    move_document(d, f)
    db.session.commit()
    return ok_response()


def get_trash_folder():
    trash_folder_path = f'roskis'
    f = Folder.find_by_path(trash_folder_path)
    if not f:
        f = Folder.create(trash_folder_path, owner_group=UserGroup.get_admin_group(), title='Roskakori')
    return f


@manage_page.route("/folders/<folder_id>", methods=["DELETE"])
def delete_folder(folder_id):
    f = get_folder_or_abort(folder_id)
    verify_ownership(f)
    if f.location == 'users':
        return abort(403, 'Personal folders cannot be deleted.')
    trash = get_trash_folder()
    if f.location == trash.path:
        abort(400, 'Folder is already deleted.')
    trash_path = find_free_name(trash, f)
    f.rename_path(trash_path)
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
    o = get_current_user_group_object()
    nf = Folder.find_by_path(dest)
    if not nf:
        validate_item_and_create_intermediate_folders(dest, BlockType.Folder, o)
        nf = Folder.create(dest, o, apply_default_rights=True)
    u = get_current_user_object()
    copy_folder(f, nf, u, compiled)
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
        preview_list.append({
            'to': join_location(dest, i.get_relative_path(f.path)),
            'from': i.path,
        })
    return json_response({
        'preview': preview_list,
        'dest_exists': Folder.find_by_path(dest) is not None,
    })


def enum_items(folder: Folder, exclude_re) -> Generator[Item, None, None]:
    for d in folder.get_all_documents(include_subdirs=False):
        if not exclude_re.search(d.path):
            yield d
    for f in folder.get_all_folders():
        if not exclude_re.search(f.path):
            yield f
            yield from enum_items(f, exclude_re)


def copy_folder(f_from: Folder, f_to: Folder, user_who_copies: User, exclude_re):
    db.session.flush()
    if not user_who_copies.can_write_to_folder(f_to):
        abort(403, f'Missing edit access to folder {f_to.path}')
    for d in f_from.get_all_documents(include_subdirs=False):
        if exclude_re.search(d.path):
            continue
        nd_path = join_location(f_to.path, d.short_name)
        if DocEntry.find_by_path(nd_path):
            abort(403, f'Document already exists at path {nd_path}')
        nd = DocEntry.create(nd_path, title=d.title)
        copy_rights(d, nd)
        nd.document.modifier_group_id = user_who_copies.get_personal_group().id
        for tr, new_tr in copy_document_and_enum_translations(d, nd, copy_uploads=True):
            copy_rights(tr, new_tr)
    for f in f_from.get_all_folders():
        if exclude_re.search(f.path):
            continue
        nf_path = join_location(f_to.path, f.short_name)
        nf = Folder.find_by_path(nf_path)
        if nf:
            pass
        else:
            nf = Folder.create(nf_path, title=f.title)
            copy_rights(f, nf)
        copy_folder(f, nf, user_who_copies, exclude_re)
