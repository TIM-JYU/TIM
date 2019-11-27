"""Routes for manage view."""
import re
from datetime import datetime, timedelta
from enum import Enum
from typing import Generator, List, Optional

from dataclasses import dataclass, field
from flask import Blueprint, render_template
from flask import abort
from flask import redirect
from flask import request
from isodate import Duration

from timApp.auth.accesshelper import verify_manage_access, verify_ownership, verify_view_access, has_ownership, \
    verify_edit_access, get_doc_or_abort, get_item_or_abort, get_folder_or_abort, verify_copy_access, AccessDenied
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import AccessTypeModel, BlockAccess
from timApp.auth.sessioninfo import get_current_user_group_object
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import copy_document_and_enum_translations
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import move_document, find_free_name
from timApp.folder.createopts import FolderCreationOptions
from timApp.folder.folder import Folder, path_includes
from timApp.item.block import BlockType, Block
from timApp.item.item import Item, copy_rights
from timApp.item.validation import validate_item, validate_item_and_create_intermediate_folders, has_special_chars
from timApp.timdb.sqa import db
from timApp.user.user import User, ItemOrBlock
from timApp.user.usergroup import UserGroup
from timApp.user.users import remove_default_access, get_default_rights_holders, get_rights_holders
from timApp.user.userutils import grant_access, grant_default_access
from timApp.util.flask.requesthelper import verify_json_params, get_option, use_model, RouteException
from timApp.util.flask.responsehelper import json_response, ok_response, get_grid_modules
from timApp.util.utils import remove_path_special_chars, split_location, join_location, get_current_time, \
    cached_property

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

    is_folder = isinstance(item, Folder)
    if not is_folder:
        item.serialize_content = True
        item.changelog_length = get_option(request, 'history', 100)

    return render_template(
        'manage.html',
        route='manage',
        translations=item.translations if not is_folder else None,
        item=item,
        js=['angular-ui-grid'],
        jsMods=get_grid_modules(),
        orgs=UserGroup.get_organizations(),
        access_types=AccessTypeModel.query.all(),
    )


@manage_page.route("/changelog/<int:doc_id>/<int:length>")
def get_changelog(doc_id, length):
    doc = get_doc_or_abort(doc_id)
    verify_manage_access(doc)
    return json_response({'versions': doc.get_changelog_with_names(length)})


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

    @cached_property
    def effective_opt(self):
        acc_to = None
        dur_from = None
        dur_to = None
        duration = None
        accessible_from = get_current_time()
        if self.type == TimeType.range:
            accessible_from = self.ffrom
            acc_to = self.to
        if self.type == TimeType.duration:
            accessible_from = None
            dur_from = self.durationFrom
            dur_to = self.durationTo
            duration = self.duration_timedelta
        return TimeOpt(
            type=self.type,
            duration=duration,
            to=acc_to,
            ffrom=accessible_from,
            durationFrom=dur_from,
            durationTo=dur_to,
        )

    @property
    def duration_timedelta(self):
        if not self.duration:
            return None
        if isinstance(self.duration, timedelta):
            return self.duration
        try:
            return self.duration.totimedelta(start=datetime.min)
        except (OverflowError, ValueError):
            abort(400, 'Duration is too long.')


class EditOption(Enum):
    Add = 0
    Remove = 1


@dataclass
class PermissionEditModel:
    type: AccessType
    time: TimeOpt
    groups: List[str]
    confirm: Optional[bool]

    @cached_property
    def group_objects(self):
        return UserGroup.query.filter(UserGroup.name.in_(self.groups)).all()

    @property
    def nonexistent_groups(self):
        return sorted(list(set(self.groups) - set(g.name for g in self.group_objects)))


@dataclass
class PermissionSingleEditModel(PermissionEditModel):
    id: int


class DefaultItemType(Enum):
    document = 0
    folder = 1


@dataclass
class DefaultPermissionModel(PermissionSingleEditModel):
    item_type: DefaultItemType


@dataclass
class PermissionRemoveModel:
    id: int
    type: AccessType
    group: int


@dataclass
class DefaultPermissionRemoveModel(PermissionRemoveModel):
    item_type: DefaultItemType


@dataclass
class PermissionMassEditModel(PermissionEditModel):
    ids: List[int]
    action: EditOption = field(metadata={'by_value': True})


@manage_page.route("/permissions/add", methods=["PUT"])
@use_model(PermissionSingleEditModel)
def add_permission(m: PermissionSingleEditModel):
    i = get_item_or_abort(m.id)
    is_owner = verify_permission_edit_access(i, m.type)
    add_perm(m, i)
    check_ownership_loss(is_owner, i)
    db.session.commit()
    return permission_response(m)


def permission_response(m: PermissionEditModel):
    return json_response({'not_exist': m.nonexistent_groups})


@manage_page.route("/permissions/confirm", methods=["PUT"])
@use_model(PermissionRemoveModel)
def confirm_permission(m: PermissionRemoveModel):
    i = get_item_or_abort(m.id)
    verify_permission_edit_access(i, m.type)
    ba: Optional[BlockAccess] = BlockAccess.query.filter_by(
        type=m.type.value,
        block_id=m.id,
        usergroup_id=m.group,
    ).first()
    if not ba:
        raise RouteException('Right not found.')
    if not ba.require_confirm:
        raise RouteException('Right does not require confirmation or it was already confirmed.')
    ba.require_confirm = False
    if not ba.duration:
        ba.accessible_from = get_current_time()
    db.session.commit()
    return ok_response()


@manage_page.route("/permissions/edit", methods=["put"])
@use_model(PermissionMassEditModel)
def edit_permissions(m: PermissionMassEditModel):
    groups = m.group_objects
    nonexistent = set(m.groups) - set(g.name for g in groups)
    if nonexistent:
        return abort(400, f'Non-existent groups: {nonexistent}')
    items = Block.query.filter(Block.id.in_(m.ids)
                               & Block.type_id.in_([BlockType.Document.value, BlockType.Folder.value])).all()
    for i in items:
        is_owner = verify_permission_edit_access(i, m.type)
        if m.action == EditOption.Add:
            add_perm(m, i)
        else:
            for g in groups:
                remove_perm(g.id, i, m.type.value)
        check_ownership_loss(is_owner, i)
    db.session.commit()
    return permission_response(m)


def add_perm(
        p: PermissionEditModel,
        item: Item,
):
    if get_current_user_object().get_personal_folder().id == item.id:
        if p.type == AccessType.owner:
            abort(403, 'You cannot add owners to your personal folder.')
    opt = p.time.effective_opt
    for group in p.group_objects:
        grant_access(
            group,
            item,
            p.type,
            accessible_from=opt.ffrom,
            accessible_to=opt.to,
            duration_from=opt.durationFrom,
            duration_to=opt.durationTo,
            duration=opt.duration,
            require_confirm=p.confirm,
            commit=False,
        )


@manage_page.route("/permissions/remove", methods=["PUT"])
@use_model(PermissionRemoveModel)
def remove_permission(m: PermissionRemoveModel):
    i = get_item_or_abort(m.id)
    had_ownership = verify_permission_edit_access(i, m.type)
    remove_perm(m.group, i.block, m.type.value)
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
    return json_response({
        'grouprights': grouprights,
        'accesstypes': AccessTypeModel.query.all(),
        'orgs': UserGroup.get_organizations(),
    })


@manage_page.route("/defaultPermissions/<object_type>/get/<int:folder_id>")
def get_default_document_permissions(folder_id, object_type):
    f = get_folder_or_abort(folder_id)
    verify_manage_access(f)
    grouprights = get_default_rights_holders(folder_id, BlockType.from_str(object_type))
    return json_response({'grouprights': grouprights})


@manage_page.route("/defaultPermissions/add", methods=["PUT"])
@use_model(DefaultPermissionModel)
def add_default_doc_permission(m: DefaultPermissionModel):
    i = get_folder_or_abort(m.id)
    verify_permission_edit_access(i, m.type)
    opt = m.time.effective_opt
    grant_default_access(
        m.group_objects,
        m.id,
        m.type,
        BlockType.from_str(m.item_type.name),
        accessible_from=opt.ffrom,
        accessible_to=opt.to,
        duration_from=opt.durationFrom,
        duration_to=opt.durationTo,
        duration=opt.duration,
    )
    db.session.commit()
    return permission_response(m)


@manage_page.route("/defaultPermissions/remove", methods=["PUT"])
@use_model(DefaultPermissionRemoveModel)
def remove_default_doc_permission(m: DefaultPermissionRemoveModel):
    f = get_folder_or_abort(m.id)
    verify_permission_edit_access(f, m.type)
    ug = UserGroup.query.get(m.group)
    if not ug:
        abort(404, 'Usergroup not found')
    remove_default_access(ug, m.id, m.type, BlockType.from_str(m.item_type.name))
    db.session.commit()
    return ok_response()


def verify_permission_edit_access(i: ItemOrBlock, perm_type: AccessType) -> bool:
    """Verifies that the user has right to edit a permission.

    :param i: The item to check for permission.
    :param perm_type: The permission type.
    :return: True if the user has ownership, False if just manage access.

    """
    if perm_type == AccessType.owner:
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
        f = Folder.create(trash_folder_path, owner_groups=UserGroup.get_admin_group(), title='Roskakori')
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
    verify_copy_access(f, message=f'Missing copy access to folder {f.path}')
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
        nf = Folder.create(dest, o, creation_opts=FolderCreationOptions(apply_default_rights=True))
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
        raise AccessDenied(f'Missing edit access to folder {f_to.path}')
    if not user_who_copies.has_copy_access(f_from):
        raise AccessDenied(f'Missing copy access to folder {f_from.path}')
    folder_opts = FolderCreationOptions(get_templates_rights_from_parent=False)
    for d in f_from.get_all_documents(include_subdirs=False):
        if exclude_re.search(d.path):
            continue
        if not user_who_copies.has_copy_access(d):
            raise AccessDenied(f'Missing copy access to document {d.path}')
        nd_path = join_location(f_to.path, d.short_name)
        if DocEntry.find_by_path(nd_path):
            raise AccessDenied(f'Document already exists at path {nd_path}')
        nd = DocEntry.create(
            nd_path,
            title=d.title,
            folder_opts=folder_opts,
        )
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
            nf = Folder.create(
                nf_path,
                title=f.title,
                creation_opts=folder_opts,
            )
            copy_rights(f, nf)
        copy_folder(f, nf, user_who_copies, exclude_re)
