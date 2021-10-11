"""Routes for manage view."""
import re
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from re import Pattern
from typing import Generator, Optional, Union

from flask import redirect
from flask import render_template, Response
from flask import request
from isodate import Duration

from timApp.auth.accesshelper import (
    verify_manage_access,
    verify_ownership,
    verify_view_access,
    has_ownership,
    verify_edit_access,
    get_doc_or_abort,
    get_item_or_abort,
    get_folder_or_abort,
    verify_copy_access,
    AccessDenied,
    get_single_view_access,
    has_edit_access,
)
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import AccessTypeModel, BlockAccess
from timApp.auth.sessioninfo import get_current_user_group_object
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import copy_document_and_enum_translations
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import move_document, find_free_name, DocInfo
from timApp.document.exceptions import ValidationException
from timApp.folder.createopts import FolderCreationOptions
from timApp.folder.folder import Folder, path_includes
from timApp.item.block import BlockType, Block
from timApp.item.copy_rights import copy_rights
from timApp.item.item import Item
from timApp.item.validation import (
    validate_item,
    validate_item_and_create_intermediate_folders,
    has_special_chars,
)
from timApp.timdb.sqa import db
from timApp.user.user import User, ItemOrBlock
from timApp.user.usergroup import UserGroup
from timApp.user.users import (
    remove_default_access,
    get_default_rights_holders,
    get_rights_holders,
    remove_access,
)
from timApp.user.userutils import grant_access, grant_default_access
from timApp.util.flask.requesthelper import (
    get_option,
    use_model,
    RouteException,
    NotExist,
)
from timApp.util.flask.responsehelper import (
    json_response,
    ok_response,
    get_grid_modules,
)
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.logger import log_info
from timApp.util.utils import (
    remove_path_special_chars,
    split_location,
    join_location,
    get_current_time,
    cached_property,
    seq_to_str,
)

manage_page = TypedBlueprint(
    "manage_page", __name__, url_prefix=""
)  # TODO: Better URL prefix.


@manage_page.get("/manage/<path:path>")
def manage(path: str) -> Union[Response, str]:
    if has_special_chars(path):
        return redirect(
            remove_path_special_chars(request.path)
            + "?"
            + request.query_string.decode("utf8")
        )
    item = DocEntry.find_by_path(path, fallback_to_id=True)
    if item is None:
        item = Folder.find_by_path(path, fallback_to_id=True)
        if item is None:
            raise NotExist()

    verify_view_access(item)

    is_folder = isinstance(item, Folder)
    if not is_folder and has_edit_access(item):
        item.serialize_content = True
        item.changelog_length = get_option(request, "history", 100)

    return render_template(
        "manage.jinja2",
        route="manage",
        translations=item.translations if not is_folder else None,
        item=item,
        js=["angular-ui-grid"],
        jsMods=get_grid_modules(),
        orgs=UserGroup.get_organizations(),
        access_types=AccessTypeModel.query.all(),
    )


@manage_page.get("/changelog/<int:doc_id>/<int:length>")
def get_changelog(doc_id: int, length: int) -> Response:
    doc = get_doc_or_abort(doc_id)
    verify_manage_access(doc)
    return json_response({"versions": doc.get_changelog_with_names(length)})


class TimeType(Enum):
    always = 0
    range = 1
    duration = 2


@dataclass
class TimeOpt:
    type: TimeType
    duration: Optional[Duration] = None
    to: Optional[datetime] = None
    ffrom: Optional[datetime] = field(metadata={"data_key": "from"}, default=None)
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
            acc_to = self.to
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
            raise RouteException("Duration is too long.")


class EditOption(Enum):
    Add = "add"
    Remove = "remove"


@dataclass
class PermissionEditModel:
    type: AccessType = field(metadata={"by_value": True})
    time: TimeOpt
    groups: list[str]
    confirm: Optional[bool]

    def __post_init__(self):
        if self.confirm and self.time.type == TimeType.range and self.time.ffrom:
            raise RouteException("Cannot require confirm with start time set")

    @cached_property
    def group_objects(self):
        return UserGroup.query.filter(UserGroup.name.in_(self.groups)).all()

    @property
    def nonexistent_groups(self):
        return sorted(list(set(self.groups) - {g.name for g in self.group_objects}))


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
    type: AccessType = field(metadata={"by_value": True})
    group: int


@dataclass
class DefaultPermissionRemoveModel(PermissionRemoveModel):
    item_type: DefaultItemType


@dataclass
class PermissionMassEditModel(PermissionEditModel):
    ids: list[int]
    action: EditOption = field(metadata={"by_value": True})


@manage_page.put("/permissions/add", own_model=True)
@use_model(PermissionSingleEditModel)
def add_permission(m: PermissionSingleEditModel):
    i = get_item_or_abort(m.id)
    is_owner = verify_permission_edit_access(i, m.type)
    accs = add_perm(m, i)
    if accs:
        a = accs[0]
        check_ownership_loss(is_owner, i)
        log_right(f"added {a.info_str} for {seq_to_str(m.groups)} in {i.path}")
        db.session.commit()
    return permission_response(m)


def permission_response(m: PermissionEditModel):
    return json_response({"not_exist": m.nonexistent_groups})


def log_right(s: str):
    u = get_current_user_object()
    log_info(f"RIGHTS: {u.name} {s}")


def get_group_and_doc(doc_id: int, username: str) -> tuple[UserGroup, DocInfo]:
    i = get_item_or_abort(doc_id)
    verify_permission_edit_access(i, AccessType.view)
    g = UserGroup.get_by_name(username)
    if not g:
        raise RouteException("User not found")
    return g, i


@manage_page.get("/permissions/expire/<int:doc_id>/<username>", own_model=True)
def expire_permission_url(doc_id: int, username: str):
    g, i = get_group_and_doc(doc_id, username)
    ba: Optional[BlockAccess] = BlockAccess.query.filter_by(
        type=AccessType.view.value,
        block_id=i.id,
        usergroup_id=g.id,
    ).first()
    if not ba:
        raise RouteException("Right not found.")
    if ba.expired:
        raise RouteException("Right is already expired.")
    ba.accessible_to = get_current_time()
    if ba.duration:
        ba.duration = None
        ba.duration_from = None
        ba.duration_to = None
    db.session.commit()
    return ok_response()


@manage_page.get("/permissions/confirm/<int:doc_id>/<username>", own_model=True)
def confirm_permission_url(doc_id: int, username: str):
    g, i = get_group_and_doc(doc_id, username)
    m = PermissionRemoveModel(id=doc_id, type=AccessType.view, group=g.id)
    return do_confirm_permission(m, i)


@manage_page.put("/permissions/confirm", own_model=True)
@use_model(PermissionRemoveModel)
def confirm_permission(m: PermissionRemoveModel) -> Response:
    i = get_item_or_abort(m.id)
    verify_permission_edit_access(i, m.type)
    return do_confirm_permission(m, i)


def do_confirm_permission(m: PermissionRemoveModel, i: DocInfo):
    ba: Optional[BlockAccess] = BlockAccess.query.filter_by(
        type=m.type.value,
        block_id=m.id,
        usergroup_id=m.group,
    ).first()
    if not ba:
        raise RouteException("Right not found.")
    if not ba.require_confirm:
        raise RouteException(
            f"{m.type.name} right for {ba.usergroup.name} does not require confirmation or it was already confirmed."
        )
    ba.do_confirm()
    ug: UserGroup = UserGroup.query.get(m.group)
    log_right(f"confirmed {ba.info_str} for {ug.name} in {i.path}")
    db.session.commit()
    return ok_response()


@manage_page.put("/permissions/edit", own_model=True)
@use_model(PermissionMassEditModel)
def edit_permissions(m: PermissionMassEditModel) -> Response:
    groups = m.group_objects
    nonexistent = set(m.groups) - {g.name for g in groups}
    if nonexistent:
        raise RouteException(f"Non-existent groups: {nonexistent}")
    items = (
        Block.query.filter(
            Block.id.in_(m.ids)
            & Block.type_id.in_([BlockType.Document.value, BlockType.Folder.value])
        )
        .order_by(Block.id)
        .all()
    )
    a = None
    owned_items_before = set()
    for i in items:
        checked_owner = verify_permission_edit_access(i, m.type)
        if checked_owner:
            owned_items_before.add(i)
        if m.action == EditOption.Add:
            accs = add_perm(m, i)
            if accs:
                a = accs[0]
        else:
            for g in groups:
                a = remove_perm(g, i, m.type) or a

    if m.type == AccessType.owner:
        owned_items_after = set()
        u = get_current_user_object()
        for i in items:
            if u.has_ownership(i):
                owned_items_after.add(i)
        if owned_items_before != owned_items_after:
            raise AccessDenied("You cannot remove ownership from yourself.")
    if a:
        action = "added" if m.action == EditOption.Add else "removed"
        log_right(
            f"{action} {a.info_str} for {seq_to_str(m.groups)} in blocks: {seq_to_str(list(str(x) for x in m.ids))}"
        )
        db.session.commit()
    return permission_response(m)


def add_perm(
    p: PermissionEditModel,
    item: Item,
) -> list[BlockAccess]:
    if get_current_user_object().get_personal_folder().id == item.id:
        if p.type == AccessType.owner:
            raise AccessDenied("You cannot add owners to your personal folder.")
    opt = p.time.effective_opt
    accs = []
    for group in p.group_objects:
        a = grant_access(
            group,
            item,
            p.type,
            accessible_from=opt.ffrom,
            accessible_to=opt.to,
            duration_from=opt.durationFrom,
            duration_to=opt.durationTo,
            duration=opt.duration,
            require_confirm=p.confirm,
        )
        accs.append(a)
    return accs


@manage_page.put("/permissions/remove", own_model=True)
@use_model(PermissionRemoveModel)
def remove_permission(m: PermissionRemoveModel) -> Response:
    i = get_item_or_abort(m.id)
    had_ownership = verify_permission_edit_access(i, m.type)
    ug: UserGroup = UserGroup.query.get(m.group)
    if not ug:
        raise RouteException("User group not found")
    a = remove_perm(ug, i.block, m.type)
    check_ownership_loss(had_ownership, i)

    log_right(f"removed {a.info_str} for {ug.name} in {i.path}")
    db.session.commit()
    return ok_response()


@dataclass
class PermissionClearModel:
    paths: list[str]
    type: AccessType = field(metadata={"by_value": True})


@manage_page.put("/permissions/clear", own_model=True)
@use_model(PermissionClearModel)
def clear_permissions(m: PermissionClearModel) -> Response:
    for p in m.paths:
        i = DocEntry.find_by_path(p, try_translation=True)
        if not i:
            i = Folder.find_by_path(p)
        if not i:
            raise RouteException(f"Item not found: {p}")
        verify_ownership(i)
        i.block.accesses = {
            (ugid, permtype): v
            for (ugid, permtype), v in i.block.accesses.items()
            if permtype != m.type.value
        }
    db.session.commit()
    return ok_response()


# noinspection PyShadowingBuiltins
@manage_page.post("/permissions/selfExpire")
def self_expire_permission(id: int) -> Response:
    i = get_item_or_abort(id)
    acc = verify_view_access(i, require=False)
    if not acc:
        return ok_response()
    acc = get_single_view_access(i)
    acc.accessible_to = get_current_time()
    log_right(f"self-expired view access in {i.path}")
    db.session.commit()
    return ok_response()


def remove_perm(group: UserGroup, b: Block, t: AccessType):
    return remove_access(group, b, t)


def check_ownership_loss(had_ownership, item):
    db.session.flush()
    db.session.refresh(item)
    if had_ownership and not has_ownership(item):
        raise AccessDenied("You cannot remove ownership from yourself.")


@manage_page.get("/alias/<int:doc_id>")
def get_doc_names(doc_id: int) -> Response:
    d = get_doc_or_abort(doc_id)
    verify_manage_access(d)
    return json_response(d.aliases)


@manage_page.put("/alias/<int:doc_id>/<path:new_alias>")
def add_alias(doc_id: int, new_alias: str, public: bool = True) -> Response:
    d = get_doc_or_abort(doc_id)
    verify_manage_access(d)

    new_alias = new_alias.strip("/")

    validate_item_and_create_intermediate_folders(
        new_alias, BlockType.Document, get_current_user_group_object()
    )
    d.add_alias(new_alias, public)
    db.session.commit()
    return ok_response()


@manage_page.post("/alias/<path:alias>")
def change_alias(alias: str, new_name: str, public: bool = True) -> Response:
    alias = alias.strip("/")
    new_alias = new_name.strip("/")

    doc = DocEntry.find_by_path(alias, try_translation=False)
    if doc is None:
        raise NotExist("The document does not exist!")

    verify_manage_access(doc)

    if alias != new_alias:
        src_f = Folder.find_first_existing(alias)
        if not get_current_user_object().can_write_to_folder(src_f):
            raise AccessDenied(
                "You don't have permission to write to the source folder."
            )
        validate_item_and_create_intermediate_folders(
            new_alias, BlockType.Document, get_current_user_group_object()
        )

    doc.name = new_alias
    doc.public = public
    if all(not a.public for a in doc.aliases):
        raise RouteException(
            "This is the only visible name for this document, so you cannot make it invisible."
        )
    db.session.commit()
    return ok_response()


@manage_page.delete("/alias/<path:alias>")
def remove_alias(alias: str) -> Response:
    alias = alias.strip("/")

    doc = DocEntry.find_by_path(alias, try_translation=False)
    if doc is None:
        raise NotExist("The document does not exist!")

    verify_manage_access(doc)

    if len(doc.aliases) <= 1:
        raise AccessDenied("You can't delete the only name the document has.")

    f = Folder.find_first_existing(alias)
    if not get_current_user_object().can_write_to_folder(f):
        raise AccessDenied("You don't have permission to write to that folder.")

    db.session.delete(doc)
    db.session.commit()
    return ok_response()


@manage_page.put("/rename/<int:item_id>")
def rename_folder(item_id: int) -> Response:
    new_name = request.get_json()["new_name"].strip("/")

    d = DocEntry.find_by_id(item_id)
    if d:
        raise AccessDenied("Rename route is no longer supported for documents.")

    f = get_folder_or_abort(item_id)
    verify_manage_access(f)

    parent, _ = split_location(new_name)
    parent_f = Folder.find_by_path(parent)

    if parent_f is None:
        # Maybe do a recursive create with permission checks here later?
        raise AccessDenied("The location does not exist.")

    if parent_f.id == item_id:
        raise AccessDenied("A folder cannot contain itself.")

    validate_item(new_name, BlockType.Folder)

    f.rename_path(new_name)
    db.session.commit()
    return json_response({"new_name": new_name})


@manage_page.get("/permissions/get/<int:item_id>")
def get_permissions(item_id: int) -> Response:
    i = get_item_or_abort(item_id)
    verify_manage_access(i)
    grouprights = get_rights_holders(item_id)
    return json_response(
        {
            "grouprights": grouprights,
            "accesstypes": AccessTypeModel.query.all(),
            "orgs": UserGroup.get_organizations(),
        },
        date_conversion=True,
    )


@manage_page.get("/defaultPermissions/<object_type>/get/<int:folder_id>")
def get_default_document_permissions(folder_id: int, object_type: str) -> Response:
    f = get_folder_or_abort(folder_id)
    verify_manage_access(f)
    grouprights = get_default_rights_holders(f, BlockType.from_str(object_type))
    return json_response({"grouprights": grouprights}, date_conversion=True)


@manage_page.put("/defaultPermissions/add", own_model=True)
@use_model(DefaultPermissionModel)
def add_default_doc_permission(m: DefaultPermissionModel) -> Response:
    i = get_folder_or_abort(m.id)
    verify_permission_edit_access(i, m.type)
    opt = m.time.effective_opt
    grant_default_access(
        m.group_objects,
        i,
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


@manage_page.put("/defaultPermissions/remove", own_model=True)
@use_model(DefaultPermissionRemoveModel)
def remove_default_doc_permission(m: DefaultPermissionRemoveModel) -> Response:
    f = get_folder_or_abort(m.id)
    verify_permission_edit_access(f, m.type)
    ug = UserGroup.query.get(m.group)
    if not ug:
        raise NotExist("Usergroup not found")
    remove_default_access(ug, f, m.type, BlockType.from_str(m.item_type.name))
    db.session.commit()
    return ok_response()


def verify_permission_edit_access(i: ItemOrBlock, perm_type: AccessType) -> bool:
    """Verifies that the user has right to edit a permission.

    :param i: The item to check for permission.
    :param perm_type: The permission type.
    :return: True if owner permission was checked, false if just manage access.

    """
    if perm_type == AccessType.owner:
        verify_ownership(i)
        return True
    else:
        verify_manage_access(i)
        return False


@manage_page.delete("/documents/<int:doc_id>")
def del_document(doc_id: int) -> Response:
    d = get_doc_or_abort(doc_id)
    verify_ownership(d)
    f = get_trash_folder()
    move_document(d, f)
    db.session.commit()
    return ok_response()


TRASH_FOLDER_PATH = f"roskis"


def get_trash_folder() -> Folder:
    f = Folder.find_by_path(TRASH_FOLDER_PATH)
    if not f:
        f = Folder.create(
            TRASH_FOLDER_PATH,
            owner_groups=UserGroup.get_admin_group(),
            title="Roskakori",
        )
    return f


@manage_page.delete("/folders/<int:folder_id>")
def delete_folder(folder_id: int) -> Response:
    f = get_folder_or_abort(folder_id)
    verify_ownership(f)
    if f.location == "users":
        raise AccessDenied("Personal folders cannot be deleted.")
    trash = get_trash_folder()
    if f.location == trash.path:
        raise RouteException("Folder is already deleted.")
    trash_path = find_free_name(trash, f)
    f.rename_path(trash_path)
    db.session.commit()
    return ok_response()


@manage_page.put("/changeTitle/<int:item_id>")
def change_title(item_id: int, new_title: str) -> Response:
    item = get_item_or_abort(item_id)
    verify_edit_access(item)
    item.title = new_title
    db.session.commit()
    return ok_response()


def get_copy_folder_params(folder_id: int, dest: str, exclude: Optional[str]):
    f = get_folder_or_abort(folder_id)
    verify_copy_access(f, message=f"Missing copy access to folder {f.path}")
    compiled = get_pattern(exclude)
    if path_includes(dest, f.path):
        raise AccessDenied("Cannot copy folder inside of itself.")
    return f, dest, compiled


@dataclass
class CopyOptions:
    copy_active_rights: bool = True
    copy_expired_rights: bool = False
    stop_on_errors: bool = True


@manage_page.post("/copy/<int:folder_id>")
def copy_folder_endpoint(
    folder_id: int,
    destination: str,
    exclude: Optional[str],
    copy_options: CopyOptions = field(default_factory=CopyOptions),
) -> Response:
    f, dest, compiled = get_copy_folder_params(folder_id, destination, exclude)
    o = get_current_user_group_object()
    nf = Folder.find_by_path(dest)
    if not nf:
        validate_item_and_create_intermediate_folders(dest, BlockType.Folder, o)
        nf = Folder.create(
            dest, o, creation_opts=FolderCreationOptions(apply_default_rights=True)
        )
    u = get_current_user_object()
    errors = copy_folder(f, nf, u, compiled, copy_options)
    if errors and copy_options.stop_on_errors:
        db.session.rollback()
    else:
        db.session.commit()
    return json_response({"new_folder": nf, "errors": [str(e) for e in errors]})


def get_pattern(exclude: Optional[str]) -> Pattern[str]:
    if not exclude:
        exclude = "a^"
    try:
        return re.compile(exclude)
    except:
        raise RouteException(f"Wrong pattern format: {exclude}")


@manage_page.post("/copy/<int:folder_id>/preview")
def copy_folder_preview(
    folder_id: int, destination: str, exclude: Optional[str]
) -> Response:
    f, dest, compiled = get_copy_folder_params(folder_id, destination, exclude)
    preview_list = []
    for i in enum_items(f, compiled):
        preview_list.append(
            {
                "to": join_location(dest, i.get_relative_path(f.path)),
                "from": i.path,
            }
        )
    return json_response(
        {
            "preview": preview_list,
            "dest_exists": Folder.find_by_path(dest) is not None,
        }
    )


def enum_items(folder: Folder, exclude_re: Pattern) -> Generator[Item, None, None]:
    for d in folder.get_all_documents(include_subdirs=False):
        if not exclude_re.search(d.path):
            yield d
    for f in folder.get_all_folders():
        if not exclude_re.search(f.path):
            yield f
            yield from enum_items(f, exclude_re)


def copy_folder(
    from_folder: Folder,
    to_folder: Folder,
    user_who_copies: User,
    exclude_re: Pattern,
    options: CopyOptions,
) -> list[ValidationException]:
    errors = []
    process_queue: list[tuple[Folder, Folder]] = [(from_folder, to_folder)]

    while process_queue:
        f_from, f_to = process_queue.pop()
        db.session.flush()
        if not user_who_copies.can_write_to_folder(f_to):
            raise AccessDenied(f"Missing edit access to folder {f_to.path}")
        if not user_who_copies.has_copy_access(f_from):
            raise AccessDenied(f"Missing copy access to folder {f_from.path}")
        folder_opts = FolderCreationOptions(get_templates_rights_from_parent=False)
        for d in f_from.get_all_documents(include_subdirs=False):
            if exclude_re.search(d.path):
                continue
            if not user_who_copies.has_copy_access(d):
                raise AccessDenied(f"Missing copy access to document {d.path}")
            nd_path = join_location(f_to.path, d.short_name)
            if DocEntry.find_by_path(nd_path):
                raise AccessDenied(f"Document already exists at path {nd_path}")
            nd = DocEntry.create(
                nd_path,
                title=d.title,
                folder_opts=folder_opts,
            )
            copy_rights(
                d,
                nd,
                new_owner=user_who_copies,
                copy_active=options.copy_active_rights,
                copy_expired=options.copy_expired_rights,
            )
            nd.document.modifier_group_id = user_who_copies.get_personal_group().id
            try:
                for tr, new_tr in copy_document_and_enum_translations(
                    d, nd, copy_uploads=True
                ):
                    copy_rights(
                        tr,
                        new_tr,
                        new_owner=user_who_copies,
                        copy_active=options.copy_active_rights,
                        copy_expired=options.copy_expired_rights,
                    )
            except ValidationException as e:
                errors.append(e)
                if options.stop_on_errors:
                    return errors

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
                copy_rights(
                    f,
                    nf,
                    new_owner=user_who_copies,
                    copy_active=options.copy_active_rights,
                    copy_expired=options.copy_expired_rights,
                )
            process_queue.append((f, nf))

    return errors
