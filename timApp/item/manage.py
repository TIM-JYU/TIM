"""Routes for manage view."""
import re
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from re import Pattern
from typing import Generator
from urllib.parse import urlparse, parse_qsl, urlencode, urlunparse

from flask import redirect
from flask import render_template, Response
from flask import request
from isodate import Duration
from sqlalchemy import inspect
from sqlalchemy.orm.state import InstanceState

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
    verify_admin,
)
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import AccessTypeModel, BlockAccess
from timApp.auth.sessioninfo import get_current_user_group_object
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import copy_document_and_enum_translations
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import find_free_name, DocInfo
from timApp.document.exceptions import ValidationException
from timApp.folder.createopts import FolderCreationOptions
from timApp.folder.folder import Folder, path_includes
from timApp.item.block import BlockType, Block, copy_default_rights
from timApp.item.copy_rights import copy_rights
from timApp.item.deleting import soft_delete_document, get_trash_folder
from timApp.item.item import Item, ItemBase
from timApp.item.validation import (
    validate_item,
    validate_item_and_create_intermediate_folders,
    has_special_chars,
)
from timApp.plugin.jsrunner.util import (
    save_fields,
    FieldSaveRequest,
    FieldSaveUserEntry,
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
from timApp.user.userutils import (
    grant_access,
    grant_default_access,
    is_some_default_right_document,
    ReplaceAccessAction,
    expire_access,
)
from timApp.util.flask.requesthelper import (
    get_option,
    RouteException,
    NotExist,
)
from timApp.util.flask.responsehelper import (
    json_response,
    ok_response,
    get_grid_modules,
    safe_redirect,
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
from timApp.velp.velp import delete_velp_group, DEFAULT_PERSONAL_VELP_GROUP_NAME
from timApp.velp.velp_models import VelpGroupsInDocument, VelpGroup
from timApp.velp.velpgroups import (
    get_groups_from_document_table,
)

manage_page = TypedBlueprint(
    "manage_page", __name__, url_prefix=""
)  # TODO: Better URL prefix.


@manage_page.get("/manage/<path:path>")
def manage(path: str) -> Response | str:
    if has_special_chars(path):
        qs = request.query_string.decode("utf-8")
        return redirect(
            remove_path_special_chars(request.path) + (f"?{qs}" if qs else "")
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
    duration: Duration | None = None
    to: datetime | None = None
    ffrom: datetime | None = field(metadata={"data_key": "from"}, default=None)
    durationTo: datetime | None = None
    durationFrom: datetime | None = None

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
class PermissionEditModelBase:
    type: AccessType = field(metadata={"by_value": True})
    time: TimeOpt
    groups: list[str]
    confirm: bool | None

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
class PermissionEditModel(PermissionEditModelBase):
    edit_velp_group_perms: bool = True
    edit_translation_perms: bool = True


@dataclass
class PermissionSingleEditModel(PermissionEditModelBase):
    id: int
    edit_velp_group_perms: bool = True
    edit_translation_perms: bool = True


class DefaultItemType(Enum):
    document = 0
    folder = 1


@dataclass
class DefaultPermissionModelBase:
    item_type: DefaultItemType


@dataclass
class DefaultPermissionModel(PermissionSingleEditModel, DefaultPermissionModelBase):
    pass


@dataclass
class PermissionRemoveModelBase:
    id: int
    type: AccessType = field(metadata={"by_value": True})
    group: int


@dataclass
class PermissionRemoveModel(PermissionRemoveModelBase):
    edit_velp_group_perms: bool = True
    edit_translation_perms: bool = True


@dataclass
class DefaultPermissionRemoveModelBase:
    item_type: DefaultItemType


@dataclass
class DefaultPermissionRemoveModel(
    PermissionRemoveModel, DefaultPermissionRemoveModelBase
):
    pass


@dataclass
class PermissionMassEditModelBase:
    ids: list[int]
    action: EditOption = field(metadata={"by_value": True})


@dataclass
class PermissionMassEditModel(PermissionEditModel, PermissionMassEditModelBase):
    pass


@manage_page.get("/permissions/add/<int:doc_id>/<username>")
def add_permission_basic(
    doc_id: int, username: str, type: str, duration: int
) -> Response:
    if type != "view":
        raise RouteException("Only 'view' is allowed to prevent misuse")

    i = get_item_or_abort(doc_id)

    settings = i.document.get_settings()
    if not settings.allow_url_permission_edits():
        raise AccessDenied(
            "The document permissions cannot be edited via URLs. Add `allow_url_permission_edits: true` to document settings to allow this."
        )

    verify_permission_edit_access(i, AccessType.view)

    p_model = PermissionEditModel(
        type=AccessType.view,
        groups=[username],
        time=TimeOpt(
            type=TimeType.duration,
            duration=Duration(hours=duration),
        ),
        confirm=False,
    )

    accs = add_perm(
        p_model,
        i,
        replace_active_duration=False,
    )
    res_message = ""
    if accs:
        a = accs[0]
        a_info: InstanceState = inspect(a)
        if a_info.transient or a_info.pending:
            log_right(f"added {a.info_str} for {username} in {i.path}")
            res_message = "Added right"
        elif a_info.modified:
            log_right(f"updated to {a.info_str} for {username} in {i.path}")
            res_message = "Updated existing right"
        else:
            log_right(
                f"skipped {a.info_str} for {username} in {i.path} because an active right already exists"
            )
            res_message = "Skipped, because an active right already exists. Expire the active right first."
        db.session.commit()
    return json_response({"message": res_message})


@manage_page.put("/permissions/add", model=PermissionSingleEditModel)
def add_permission(m: PermissionSingleEditModel):
    i = get_item_or_abort(m.id)
    is_owner = verify_permission_edit_access(i, m.type)
    accs = add_perm(m, i)
    if accs:
        check_ownership_loss(is_owner, i)

        log_right(
            f"added {accs[0].info_str} for {seq_to_str(m.groups)} in {seq_to_str(list(str(x.block.id) for x in accs))}"
        )

        db.session.commit()
    return permission_response(m)


def permission_response(m: PermissionEditModel):
    return json_response({"not_exist": m.nonexistent_groups})


def log_right(s: str):
    u = get_current_user_object()
    log_info(f"RIGHTS: {u.name} {s}")


def log_task_block(s: str):
    u = get_current_user_object()
    log_info(f"TASKBLOCK: {u.name} {s}")


def get_group_and_doc(doc_id: int, username: str) -> tuple[UserGroup, DocInfo]:
    i = get_item_or_abort(doc_id)
    verify_permission_edit_access(i, AccessType.view)
    g = UserGroup.get_by_name(username)
    if not g:
        raise RouteException("User not found")
    return g, i


def raise_or_redirect(message: str, redir: str | None = None) -> Response:
    if not redir:
        raise RouteException(message)
    url = list(urlparse(redir))
    query = dict(parse_qsl(url[4]))
    query |= {"error": message}
    url[4] = urlencode(query)
    return safe_redirect(urlunparse(url))


def is_velp_group_in_document(vg: VelpGroup, d: ItemOrBlock) -> bool:
    """Check that the velp group is in the correct path in relation to the parent document"""
    res = False
    vg_path = get_doc_or_abort(vg.id).path
    doc_name = d.short_name
    doc_folder = d.path_without_lang.rsplit("/", maxsplit=1)[0]
    if vg_path == f"{doc_folder}/velp-groups/{doc_name}/{vg.name}":
        res = True
    return res


@manage_page.get("/permissions/expire/<int:doc_id>/<username>")
def expire_permission_url(doc_id: int, username: str, redir: str | None = None):
    g, i = get_group_and_doc(doc_id, username)
    ba, was_expired = expire_access(g, i, AccessType.view)
    if not ba:
        return raise_or_redirect("Right not found.", redir)
    if was_expired:
        return raise_or_redirect("Right is already expired.", redir)
    # also expire permissions for document's velp groups and translations
    expire_doc_velp_groups_perms(i.id, g)
    expire_doc_translation_perms(i.id, g)

    db.session.commit()
    return ok_response() if not redir else safe_redirect(redir)


def expire_doc_velp_groups_perms(doc_id: int, ug: UserGroup) -> None:
    """Expire view permissions for a document's VelpGroups for a specific UserGroup

    :param doc_id: ID for the document
    :param ug: UserGroup whose permissions will be expired
    """
    vgs = get_groups_from_document_table(doc_id, None)
    doc = get_doc_or_abort(doc_id)
    accs: list[BlockAccess] = []
    for vg in vgs:
        # Only expire permissions from velp groups attached to the document
        if is_velp_group_in_document(vg, doc):
            # TODO Should this apply to ALL permissions, instead of just 'view'?
            acc: BlockAccess | None = BlockAccess.query.filter_by(
                type=AccessType.view.value,
                block_id=vg.id,
                usergroup_id=ug.id,
            ).first()
            if acc:
                accs.append(acc)
    for a in accs:
        a.accessible_to = get_current_time()
        if a.duration:
            a.duration, a.duration_from, a.duration_to = None, None, None


def expire_doc_translation_perms(doc_id: int, ug: UserGroup) -> None:
    """Expire view permissions for a document's translations for a specific UserGroup

    :param doc_id: ID for the document
    :param ug: UserGroup whose permissions will be expired
    """
    doc = get_doc_or_abort(doc_id)
    accs: list[BlockAccess] = []
    for tr in doc.translations:
        if tr.id == doc.id:
            continue
        # TODO Should this apply to ALL permissions, instead of just 'view'?
        acc: BlockAccess | None = BlockAccess.query.filter_by(
            type=AccessType.view.value,
            block_id=tr.id,
            usergroup_id=ug.id,
        ).first()
        if acc:
            accs.append(acc)
    for a in accs:
        a.accessible_to = get_current_time()
        if a.duration:
            a.duration, a.duration_from, a.duration_to = None, None, None


@manage_page.get("/permissions/confirm/<int:doc_id>/<username>")
def confirm_permission_url(doc_id: int, username: str, redir: str | None = None):
    g, i = get_group_and_doc(doc_id, username)
    m = PermissionRemoveModel(id=doc_id, type=AccessType.view, group=g.id)
    return do_confirm_permission(m, i, redir)


@manage_page.put("/permissions/confirm", model=PermissionRemoveModel)
def confirm_permission(m: PermissionRemoveModel) -> Response:
    i = get_item_or_abort(m.id)
    verify_permission_edit_access(i, m.type)
    return do_confirm_permission(m, i)


def do_confirm_permission(
    m: PermissionRemoveModel,
    i: DocInfo,
    redir: str | None = None,
    confirm_translations: bool = True,
):
    ba: BlockAccess | None = BlockAccess.query.filter_by(
        type=m.type.value,
        block_id=m.id,
        usergroup_id=m.group,
    ).first()
    if not ba:
        return raise_or_redirect("Right not found.", redir)
    if not ba.require_confirm:
        return raise_or_redirect(
            f"{m.type.name} right for {ba.usergroup.name} does not require confirmation or it was already confirmed.",
            redir,
        )
    ba.do_confirm()
    ug: UserGroup = UserGroup.query.get(m.group)
    log_right(f"confirmed {ba.info_str} for {ug.name} in {i.path}")

    if confirm_translations and i.is_original_translation:
        for tr in i.translations:
            if tr.is_original_translation:
                continue
            try:
                do_confirm_permission(
                    PermissionRemoveModel(tr.block.id, m.type, m.group),
                    tr,
                    redir,
                    confirm_translations=True,
                )
            except RouteException:
                # TODO Do proper exception management here, user should have a list/dialog shown that specifies
                #  which permissions were not set (see /static/scripts/tim/item/rightsEditor.ts)
                pass

    db.session.commit()

    return ok_response() if not redir else safe_redirect(redir)


@manage_page.put("/permissions/edit", model=PermissionMassEditModel)
def edit_permissions(m: PermissionMassEditModel) -> Response:
    groups = m.group_objects
    nonexistent = set(m.groups) - {g.name for g in groups}
    if nonexistent:
        raise RouteException(f"Non-existent groups: {nonexistent}")
    items: list[ItemOrBlock] = (
        Block.query.filter(
            Block.id.in_(m.ids)
            & Block.type_id.in_([BlockType.Document.value, BlockType.Folder.value])
        )
        .order_by(Block.id)
        .all()
    )

    modified_permissions = []
    owned_items_before = set()
    for i in items:
        checked_owner = verify_permission_edit_access(i, m.type)
        if checked_owner:
            owned_items_before.add(i)
        if m.action == EditOption.Add:
            accs = add_perm(m, i)
            if accs:
                modified_permissions.extend(accs)
        else:
            for g in groups:
                removed = remove_perm(
                    g, i, m.type, m.edit_translation_perms, m.edit_velp_group_perms
                )
                modified_permissions.extend(removed)

    if m.type == AccessType.owner:
        owned_items_after = set()
        u = get_current_user_object()
        for i in items:
            if u.has_ownership(i):
                owned_items_after.add(i)
        if owned_items_before != owned_items_after:
            raise AccessDenied("You cannot remove ownership from yourself.")

    if modified_permissions:
        action = "added" if m.action == EditOption.Add else "removed"
        for p in modified_permissions:
            log_right(
                f"{action} {p.info_str} for {seq_to_str(m.groups)} in {seq_to_str(list(str(x) for x in m.ids))}"
            )
        db.session.commit()

    return permission_response(m)


def add_perm(
    p: PermissionEditModel,
    item: ItemBase | Block,
    replace_active_duration: bool
    | ReplaceAccessAction = ReplaceAccessAction.AlwaysReplace,
) -> list[BlockAccess]:
    if get_current_user_object().get_personal_folder().id == item.id:
        if p.type == AccessType.owner:
            raise AccessDenied("You cannot add owners to your personal folder.")
    opt = p.time.effective_opt
    accs = []

    doc = item.docentries[0] if isinstance(item, Block) and item.docentries else item
    docs: list[Block] = (
        doc.translations
        if (
            isinstance(doc, DocInfo)
            and p.edit_translation_perms
            and doc.is_original_translation
        )
        else [doc]
    )

    if p.edit_velp_group_perms:
        # Unfortunately we have to do a db query here
        if isinstance(item, DocInfo):
            vgs = get_groups_from_document_table(item.id, None)
            for vg in vgs:
                # Don't add permissions to users' personal velp groups,
                # only add perms to document velp groups in the path:
                # [doc_folder]/velp-groups/[doc-name]/[velp_group]
                if (
                    not vg.name == DEFAULT_PERSONAL_VELP_GROUP_NAME
                    and is_velp_group_in_document(vg, doc)
                ):
                    docs.append(vg.block)

    for d in docs:
        for group in p.group_objects:
            a = grant_access(
                group,
                d,
                p.type,
                accessible_from=opt.ffrom,
                accessible_to=opt.to,
                duration_from=opt.durationFrom,
                duration_to=opt.durationTo,
                duration=opt.duration,
                require_confirm=p.confirm,
                replace_active_duration=replace_active_duration,
            )
            accs.append(a)
    return accs


@manage_page.put("/permissions/remove", model=PermissionRemoveModel)
def remove_permission(m: PermissionRemoveModel) -> Response:
    i = get_item_or_abort(m.id)
    had_ownership = verify_permission_edit_access(i, m.type)
    # ug: UserGroup = UserGroup.query.get(m.group)  # query.get() is deprecated
    ug: UserGroup = UserGroup.query.filter_by(id=m.group).first()
    if not ug:
        raise RouteException("User group not found")
    a = remove_perm(
        ug, i.block, m.type, m.edit_translation_perms, m.edit_velp_group_perms
    )
    check_ownership_loss(had_ownership, i)

    log_right(
        f"removed {a[0].info_str} for {ug.name} in {seq_to_str(list(str(x.block_id) for x in a))}"
    )

    db.session.commit()
    return ok_response()


@dataclass
class PermissionClearModelBase:
    paths: list[str]
    type: AccessType = field(metadata={"by_value": True})


@dataclass
class PermissionClearModel(PermissionClearModelBase):
    edit_velp_group_perms: bool = True
    edit_translation_perms: bool = True


@manage_page.put("/permissions/clear", model=PermissionClearModel)
def clear_permissions(m: PermissionClearModel) -> Response:
    for p in m.paths:
        i = DocEntry.find_by_path(p, try_translation=True)
        if not i:
            i = Folder.find_by_path(p)
        if not i:
            raise RouteException(f"Item not found: {p}")
        verify_ownership(i)
        clear_doc_permissions(i, m.type)

        # Clear permissions from document's velp groups and translations
        if m.edit_velp_group_perms and isinstance(i, DocInfo | DocEntry):
            vgs = get_groups_from_document_table(i.id, None)
            for vg in vgs:
                # Only clear perms from velp groups attached to the document
                if is_velp_group_in_document(vg, i):
                    clear_doc_permissions(vg, m.type)

        if m.edit_translation_perms:
            for tr in i.translations:
                clear_doc_permissions(tr, m.type)

    db.session.commit()
    return ok_response()


def clear_doc_permissions(doc: DocInfo | DocEntry, a: AccessType) -> None:
    doc.block.accesses = {
        (ugid, permtype): v
        for (ugid, permtype), v in doc.block.accesses.items()
        if permtype != a.value
    }


# noinspection PyShadowingBuiltins
@manage_page.post("/permissions/selfExpire")
def self_expire_permission(id: int, set_field: str | None = None) -> Response:
    i = get_item_or_abort(id)
    if verify_admin(require=False):
        raise RouteException("Admins cannot expire their own permissions.")

    acc = verify_view_access(i, require=False)
    if not acc:
        return ok_response()
    acc = get_single_view_access(i)

    if set_field:
        cur_user = get_current_user_object()
        save_fields(
            FieldSaveRequest(
                savedata=[
                    FieldSaveUserEntry(
                        user=cur_user.id,
                        fields={set_field: "1"},
                    ),
                ]
            ),
            curr_user=cur_user,
            allow_non_teacher=True,
            current_doc=i,
        )

    acc.accessible_to = get_current_time()

    # Expire perms for document's velp groups and translations
    if isinstance(i, DocInfo | DocEntry):
        expire_doc_velp_groups_perms(i.id, acc.usergroup)
        expire_doc_translation_perms(i.id, acc.usergroup)

    db.session.commit()
    log_right(f"self-expired view access in {i.path}")
    return ok_response()


def remove_perm(
    group: UserGroup,
    b: Block,
    t: AccessType,
    process_translations: bool = True,
    process_velp_groups: bool = True,
) -> list[BlockAccess]:
    """
    Remove permissions from items.
    :param group: UserGroup whose permissions will be revoked.
    :param b: Item to remove permissions from.
    :param t: Type of permission to remove.
    :param process_translations: Whether a document's Translations should also have the corresponding
                              permissions removed.
    :param process_velp_groups: Whether a document's VelpGroups should also have the corresponding
                              permissions removed.
    :return: List of removed permissions.
    """

    item = b.docentries[0] if (isinstance(b, Block) and b.docentries) else b
    items: list[Block] = (
        item.translations
        if (
            isinstance(item, DocInfo)
            and process_translations
            and item.is_original_translation
        )
        else [item]
    )

    if process_velp_groups:
        # Unfortunately we have to do a db query here
        vgs = get_groups_from_document_table(b.docentries[0].id, None)
        for vg in vgs:
            # Remove perms only from velp groups attached to the document
            if is_velp_group_in_document(vg, b):
                items.append(vg.block)

    removed_perms = []
    for d in items:
        perm = remove_access(group, d, t)
        # remove_access returns None if the permissions wasn't found in the item's BlockAccess list,
        # we do not want to carry it further
        if perm:
            removed_perms.append(perm)
    return removed_perms


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
        dst_f = Folder.find_first_existing(new_alias)
        if not get_current_user_object().can_write_to_folder(dst_f):
            raise AccessDenied(
                "You don't have permission to write to the destination folder."
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


@manage_page.put("/defaultPermissions/add", model=DefaultPermissionModel)
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


@manage_page.put("/defaultPermissions/remove", model=DefaultPermissionRemoveModel)
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
    soft_delete_document(d)

    # remove attached velp groups
    vgs: list[VelpGroup] = get_groups_from_document_table(d.id, None)
    for vg in vgs:
        # remove all permissions from velp groups attached to the document
        if is_velp_group_in_document(vg, d):
            vg.block.accesses.clear()
            # delete velp group and db references
            delete_velp_group(vg)

    db.session.commit()
    return ok_response()


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


def get_copy_folder_params(folder_id: int, dest: str, exclude: str | None):
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
    exclude: str | None,
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


def get_pattern(exclude: str | None) -> Pattern[str]:
    if not exclude:
        exclude = "a^"
    try:
        return re.compile(exclude)
    except:
        raise RouteException(f"Wrong pattern format: {exclude}")


@manage_page.post("/copy/<int:folder_id>/preview")
def copy_folder_preview(
    folder_id: int, destination: str, exclude: str | None
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
    user_who_copies_group = user_who_copies.get_personal_group()
    default_right_documents = []
    folder_opts = FolderCreationOptions(get_templates_rights_from_parent=False)

    def do_copy_doc(doc: DocInfo, folder_to: Folder) -> bool | None:
        if exclude_re.search(doc.path):
            return False
        if not user_who_copies.has_copy_access(doc):
            raise AccessDenied(f"Missing copy access to document {doc.path}")
        nd_path = join_location(folder_to.path, doc.short_name)
        if DocEntry.find_by_path(nd_path):
            raise AccessDenied(f"Document already exists at path {nd_path}")
        nd = DocEntry.create(
            nd_path,
            title=doc.title,
            folder_opts=folder_opts,
        )
        copy_rights(
            doc,
            nd,
            new_owner=user_who_copies,
            copy_active=options.copy_active_rights,
            copy_expired=options.copy_expired_rights,
        )
        copy_default_rights(
            nd, BlockType.Document, owners_to_skip=[user_who_copies_group]
        )
        nd.document.modifier_group_id = user_who_copies_group.id
        try:
            for tr, new_tr in copy_document_and_enum_translations(
                doc, nd, copy_uploads=True
            ):
                copy_rights(
                    tr,
                    new_tr,
                    new_owner=user_who_copies,
                    copy_active=options.copy_active_rights,
                    copy_expired=options.copy_expired_rights,
                )
                copy_default_rights(
                    new_tr,
                    BlockType.Document,
                    owners_to_skip=[user_who_copies_group],
                )
        except ValidationException as e:
            errors.append(e)
            if options.stop_on_errors:
                return None
        return True

    while process_queue:
        f_from, f_to = process_queue.pop()
        db.session.flush()
        if not user_who_copies.can_write_to_folder(f_to):
            raise AccessDenied(f"Missing edit access to folder {f_to.path}")
        if not user_who_copies.has_copy_access(f_from):
            raise AccessDenied(f"Missing copy access to folder {f_from.path}")

        for d in f_from.get_all_documents(include_subdirs=False):
            if is_some_default_right_document(d):
                default_right_documents.append((d, f_to))
                continue

            match do_copy_doc(d, f_to):
                case False:
                    continue
                case None:
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
                copy_default_rights(
                    nf,
                    BlockType.Folder,
                    owners_to_skip=[user_who_copies_group],
                )
            process_queue.append((f, nf))

    # Copy default permissions last to ensure they are not re-applied during copying, which
    # can cause DB persistence errors
    for d, f_to in default_right_documents:
        db.session.flush()
        match do_copy_doc(d, f_to):
            case False:
                continue
            case None:
                return errors

    return errors
