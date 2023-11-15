import base64
import time
from enum import Flag
from dataclasses import dataclass
from operator import attrgetter
from typing import Any

from flask import Response, request
from sqlalchemy import select, delete

from timApp.auth.accesshelper import (
    verify_admin,
    check_admin_access,
    AccessDenied,
    verify_logged_in,
)
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.auth.sessioninfo import (
    get_current_user_object,
    get_current_user_group_object,
)
from timApp.document.create_item import apply_template, create_document
from timApp.document.docinfo import DocInfo
from timApp.item.validation import ItemValidationRule
from timApp.timdb.sqa import db, run_sql
from timApp.user.special_group_names import (
    SPECIAL_GROUPS,
    PRIVILEGED_GROUPS,
    SPECIAL_USERNAMES,
)
from timApp.user.user import User, view_access_set, edit_access_set
from timApp.user.usergroup import UserGroup, get_groups_by_ids
from timApp.util.flask.requesthelper import load_data_from_req, RouteException, NotExist
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import remove_path_special_chars, get_current_time
from timApp.util.logger import log_error
from tim_common.marshmallow_dataclass import class_schema

groups = TypedBlueprint("groups", __name__, url_prefix="/groups")

USER_NOT_FOUND = "User not found"


class NameValidationFlags(Flag):
    RequireDigits = 1
    AllowUpperCase = 2
    AllowBase64 = 4


def verify_groupadmin(
    require: bool = True,
    user: User | None = None,
    action: str | None = None,
    msg: str | None = None,
):
    curr_user = user
    if curr_user is None:
        curr_user = get_current_user_object()
    if not check_admin_access(user=user):
        if not UserGroup.get_groupadmin_group() in curr_user.groups:
            if require:
                msg = msg or "This action requires group administrator rights."
                if action:
                    msg = action + ": " + msg
                raise AccessDenied(msg)
            else:
                return False
    return True


def get_uid_gid(
    group_name: str, usernames_or_emails: list[str]
) -> tuple[UserGroup, list[User]]:
    users = (
        run_sql(
            select(User).filter(
                User.name.in_(usernames_or_emails) | User.email.in_(usernames_or_emails)
            )
        )
        .scalars()
        .all()
    )
    group = (
        run_sql(select(UserGroup).filter_by(name=group_name).limit(1)).scalars().first()
    )
    raise_group_not_found_if_none(group_name, group)
    return group, users


@dataclass(frozen=True, slots=True)
class UserGroupMeta:
    id: int
    name: str
    managed: bool


@groups.get("/special")
def get_special_groups() -> Response:
    """
    Gets a list of special public metagroups that can be used to target users.

    :return: A JSON list of common metagroups (logged-in users, anonymous users, etc.)
    """
    res = [UserGroup.get_anonymous_group(), UserGroup.get_logged_in_group()]
    return json_response(
        [UserGroupMeta(id=ug.id, name=ug.name, managed=False) for ug in res]
    )


@groups.get("/getOrgs")
def get_organizations() -> Response:
    return json_response(UserGroup.get_organizations())


@groups.get("/show/<group_name>")
def show_members(group_name: str) -> Response:
    ug = get_group_or_abort(group_name)
    verify_group_view_access(ug)
    return json_response(sorted(list(ug.users), key=attrgetter("id")))


@groups.get("/usergroups")
def get_current() -> Response:
    """
    Gets a list of all user groups the current logged-in user belongs to.

    :return: A JSON list of user groups.
    """
    verify_logged_in()
    u = get_current_user_object()

    # TODO: Additionally maybe return admin doc path if the user has access to it
    return json_response(
        [
            UserGroupMeta(id=ug.id, name=ug.name, managed=ug.admin_doc is not None)
            for ug in u.groups
        ]
    )


@groups.get("/usergroups/<username>")
def show_usergroups(username: str) -> Response:
    verify_admin()
    u = User.get_by_name(username)
    if not u:
        raise NotExist(USER_NOT_FOUND)
    return json_response(
        run_sql(u.get_groups(include_special=False).order_by(UserGroup.name))
        .scalars()
        .all()
    )


@groups.get("/belongs/<username>/<group_name>")
def belongs(username: str, group_name: str) -> Response:
    ug = get_group_or_abort(group_name)
    verify_group_view_access(ug)
    u = User.get_by_name(username)
    if not u:
        raise NotExist(USER_NOT_FOUND)
    return json_response({"status": ug in u.groups})


def get_group_or_abort(group_name: str):
    ug = UserGroup.get_by_name(group_name)
    raise_group_not_found_if_none(group_name, ug)
    return ug


def raise_group_not_found_if_none(group_name: str, ug: UserGroup | None):
    if not ug:
        raise RouteException(f'User group "{group_name}" not found')


@groups.get("/create/<path:group_path>")
def create_group(group_path: str) -> Response:
    """Route for creating a user group.

    The name of user group has the following restrictions:

     1. The name must have at least one digit.
     2. The name must have at least one alphabetic character.
     3. The name must NOT have any non-alphanumeric characters, with the exception that spaces are allowed.

    These restrictions are needed in order to distinguish manually-created groups from personal user groups.
    Personal user group names are either

     1. email addresses (containing '@' character), or
     2. lowercase ASCII strings (Korppi users) with length being in range [2,8].

    """
    encode_name = request.args.get("encodeGroupName")
    _, doc = do_create_group(group_path, encode_name)
    db.session.commit()
    return json_response(doc)


def do_create_group(
    group_path: str, encode_name: bool | None = None
) -> tuple[UserGroup, DocInfo]:
    group_path = group_path.strip("/ ")

    # The name of the user group is separated from the path.
    # Does not check whether a name or a path is missing.
    group_name = group_path.split("/")[-1]

    validation_flags = None
    if encode_name:
        timestamp: int = int(time.time_ns() / 1_000_000)

        # We need a prefix marker to signify that the group name is in encoded form
        encoding = "b64_"
        encoded_name_str = str(
            base64.standard_b64encode(f"{group_name}_{timestamp}".encode()),
            encoding="utf-8",
        )
        name = f"{encoding}{encoded_name_str}"
        group_name = name
        validation_flags = (
            NameValidationFlags.AllowBase64 | NameValidationFlags.AllowUpperCase
        )

    if UserGroup.get_by_name(group_name):
        raise RouteException("User group already exists.")

    verify_groupadmin(action=f"Creating group {group_name}")

    validate_groupname(group_name, validation_flags)

    # To support legacy code:
    # The group administrator has always writing permission to the groups' root folder.
    # Creating a new user group into the root folder named groups is always allowed.
    # Elsewhere, the current user must have group administrator rights.
    creating_subdirectory = "/" in group_path
    parent_owner = (
        get_current_user_group_object()
        if creating_subdirectory
        else UserGroup.get_admin_group()
    )

    # TODO Will special chars in encoded group name be a problem for creating related
    #      folders/documents?
    doc = create_document(
        f"groups/{remove_path_special_chars(group_path)}",
        group_name,
        validation_rule=ItemValidationRule(check_write_perm=creating_subdirectory),
        parent_owner=parent_owner,
    )
    apply_template(doc)
    update_group_doc_settings(doc, group_name)
    add_group_infofield_template(doc)
    u = UserGroup.create(group_name)
    u.admin_doc = doc.block
    f = doc.parent
    if len(f.block.accesses) == 1:
        logged_group = UserGroup.get_logged_in_group()
        f.block.accesses[(logged_group.id, AccessType.view.value)] = BlockAccess(
            usergroup_id=logged_group.id,
            type=AccessType.view.value,
            accessible_from=get_current_time(),
        )
    return u, doc


def add_group_infofield_template(doc: DocInfo):
    text = """
## Omia kenttiä {defaultplugin="textfield" readonly="view" .hidden-print}
{#info autosave: true #}    
    """
    doc.document.add_text(text)


def update_group_doc_settings(
    doc: DocInfo, group_name: str, extra_macros: dict[str, Any] = None
):
    s = doc.document.get_settings().get_dict().get("macros", {})
    s["group"] = group_name
    s["fields"] = ["info"]
    s["maxRows"] = "40em"  # max rows for group list
    if extra_macros:
        s.update(extra_macros)
    doc.document.add_setting("macros", s)


def validate_groupname(group_name: str, flags: NameValidationFlags | None = None):
    has_digits = False
    has_letters = False
    has_upper_letters = False
    has_non_alnum = False
    # Note: If future expansion is needed, create a new variable to hold the chars,
    #       do *not* expand the existing one unless the leveraged spec for Base64 has changed.
    # TODO: Should we make this a configurable property?
    base64_chars = "+/=" if NameValidationFlags.AllowBase64 in flags else ""
    allowed_special_chars = f"-_{base64_chars}"
    for c in group_name:
        has_digits = has_digits or c.isdigit()
        has_letters = has_letters or c.isalpha()
        has_upper_letters = has_upper_letters or c.isupper()
        has_non_alnum = has_non_alnum or not (
            c.isalnum() or c.isspace() or c in allowed_special_chars
        )
    if (
        (NameValidationFlags.RequireDigits in flags and not has_digits)
        or not has_letters
        or has_non_alnum
        or (NameValidationFlags.AllowUpperCase not in flags and has_upper_letters)
    ):
        raise RouteException(
            'User group must contain at least one digit and one letter and must not have uppercase or special chars: "'
            + group_name
            + '"'
        )


def verify_group_access(ug: UserGroup, access_set, u=None, require=True):
    if ug.name in PRIVILEGED_GROUPS:
        return verify_admin(require=require, user=u)
    if not u:
        u = get_current_user_object()
    if u.get_personal_group() == ug:
        return True
    b = ug.admin_doc
    no_access_msg = f"No access for group {ug.name}"
    if not b:
        return verify_groupadmin(require=require, user=u, msg=no_access_msg)
    else:
        if not u.has_some_access(b, access_set):
            return verify_groupadmin(require=require, user=u, msg=no_access_msg)
        return True


def verify_group_edit_access(ug: UserGroup, user: User | None = None, require=True):
    if ug.name in SPECIAL_GROUPS:
        raise RouteException(f"Cannot edit special group: {ug.name}")
    if User.get_by_name(ug.name):
        raise RouteException(f"Cannot edit personal group: {ug.name}")
    if ug.name.startswith("cumulative:") or ug.name.startswith("deleted:"):
        raise RouteException(f"Cannot edit special Sisu group: {ug.name}")
    return verify_group_access(ug, edit_access_set, user, require=require)


def verify_group_view_access(ug: UserGroup, user=None, require=True):
    return verify_group_access(ug, view_access_set, user, require=require)


@dataclass(slots=True, frozen=True)
class MemberInfos:
    existing_ids: set[int]
    group: UserGroup
    not_exist: list[str]
    users: list[User]


def get_member_infos(group_name: str, usernames_or_emails: list[str]) -> MemberInfos:
    usernames_or_emails = get_usernames(usernames_or_emails)
    group, users = get_uid_gid(group_name, usernames_or_emails)
    verify_group_edit_access(group)
    existing_usernames = {u.name for u in users}
    existing_emails = {u.email for u in users}
    existing_ids = {u.id for u in group.users}
    not_exist = [
        name
        for name in usernames_or_emails
        if name not in existing_usernames and name not in existing_emails
    ]

    return MemberInfos(existing_ids, group, not_exist, users)


@dataclass
class NamesModel:
    names: list[str]


NamesModelSchema = class_schema(NamesModel)


@groups.post("/addmember/<group_name>")
def add_member(group_name: str) -> Response:
    nm: NamesModel = load_data_from_req(NamesModelSchema)
    mi = get_member_infos(group_name, nm.names)
    found_user_names = {u.name for u in mi.users}
    if found_user_names & SPECIAL_USERNAMES:
        raise RouteException("Cannot add special users.")
    user_names = {u.name for u in mi.group.users}
    already_exists = user_names & found_user_names
    added = []
    curr = get_current_user_object()
    for u in mi.users:
        if u.id not in mi.existing_ids:
            u.add_to_group(mi.group, added_by=curr)
            added.append(u.name)
    db.session.commit()
    return json_response(
        {
            "already_belongs": sorted(list(already_exists)),
            "added": sorted(added),
            "not_exist": sorted(mi.not_exist),
        }
    )


@groups.post("/removemember/<group_name>")
def remove_member(group_name: str) -> Response:
    nm: NamesModel = load_data_from_req(NamesModelSchema)
    mi = get_member_infos(group_name, nm.names)
    removed = []
    does_not_belong = []
    ensure_manually_added = mi.group.is_sisu
    su = User.get_scimuser()
    for u in mi.users:
        if u.id not in mi.existing_ids:
            does_not_belong.append(u.name)
            continue
        if ensure_manually_added and mi.group.current_memberships[u.id].adder == su:
            raise RouteException(
                "Cannot remove not-manually-added users from Sisu groups."
            )
        mi.group.current_memberships[u.id].set_expired()
        removed.append(u.name)
    db.session.commit()
    return json_response(
        {
            "removed": sorted(removed),
            "does_not_belong": sorted(does_not_belong),
            "not_exist": sorted(mi.not_exist),
        }
    )


def get_usernames(usernames: list[str]):
    usernames = list({n for name in usernames if (n := name.strip())})
    usernames.sort()
    return usernames


@groups.post("/copymemberships/<source>/<target>")
def copy_members(source: str, target: str) -> Response:
    """
    Copies group memberships from one UserGroup to another.

    Note: this function is intended to be used in conjunction with the group-management component,
    see `timApp/static/scripts/tim/ui/group-management.component.ts`. We should probably limit
    the database queries to only the base64-encoded names, since the group-management
    component is currently configured to produce such names by default.
    :param source: source UserGroup name
    :param target: target UserGroup name
    :return: Response with added member names, or error message
    """
    from timApp.auth.login_code.routes import decode_name

    if source == target:
        return json_response(
            status_code=400,
            jsondata={
                "result": {
                    "error": f"Copying group members failed: source ('{source}') and target ('{target}') are the same."
                },
            },
        )

    # We need to do some shenanigans here because group names might be base64-encoded.
    # Try plain names first, then base64-encoded ones.

    source_group = (
        run_sql(select(UserGroup).filter_by(name=source).limit(1)).scalars().first()
    )
    target_group = (
        run_sql(select(UserGroup).filter_by(name=target).limit(1)).scalars().first()
    )
    if not source_group or not target_group:
        b64groups: list[UserGroup] = list(
            run_sql(select(UserGroup).where(UserGroup.name.like("b64_%")))
            .scalars()
            .all()
        )
        for ug in b64groups:
            ug_plain_name = decode_name(ug.name)
            if not source_group and ug_plain_name == source:
                source_group = ug
            elif not target_group and ug_plain_name == target:
                target_group = ug

        missing_groups = []
        if not source_group:
            missing_groups.append(source)
        if not target_group:
            missing_groups.append(target)

        if missing_groups:
            return json_response(
                status_code=404,
                jsondata={
                    "result": {
                        "error": f"Copying group members failed: groups {missing_groups} do not exist."
                    },
                },
            )

    current_user = get_current_user_object()

    from timApp.auth.accesshelper import verify_ownership
    from timApp.document.routes import get_doc_or_abort
    from timApp.user.usergroup import UserGroupDoc

    source_group_doc_id: int = (
        run_sql(
            select(UserGroupDoc.doc_id)
            .where(UserGroupDoc.group_id == source_group.id)
            .limit(1)
        )
        .scalars()
        .first()
    )
    target_group_doc_id: int = (
        run_sql(
            select(UserGroupDoc.doc_id)
            .where(UserGroupDoc.group_id == target_group.id)
            .limit(1)
        )
        .scalars()
        .first()
    )

    source_group_doc = get_doc_or_abort(source_group_doc_id)
    target_group_doc = get_doc_or_abort(target_group_doc_id)

    if (
        not verify_groupadmin(user=current_user)
        or not verify_ownership(b=source_group_doc)
        or not verify_ownership(b=target_group_doc)
    ):
        return json_response(
            status_code=403,
            jsondata={
                "result": {
                    "error": f"Copying group members failed: insufficient permissions."
                },
            },
        )

    members: list[User] = list(source_group.users)
    added_memberships = []
    for u in members:
        u.add_to_group(target_group, current_user)
        added_memberships.append(u.name)
    db.session.commit()
    return json_response(
        status_code=200,
        jsondata={
            "added_members": added_memberships,
        },
    )


@groups.delete("/delete/<int:group_id>")
def delete_group(group_id: int) -> Response:
    """Route for deleting a user group.
    Permanently deletes a UserGroup from the database.
    When calling this function, user should be notified that the operation cannot be undone.

    The group documents will remain in the TIM 'trash' folder, but without database entries they
    should not be able to reference any data.

    :param group_id: ID of the group that should be deleted
    """
    # TODO Return a fail response to user instead of raising RouteException?
    group_name = "\n".join(do_delete_groups([group_id]))
    db.session.commit()
    return json_response(
        status_code=200,
        jsondata={"message": f"Successfully deleted group: {group_name}"},
    )


@groups.delete("/delete")
def mass_delete_groups() -> Response:
    """Route for mass deleting UserGroups.
    Permanently deletes UserGroups from the database.
    When calling this function, user should be notified that the operation cannot be undone.

    The group documents will remain in the TIM 'trash' folder, but without database entries they
    should not be able to reference any data.
    """

    req: dict = request.get_json()
    group_ids: list[int] = req["ids"]

    # TODO Return a fail response to user instead of raising RouteException?
    group_names = "\n".join(do_delete_groups(group_ids))
    db.session.commit()
    return json_response(
        status_code=200,
        jsondata={"message": f"Successfully deleted groups:\n{group_names}"},
    )


def do_delete_groups(group_ids: list[int]) -> list[str]:
    del_groups = get_groups_by_ids(group_ids)
    group_names = list(group.name for group in del_groups)
    # for g in del_groups:
    #     verify_group_edit_access(g)

    from timApp.document.routes import get_doc_or_abort
    from timApp.user.usergroup import UserGroupDoc, UserGroupMember
    from timApp.item.deleting import soft_delete_document

    group_doc_ids: list[int] = list(
        run_sql(
            select(UserGroupDoc.doc_id).filter(UserGroupDoc.group_id.in_(group_ids))
        )
        .scalars()
        .all()
    )

    current_user = get_current_user_object()
    from timApp.auth.accesshelper import verify_ownership

    group_docs = list(get_doc_or_abort(gdid) for gdid in group_doc_ids)
    for group_doc in group_docs:
        # Preferably we should do permissions checks earlier, but we need the references to check for doc ownership
        if not verify_groupadmin(user=current_user) or not verify_ownership(
            b=group_doc
        ):
            log_error(
                f"User {current_user} does not have permission to delete group {group_doc.document.docinfo.title}"
            )
            raise RouteException(
                f"Insufficient permissions to delete group: {group_doc.document.docinfo.title}"
            )

        soft_delete_document(group_doc)
        # TODO Since we already have to clear the database references,
        #  we might as well delete the usergroup doc from disk

    run_sql(
        delete(UserGroupDoc)
        .where(UserGroupDoc.group_id.in_(group_ids))
        .execution_options(synchronize_session="auto")
    )

    run_sql(
        delete(UserGroupMember)
        .where(UserGroupMember.usergroup_id.in_(group_ids))
        .execution_options(synchronize_session="auto")
    )

    run_sql(
        delete(UserGroup)
        .where(UserGroup.id.in_(group_ids))
        .execution_options(synchronize_session="auto")
    )

    return group_names
