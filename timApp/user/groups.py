import dataclasses
from dataclasses import dataclass
from enum import Flag
from operator import attrgetter
from typing import Any

from flask import Response, current_app
from sqlalchemy import select

from timApp.auth.accesshelper import (
    verify_admin,
    check_admin_access,
    AccessDenied,
    verify_logged_in,
    verify_teacher_access,
)
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.auth.sessioninfo import (
    get_current_user_object,
    get_current_user_group_object,
)
from timApp.document.create_item import apply_template, create_document
from timApp.document.docinfo import DocInfo
from timApp.gamification.badge.routes import verify_access
from timApp.item.validation import ItemValidationRule
from timApp.notification.send_email import multi_send_email
from timApp.timdb.sqa import db, run_sql
from timApp.user.special_group_names import (
    SPECIAL_GROUPS,
    PRIVILEGED_GROUPS,
    SPECIAL_USERNAMES,
)
from timApp.user.user import (
    User,
    view_access_set,
    edit_access_set,
    teacher_access_set,
    UserInfo,
    UserOrigin,
)
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import load_data_from_req, RouteException, NotExist
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.logger import log_info
from timApp.util.utils import (
    remove_path_special_chars,
    get_current_time,
    is_valid_email,
    partition,
)
from tim_common.utils import render_raw_template_string
from tim_common.marshmallow_dataclass import class_schema

groups = TypedBlueprint("groups", __name__, url_prefix="/groups")

USER_NOT_FOUND = "User not found"


class NameValidationFlags(Flag):
    RequireDigits = 1
    AllowUpperCase = 2


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


@groups.get("/usergroups/<username>/<group_name>")
def show_team_by_name(username: str, group_name: str) -> Response:
    # TODO Implement proper access right verification
    if not group_name.startswith("oscar"):
        raise AccessDenied("Only oscar groups are allowed")

    u = User.get_by_name(username)
    if not u:
        raise NotExist(USER_NOT_FOUND)

    return json_response(
        run_sql(
            u.get_groups(include_special=False)
            .filter(UserGroup.name.like(f"{group_name}-%"))
            .order_by(UserGroup.name)
        )
        .scalars()
        .all()
    )


def get_group_or_abort(group_name: str):
    ug = UserGroup.get_by_name(group_name)
    raise_group_not_found_if_none(group_name, ug)
    return ug


def raise_group_not_found_if_none(group_name: str, ug: UserGroup | None):
    if not ug:
        raise RouteException(f'User group "{group_name}" not found')


# FIXME: Implement
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
    _, doc = do_create_group(group_path)
    db.session.commit()
    return json_response(doc)


def do_create_group(group_path: str) -> tuple[UserGroup, DocInfo]:
    group_path = group_path.strip("/ ")

    # The name of the user group is separated from the path.
    # Does not check whether a name or a path is missing.
    group_name = group_path.split("/")[-1]

    verify_groupadmin(action=f"Creating group {group_name}")

    return do_create_group_impl(group_path, group_name)


def do_create_group_impl(group_path: str, group_name: str) -> tuple[UserGroup, DocInfo]:
    if UserGroup.get_by_name(group_name):
        raise RouteException("User group already exists.")

    group_name = group_name.strip()
    validate_groupname(group_name)

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


def validate_groupname(group_name: str):
    has_digits = False
    has_letters = False
    has_upper_letters = False
    has_non_alnum = False
    for c in group_name:
        has_digits = has_digits or c.isdigit()
        has_letters = has_letters or c.isalpha()
        has_upper_letters = has_upper_letters or c.isupper()
        has_non_alnum = has_non_alnum or not (c.isalnum() or c.isspace() or c in "-_")
    if not has_digits or not has_letters or has_non_alnum or has_upper_letters:
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
def add_member(
    group_name: str,
    names: list[str],
    create_missing_users: bool = False,
    notify_new: bool = False,
) -> Response:
    if (
        create_missing_users
        and not current_app.config["GROUPS_MISSING_USER_CREATE_ALLOW"]
    ):
        raise RouteException("Creating missing users is not allowed.")

    mi = get_member_infos(group_name, names)
    found_user_names = {u.name for u in mi.users}
    if found_user_names & SPECIAL_USERNAMES:
        raise RouteException("Cannot add special users.")
    user_names = {u.name for u in mi.group.users}
    already_exists = user_names & found_user_names
    added = []
    curr = get_current_user_object()

    notify_add_created_users = []
    notify_add_existing_users = []

    if mi.not_exist and create_missing_users:
        not_exist_emails, rest = partition(is_valid_email, mi.not_exist)
        new_users = []
        for email in not_exist_emails:
            user_info = UserInfo(username=email, email=email, origin=UserOrigin.Email)
            u, _ = User.create_with_group(user_info)
            new_users.append(u)
            notify_add_created_users.append(u.email)

        mi = dataclasses.replace(mi, not_exist=list(rest), users=mi.users + new_users)

    for u in mi.users:
        if u.id not in mi.existing_ids:
            u.add_to_group(mi.group, added_by=curr)
            added.append(u.name)
            if u.email not in notify_add_created_users:
                notify_add_existing_users.append(u.email)

    db.session.commit()

    def do_notify(emails: list[str], head_key: str, body_key: str) -> None:
        ctx = {
            "group_name": mi.group.human_name,
            "host": current_app.config["TIM_HOST"],
            "support_contact": current_app.config["HELP_EMAIL"],
        }
        subject_text = render_raw_template_string(
            current_app.config[head_key],
            **ctx,
        )
        body_text = render_raw_template_string(
            current_app.config[body_key],
            **ctx,
        )
        multi_send_email(
            ";".join(emails),
            subject_text,
            body_text,
            with_signature=True,
        )

    if notify_add_created_users and notify_new:
        do_notify(
            notify_add_created_users,
            "GROUPS_MISSING_USER_ADD_NOTIFY_HEAD",
            "GROUPS_MISSING_USER_ADD_NOTIFY_BODY",
        )

    if notify_add_existing_users and notify_new:
        do_notify(
            notify_add_existing_users,
            "GROUPS_EXISTING_USER_ADD_NOTIFY_EXISTING_HEAD",
            "GROUPS_EXISTING_USER_ADD_NOTIFY_EXISTING_BODY",
        )

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


@groups.get("/subgroups/<group_name_prefix>")
def get_subgroups(group_name_prefix: str) -> Response:
    """
    Fetches user groups that have a name that starts with the given prefix but is not the exact prefix.
    :param group_name_prefix: Prefix of the user groups
    :return: List of user groups sorted by name
    """
    context_usergroup = UserGroup.get_by_name(group_name_prefix)
    # if not context_usergroup:
    #     raise NotExist(f"{context_usergroup} not found.")
    # verify_teacher_access(context_usergroup.admin_doc)
    verify_access("teacher", context_usergroup, user_group_name=group_name_prefix)

    subgroups = (
        run_sql(
            select(UserGroup)
            .filter(
                UserGroup.name.like(group_name_prefix + "%"),
                UserGroup.name != group_name_prefix,
            )
            .order_by(UserGroup.name)
        )
        .scalars()
        .all()
    )
    subgroups_json = []
    for subgroup in subgroups:
        subgroups_json.append(subgroup.to_json())
    return json_response(subgroups_json)


@groups.get("/prefix_groups/<int:user_id>/<group_name_prefix>")
def get_users_subgroups(user_id: int, group_name_prefix: str) -> Response:
    """
    Fetches user groups that user with given user_id belongs. Fetched user groups also
    have a name that starts with the given prefix but is not the exact prefix.
    :param user_id: ID of the user
    :param group_name_prefix: Prefix of the user groups
    :return: List of user groups sorted by name
    """
    user = User.get_by_id(user_id)
    if not user:
        # we got the user's personal group's id, get the user from the personal group
        ug = UserGroup.get_by_id(user_id)
        if not ug:
            raise NotExist(f'User with id "{user_id}" not found')
        user = ug.personal_user if hasattr(ug, "personal_user") else None
        if not user:
            raise NotExist(f'User with id "{user_id}" not found')

    current_user = get_current_user_object()
    if current_user.id != user.id:
        context_usergroup = UserGroup.get_by_name(group_name_prefix)
        # verify_teacher_access(context_usergroup.admin_doc)
        verify_access("teacher", context_usergroup, user_group_name=group_name_prefix)

    users_subgroups_json = []
    for ug in user.groups:
        if ug.name.startswith(group_name_prefix) and len(ug.name) > len(
            group_name_prefix
        ):
            group = dict(id=ug.id, name=ug.name, description=ug.human_name)
            users_subgroups_json.append(group)
    return json_response(users_subgroups_json)


@groups.get("/personal_group/<name>")
def get_personal_group(name: str) -> Response:
    """
    Fetches user's personal user group.
    :param name: User's username
    :return: User account and personal user group in json format
    """
    user = User.get_by_name(name)
    if not user:
        raise NotExist(f'User "{name}" not found')
    else:
        p_group = user.get_personal_group()
        p_group.load_personal_user()
        return json_response(p_group)


@groups.get("/members/<group_name>")
def get_usergroup_members(group_name: str) -> Response:
    """
    Fetches user group's members.
    :param group_name: User group's name
    :return: Alphabetically sorted list of users
    """
    ug = UserGroup.get_by_name(group_name)
    # raise_group_not_found_if_none(group_name, ug)
    current_user = get_current_user_object()

    if ug not in current_user.groups:
        # verify_view_access(ug.admin_doc)
        verify_access("view", ug, user_group_name=group_name)

    return json_response(sorted(list(ug.users), key=attrgetter("real_name")))


@groups.get("/pretty_name/<group_name>")
def pretty_name(group_name: str) -> Response:
    group = UserGroup.get_by_name(group_name)
    verify_access("teacher", group, user_group_name=group_name)
    return json_response(group.human_name)


@groups.get("/groupinfo/<group_name>")
def get_groupinfo_with_pretty_name(group_name: str) -> Response:
    group = UserGroup.get_by_name(group_name)
    verify_access("view", group, user_group_name=group_name)
    return json_response(
        {"id": group.id, "name": group.name, "description": group.admin_doc.description}
    )


@groups.post("/pretty_name/<group_name>/<new_name>")
def change_pretty_name(group_name: str, new_name: str) -> Response:
    """
    Changes group's admin_doc's description (pretty name)
    :param group_name: Full group name
    :param new_name: New group's admin_doc's description (pretty name)
    :return: Group data in json format
    """
    group = UserGroup.get_by_name(group_name)
    # raise_group_not_found_if_none(group_name, group)
    if not group:
        raise NotExist(f'User group "{group_name}" not found')
    doc_entries = group.admin_doc.docentries
    settings = doc_entries[0].document.get_settings()

    current_user = get_current_user_object()
    in_group = group in current_user.groups
    log_info(f"{current_user.name} {in_group} {group.name}")
    if (
        not in_group
    ):  # or (in_group and not settings.allow_name_edit_by_group_members()):
        verify_teacher_access(
            group.admin_doc,
            message=f'Sorry, you don\'t have permission to use this resource. If you are a teacher of "{group_name}", please contact TIM admin.',
        )

    if len(new_name.strip()) > 0:
        group_doc = group.admin_doc
        log_info(f"GROUP DESCRIPTION: {group_doc.description}")
        group_doc.description = new_name
        db.session.commit()
        log_info(f"GROUP DESCRIPTION AFTER COMMIT: {group_doc.description}")
    else:
        raise RouteException("Group name cannot be empty")

    return json_response(
        {"id": group.id, "name": group.name, "description": group.admin_doc.description}
    )


@groups.get("/hasTeacherRightTo/<int:group_id>")
def get_has_teacher_right_to_group(group_id: int) -> Response:
    group = UserGroup.get_by_id(group_id)
    verify_group_access(group, teacher_access_set)
    return ok_response()
