"""
Common routes for access management.
"""

from dataclasses import field
from typing import Sequence

from flask import Response
from sqlalchemy import select

from timApp.auth.access.util import set_locked_access_type, set_locked_active_groups
from timApp.auth.accesshelper import verify_logged_in, AccessDenied
from timApp.auth.accesstype import AccessType
from timApp.auth.sessioninfo import get_current_user_object
from timApp.timdb.sqa import run_sql
from timApp.user.groups import (
    verify_group_edit_access,
    get_group_or_abort,
    UserGroupMeta,
)
from timApp.user.usergroup import (
    UserGroup,
    get_logged_in_group_id,
    get_anonymous_group_id,
)
from timApp.util.flask.responsehelper import ok_response, json_response
from timApp.util.flask.typedblueprint import TypedBlueprint

access = TypedBlueprint("access", __name__, url_prefix="/access")


@access.post("lock")
def lock_access(
    access_type: AccessType | None = field(metadata={"by_value": True}),
) -> Response:
    """
    Lock user's access level.
    Users with locked access level can preview documents with lower permissions than their own.

    :param access_type: Access type to limit access to. If None, resets access level.
    :return: OK response.
    """
    verify_logged_in()
    set_locked_access_type(access_type)
    return ok_response()


@access.post("groups/lock")
def lock_active_groups(group_ids: list[int] | None) -> Response:
    """
    Lock users active group list.
    Users with locked active group list can preview documents as member of only the specified groups.

    :param group_ids: List of group IDs that the user should be locked to.
                      The user must be either a member of the group or can have edit access to it.
    :return: OK response.
    """
    verify_logged_in()

    if group_ids is None:
        set_locked_active_groups(None)
        return ok_response()

    user = get_current_user_object()
    user.skip_access_lock = True

    group_ids_set = set(group_ids)
    group_ids_set -= set(ug.id for ug in user.groups)
    group_ids_set -= {
        get_logged_in_group_id(),
        get_anonymous_group_id(),
    }

    if not user.is_admin:
        groups: Sequence[UserGroup] = (
            run_sql(select(UserGroup).filter(UserGroup.id.in_(group_ids_set)))
            .scalars()
            .all()
        )
        for ug in groups:
            if not verify_group_edit_access(ug, user, require=False):
                raise AccessDenied(
                    f"You must be a member of or have edit access to group {ug.name} in order to lock your role."
                )

    # Ensure anonymous user group is always present
    # This is not necessary but it generally doesn't make sense not to include it by default
    # as all users inherit the anonymous group's rights.
    locked_groups = set(group_ids) | {get_anonymous_group_id()}
    set_locked_active_groups(locked_groups)
    return ok_response()


@access.get("/groups/editable/info/<group_name>")
def show_edit_info(group_name: str) -> Response:
    """
    Get basic info about a group if the user has edit access to it.

    :param group_name: Group name.
    :return: JSON of group ID and group name if the user has edit access to the group.
    """
    verify_logged_in()
    user = get_current_user_object()
    user.skip_access_lock = True
    ug = get_group_or_abort(group_name)
    if not user.is_admin:
        verify_group_edit_access(ug)
    return json_response(
        UserGroupMeta(id=ug.id, name=ug.name, managed=ug.admin_doc is not None)
    )


@access.get("/groups/editable/find")
def find_editable_groups(
    group_ids: list[int] = field(
        default_factory=list, metadata={"list_type": "delimited"}
    )
) -> Response:
    """
    Gets a list of groups that the user has edit access to.

    :param group_ids: List of group IDs to filter by.
    :return: JSON of group IDs and group names that the user has edit access to.
    """
    verify_logged_in()
    user = get_current_user_object()
    user.skip_access_lock = True
    ugs = run_sql(select(UserGroup).filter(UserGroup.id.in_(group_ids))).scalars().all()
    visible_ugs = [
        ug for ug in ugs if user.is_admin or verify_group_edit_access(ug, require=False)
    ]
    return json_response(
        [
            UserGroupMeta(id=ug.id, name=ug.name, managed=ug.admin_doc is not None)
            for ug in visible_ugs
        ]
    )
