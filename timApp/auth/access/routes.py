from dataclasses import field

from flask import Response

from timApp.auth.access.util import set_locked_access_type, set_locked_active_groups
from timApp.auth.accesshelper import verify_logged_in, AccessDenied
from timApp.auth.accesstype import AccessType
from timApp.auth.sessioninfo import get_current_user_object
from timApp.user.groups import verify_group_edit_access
from timApp.user.usergroup import UserGroup
from timApp.util.flask.responsehelper import ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint

access = TypedBlueprint("access", __name__, url_prefix="/access")


@access.post("lock")
def lock_access(
    access_type: AccessType | None = field(metadata={"by_value": True}),
) -> Response:
    verify_logged_in()
    set_locked_access_type(access_type)
    return ok_response()


@access.post("groups/lock")
def lock_active_groups(group_ids: list[int] | None) -> Response:
    verify_logged_in()

    if group_ids is None:
        set_locked_active_groups(None)
        return ok_response()

    user = get_current_user_object()

    group_ids_set = set(group_ids)
    group_ids_set -= set(ug.id for ug in user.groups)
    group_ids_set -= {
        UserGroup.get_logged_in_group().id,
        UserGroup.get_anonymous_group().id,
    }

    groups: list[UserGroup] = UserGroup.query.filter(
        UserGroup.id.in_(group_ids_set)
    ).all()
    for ug in groups:
        if not verify_group_edit_access(ug, user, require=False):
            raise AccessDenied(
                f"You must be a member of or have edit access to group {ug.name} in order to lock your role."
            )

    set_locked_active_groups(set(group_ids))
    return ok_response()
