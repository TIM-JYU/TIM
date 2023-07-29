from dataclasses import dataclass, field
from typing import Callable

from flask import render_template_string, Response
from marshmallow import missing
from sqlalchemy import select

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.sessioninfo import get_current_user_object
from timApp.bookmark.course import update_user_course_bookmarks
from timApp.document.docentry import DocEntry
from timApp.document.docsettings import GroupSelfJoinSettings
from timApp.timdb.sqa import db, run_sql
from timApp.user.groups import verify_group_edit_access
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    PluginReqs,
    EditorTab,
    register_html_routes,
)
from tim_common.utils import DurationSchema, Missing

group_join_plugin = TypedBlueprint("groupJoin", __name__, url_prefix="/groupJoin")


@dataclass
class GroupJoinInputModel:
    pass


@dataclass
class GroupJoinTexts:
    join: str | None | Missing = missing
    joined: str | None | Missing = missing
    leave: str | None | Missing = missing
    left: str | None | Missing = missing
    joinConfirmTitle: str | None | Missing = missing
    joinConfirmMessage: str | None | Missing = missing
    leaveConfirmTitle: str | None | Missing = missing
    leaveConfirmMessage: str | None | Missing = missing


@dataclass
class GroupJoinMarkupModel(GenericMarkupModel):
    groups: list[str] = field(default_factory=list)
    join: bool = True
    leave: bool = True
    confirm: bool = False
    autoRefresh: bool = False
    texts: GroupJoinTexts = field(default_factory=GroupJoinTexts)


@dataclass
class GroupJoinStateModel:
    pass


@group_join_plugin.post("/joinGroups")
def join_groups(groups: list[str]) -> Response:
    """
    Join the currently logged user to a list of groups.

    The groups must have self-join enabled via document settings.

    :param groups: List of groups to join.
    :return: JSON response with list of joined groups and possible status messages.
    """
    verify_logged_in()
    current_user = get_current_user_object()

    def do_join(user: User, group: UserGroup) -> None:
        user.add_to_group(group, added_by=user)

    all_ok, is_course, result = _do_group_op(
        groups, current_user, "join", False, lambda i: i.canJoin, do_join
    )

    if is_course:
        db.session.refresh(current_user)
        update_user_course_bookmarks()

    db.session.commit()

    return json_response({"ok": all_ok, "result": result})


@group_join_plugin.post("/leaveGroups")
def leave_groups(groups: list[str]) -> Response:
    """
    Remove the current user from the groups.

    The groups must have self-remove enabled via document settings.

    :param groups: List of groups to leave from.
    :return: JSON response with list of left groups and possible status message.
    """
    verify_logged_in()
    current_user = get_current_user_object()

    def do_leave(user: User, group: UserGroup) -> None:
        membership: UserGroupMember = user.active_memberships.get(group.id)
        membership.set_expired()

    all_ok, _, result = _do_group_op(
        groups, current_user, "leave", True, lambda i: i.canLeave, do_leave
    )

    db.session.commit()

    return json_response({"ok": all_ok, "result": result})


def _check_self_join(
    group: UserGroup, user: User, check: Callable[[GroupSelfJoinSettings], bool]
) -> bool:
    admin_doc: DocEntry = group.admin_doc.docentries[0] if group.admin_doc else None
    if admin_doc is None:
        return False
    self_join_info = admin_doc.document.get_settings().group_self_join_info()
    # The order matters: group edit access also verifies that the group is not special
    return verify_group_edit_access(group, user, require=False) or check(self_join_info)


def _do_group_op(
    groups: list[str],
    user: User,
    action: str,
    ensure_joined: bool,
    check_info: Callable[[GroupSelfJoinSettings], bool],
    apply: Callable[[User, UserGroup], None],
) -> tuple[bool, bool, dict[str, str]]:
    user_groups: set[str] = set(
        g
        for g, in run_sql(
            user.get_groups(include_expired=False).with_only_columns(UserGroup.name)
        )
    )

    result = dict.fromkeys(groups, "")
    ugs: list[UserGroup] = (
        run_sql(select(UserGroup).filter(UserGroup.name.in_(groups))).scalars().all()
    )

    all_ok = True
    is_course = False
    for ug in ugs:
        if (ensure_joined and ug.name not in user_groups) or (
            not ensure_joined and ug.name in user_groups
        ):
            result[ug.name] = f"User is {'not' if ensure_joined else ''} in this group"
            all_ok = False
            continue
        if not _check_self_join(ug, user, check_info):
            result[ug.name] = f"Self-{action} not enabled for this group"
            all_ok = False
            continue
        apply(user, ug)
        is_course = is_course or ug.is_self_join_course
        result[ug.name] = "OK"

    for g in result:
        if result[g] == "":
            result[g] = "Group not found"
            all_ok = False

    return all_ok, is_course, result


@dataclass
class GroupJoinHtmlModel(
    GenericHtmlModel[GroupJoinInputModel, GroupJoinMarkupModel, GroupJoinStateModel]
):
    def get_component_html_name(self) -> str:
        return "group-join"

    def get_static_html(self) -> str:
        return render_template_string(
            """
                <div>Group join</div>
            """
        )


def _reqs_handler() -> PluginReqs:
    template = """
``` {plugin="groupJoin"}
groups:            # List of groups to join/leave when the button is pressed
    - groupname
join: true         # Enable join button? Group must have self-join enabled in group document settings.
leave: false       # Enable leave button? Group must have self-remove enabled in group document settings.
confirm: false     # Show confirmation dialog?
autoRefresh: false # Refresh the page after joining/leaving?
texts:             # Custom texts for buttons and dialogs. Leave empty for default texts.
    join:
    joined:
    leave:
    left:
    joinConfirmTitle:
    joinConfirmMessage:
    leaveConfirmTitle:
    leaveConfirmMessage:
```
"""
    editor_tabs: list[EditorTab] = [
        {
            "text": "plugins",
            "items": [
                {
                    "text": "Others",
                    "items": [
                        {
                            "data": template.strip(),
                            "text": "Group self-join",
                            "expl": "Allow users to self-join and self-leave a group",
                        }
                    ],
                },
            ],
        },
    ]
    return {
        "js": ["groupJoin"],
        "multihtml": True,
        "editor_tabs": editor_tabs,
    }


register_html_routes(
    group_join_plugin,
    class_schema(GroupJoinHtmlModel, base_schema=DurationSchema),
    _reqs_handler,
)
