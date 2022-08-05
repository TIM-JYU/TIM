from dataclasses import dataclass, field

from flask import render_template_string, Response
from marshmallow import missing

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry
from timApp.timdb.sqa import db
from timApp.user.groups import verify_group_edit_access
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
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
    confirmTitle: str | None | Missing = missing
    confirmMessage: str | None | Missing = missing


@dataclass
class GroupJoinMarkupModel(GenericMarkupModel):
    groups: list[str] = field(default_factory=list)
    confirm: bool = False
    texts: GroupJoinTexts = field(default_factory=GroupJoinTexts)


@dataclass
class GroupJoinStateModel:
    pass


def _can_self_join(group: UserGroup, user: User) -> bool:
    admin_doc: DocEntry = group.admin_doc.docentries[0] if group.admin_doc else None
    if admin_doc is None:
        return False
    self_join_info = admin_doc.document.get_settings().group_self_join_info()
    # The order matters: group edit access also verifies that the group is not special
    return (
        verify_group_edit_access(group, user, require=False) or self_join_info.canJoin
    )


@group_join_plugin.post("/joinGroups")
def join_group(groups: list[str]) -> Response:
    """
    Join the currently logged user to a list of groups.

    The groups must have self-join enable via document settings.

    :param groups: List of groups to join.
    :return: JSON response with list of joined groups and possible status messages.
    """
    verify_logged_in()
    current_user = get_current_user_object()
    current_user_groups: set[str] = set(
        g
        for g, in current_user.get_groups(include_expired=False).with_entities(
            UserGroup.name
        )
    )

    result = dict.fromkeys(groups, "")
    ugs: list[UserGroup] = UserGroup.query.filter(UserGroup.name.in_(groups)).all()

    all_ok = True
    for ug in ugs:
        if ug.name in current_user_groups:
            result[ug.name] = "You are already in this group"
            all_ok = False
            continue
        if not _can_self_join(ug, current_user):
            result[ug.name] = "Self-join not enabled for this group"
            all_ok = False
            continue
        current_user.add_to_group(ug, added_by=current_user)
        result[ug.name] = "Added to group"

    for g in result:
        if result[g] == "":
            result[g] = "Group not found"
            all_ok = False

    db.session.commit()

    return json_response({"ok": all_ok, "result": result})


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
``` {#group_join plugin="groupJoin"}
groups:
    - groupname
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
