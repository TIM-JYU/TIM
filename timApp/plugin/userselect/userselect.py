from dataclasses import dataclass, field
from typing import List, Optional, Union

from flask import render_template_string, Response

from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.item.manage import TimeOpt, verify_permission_edit_access, PermissionEditModel, add_perm, \
    log_right, remove_perm
from timApp.plugin.plugin import find_plugin_by_task_id
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import NotExist, RouteException
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.get_fields import get_fields_and_users, RequestedGroups
from timApp.util.utils import seq_to_str
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import GenericHtmlModel, PluginReqs, register_html_routes
from tim_common.utils import DurationSchema

user_select_plugin = TypedBlueprint("userSelect", __name__, url_prefix="/userSelect")


@dataclass
class UserSelectInputModel:
    pass


@dataclass
class PermissionActionBase:
    doc_path: str
    type: AccessType


@dataclass
class AddPermission(PermissionActionBase):
    time: TimeOpt
    confirm: bool = False


@dataclass
class RemovePermission(PermissionActionBase):
    pass


@dataclass
class PermissionsCollection:
    add: List[AddPermission] = field(default_factory=list)
    remove: List[RemovePermission] = field(default_factory=list)


@dataclass
class UserSelectMarkupModel(GenericMarkupModel):
    inputMinLength: int = 3
    autoSearchDelay: float = 0.0
    maxMatches: int = 10
    groups: List[str] = field(default_factory=list)
    fields: List[str] = field(default_factory=list)
    permissions: Optional[PermissionsCollection] = None


UserSelectMarkupModelSchema = class_schema(UserSelectMarkupModel, base_schema=DurationSchema)


@dataclass
class UserSelectStateModel:
    pass


@dataclass
class UserSelectHtmlModel(GenericHtmlModel[UserSelectInputModel, UserSelectMarkupModel, UserSelectStateModel]):
    def get_component_html_name(self) -> str:
        return 'user-selector'

    def get_static_html(self) -> str:
        return render_template_string(
            """
                <div>User selector</div>
            """
        )


def reqs_handler() -> PluginReqs:
    return {
        "js": ["userSelect"],
        "multihtml": True
    }


@user_select_plugin.route('/search')
def search_users(task_id: str, search_string: str) -> Response:
    plug, doc, user, view_ctx = find_plugin_by_task_id(task_id)
    if not user:
        raise RouteException("Need to be logged in")
    model: UserSelectMarkupModel = UserSelectMarkupModelSchema().load(plug.values)
    field_data, _, field_names, _ = get_fields_and_users(
        model.fields,
        RequestedGroups.from_name_list(model.groups),
        doc,
        user,
        view_ctx
    )

    matched_field_data = []
    for field_obj in field_data:
        fields = field_obj["fields"]
        usr = field_obj["user"]
        values_to_check: List[Optional[Union[str, float, None]]] = [usr.name, usr.real_name, usr.email, *fields.values()]
        for field_val in values_to_check:
            if not field_val:
                continue
            val = field_val if isinstance(field_val, str) else str(field_val)
            if search_string in val.lower():
                matched_field_data.append(field_obj)
                break

    match_count = len(matched_field_data)
    if match_count > model.maxMatches:
        matched_field_data = matched_field_data[0:model.maxMatches]

    return json_response({
        "matches": [
            {
                "user": field_obj["user"],
                "fields": field_obj["fields"]
            }
            for field_obj in matched_field_data
        ],
        "allMatchCount": match_count,
        "fieldNames": field_names
    })


@user_select_plugin.route("/apply", methods=["POST"])
def apply(task_id: str, username: str) -> Response:
    plug, doc, user, view_ctx = find_plugin_by_task_id(task_id)
    model: UserSelectMarkupModel = UserSelectMarkupModelSchema().load(plug.values)
    # No permissions to apply, simply remove
    if not model.permissions:
        return ok_response()

    user = UserGroup.get_by_name(username)
    if not user:
        raise RouteException(f"Cannot find user {username}")

    all_permissions: List[PermissionActionBase] = [*model.permissions.add, *model.permissions.remove]
    doc_entries = {}

    # Verify first that all documents can be accessed and permissions edited + cache doc entries
    for perm in all_permissions:
        if perm.doc_path in doc_entries:
            continue
        doc_entry = DocEntry.find_by_path(perm.doc_path, fallback_to_id=True, try_translation=False)
        if not doc_entry:
            raise NotExist(f"Can't find document {perm.doc_path}")
        verify_permission_edit_access(doc_entry, perm.type)
        doc_entries[perm.doc_path] = doc_entry

    for add in model.permissions.add:
        doc_entry = doc_entries[add.doc_path]
        # Don't throw if we try to remove a permission from ourselves, just ignore it
        accs = add_perm(PermissionEditModel(add.type, add.time, [username], add.confirm), doc_entry)
        if accs:
            log_right(f'added {accs[0].info_str} for {username} in {doc_entry.path}')

    for remove in model.permissions.remove:
        doc_entry = doc_entries[remove.doc_path]
        a = remove_perm(user, doc_entry.block, remove.type)
        log_right(f'removed {a.info_str} for {user.name} in {doc_entry.path}')

    db.session.commit()
    return ok_response()


register_html_routes(
    user_select_plugin,
    class_schema(UserSelectHtmlModel, base_schema=DurationSchema),
    reqs_handler
)
