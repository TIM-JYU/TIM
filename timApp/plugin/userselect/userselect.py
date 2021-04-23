from dataclasses import dataclass, field
from typing import List, Optional

from flask import render_template_string, Response

from timApp.auth.accesstype import AccessType
from timApp.item.manage import TimeOpt
from timApp.plugin.plugin import find_plugin_by_task_id
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.get_fields import get_fields_and_users, RequestedGroups
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import GenericHtmlModel, PluginReqs, register_html_routes
from tim_common.utils import DurationSchema

user_select_plugin = TypedBlueprint("userSelect", __name__, url_prefix="/userSelect")


@dataclass
class UserSelectInputModel:
    pass


@dataclass
class Permission:
    doc_path: str
    type: AccessType
    time: TimeOpt
    confirm: bool = False


@dataclass
class UserSelectMarkupModel(GenericMarkupModel):
    inputMinLength: int = 3
    autoSearchDelay: float = 0.0
    maxMatches: int = 10
    groups: List[str] = field(default_factory=list)
    fields: List[str] = field(default_factory=list)
    permissions: List[Permission] = field(default_factory=list)


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


# user_select_plugin = create_nontask_blueprint_schema(
#     __name__,
#     'userSelect',
#     UserSelectMarkupModelSchema,
#     reqs_handler,
# )


@user_select_plugin.route('/search')
def search_users(task_id: str, search_string: str) -> Response:
    plug, doc, user, view_ctx = find_plugin_by_task_id(task_id)
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
        values_to_check: List[Optional[str, float, None]] = [usr.name, usr.real_name, usr.email, *fields.values()]
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


register_html_routes(
    user_select_plugin,
    class_schema(UserSelectHtmlModel, base_schema=DurationSchema),
    reqs_handler
)
