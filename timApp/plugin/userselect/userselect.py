from dataclasses import dataclass, field
from typing import List, Optional

from flask import render_template_string, Response

from timApp.plugin.plugin import find_plugin_by_task_id
from timApp.util.flask.requesthelper import use_model
from timApp.util.flask.responsehelper import json_response
from timApp.util.get_fields import get_fields_and_users, RequestedGroups
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import GenericHtmlModel, create_nontask_blueprint, PluginReqs


@dataclass
class UserSelectInputModel:
    pass


@dataclass
class UserSelectMarkupModel(GenericMarkupModel):
    inputMinLength: int = 3
    autoSearchDelay: float = 0.0
    maxMatches: int = 10
    groups: List[str] = field(default_factory=list)
    fields: List[str] = field(default_factory=list)


UserSelectMarkupModelSchema = class_schema(UserSelectMarkupModel)


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


user_select_plugin = create_nontask_blueprint(
    __name__,
    'userSelect',
    UserSelectHtmlModel,
    reqs_handler,
)


@dataclass
class SearchUsersOptions:
    task_id: str
    search_string: str


@user_select_plugin.route('/search')
@use_model(SearchUsersOptions)
def search_users(opts: SearchUsersOptions) -> Response:
    plug, doc, user, view_ctx = find_plugin_by_task_id(opts.task_id)
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
            if opts.search_string in val:
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
