from dataclasses import dataclass, field
from enum import Enum
from typing import List, Optional, Union, Tuple

from flask import render_template_string, Response

from timApp.answer.routes import save_fields, FieldSaveRequest, FieldSaveUserEntry
from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.accesstype import AccessType
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.editing.globalparid import GlobalParId
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewRoute, ViewContext
from timApp.item.manage import TimeOpt, verify_permission_edit_access, PermissionEditModel, add_perm, \
    log_right, remove_perm
from timApp.plugin.plugin import Plugin
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import NotExist, RouteException, view_ctx_with_urlmacros
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.get_fields import get_fields_and_users, RequestedGroups
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import GenericHtmlModel, PluginReqs, register_html_routes
from tim_common.utils import DurationSchema, Missing

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
class SetTaskValueAction:
    taskId: str
    value: str


@dataclass
class ActionCollection:
    addPermission: List[AddPermission] = field(default_factory=list)
    removePermission: List[RemovePermission] = field(default_factory=list)
    setValue: List[SetTaskValueAction] = field(default_factory=list)


@dataclass
class ScannerOptions:
    applyOnMatch: bool = False
    continuousMatch: bool = False
    enabled: bool = False
    scanInterval: float = 1.5
    waitBetweenScans: float = 0.0
    beepOnSuccess: bool = False
    beepOnFailure: bool = False


@dataclass
class TextOptions:
    apply: Optional[str] = None
    cancel: Optional[str] = None
    success: Optional[str] = None


@dataclass
class UserSelectMarkupModel(GenericMarkupModel):
    preFetch: bool = False
    inputMinLength: int = 3
    autoSearchDelay: float = 0.0
    maxMatches: int = 10
    scanner: ScannerOptions = field(default_factory=ScannerOptions)
    groups: List[str] = field(default_factory=list)
    fields: List[str] = field(default_factory=list)
    actions: Optional[ActionCollection] = None
    text: TextOptions = field(default_factory=TextOptions)
    displayFields: List[str] = field(default_factory=lambda: ["$.name", "$.real_name"])


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


def get_plugin_markup(task_id: Optional[str], par: Optional[GlobalParId]) \
        -> Tuple[UserSelectMarkupModel, DocInfo, User, ViewContext]:
    verify_logged_in()
    user = get_current_user_object()
    user_ctx = UserContext.from_one_user(user)
    view_ctx = view_ctx_with_urlmacros(ViewRoute.Unknown)

    if task_id:
        plugin, doc = Plugin.from_task_id(task_id, user_ctx, view_ctx)
    elif par:
        plugin, doc = Plugin.from_global_par(par, user_ctx, view_ctx)
    else:
        raise RouteException("Either task_id or par must be specified")
    model: UserSelectMarkupModel = UserSelectMarkupModelSchema().load(plugin.values)
    return model, doc, user, view_ctx


@user_select_plugin.route('/fetchUsers')
def fetch_users(task_id: Optional[str] = None, doc_id: Optional[int] = None, par_id: Optional[str] = None) -> Response:
    model, doc, user, view_ctx = get_plugin_markup(task_id, GlobalParId(doc_id, par_id) if doc_id and par_id else None)
    field_data, _, field_names, _ = get_fields_and_users(
        model.fields,
        RequestedGroups.from_name_list(model.groups),
        doc,
        user,
        view_ctx
    )
    return json_response({
        "users": [
            {
                "user": field_obj["user"],
                "fields": field_obj["fields"]
            }
            for field_obj in field_data
        ],
        "fieldNames": field_names
    })


def match_query(query_words: List[str], keywords: List[str]):
    kw = set(keywords)
    for qw in query_words:
        found = next((k for k in kw if qw in k), None)
        if found is None:
            return False
        kw.remove(found)
    return True


@user_select_plugin.route('/search', methods=['POST'])
def search_users(search_strings: List[str], task_id: Optional[str] = None,
                 par: Optional[GlobalParId] = None) -> Response:
    model, doc, user, view_ctx = get_plugin_markup(task_id, par)
    field_data, _, field_names, _ = get_fields_and_users(
        model.fields,
        RequestedGroups.from_name_list(model.groups),
        doc,
        user,
        view_ctx
    )

    # If query contains spaces, split into sub-queries that all must match
    # In each subquery, match by longest word first to ensure best match
    search_query_words = [sorted(s.lower().split(), key=lambda s: len(s), reverse=True) for s in search_strings]
    matched_field_data = []
    for field_obj in field_data:
        fields = field_obj["fields"]
        usr = field_obj["user"]
        values_to_check: List[Optional[Union[str, float, None]]] = [usr.name, usr.real_name, usr.email,
                                                                    *fields.values()]

        for field_val in values_to_check:
            if not field_val:
                continue
            val = (field_val if isinstance(field_val, str) else str(field_val)).lower().split()
            if next((qws for qws in search_query_words if match_query(qws, val)), None):
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
def apply(username: str, task_id: Optional[str] = None, par: Optional[GlobalParId] = None) -> Response:
    verify_logged_in()
    model, _, _, _ = get_plugin_markup(task_id, par)
    # No permissions to apply, simply remove
    if not model.actions:
        return ok_response()

    cur_user = get_current_user_object()
    user_group = UserGroup.get_by_name(username)
    if not user_group:
        raise RouteException(f"Cannot find user {username}")

    permission_actions: List[PermissionActionBase] = [*model.actions.addPermission, *model.actions.removePermission]
    doc_entries = {}

    # Verify first that all documents can be accessed and permissions edited + cache doc entries
    for perm in permission_actions:
        if perm.doc_path in doc_entries:
            continue
        doc_entry = DocEntry.find_by_path(perm.doc_path, fallback_to_id=True, try_translation=False)
        if not doc_entry:
            raise NotExist(f"Can't find document {perm.doc_path}")
        verify_permission_edit_access(doc_entry, perm.type)
        doc_entries[perm.doc_path] = doc_entry

    update_messages = []

    for add in model.actions.addPermission:
        doc_entry = doc_entries[add.doc_path]
        # Don't throw if we try to remove a permission from ourselves, just ignore it
        accs = add_perm(PermissionEditModel(add.type, add.time, [username], add.confirm), doc_entry)
        if accs:
            update_messages.append(f'added {accs[0].info_str} for {username} in {doc_entry.path}')

    for remove in model.actions.removePermission:
        doc_entry = doc_entries[remove.doc_path]
        a = remove_perm(user_group, doc_entry.block, remove.type)
        update_messages.append(f'removed {a.info_str} for {user_group.name} in {doc_entry.path}')

    fields_to_save = {
        set_val.taskId: set_val.value for set_val in model.actions.setValue
    }
    if fields_to_save:
        user_acc = User.get_by_name(user_group.name)
        assert user_acc is not None
        # Reuse existing helper for answer route to save field values quickly
        save_fields(
            FieldSaveRequest(savedata=[FieldSaveUserEntry(user=user_acc.id, fields=fields_to_save)]),
            cur_user,
            allow_non_teacher=False)

    db.session.commit()

    for msg in update_messages:
        log_right(msg)

    return ok_response()


register_html_routes(
    user_select_plugin,
    class_schema(UserSelectHtmlModel, base_schema=DurationSchema),
    reqs_handler
)
