"""
TIM example plugin: a tableFormndrome checker.
"""
import json
import os
from typing import Union, List, Optional

import attr
from flask import jsonify, render_template_string, request, abort
from marshmallow import Schema, fields, post_load, validates, ValidationError
from marshmallow.utils import missing
from sqlalchemy.orm import joinedload
from webargs.flaskparser import use_args

from pluginserver_flask import GenericMarkupModel, GenericMarkupSchema, GenericHtmlSchema, GenericHtmlModel, \
    GenericAnswerSchema, GenericAnswerModel, Missing, \
    InfoSchema, create_blueprint
from timApp.auth.accesshelper import get_doc_or_abort
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docinfo import DocInfo
from timApp.document.timjsonencoder import TimJsonEncoder
from timApp.item.block import Block
from timApp.item.tag import Tag, TagType, GROUP_TAG_PREFIX
from timApp.plugin.plugin import find_plugin_from_document, TaskNotFoundException
from timApp.plugin.taskid import TaskId
from timApp.sisu.sisu import get_potential_groups, parse_sisu_group_display_name
from timApp.tim_app import csrf
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember
from timApp.util.flask.responsehelper import csv_response, json_response
from timApp.util.get_fields import get_fields_and_users, GroupFilter
from timApp.util.utils import get_boolean


@attr.s(auto_attribs=True)
class TableFormStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    # TODO: Save user given table layouts like in timTable


class TableFormStateSchema(Schema):
    @post_load
    def make_obj(self, data):
        return TableFormStateModel(**data)


@attr.s(auto_attribs=True)
class TableFormMarkupModel(GenericMarkupModel):
    groups: Union[List[str], Missing] = missing
    table: Union[bool, Missing] = missing
    report: Union[bool, Missing] = missing
    separator: Union[str, Missing] = missing
    anonNames: Union[bool, Missing] = missing
    sortBy: Union[str, Missing] = missing
    dataCollection: Union[str, Missing] = missing
    autosave: Union[bool, Missing] = missing
    buttonText: Union[str, Missing] = missing
    hideButtonText: Union[str, Missing] = missing
    openButtonText: Union[str, Missing] = missing
    reportButton: Union[str, Missing] = missing
    realnames: Union[bool, Missing] = missing
    usernames: Union[bool, Missing] = missing
    emails: Union[bool, Missing] = missing
    maxWidth: Union[str, Missing] = missing
    minWidth: Union[str, Missing] = missing
    singleLine: Union[bool, Missing] = missing
    removeDocIds: Union[bool, Missing] = True
    open: Union[bool, Missing] = True
    hiddenColumns: Union[List[int], Missing] = missing
    hiddenRows: Union[List[int], Missing] = missing
    filterRow: Union[bool, Missing] = missing
    cbColumn: Union[bool, Missing] = missing
    nrColumn: Union[bool, Missing] = missing
    removeUsersButtonText: Union[str, Missing] = missing
    userListButtonText: Union[str, Missing] = missing
    emailUsersButtonText: Union[str, Missing] = missing
    maxRows: Union[str, Missing] = missing
    maxCols: Union[str, Missing] = missing
    fontSize: Union[str, Missing] = missing
    fixedColor: Union[str, Missing] = missing
    toolbarTemplates: Union[List[dict], Missing] = missing
    saveStyles: Union[bool, Missing] = True
    showToolbar: Union[bool, Missing] = missing
    sisugroups: Union[str, Missing] = missing
    autoUpdateFields: Union[bool, Missing] = True
    autoUpdateTables: Union[bool, Missing] = True
    hide: Union[dict, Missing] = missing
    runScripts: Union[List[str], Missing] = missing
    includeUsers: str = 'active'
    fields: Union[List[str], Missing] = missing


class TableFormMarkupSchema(GenericMarkupSchema):
    groups = fields.List(fields.Str())
    table = fields.Boolean()
    report = fields.Boolean()
    separator = fields.Str(allow_none=True)
    anonNames = fields.Boolean()
    sortBy = fields.Str(allow_none=True)
    dataCollection = fields.Str(allow_none=True)
    autosave = fields.Boolean()
    buttonText = fields.Str(allow_none=True)
    hideButtonText = fields.Str(allow_none=True)
    openButtonText = fields.Str(allow_none=True)
    reportButton = fields.Str(allow_none=True)
    realnames = fields.Boolean()
    usernames = fields.Boolean()
    emails = fields.Boolean()
    singleLine = fields.Boolean(allow_none=True)
    removeDocIds = fields.Boolean(default=True)
    maxWidth = fields.Str()
    minWidth = fields.Str(allow_none=True)
    open = fields.Boolean(default=True)
    filterRow = fields.Boolean(allow_none=True)
    cbColumn = fields.Boolean(allow_none=True)
    nrColumn = fields.Boolean(allow_none=True)
    hiddenColumns = fields.List(fields.Number(allow_none=True))
    hiddenRows = fields.List(fields.Number(allow_none=True))
    removeUsersButtonText = fields.Str(allow_none=True)
    userListButtonText = fields.Str(allow_none=True)
    emailUsersButtonText = fields.Str(allow_none=True)
    maxRows = fields.Str(allow_none=True)
    maxCols = fields.Str(allow_none=True)
    toolbarTemplates = fields.List(fields.Dict())
    fontSize = fields.Str(allow_none=True)
    fixedColor = fields.Str(allow_none=True)
    saveStyles = fields.Boolean(default=True)
    showToolbar = fields.Boolean(allow_none=True)
    sisugroups = fields.Str()
    autoUpdateFields = fields.Boolean(default=True)
    autoUpdateTables = fields.Boolean(default=True)
    hide = fields.Dict(allow_none=True)
    runScripts = fields.List(fields.Str())
    includeUsers = fields.Str(default='active')
    fields = fields.List(fields.Str())  # Keep this last - bad naming

    @validates('includeUsers')
    def validate_include_users(self, value):
        try:
            GroupFilter(value)
        except ValueError:
            raise ValidationError("Invalid includeUsers value. Must be one of 'all', 'active', 'inactive'.")

    @post_load
    def make_obj(self, data):
        return TableFormMarkupModel(**data)


@attr.s(auto_attribs=True)
class TableFormInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    replyRows: dict
    nosave: bool


class TableFormInputSchema(Schema):
    replyRows = fields.Dict()
    nosave = fields.Bool()

    @post_load
    def make_obj(self, data):
        return TableFormInputModel(**data)


class TableFormAttrs(Schema):
    """Common fields for HTML and answer routes."""
    markup = fields.Nested(TableFormMarkupSchema)
    state = fields.Nested(TableFormStateSchema, allow_none=True, required=True)


def get_sisu_group_desc_for_table(g: UserGroup):
    p = parse_sisu_group_display_name(g.display_name)
    if g.external_id.is_studysubgroup:
        return p.desc
    # We want the most important groups to be at the top of the table.
    # The '(' at the beginning changes the sort order.
    return f'({p.desc})'


def get_sisugroups(user: User, sisu_id: Optional[str]):
    gs = get_potential_groups(user, sisu_id)
    docs_with_course_tag = Tag.query.filter_by(type=TagType.CourseCode).with_entities(Tag.block_id).subquery()
    tags = (Tag.query
            .filter(Tag.name.in_([GROUP_TAG_PREFIX + g.name for g in gs]) & Tag.block_id.in_(docs_with_course_tag))
            .options(joinedload(Tag.block).joinedload(Block.docentries))
            .all())
    tag_map = {t.name[len(GROUP_TAG_PREFIX):]: t for t in tags}

    def get_course_page(ug: UserGroup):
        t: Tag = tag_map.get(ug.name)
        if t:
            return f'<a href="{t.block.docentries[0].url_relative}">URL</a>'
        else:
            return None

    return {
        'rows': {
            g.external_id.external_id: {
                'TIM-nimi': g.name,
                'URL': f'<a href="{g.admin_doc.docentries[0].url_relative}">URL</a>' if g.admin_doc else None,
                'Jäseniä': len(g.active_memberships),
                'Kurssisivu': get_course_page(g),
            } for g in gs
        },
        'realnamemap': {
            g.external_id.external_id: get_sisu_group_desc_for_table(g) if sisu_id else g.display_name for g in gs
        },
        'emailmap': {
            g.external_id.external_id: '' for g in gs
        },
        'fields': ['Jäseniä', 'TIM-nimi', 'URL', 'Kurssisivu'],
        'aliases': {
            'TIM-nimi': 'TIM-nimi',
            'URL': 'URL',
            'Jäseniä': 'Jäseniä',
            'Kurssisivu': 'Kurssisivu',
        },
        'styles': {
            g.external_id.external_id: {} for g in gs
        },
    }


@attr.s(auto_attribs=True)
class TableFormHtmlModel(GenericHtmlModel[TableFormInputModel, TableFormMarkupModel, TableFormStateModel]):
    def get_component_html_name(self) -> str:
        return 'tableform-runner'

    def show_in_view_default(self) -> bool:
        return False

    def get_json_encoder(self):
        return TimJsonEncoder

    def get_static_html(self) -> str:
        return render_static_table_form(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        if self.markup.open:
            tid = TaskId.parse(self.taskID)
            d = get_doc_or_abort(tid.doc_id)
            user = User.get_by_name(self.current_user_id)
            if self.markup.sisugroups:
                f = get_sisugroups(user, self.markup.sisugroups)
            else:
                f = tableform_get_fields(
                    self.markup.fields,
                    self.markup.groups,
                    d,
                    user,
                    self.markup.removeDocIds,
                    self.markup.showInView,
                    group_filter_type=GroupFilter(self.markup.includeUsers),
                )
            r = {**r, **f}
        return r


class TableFormHtmlSchema(TableFormAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return TableFormHtmlModel(**data)


@attr.s(auto_attribs=True)
class TableFormAnswerModel(GenericAnswerModel[TableFormInputModel, TableFormMarkupModel, TableFormStateModel]):
    pass


class TableFormAnswerSchema(TableFormAttrs, GenericAnswerSchema):
    input = fields.Nested(TableFormInputSchema, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return TableFormAnswerModel(**data)


def render_static_table_form(m: TableFormHtmlModel):
    return render_template_string(
        """
<div class="tableform">
<button class="timButton">
    Avaa Taulukko/Raporttinäkymä
</button>
</div>
<br>
        """,
        **attr.asdict(m.markup),
        # userword=m.state.userword if m.state else '',
    )


tableForm_plugin = create_blueprint(__name__, 'tableForm', TableFormHtmlSchema(), csrf)


class GenerateCSVSchema(Schema):
    docId = fields.Str(required=True)
    groups = fields.List(fields.Str(), required=True)
    separator = fields.Str(required=True)
    realnames = fields.Boolean()
    usernames = fields.Boolean()
    emails = fields.Boolean()
    removeDocIds = fields.Boolean()
    fields = fields.List(fields.Str(), required=True)

    @post_load
    def make_obj(self, data):
        return GenerateCSVModel(**data)


@attr.s(auto_attribs=True)
class GenerateCSVModel:
    docId: str
    fields: List[str]
    groups: List[str]
    separator: str = ","
    realnames: Union[bool, Missing] = True
    usernames: Union[bool, Missing] = True
    emails: Union[bool, Missing] = False
    removeDocIds: Union[bool, Missing] = True


@tableForm_plugin.route('/generateCSV')
@use_args(GenerateCSVSchema())
def gen_csv(args: GenerateCSVModel):
    """
    Generates a report defined by tableForm attributes
    # TODO: generic, move
    # TODO: add schemas
    :return: CSV containing headerrow and rows for users and values
    """
    curr_user = get_current_user_object()
    docid, groups, separator, real_names, user_names, emails, removeDocIds, fields = \
        args.docId, args.groups, args.separator, args.realnames, \
        args.usernames, args.emails, args.removeDocIds, args.fields
    if len(separator) > 1:
        # TODO: Add support >1 char strings like in Korppi
        return "Only 1-character string separators supported for now"
    doc = get_doc_or_abort(docid)
    r = tableform_get_fields(fields, groups,
                             doc, curr_user, removeDocIds, allow_non_teacher=True)
    rowkeys = list(r['rows'].keys())
    rowkeys.sort()
    data = [[] for i in range(len(rowkeys) + 1)]
    if real_names:
        data[0].append("Real name")
    if user_names:
        data[0].append("Username")
    if emails:
        data[0].append("email")
    data[0] = data[0] + r['fields']
    y_offset = 1
    for ycoord, rowkey in enumerate(rowkeys):
        row = r['rows'].get(rowkey)
        if real_names:
            data[ycoord + y_offset].append(r['realnamemap'].get(rowkey))
        if user_names:
            data[ycoord + y_offset].append(rowkey)
        if emails:
            data[ycoord + y_offset].append(r['emailmap'].get(rowkey))
        for field in r['fields']:
            data[ycoord + y_offset].append(row.get(field))
    return csv_response(data, 'excel', separator)

@tableForm_plugin.route('/fetchTableData')
def fetch_rows():
    # r = {}
    curr_user = get_current_user_object()
    taskid = request.args.get("taskid")
    tid = TaskId.parse(taskid, require_doc_id=False, allow_block_hint=False)
    doc = get_doc_or_abort(tid.doc_id)
    try:
        plug = find_plugin_from_document(doc.document, tid, curr_user)
    except TaskNotFoundException:
        return abort(404, f'Table not found: {tid}')
    # debug = plug.values
    r = tableform_get_fields(plug.values.get("fields",[]), plug.values.get("groups", []),
                             doc, curr_user, plug.values.get("removeDocIds", True),
                             plug.values.get("showInView"))
    return json_response(r, headers={"No-Date-Conversion": "true"})

@tableForm_plugin.route('/fetchTableDataPreview')
def fetch_rows_preview():
    curr_user = get_current_user_object()
    taskid = request.args.get("taskid")
    tid = TaskId.parse(taskid, require_doc_id=False, allow_block_hint=False)
    doc = get_doc_or_abort(tid.doc_id)
    # With this route we can't be certain about showInView so we just check for edit access
    # whoever can open the plugin in preview should have that right
    if not curr_user.has_edit_access(doc):
        return abort(403, f'Missing edit access for document {doc.id}')
    fields = request.args.getlist("fields")
    groups = request.args.getlist("groups")
    removeDocIds = get_boolean(request.args.get("removeDocIds"), True)
    r = tableform_get_fields(fields, groups,
                             doc, curr_user, removeDocIds, allow_non_teacher=True)
    return json_response(r, headers={"No-Date-Conversion": "true"})


@tableForm_plugin.route('/updateFields')
def update_fields():
    r = {}
    fields_to_update = request.args.getlist("fields")
    taskid = request.args.get("taskid")
    tid = TaskId.parse(taskid, require_doc_id=False, allow_block_hint=False)
    doc = get_doc_or_abort(tid.doc_id)
    curr_user = get_current_user_object()
    try:
        plug = find_plugin_from_document(doc.document, tid, curr_user)
    except TaskNotFoundException:
        return abort(404, f'Table not found: {tid}')
    groupnames = plug.values.get("groups",[])
    queried_groups = UserGroup.query.filter(UserGroup.name.in_(groupnames))
    fielddata, _, field_names, _ = get_fields_and_users(fields_to_update, queried_groups, doc,
                         curr_user, plug.values.get("removeDocIds", True), add_missing_fields=True,
                         allow_non_teacher=plug.values.get("showInView"))
    rows = {}
    styles = {}
    for f in fielddata:
        username = f['user'].name
        rows[username] = dict(f['fields'])
        for key, content in rows[username].items():
            if type(content) is dict:
                rows[username][key] = json.dumps(content)
        styles[username] = dict(f['styles'])
    r['rows'] = rows
    r['styles'] = styles
    r['fields'] = field_names
    return json_response(r, headers={"No-Date-Conversion": "true"})


def tableform_get_fields(
        flds: List[str],
        groupnames: List[str],
        doc: DocInfo,
        curr_user: User,
        remove_doc_ids: bool,
        allow_non_teacher: bool,
        group_filter_type = GroupFilter.Active,
):
    queried_groups = UserGroup.query.filter(UserGroup.name.in_(groupnames))
    fielddata, aliases, field_names, groups = \
        get_fields_and_users(flds, queried_groups, doc,
                             curr_user, remove_doc_ids, add_missing_fields=True,
                             allow_non_teacher = allow_non_teacher,
                             group_filter_type=group_filter_type)
    rows = {}
    realnames = {}
    emails = {}
    styles = {}
    group_ids = set(g.id for g in groups)
    membershipmap = {}
    for f in fielddata:
        u: User = f['user']
        username = u.name
        rows[username] = dict(f['fields'])
        for key, content in rows[username].items():
            if type(content) is dict:
                rows[username][key] = json.dumps(content)
        realnames[username] = f['user'].real_name
        emails[username] = f['user'].email
        styles[username] = dict(f['styles'])
        if group_filter_type != GroupFilter.Active:
            relevant_memberships: List[UserGroupMember] = [m for m in u.memberships if m.usergroup_id in group_ids]
            membership_end = None
            # If the user is not active in any of the groups, we'll show the lastly-ended membership.
            # TODO: It might be possible in the future that the membership_end is in the future.
            if all(m.membership_end is not None for m in relevant_memberships):
                membership_end = max(m.membership_end for m in relevant_memberships)
            membershipmap[username] = membership_end
    r = dict()
    r['rows'] = rows
    r['realnamemap'] = realnames
    r['emailmap'] = emails
    r['membershipmap'] = membershipmap
    r['fields'] = field_names
    r['aliases'] = aliases
    r['styles'] = styles
    return r


@tableForm_plugin.route('/answer/', methods=['put'])
@csrf.exempt
@use_args(TableFormAnswerSchema(), locations=("json",))
def answer(args: TableFormInputModel):
    rows = args.input.replyRows
    save_rows = []
    for u, r in rows.items():
        user = User.get_by_name(u)
        save_rows.append({'user': user.id, 'fields': r})

    web = {}
    result = {'web': web}
    savedata = save_rows
    result["savedata"] = savedata
    web['result'] = "saved"
    return jsonify(result)


@tableForm_plugin.route('/reqs/')
@tableForm_plugin.route('/reqs')
def reqs():
    """Introducing templates for tableForm plugin"""
    templates = ["""
``` {#tableForm_table plugin="tableForm"}
groups: 
 - Group Name   # Use Group Name here
fields:
 - d1=demo1     # List your fields here, = for alias
table: true
report: false
maxRows: 40em     # max height for the table before scrollbar 
realnames: true   # Show full name in 2nd column, true or false
buttonText:       # Name your save button here
autosave: true    # autosave, true or false
cbColumn: true    # show checkboxes
nrColumn: true    # show numbers
filterRow: true   # show filters 
singleLine: true  #
emailUsersButtonText: "Lähetä sähköpostia valituille" # jos halutaan lähettää sähköpostia 
```""", """
``` {#tableForm_table_report plugin="tableForm"}
groups: 
 - Group Name   # Use Group Name here
fields:
 - d1=demo1     # List your fields here, = for alias
table: true
report: true
maxRows: 40em   # max height for the table before scrollbar 
realnames: true # Show full name in 2nd column, true or false
buttonText:     # Name your save table button here
autosave: true  # autosave, true or false
separator: ";"  # Define your report value separator here, ";" by default
anonNames: true # To show or hide user (and full) names in report, true or false
reportButton: "Name your generate report button here"
```""", """
``` {#tableForm_report plugin="tableForm"}
groups: 
 - Group Name   # Use Group Name here
fields:
 - d1=demo1     # List your fields here, = for alias
table: false
report: true
separator: ";"  # Define your value separator here, ";" as default
anonNames: true # To show or hide user (and full) names in report, true or false
reportButton: "Name your generate report button here"
```"""]
    editor_tabs = [
            {
                'text': 'Fields',
                'items': [
                    {
                        'text': 'Tables',
                        'items': [
                            {
                                'data': templates[0].strip(),
                                'text': 'Table only',
                                'expl': 'Form a table for editing forms',
                            },
                            {
                                'data': templates[1].strip(),
                                'text': 'Table and report',
                                'expl': 'Form a table for editing forms, that can be converted to report',
                            },
                            {
                                'data': templates[2].strip(),
                                'text': 'Report only',
                                'expl': 'Form a report',
                            },
                        ],
                    },
                ],
            },
        ]
    if os.environ.get('SHOW_TEMPLATES', "True") == "False":
        editor_tabs = None
    return jsonify({
        "js": [],
        "multihtml": True,
        'editor_tabs': editor_tabs,
    },
    )
