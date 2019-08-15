"""
TIM example plugin: a tableFormndrome checker.
"""
import json
import os
from typing import Union, List

import attr
from flask import jsonify, render_template_string, request, abort
from marshmallow import Schema, fields, post_load
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from pluginserver_flask import GenericMarkupModel, GenericMarkupSchema, GenericHtmlSchema, GenericHtmlModel, \
    GenericAnswerSchema, GenericAnswerModel, Missing, \
    InfoSchema, create_blueprint
from timApp.util.answerutil import get_fields_and_users
from timApp.auth.accesshelper import get_doc_or_abort
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docinfo import DocInfo
from timApp.plugin.plugin import find_plugin_from_document, TaskNotFoundException
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.taskid import TaskId
from timApp.tim_app import csrf
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import verify_json_params
from timApp.util.flask.responsehelper import csv_response, json_response
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
    showToolbar: Union[bool, Missing] = True
    sisugroups: Union[bool, Missing] = missing
    autoUpdateFields: Union[bool, Missing] = True
    autoUpdateTables: Union[bool, Missing] = True
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
    showToolbar = fields.Boolean(default=True)
    sisugroups = fields.Boolean(default=True)
    autoUpdateFields = fields.Boolean(default=True)
    autoUpdateTables = fields.Boolean(default=True)
    fields = fields.List(fields.Str())  # Keep this last - bad naming

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


def get_sisugroups(user):
    return {
        'rows': {
            'sisu:jy-CUR-4668-jy-studysubgroup-9515-teachers': {
                'timname': 'ohj1s19c',
                'url': '<a href="/view/groups/2019/ITKP102/ohj1s19">URL</a>',
            },
            'sisu:jy-CUR-4668-jy-studysubgroup-9515-students': {
                'timname': 'ohj1s19',
                'url': '<a href="/view/groups/2019/ITKP102/ohj1s19">URL</a>',
            },
            'sisu:jy-CUR-4668-jy-studysubgroup-9516-students': {
                'timname': '',
                'url': '',
            },
        },
        'realnamemap': {
            'sisu:jy-CUR-4668-jy-studysubgroup-9515-teachers': 'ITKP102 2019-09-09--2019-12-20: Luento 1: Opettajat',
            'sisu:jy-CUR-4668-jy-studysubgroup-9515-students': 'ITKP102 2019-09-09--2019-12-20: Luento 1: Opiskelijat',
            'sisu:jy-CUR-4668-jy-studysubgroup-9516-students': 'ITKP102 2019-09-09--2019-12-20: Luento 2: Opiskelijat',
        },
        'emailmap': {
            'sisu:jy-CUR-4668-jy-studysubgroup-9515-teachers': '',
            'sisu:jy-CUR-4668-jy-studysubgroup-9515-students': '',
            'sisu:jy-CUR-4668-jy-studysubgroup-9516-students': '',
        },
        'fields': ['timname', "url"],
        'aliases': {
            'timname': 'timname',
            'url': 'url'
    },
        'styles': {
            'sisu:jy-CUR-4668-jy-studysubgroup-9515-teachers': {},
            'sisu:jy-CUR-4668-jy-studysubgroup-9515-students': {},
            'sisu:jy-CUR-4668-jy-studysubgroup-9516-students': {},
        },
    }


@attr.s(auto_attribs=True)
class TableFormHtmlModel(GenericHtmlModel[TableFormInputModel, TableFormMarkupModel, TableFormStateModel]):
    def get_component_html_name(self) -> str:
        return 'tableform-runner'

    def show_in_view_default(self) -> bool:
        return False

    def get_static_html(self) -> str:
        return render_static_table_form(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        if self.markup.open:
            tid = TaskId.parse(self.taskID)
            d = get_doc_or_abort(tid.doc_id)
            user = User.get_by_name(self.current_user_id)
            if self.markup.sisugroups:
                f = get_sisugroups(user)  # TODO: SISU change a proper call hera
            else:
                f = tableform_get_fields(self.markup.fields, self.markup.groups, d,
                                     user, self.markup.removeDocIds, self.markup.showInView)
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


@tableForm_plugin.route('/generateCSV')
def gen_csv():
    temp = json.loads(request.args.get('data'))
    if len(request.args.get('separator')) > 1:
        # TODO: Add support >1 char strings like in Korppi
        return "Only 1-character string separators supported for now" 
    return csv_response(temp, 'excel', request.args.get('separator'))

@tableForm_plugin.route('/fetchTableData')
def fetch_rows():
    # r = {}
    curr_user = get_current_user_object()
    taskid = request.args.get("taskid")
    tid = TaskId.parse(taskid, False, False)
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
    tid = TaskId.parse(taskid, False, False)
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
    tid = TaskId.parse(taskid, False, False)
    doc = get_doc_or_abort(tid.doc_id)
    curr_user = get_current_user_object()
    plug = find_plugin_from_document(doc.document, tid, curr_user)
    if not plug:
        return abort(404, f'Table not found: {tid}')
    groups = plug.values.get("groups",[])
    queried_groups = UserGroup.query.filter(UserGroup.name.in_(groups))
    fielddata, _, field_names = get_fields_and_users(fields_to_update, queried_groups, doc,
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


def tableform_get_fields(flds: List[str], groups: List[str],
                         doc: DocInfo, curr_user: User, remove_doc_ids: bool, allow_non_teacher: bool):
    queried_groups = UserGroup.query.filter(UserGroup.name.in_(groups))
    fielddata, aliases, field_names = \
        get_fields_and_users(flds, queried_groups, doc,
                             curr_user, remove_doc_ids, add_missing_fields=True,
                             allow_non_teacher = allow_non_teacher)
    rows = {}
    realnames = {}
    emails = {}
    styles = {}
    for f in fielddata:
        username = f['user'].name
        rows[username] = dict(f['fields'])
        for key, content in rows[username].items():
            if type(content) is dict:
                rows[username][key] = json.dumps(content)
        realnames[username] = f['user'].real_name
        emails[username] = f['user'].email
        styles[username] = dict(f['styles'])
    r = dict()
    r['rows'] = rows
    r['realnamemap'] = realnames
    r['emailmap'] = emails
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
