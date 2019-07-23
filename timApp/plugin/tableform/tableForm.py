"""
TIM example plugin: a tableFormndrome checker.
"""
import json
import os
from typing import Union, List

import attr
from flask import jsonify, render_template_string, request
from marshmallow import Schema, fields, post_load
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from pluginserver_flask import GenericMarkupModel, GenericMarkupSchema, GenericHtmlSchema, GenericHtmlModel, \
    GenericAnswerSchema, GenericAnswerModel, Missing, \
    InfoSchema, create_blueprint
from timApp.answer.routes import get_fields_and_users
from timApp.auth.accesshelper import get_doc_or_abort
from timApp.auth.sessioninfo import get_current_user_object
from timApp.plugin.plugin import find_plugin_from_document
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.taskid import TaskId
from timApp.tim_app import csrf
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import verify_json_params
from timApp.util.flask.responsehelper import csv_response, json_response


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
    open: Union[bool, Missing] = missing
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
    open = fields.Boolean(allow_none=True)
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


@attr.s(auto_attribs=True)
class TableFormHtmlModel(GenericHtmlModel[TableFormInputModel, TableFormMarkupModel, TableFormStateModel]):
    def get_component_html_name(self) -> str:
        return 'tableform-runner'

    def show_in_view_default(self) -> bool:
        return False

    def get_static_html(self) -> str:
        return render_static_tableForm(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        if self.markup.groups and self.markup.fields:
            groups = UserGroup.query.filter(UserGroup.name.in_(self.markup.groups))
            try:
                tid = TaskId.parse(self.taskID)
            except PluginException:
                return
            d = get_doc_or_abort(tid.doc_id)
            user = User.get_by_name(self.current_user_id)
            siw = self.markup.showInView
            fielddata, aliases, field_names = \
                get_fields_and_users(self.markup.fields, groups, d,
                                     user, self.markup.removeDocIds, add_missing_fields=True, allow_non_teacher=siw)
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
            r['rows'] = rows
            r['realnamemap'] = realnames
            r['emailmap'] = emails
            r['fields'] = field_names
            r['aliases'] = aliases
            r['styles'] = styles
            # TODO else return "no groups/no fields"

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


def render_static_tableForm(m: TableFormHtmlModel):
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
    # TODO: Refactor - repeated lines from get_browser_json
    # TODO: check for correct plugin
    r = {}
    curr_user = get_current_user_object()
    taskid = request.args.get("taskid")
    tid = TaskId.parse(taskid, False, False)
    doc = get_doc_or_abort(tid.doc_id)
    plug = find_plugin_from_document(doc.document, tid, curr_user)
    debug = plug.values
    groups = UserGroup.query.filter(UserGroup.name.in_(plug.values.get("groups")))
    fielddata, aliases, field_names = \
        get_fields_and_users(plug.values.get("fields"), groups, doc,
                             curr_user, plug.values.get("removeDocIds", True), add_missing_fields=True)
    debug = plug.values
    rows = {}
    realnames = {}
    emails = {}
    for f in fielddata:
        username = f['user'].name
        rows[username] = dict(f['fields'])
        realnames[username] = f['user'].real_name
        emails[username] = f['user'].email
    r['rows'] = rows
    r['realnamemap'] = realnames
    r['emailmap'] = emails
    r['fields'] = field_names
    r['aliases'] = aliases
    return json_response(r)


@tableForm_plugin.route('/answer/', methods=['put'])
@csrf.exempt
@use_args(TableFormAnswerSchema(), locations=("json",))
def answer(args: TableFormInputModel):
    rows = args.input.replyRows
    saveRows = []
    for u, r in rows.items():
        user = User.get_by_name(u)
        saveRows.append({'user': user.id, 'fields': r})

    web = {}
    result = {'web': web}
    savedata = saveRows
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
                'text': 'Plugins',
                'items': [
                    {
                        'text': 'Fields',
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
