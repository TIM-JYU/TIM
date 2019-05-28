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
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.taskid import TaskId
from timApp.tim_app import csrf
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.responsehelper import csv_response


@attr.s(auto_attribs=True)
class TableFormStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    #TODO: Save user given table layouts like in timTable


class TableFormStateSchema(Schema):
    @post_load
    def make_obj(self, data):
        return TableFormStateModel(**data)

    class Meta:
        strict = True


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
    reportButton: Union[str, Missing] = missing
    realnames: Union[bool, Missing] = missing
    maxWidth: Union[str, Missing] = missing
    minWidth: Union[str, Missing] = missing
    singleLine: Union[bool, Missing] = missing
    open: Union[bool, Missing] = missing
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
    reportButton = fields.Str(allow_none=True)
    realnames = fields.Boolean()
    singleLine = fields.Boolean(allow_none=True)
    maxWidth = fields.Str()
    minWidth = fields.Str(allow_none=True)
    open = fields.Boolean(allow_none=True)
    fields = fields.List(fields.Str()) #Keep this last - bad naming

    @post_load
    def make_obj(self, data):
        return TableFormMarkupModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class TableFormInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    replyRows: dict


class TableFormInputSchema(Schema):
    replyRows = fields.Dict();

    @post_load
    def make_obj(self, data):
        return TableFormInputModel(**data)


class TableFormAttrs(Schema):
    """Common fields for HTML and answer routes."""
    markup = fields.Nested(TableFormMarkupSchema)
    state = fields.Nested(TableFormStateSchema, allow_none=True, required=True)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class TableFormHtmlModel(GenericHtmlModel[TableFormInputModel, TableFormMarkupModel, TableFormStateModel]):
    def get_component_html_name(self) -> str:
        return 'tableform-runner'

    def get_static_html(self) -> str:
        return render_static_tableForm(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        if self.markup.groups and self.markup.fields:
            groups = UserGroup.query.filter(UserGroup.name.in_(self.markup.groups))
            try:
                tid = TaskId.parse(self.taskID)
            except PluginException as e:
                return
            d = get_doc_or_abort(tid.doc_id)
            user = User.get_by_name(self.user_id)
            userfields = get_fields_and_users(self.markup.fields, groups, d, user)
            rows = {}
            for f in userfields[0]:
                rows[f['user'].name] = dict(f['fields'])
                rows[f['user'].name]['realname'] = f['user'].real_name
            r['rows'] = rows
            try:
                r['fields'] = list(userfields[0][0]['fields'].keys())
            except IndexError:
                r['fields'] = []
            r['aliases'] = userfields[1]
            r['contentMap'] = userfields[2]
            #TODO else return "no groups/no fields"?

        return r

    class Meta:
        strict = True


class TableFormHtmlSchema(TableFormAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return TableFormHtmlModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class TableFormAnswerModel(GenericAnswerModel[TableFormInputModel, TableFormMarkupModel, TableFormStateModel]):
    pass


class TableFormAnswerSchema(TableFormAttrs, GenericAnswerSchema):
    input = fields.Nested(TableFormInputSchema, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return TableFormAnswerModel(**data)

    class Meta:
        strict = True


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
        userword=m.state.userword if m.state else '',
    )


tableForm_plugin = create_blueprint(__name__, 'tableForm', TableFormHtmlSchema(), csrf)


@tableForm_plugin.route('/generateCSV')
def gen_csv():
    temp = json.loads(request.args.get('data'))
    return csv_response(temp, 'excel', request.args.get('separator'))


@tableForm_plugin.route('/answer/', methods=['put'])
@csrf.exempt
@use_args(TableFormAnswerSchema(strict=True), locations=("json",))
def answer(args: TableFormInputModel):
    rows = args.input.replyRows
    saveRows = []
    for u, r in rows.items():
        user = User.get_by_name(u)
        saveRows.append({'user':user.id, 'fields':r})

    web = {}
    result = {'web': web}
    save = saveRows
    result["save"] = save
    web['result'] = "saved"
    return jsonify(result)


@tableForm_plugin.route('/reqs/')
@tableForm_plugin.route('/reqs')
def reqs():
    """Introducing templates for tableForm plugin"""
    templates = ["""
``` {#tableForm_table plugin="tableForm"}
groups: 
 - Group Name #Use Group Name here
fields:
 - d1=demo1 #List your fields here, = for alias
table: true
report: false
realnames: true #Show full name in 2nd column, true or false
buttonText: #Name your save button here
autosave: true #autosave, true or false
```""", """
``` {#tableForm_table_report plugin="tableForm"}
groups: 
 - Group Name #Use Group Name here
fields:
 - d1=demo1 #List your fields here, = for alias
table: true
report: true
realnames: true #Show full name in 2nd column, true or false
buttonText: #Name your save table button here
autosave: true #autosave, true or false
separator: ";" #Define your report value separator here, ";" by default
anonNames: true #To show or hide user (and full) names in report, true or false
reportButton: "Name your generate report button here
```""", """
``` {#tableForm_report plugin="tableForm"}
groups: 
 - Group Name #Use Group Name here
fields:
 - d1=demo1 #List your fields here, = for alias
table: false
report: true
separator: ";" #Define your value separator here, ";" as default
anonNames: true #To show or hide user (and full) names in report, true or false
reportButton: "Name your generate report button here
```"""]
    editor_tabs = [
            {
                'text': 'Plugins',
                'items': [
                    {
                        'text': 'TableForm',
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
