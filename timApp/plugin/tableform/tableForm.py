"""
TIM example plugin: a tableFormndrome checker.
"""
import json
import os
import re
import string
from typing import Union, List

import attr
from flask import jsonify, render_template_string, Blueprint, request
from marshmallow import Schema, fields, post_load, validates, ValidationError
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from pluginserver_flask import GenericMarkupModel, GenericMarkupSchema, GenericHtmlSchema, GenericHtmlModel, \
    GenericAnswerSchema, GenericAnswerModel, Missing, \
    InfoSchema, create_blueprint

from timApp.auth.accesshelper import get_doc_or_abort
from timApp.auth.sessioninfo import get_current_user_object, get_current_user, get_current_user_id
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.taskid import TaskId
from timApp.plugin.timtable.timTable import colnum_to_letters
from timApp.tim_app import csrf
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.answer.routes import get_fields_and_users
from timApp.util.flask.responsehelper import ok_response, json_response, csv_response


@attr.s(auto_attribs=True)
class TableFormStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    #userword: str
    #TODO: Tallenna taulukon data sellaisenaan / tallenna käyttäjän taulukkomäärityksiä?


class TableFormStateSchema(Schema):
    #userword = fields.Str(required=True)

    @post_load
    def make_obj(self, data):
        return TableFormStateModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class TableFormMarkupModel(GenericMarkupModel):
    initword: Union[str, Missing] = missing
    groups: Union[List[str], Missing] = missing
    table: Union[bool, Missing] = missing
    report: Union[bool, Missing] = missing
    separator: Union[str, Missing] = missing
    shownames: Union[bool, Missing] = missing
    sortBy: Union[str, Missing] = missing
    dataCollection: Union[str, Missing] = missing
    autosave: Union[bool, Missing] = missing
    buttonText: Union[str, Missing] = missing
    reportButton: Union[str, Missing] = missing
    realnames: Union[bool, Missing] = missing
    maxWidth: Union[str, Missing] = missing
    minWidth: Union[str, Missing] = missing
    singleLine: Union[bool, Missing] = missing
    fields: Union[List[str], Missing] = missing



class TableFormMarkupSchema(GenericMarkupSchema):
    initword = fields.Str()
    groups = fields.List(fields.Str())
    table = fields.Boolean()
    report = fields.Boolean()
    separator = fields.Str(allow_none=True)
    shownames = fields.Boolean()
    sortBy = fields.Str(allow_none=True)
    dataCollection = fields.Str(allow_none=True)
    autosave = fields.Boolean()
    buttonText = fields.Str(allow_none=True)
    reportButton = fields.Str(allow_none=True)
    realnames = fields.Boolean()
    singleLine = fields.Boolean(allow_none=True)
    maxWidth = fields.Str()
    minWidth = fields.Str(allow_none=True)
    fields = fields.List(fields.Str())

    @post_load
    def make_obj(self, data):
        return TableFormMarkupModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class TableFormInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    #answers:
    replyRows: dict
    nosave: bool = missing


class TableFormInputSchema(Schema):
    nosave = fields.Bool()
    replyRows = fields.Dict();

    # @validates('userword')
    # def validate_userword(self, word):
    #     if not word:
    #         raise ValidationError('Must not be empty.')

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
            #r['fields'] = []
            # for field in self.markup.fields: #TODO: Read fieldnames from first row of userfields response
            #     task = field.split('=', 1)[0]
            #     task = task.split('>', 1)[0]
            #     task_id = TaskId.parse(task, False, False)  # Todo check if simpler way to simply add missing docid prefix to field
            #     if not task_id.doc_id:
            #         task_id.doc_id = d.id
            #     if task_id.extended_or_doc_task in userfields[1].values():
            #         r['fields'].append(list(userfields[1].keys())[list(userfields[1].values()).index(task_id.extended_or_doc_task)])
            #     else:
            #         r['fields'].append(task_id.extended_or_doc_task)
            try:
                r['fields'] = list(userfields[0][0]['fields'].keys())
            except IndexError:
                r['fields'] = []
            r['aliases'] = userfields[1]
            r['contentMap'] = userfields[2]
            #TODO else return "no groups/no fields"

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
<div class="csRunDiv no-popup-menu">
<h4>{{ header }}</h4>
<p class="stem">{{ stem }}</p>
<div><label>{{ inputstem or '' }} <span>
<input type="text"
    class="form-control"
    placeholder="{{inputplaceholder or ''}}"
    value="{{userword or ''}}"
    size="{{cols}}"></span></label>
</div>
<button class="timButton">
    {{ buttonText or button or "Save" }}
</button>
<a>{{ resetText }}</a>
<p class="plgfooter">{{ footer }}</p>
</div>
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
    nosave = args.input.nosave
    if not nosave:
        save = saveRows
        result["save"] = save
        web['result'] = "saved"
    return jsonify(result)


def check_letters(word: str, needed_len: int) -> bool:
    """Checks if word has needed amount of chars.

    :param word: word to check
    :param needed_len: how many letters needed
    :return: true if len match

    """
    s = word.upper()
    return len(re.sub("[^[A-ZÅÄÖ]", "", s)) == needed_len


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
shownames: true #To show or hide user (and full) names in report, true or false
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
shownames: true #To show or hide user (and full) names in report, true or false
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
