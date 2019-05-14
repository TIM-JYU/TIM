"""
TIM example plugin: a tableFormndrome checker.
"""
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
    usednames: Union[str, Missing] = missing
    sortBy: Union[str, Missing] = missing
    dataCollection: Union[str, Missing] = missing
    print: Union[bool, Missing] = missing
    fields: Union[List[str], Missing] = missing
    autosave: Union[bool, Missing] = missing


class TableFormMarkupSchema(GenericMarkupSchema):
    initword = fields.Str()
    groups = fields.List(fields.Str())
    table = fields.Boolean()
    report = fields.Boolean()
    separator = fields.Str(allow_none=True)
    usednames = fields.Str(allow_none=True)
    sortBy = fields.Str(allow_none=True)
    dataCollection = fields.Str(allow_none=True)
    print = fields.Boolean()
    autosave = fields.Boolean()
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
        # if self.state:
        #     r['userword'] = self.state.userword
        # if self.markup.groups:
            # ug = UserGroup.get_by_name(self.markup.groups[0]) #! Lista vs 1 ryhmä
            # members = ug.users.all()
            # # TODO: Check if user has right to see group members
            # rows = {}
            # # for i, m in enumerate(members):
            # #     #colnum_to_letters()
            # #     #100.textfield_A1.IN4wKoImZc5b - 100.textfield_A6.IN4wKoImZc5b
            # #     userdata['cells']["A" + str(i+2)] = m.name
            # #     if self.markup.fields:
            # #         for j, t in enumerate(self.markup.fields):
            # #             ans = m.get_answers_for_task(t).first()
            # #             # TODO: Check if user has right to see answers for task/user
            # #             # TODO: Save cells (user/task) as matrix in global variable?
            # #             if ans:
            # #                 userdata['cells'][colnum_to_letters(j+1) + str(i+2)] = ans.content
            # #                 # TODO: Parse content? {"userword": "2"}
            # for i, m in enumerate(members):
            #     #colnum_to_letters()
            #     #100.textfield_A1.IN4wKoImZc5b - 100.textfield_A6.IN4wKoImZc5b
            #     rows[m.name] = {}
            #     if self.markup.fields:
            #         for j, t in enumerate(self.markup.fields):
            #             ans = m.get_answers_for_task(t).first() # TODO Check optimal request
            #             # TODO: ^Make custom db request with arrays of users and tasks?
            #             ans2 = m.get_answers_for_task(t)
            #             # TODO: Check if user has right to see answers for task/user
            #             # TODO: Save cells (user/task) as matrix in global variable?
            #             if ans:
            #                 #TODO: 204.textfield_d1.args, 204.textfield_d1.usercode
            #                 #^ check for multiple fields in answerstable, use first field if not given specific field
            #                 rows[m.name][t] = ans.content
            #                 # TODO: Parse content? {"userword": "2"}
            # # if self.markup.fields:
            # #     userdata['tasks'] = self.markup.fields
        if self.markup.groups and self.markup.fields:
            groups = []
            for g in self.markup.groups:
                groups.append(UserGroup.get_by_name(g))
            #print(self.taskID)
            try:
                tid = TaskId.parse(self.taskID)
            except PluginException as e:
                return
            d = get_doc_or_abort(tid.doc_id)
            # user1= get_current_user()
            # user2 = get_current_user_object()
            user = User.get_by_name(self.user_id)
            userfields = get_fields_and_users(self.markup.fields, groups, d, user)
            rows = {}
            for f in userfields:
                rows[f['user'].name] = f['fields']
            r['rows'] = rows
            r['fields'] = []
            for field in self.markup.fields: #Todo check if simpler way to simply add missing docid prefix to field
                task_id = TaskId.parse(field, False, False)
                if not task_id.doc_id:
                    task_id.doc_id = d.id
                r['fields'].append(task_id.extended_or_doc_task)
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


@tableForm_plugin.route('/answer/', methods=['put'])
@csrf.exempt
@use_args(TableFormAnswerSchema(strict=True), locations=("json",))
def answer(args: TableFormInputModel):
    args2 = request.get_json() #TODO: Schema/Model
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
 - Group Name #TODO ryhmä listana vai max 1 ryhmä?
table: true
report: false
```""", """
``` {#tableForm_table_report plugin="tableForm"}
groups: 
 - Group Name #TODO ryhmä listana vai max 1 ryhmä?
table: true
report: true
```""", """
``` {#tableForm_report plugin="tableForm"}
groups: 
 - Group Name #TODO ryhmä listana vai max 1 ryhmä?
table: false
report: true
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
