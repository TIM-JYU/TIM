"""
TIM example plugin: a ImportData
"""
import json
import os
from typing import Union, List
from flask import abort

import attr
from timApp.plugin.containerLink import get_plugin
import requests

from flask import jsonify, render_template_string, request
from marshmallow import Schema, fields, post_load
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from pluginserver_flask import GenericMarkupModel, GenericMarkupSchema, GenericHtmlSchema, GenericHtmlModel, \
    GenericAnswerSchema, GenericAnswerModel, Missing, \
    InfoSchema, create_blueprint
from timApp.answer.routes import get_fields_and_users, handle_jsrunner_response
from timApp.auth.accesshelper import get_doc_or_abort
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.taskid import TaskId
from timApp.tim_app import csrf
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.responsehelper import csv_response


@attr.s(auto_attribs=True)
class ImportDataStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    url: Union[str, Missing] = None
    separator: Union[str, Missing] = None

class ImportDataStateSchema(Schema):
    url = fields.Str(allow_none=True)
    separator = fields.Str(allow_none=True)

    @post_load
    def make_obj(self, data):
        res = ImportDataStateModel(**data)
        return res


@attr.s(auto_attribs=True)
class ImportDataMarkupModel(GenericMarkupModel):
    buttonText: Union[str, Missing] = missing
    docid: Union[int, Missing] = missing
    open: Union[bool, Missing] = missing
    borders: Union[bool, Missing] = missing
    upload: Union[bool, Missing] = missing
    useurl: Union[bool, Missing] = missing
    useseparator: Union[bool, Missing] = missing
    uploadstem: Union[str, Missing] = missing
    urlstem: Union[str, Missing] = missing
    loadButtonText: Union[str, Missing] = missing
    url: Union[str, Missing] = missing
    beforeOpen: Union[str, Missing] = missing
    separator: Union[str, Missing] = missing
    prefilter: Union[str, Missing] = missing
    placeholder: Union[str, Missing] = missing



class ImportDataMarkupSchema(GenericMarkupSchema):
    buttonText = fields.Str(allow_none=True)
    docid = fields.Int(allow_none=True)
    open = fields.Bool(allow_none=True)
    borders = fields.Bool(allow_none=True)
    upload = fields.Bool(allow_none=True)
    useurl = fields.Bool(allow_none=True)
    useseparator = fields.Bool(allow_none=True)
    uploadstem = fields.Str(allow_none=True)
    urlstem = fields.Str(allow_none=True)
    loadButtonText = fields.Str(allow_none=True)
    url = fields.Str(allow_none=True)
    beforeOpen = fields.Str(allow_none=True)
    separator = fields.Str(allow_none=True)
    prefilter = fields.Str(allow_none=True)
    placeholder = fields.Str(allow_none=True)


    @post_load
    def make_obj(self, data):
        return ImportDataMarkupModel(**data)


@attr.s(auto_attribs=True)
class ImportDataInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    data: str
    separator: str
    url: str

class ImportDataInputSchema(Schema):
    data = fields.Str(required=True)
    separator = fields.Str(required=False)
    url = fields.Str(required=False)

    @post_load
    def make_obj(self, data):
        return ImportDataInputModel(**data)


class ImportDataAttrs(Schema):
    """Common fields for HTML and answer routes."""
    markup = fields.Nested(ImportDataMarkupSchema)
    state = fields.Nested(ImportDataStateSchema, allow_none=True, required=True)


@attr.s(auto_attribs=True)
class ImportDataHtmlModel(GenericHtmlModel[ImportDataInputModel, ImportDataMarkupModel, ImportDataStateModel]):
    def get_component_html_name(self) -> str:
        return 'importdata-runner'

    def show_in_view(self) -> bool:
        return False

    def get_static_html(self) -> str:
        s = self.markup.beforeOpen or "+ Open Import"
        return render_static_ImportData(self, s)

    def get_browser_json(self):
        r = super().get_browser_json()
        # r['state']['separator'] = ";"
        return r


class ImportDataHtmlSchema(ImportDataAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return ImportDataHtmlModel(**data)


@attr.s(auto_attribs=True)
class ImportDataAnswerModel(GenericAnswerModel[ImportDataInputModel, ImportDataMarkupModel, ImportDataStateModel]):
    pass


class ImportDataAnswerSchema(ImportDataAttrs, GenericAnswerSchema):
    input = fields.Nested(ImportDataInputSchema, required=False)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return ImportDataAnswerModel(**data)


def render_static_ImportData(m: ImportDataHtmlModel, s: str):
    return render_template_string(
        f"""
<div class="ImportData">
 {s}
</div>
<br>
        """,
        **attr.asdict(m.markup),
    )


importData_plugin = create_blueprint(__name__, 'importData', ImportDataHtmlSchema(), csrf)

@importData_plugin.route('/answer/', methods=['put'])
@csrf.exempt
@use_args(ImportDataAnswerSchema(), locations=("json",))
def answer(args: ImportDataAnswerModel):
    sdata = args.input.data
    data = sdata.split("\n")
    output = ""
    if args.markup.prefilter:
        params = {'code': args.markup.prefilter, 'data': data}
        runurl = get_plugin('jsrunner').get("host") + 'runScript/'
        r = requests.request('post', runurl, data=params)
        result = json.loads(r.text)
        error = result.get('error', '')
        if error:
            abort(400, error)
        data = result.get("result",[])
        output = result.get("output", "")
    did = int(args.taskID.split(".")[0])
    defaultseparator = args.markup.separator or ";"
    separator = args.input.separator or defaultseparator
    if args.markup.docid:
        did = args.markup.docid
    rows = []
    wrong = 0
    wrongs = ""
    for r in data:
        if not r:
            continue
        parts = r.split(separator)
        u = None
        error = ": unknown name"
        if len(parts) >= 3:
            u = User.get_by_name(parts[0])
        else:
            error = ": too few parts"
        if not u:
            wrong += 1
            wrongs += "\n" + r + error
            continue
        tname = parts[1]
        value = parts[2]
        uid = u.id
        if tname.find('.') < 0:
            tname = f"{did}.{tname}"
        ur = { 'user': uid, 'fields': {tname: value }}
        rows.append(ur)

    if output:
        output = output + "\n"

    if wrong:
        wrongs = "\nWrong lines: " + str(wrong) + "\n" + wrongs
    jsonresp = { 'savedata': rows, 'web' : { 'result': output + "Imported " + str(len(rows)) + wrongs} }

    save = {}

    if args.input.url != args.markup.url or (args.state.url and args.state.url != args.input.url):
        save['url'] = args.input.url

    if separator != defaultseparator or (args.state.separator and args.state.separator != separator):
        save['separator'] = separator
    if save:
        jsonresp["save"] = save
    # ret = handle_jsrunner_response(jsonresp, result, current_doc)
    # save = saveRows
    # result["save"] = save
    return jsonify(jsonresp)


@importData_plugin.route('/reqs/')
@importData_plugin.route('/reqs')
def reqs():
    """Introducing templates for ImportData plugin"""
    templates = ["""
``` {#ImportData plugin="importData"}
buttonText: Import
```"""]
    editor_tabs = [
            {
                'text': 'Plugins',
                'items': [
                    {
                        'text': 'ImportData',
                        'items': [
                            {
                                'data': templates[0].strip(),
                                'text': 'Import data',
                                'expl': 'Import data from text',
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
