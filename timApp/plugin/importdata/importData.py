"""
TIM example plugin: a ImportData
"""
import json
import os
from typing import Union, List

import requests
from dataclasses import dataclass, asdict
from flask import abort
from flask import jsonify, render_template_string
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from marshmallow_dataclass import class_schema
from pluginserver_flask import GenericMarkupModel, GenericHtmlModel, \
    GenericAnswerModel, Missing, \
    create_blueprint
from timApp.plugin.containerLink import get_plugin
from timApp.tim_app import csrf
from timApp.user.user import User
from timApp.util.utils import widen_fields


@dataclass
class ImportDataStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    url: Union[str, Missing, None] = missing
    separator: Union[str, Missing, None] = missing
    fields: Union[List[str], Missing] = missing


@dataclass
class ImportDataMarkupModel(GenericMarkupModel):
    beforeOpen: Union[str, Missing, None] = missing
    borders: Union[bool, Missing, None] = missing
    docid: Union[int, Missing, None] = missing
    fields: Union[List[str], Missing] = missing
    fieldsReqExps: Union[List[str], Missing] = missing
    loadButtonText: Union[str, Missing, None] = missing
    open: Union[bool, Missing, None] = missing
    placeholder: Union[str, Missing, None] = missing
    prefilter: Union[str, Missing, None] = missing
    separator: Union[str, Missing, None] = missing
    userprefix: Union[str, Missing, None] = missing
    userIdField: Union[int, Missing, None] = missing
    upload: Union[bool, Missing, None] = missing
    uploadstem: Union[str, Missing, None] = missing
    url: Union[str, Missing, None] = missing
    urlstem: Union[str, Missing, None] = missing
    usefields: Union[bool, Missing, None] = missing
    useseparator: Union[bool, Missing, None] = missing
    useurl: Union[bool, Missing, None] = missing
    useurltoken: Union[bool, Missing, None] = missing
    ignoreMissing: Union[bool, Missing, None] = missing


@dataclass
class ImportDataInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    data: str
    separator: Union[str, Missing] = missing
    url: Union[str, Missing] = missing
    fields: Union[List[str], Missing] = missing


@dataclass
class ImportDataHtmlModel(GenericHtmlModel[ImportDataInputModel, ImportDataMarkupModel, ImportDataStateModel]):
    def get_component_html_name(self) -> str:
        return 'importdata-runner'

    def show_in_view_default(self) -> bool:
        return False

    def get_static_html(self) -> str:
        s = self.markup.beforeOpen or "+ Open Import"
        return render_static_import_data(self, s)

    def get_md(self):
        return ""


@dataclass
class ImportDataAnswerModel(GenericAnswerModel[ImportDataInputModel, ImportDataMarkupModel, ImportDataStateModel]):
    pass


def render_static_import_data(m: ImportDataHtmlModel, s: str):
    return render_template_string(
        f"""
<div class="ImportData">
 {s}
</div>
<br>
        """,
        **asdict(m.markup),
    )


ImportDataHtmlSchema = class_schema(ImportDataHtmlModel)
ImportDataAnswerSchema = class_schema(ImportDataAnswerModel)


importData_plugin = create_blueprint(__name__, 'importData', ImportDataHtmlSchema, csrf)

def conv_data_csv(data, field_names, separator):
    """
    Convert csv format "akankka;1,2,3" to TIM-format ["akankka;d1;1", "akankks;d2;2" ...]
    using field names.  If there is too less fields on data, only those
    are used.  If there is more columns in data thatn fields, omit extra columns
    :param data: data in csv-format to convert
    :param field_names: list of filednames to use for columns
    :param separator: separator to use to separate items
    :return: converted data in TIM-format
    """
    field_names = widen_fields(field_names)
    res = []
    for r in data:
        parts = r.split(separator)
        if len(parts) < 2:
            continue
        row = f"{parts[0]}"
        for i in range(1, len(parts)):
            if i-1 >= len(field_names):
                break
            name = field_names[i-1].strip()
            row += f"{separator}{name}{separator}{parts[i]}"
        res.append(row)
    return res


def conv_data_field_names(data, field_names, separator):
    """
    Convert field names on TIM-format akankka;demo;2 so that demo is changes if
    found from field_names
    :param data: data to convert
    :param field_names: lits off fileds ana aliases in format "demo=d1"
    :param separator: separator for items
    :return: converted data
    """
    field_names = widen_fields(field_names)
    fconv = {}
    res = []
    use_all = False
    for fn in field_names:
        pcs = fn.split("=")
        ffrom = pcs[0].strip()
        fto = ffrom
        if len(pcs) >= 2:
            fto = pcs[1].strip()
        if ffrom == '*':
            use_all = True
        else:
            fconv[ffrom] = fto
    for r in data:
        parts = r.split(separator)
        if len(parts) < 3:
            continue
        row = ""
        for i in range(1, len(parts)-1, 2):
            tname = parts[i]
            name = fconv.get(tname, None)
            if not name:
                if use_all:
                    name = tname
                else:
                    continue
            value = parts[i+1]
            row += f"{separator}{name}{separator}{value}"

        if row:
            res.append(f"{parts[0]}" + row)
    return res


def convert_data(data, field_names, separator):
    """
    If there is field_names, then convert data either by changing names (field_names has =)
    or csv data
    :param data: data to convert
    :param field_names: list of fieldnames or fieldnames and aliases
    :param separator: separator to use between items
    :return: converted data or data as it is
    """
    if not field_names or len(field_names) <= 0:
        return data
    f0 = field_names[0]
    if f0.find("=") > 0: # convert names
       return conv_data_field_names(data, field_names, separator)
    return conv_data_csv(data, field_names, separator)


@importData_plugin.route('/answer/', methods=['put'])
@csrf.exempt
@use_args(ImportDataAnswerSchema(), locations=("json",))
def answer(args: ImportDataAnswerModel):
    sdata = args.input.data
    defaultseparator = args.markup.separator or ";"
    separator = args.input.separator or defaultseparator
    fieldsReqExps = args.markup.fieldsReqExps
    userIdField = args.markup.userIdField or -1
    data = sdata.split("\n")
    output = ""
    field_names = args.input.fields
    """
    if separator != ',' and field_names:
        for i in range(0, len(field_names)):
            fn = field_names[i]
            fn = fn.replace(separator, ';')
    """
    data = convert_data(data, field_names, separator)
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
    if args.markup.docid:
        did = args.markup.docid
    rows = []
    wrong = 0
    wrongs = ""
    users = {}
    for r in data:
        if not r:
            continue
        parts = r.split(separator)
        u = None
        first_time = False
        error = ": unknown name"
        if len(parts) >= 3:
            uname = parts[0]
            u = users.get(uname, -1)
            if u == -1:
                u = User.get_by_name(uname)
                users[uname] = u
                first_time = True
        else:
            error = ": too few parts"
        if not u:
            if first_time:
                wrong += 1
                wrongs += "\n" + r + error
            continue
        uid = u.id
        ur = { 'user': uid, 'fields': {}}
        for i in range(1, len(parts)-1, 2):
            tname = parts[i]
            value = parts[i+1]
            if tname.find('.') < 0:
                tname = f"{did}.{tname}"
            ur['fields'][tname] = value
        rows.append(ur)

    if output:
        output = output + "\n"

    if wrong:
        wrongs = "\nWrong lines: " + str(wrong) + "\n" + wrongs
    jsonresp = { 'ignoreMissing': args.markup.ignoreMissing,
                 'savedata': rows,
                 'web' : { 'result': output + "Imported " + str(len(rows)) + wrongs} }

    save = {}

    if args.input.url != args.markup.url or \
            (args.state and args.state.url and args.state.url != args.input.url):
        save['url'] = args.input.url

    if separator != defaultseparator or \
            (args.state and args.state.separator and args.state.separator != separator):
        save['separator'] = separator
    if not field_names:
        field_names = []
    if not args.markup.fields:
        args.markup.fields = []
    if field_names != args.markup.fields:
        save['fields'] = field_names
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
                'text': 'Fields',
                'items': [
                    {
                        'text': 'Save/Import',
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
        "multimd": True,
        'editor_tabs': editor_tabs,
    },
    )
