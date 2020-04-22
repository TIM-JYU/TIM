"""
A plugin for importing data to TIM fields.
"""
import re
from collections import defaultdict
from dataclasses import dataclass, asdict, field
from enum import Enum
from typing import Union, List, DefaultDict, Dict

from flask import jsonify, render_template_string
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from marshmallow_dataclass import class_schema
from pluginserver_flask import GenericHtmlModel, \
    GenericAnswerModel, create_blueprint
from markupmodels import GenericMarkupModel
from utils import Missing
from timApp.plugin.jsrunner import jsrunner_run, JsRunnerParams, JsRunnerError
from timApp.tim_app import csrf
from timApp.user.hakaorganization import HakaOrganization
from timApp.user.personaluniquecode import PersonalUniqueCode
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
    loadButtonText: Union[str, Missing, None] = missing
    open: Union[bool, Missing, None] = missing
    placeholder: Union[str, Missing, None] = missing
    prefilter: Union[str, Missing, None] = missing
    separator: Union[str, Missing, None] = missing
    upload: Union[bool, Missing, None] = missing
    uploadstem: Union[str, Missing, None] = missing
    url: Union[str, Missing, None] = missing
    urlstem: Union[str, Missing, None] = missing
    usefields: Union[bool, Missing, None] = missing
    useseparator: Union[bool, Missing, None] = missing
    useurl: Union[bool, Missing, None] = missing
    useurltoken: Union[bool, Missing, None] = missing
    ignoreMissing: Union[bool, Missing, None] = missing
    joinProperty: Union[str, Missing, None] = missing


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


class LineErrorKind(Enum):
    TooFewParts = 0
    OddNumberOfFieldValuePairs = 1

    def __str__(self):
        if self == LineErrorKind.TooFewParts:
            return 'too few parts'
        else:
            return 'odd number of field-value pairs'


@dataclass
class FieldLineError:
    line: str
    kind: LineErrorKind

    def __str__(self):
        return f'{self.line}: {self.kind}'


@dataclass
class FieldValues:
    values: DefaultDict[str, Dict[str, str]] = field(default_factory=lambda: defaultdict(dict))
    error_lines: List[FieldLineError] = field(default_factory=list)

    def add_val(self, user_ident: str, field_name: str, value: str):
        self.values[user_ident][field_name] = value

    def to_tim_format(self, separator=';') -> List[str]:
        return [f'{u}{separator}{separator.join(f"{n}{separator}{v}" for n, v in vals.items())}' for u, vals in
                self.values.items()]

    @staticmethod
    def from_tim_format(data: List[str], separator=';'):
        v = FieldValues()
        for d in data:
            parts = d.split(separator)
            if len(parts) < 3:
                v.error_lines.append(FieldLineError(line=d, kind=LineErrorKind.TooFewParts))
            elif len(parts) % 2 == 0:
                v.error_lines.append(FieldLineError(line=d, kind=LineErrorKind.OddNumberOfFieldValuePairs))
            else:
                for name, value in zip(parts[1::2], parts[2::2]):
                    v.add_val(parts[0], name, value)
        return v

    def list_errors(self):
        for e in self.error_lines:
            yield str(e)


def conv_data_csv(data: List[str], field_names: List[str], separator: str) -> FieldValues:
    """
    Convert csv format "akankka;1,2,3" to TIM format ["akankka;d1;1", "akankka;d2;2" ...]
    using field names.  If there are too few fields on data, only those
    are used.  If there are more columns in data than fields, omit extra columns.
    :param data: data in CSV format to convert
    :param field_names: list of fieldnames to use for columns
    :param separator: separator to use to separate items
    :return: converted data in TIM format
    """
    field_names = widen_fields(field_names)
    res = FieldValues()
    for r in data:
        parts = r.split(separator)
        if len(parts) < 2:
            continue
        ident = parts[0]
        for i in range(1, len(parts)):
            if i - 1 >= len(field_names):
                break
            name = field_names[i - 1].strip()
            res.add_val(ident, name, parts[i])
    return res


def conv_data_field_names(data: List[str], field_names: List[str], separator: str) -> FieldValues:
    """
    Converts field names on TIM format akankka;demo;2 so that demo is changed if
    found from field_names.
    :param data: data to convert
    :param field_names: list of fields and aliases in format "demo=d1"
    :param separator: separator for items
    :return: converted data
    """
    field_names = widen_fields(field_names)
    fconv = {}
    res = FieldValues()
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
        for i in range(1, len(parts) - 1, 2):
            tname = parts[i]
            name = fconv.get(tname)
            if not name:
                if use_all:
                    name = tname
                else:
                    continue
            value = parts[i + 1]
            res.add_val(parts[0], name, value)
    return res


def convert_data(data: List[str], field_names: List[str], separator: str) -> FieldValues:
    """
    If there is field_names, then convert data either by changing names (field_names has =)
    or csv data
    :param data: data to convert
    :param field_names: list of fieldnames or fieldnames and aliases
    :param separator: separator to use between items
    :return: converted data or data as it is
    """
    if not field_names:
        return FieldValues.from_tim_format(data)
    f0 = field_names[0]
    if f0.find("=") > 0:  # convert names
        return conv_data_field_names(data, field_names, separator)
    return conv_data_csv(data, field_names, separator)


@importData_plugin.route('/answer', methods=['put'])
@csrf.exempt
@use_args(ImportDataAnswerSchema(), locations=("json",))
def answer(args: ImportDataAnswerModel):
    sdata = args.input.data
    defaultseparator = args.markup.separator or ";"
    separator = args.input.separator or defaultseparator
    data = sdata.splitlines()
    output = ""
    field_names = args.input.fields
    vals = convert_data(data, field_names, separator)
    if field_names:
        data = vals.to_tim_format()
    if args.markup.prefilter:
        params = JsRunnerParams(code=args.markup.prefilter, data=data)
        try:
            data, output = jsrunner_run(params)
        except JsRunnerError as e:
            return args.make_answer_error('Error in JavaScript: ' + e.args[0])
        vals = FieldValues.from_tim_format(data)
    did = int(args.taskID.split(".")[0])
    if args.markup.docid:
        did = args.markup.docid
    rows = []
    wrong_count = 0
    wrongs = []
    field_count = 0
    id_prop = args.markup.joinProperty or 'username'
    idents = [r for r in vals.values.keys()]

    m = re.fullmatch(r'studentID\((?P<org>[a-z.]+)\)', id_prop)
    users: Dict[str, User]
    if m:
        org = m.group('org')
        q = (User.query.join(PersonalUniqueCode)
             .filter(PersonalUniqueCode.code.in_(idents))
             .join(HakaOrganization)
             .filter_by(name=org)
             .with_entities(PersonalUniqueCode.code, User))
        users = {c: u for c, u in q.all()}
    elif id_prop == 'username':
        q = User.query.filter(User.name.in_(idents))
        users = {u.name: u for u in q.all()}
    elif id_prop == 'id':
        try:
            q = User.query.filter(User.id.in_([int(i) for i in idents]))
        except ValueError as e:
            return args.make_answer_error(f'User ids must be ints ({e})')
        users = {str(u.id): u for u in q.all()}
    elif id_prop == 'email':
        q = User.query.filter(User.email.in_(idents))
        users = {u.email: u for u in q.all()}
    else:
        return args.make_answer_error(f'Invalid joinProperty: {args.markup.joinProperty}')

    for uident, vs in vals.values.items():
        u = users.get(uident)
        if not u:
            wrong_count += len(vs)
            wrongs.append(f'{uident}: user not found')
            continue
        flds = {}
        ur = {'user': u.id, 'fields': flds}
        for k, v in vs.items():
            if '.' not in k:
                k = f'{did}.{k}'
            flds[k] = v
        rows.append(ur)
        field_count += len(flds)

    if output:
        output = output + "\n"
    wrong_count += len(vals.error_lines)
    wrongs += list(vals.list_errors())
    if wrong_count:
        wrongstr = "\nWrong lines: " + str(wrong_count) + "\n\n" + '\n'.join(wrongs)
    else:
        wrongstr = ''
    jsonresp = {
        'ignoreMissing': args.markup.ignoreMissing,
        'savedata': rows,
        'web': {'result': f"{output}Imported {field_count} values for {len(rows)} users{wrongstr}"},
    }

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
    return jsonify(jsonresp)


@importData_plugin.route('/reqs')
def reqs():
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
    return jsonify({
        "js": ["importData"],
        "multihtml": True,
        "multimd": True,
        'editor_tabs': editor_tabs,
    },
    )
