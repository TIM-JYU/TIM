"""
A plugin for importing data to TIM fields.
"""
import re
from collections import defaultdict
from dataclasses import dataclass, asdict, field
from enum import Enum
from typing import DefaultDict, Generator, Any

import requests
from flask import render_template_string
from marshmallow.utils import missing
from sqlalchemy import select

from timApp.plugin.jsrunner.jsrunner import jsrunner_run, JsRunnerParams, JsRunnerError
from timApp.tim_app import csrf
from timApp.timdb.sqa import run_sql
from timApp.user.hakaorganization import HakaOrganization
from timApp.user.personaluniquecode import PersonalUniqueCode, SchacPersonalUniqueCode
from timApp.user.user import User, UserInfo
from timApp.util.utils import widen_fields
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    GenericAnswerModel,
    create_blueprint,
    value_or_default,
    PluginAnswerResp,
    PluginReqs,
    EditorTab,
)
from tim_common.utils import Missing


@dataclass
class ImportDataStateModel:
    """Model for the information that is stored in TIM database for each answer."""

    url: str | Missing | None = missing
    separator: str | Missing | None = missing
    fields: list[str] | Missing = missing


@dataclass
class AplusData:
    course: int


@dataclass
class ImportDataMarkupModel(GenericMarkupModel):
    allowMissing: bool | Missing | None = missing
    beforeOpen: str | Missing | None = missing
    docid: int | Missing | None = missing
    fields: list[str] | Missing = missing
    loadButtonText: str | Missing | None = missing
    open: bool | Missing | None = missing
    placeholder: str | Missing | None = missing
    prefilter: str | Missing | None = missing
    separator: str | Missing | None = missing
    upload: bool | Missing | None = missing
    uploadstem: str | Missing | None = missing
    url: str | Missing | None = missing
    urlstem: str | Missing | None = missing
    usefields: bool | Missing | None = missing
    useseparator: bool | Missing | None = missing
    useurl: bool | Missing | None = missing
    useurltoken: bool | Missing | None = missing
    ignoreMissing: bool | Missing | None = missing
    joinProperty: str | Missing | None = missing
    createMissingUsers: bool | Missing = missing
    addUsersToGroup: str | Missing = missing
    nextRunner: str | Missing = missing

    aplus: AplusData | Missing = missing


@dataclass
class ImportDataInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""

    data: str | None = None
    token: str | None = None
    createMissingUsers: bool | Missing = missing
    separator: str | Missing = missing
    url: str | Missing = missing
    fields: list[str] | Missing = missing


@dataclass
class ImportDataHtmlModel(
    GenericHtmlModel[ImportDataInputModel, ImportDataMarkupModel, ImportDataStateModel]
):
    def get_component_html_name(self) -> str:
        return "importdata-runner"

    def show_in_view_default(self) -> bool:
        return False

    def get_static_html(self) -> str:
        s = self.markup.beforeOpen
        if not isinstance(s, str):
            s = "Open import"
        return render_static_import_data(self, s)

    def get_md(self) -> str:
        return ""


@dataclass
class ImportDataAnswerModel(
    GenericAnswerModel[
        ImportDataInputModel, ImportDataMarkupModel, ImportDataStateModel
    ]
):
    pass


def render_static_import_data(m: ImportDataHtmlModel, s: str) -> str:
    return render_template_string(
        f"""
<div class="ImportData">
 {s}
</div>
<br>
        """,
        **asdict(m.markup),
    )


class LineErrorKind(Enum):
    TooFewParts = 0
    OddNumberOfFieldValuePairs = 1

    def __str__(self) -> str:
        if self == LineErrorKind.TooFewParts:
            return "too few parts"
        else:
            return "odd number of field-value pairs"


@dataclass
class FieldLineError:
    line: str
    kind: LineErrorKind

    def __str__(self) -> str:
        return f"{self.line}: {self.kind}"


@dataclass
class FieldValues:
    values: DefaultDict[str, dict[str, str]] = field(
        default_factory=lambda: defaultdict(dict)
    )
    error_lines: list[FieldLineError] = field(default_factory=list)

    def add_val(self, user_ident: str, field_name: str, value: str) -> None:
        self.values[user_ident][field_name] = value

    def to_tim_format(self, separator: str = ";") -> list[str]:
        return [
            f'{u}{separator}{separator.join(f"{n}{separator}{v}" for n, v in vals.items())}'
            for u, vals in self.values.items()
        ]

    @staticmethod
    def from_tim_format(data: list[str], separator: str = ";") -> "FieldValues":
        v = FieldValues()
        for d in data:
            parts = d.split(separator)
            if len(parts) < 3:
                v.error_lines.append(
                    FieldLineError(line=d, kind=LineErrorKind.TooFewParts)
                )
            elif len(parts) % 2 == 0:
                v.error_lines.append(
                    FieldLineError(
                        line=d, kind=LineErrorKind.OddNumberOfFieldValuePairs
                    )
                )
            else:
                for name, value in zip(parts[1::2], parts[2::2]):
                    v.add_val(parts[0], name, value)
        return v

    def list_errors(self) -> Generator[str, None, None]:
        for e in self.error_lines:
            yield str(e)


def conv_data_csv(
    data: list[str], field_names: list[str], separator: str
) -> FieldValues:
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


def conv_data_field_names(
    data: list[str], field_names: list[str], separator: str
) -> FieldValues:
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
        if ffrom == "*":
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


def convert_data(
    data: list[str], field_names: list[str], separator: str
) -> FieldValues:
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


@dataclass
class MissingUser:
    user: UserInfo
    fields: dict[str, str]


class ImportDataAnswerResp(PluginAnswerResp, total=False):
    ignoreMissing: bool | Missing | None
    allowMissing: bool | Missing | None
    savedata: list[dict[str, Any]]
    groups: dict[str, dict[str, list[str]]] | None
    createMissingUsers: bool
    missingUsers: list[MissingUser]


def answer(args: ImportDataAnswerModel) -> PluginAnswerResp:
    sdata = args.input.data
    aalto_data = None
    if sdata is None:
        if isinstance(args.markup.aplus, Missing):
            return args.make_answer_error("input data missing")
        if not args.input.token:
            return args.make_answer_error("token missing")
        headers = {"Authorization": f"Token {args.input.token}"}
        try:
            r = requests.get(
                f"https://plus.cs.aalto.fi/api/v2/courses/{args.markup.aplus.course}/aggregatedata/",
                params={"format": "json"},
                headers=headers,
            )
            if r.status_code != 200:
                return args.make_answer_error(
                    f"unexpected status from A+: {r.status_code}"
                )
            aalto_data = r.json()
        except requests.exceptions.ConnectionError:
            return args.make_answer_error("error connecting to A+")
        except requests.exceptions.Timeout:
            return args.make_answer_error("timeout connecting to A+")
    elif args.markup.aplus:
        return args.make_answer_error("cannot send data from browser if aplus is given")
    output = ""
    defaultseparator = value_or_default(args.markup.separator, ";")
    separator = value_or_default(args.input.separator, defaultseparator)
    field_names = args.input.fields
    if sdata is not None:
        data = sdata.splitlines()
        vals = convert_data(data, value_or_default(field_names, []), separator)
        if field_names:
            data = vals.to_tim_format()
        if isinstance(args.markup.prefilter, str):
            params = JsRunnerParams(code=args.markup.prefilter, data=data)
            try:
                processed_data, output = jsrunner_run(params)
            except JsRunnerError as e:
                return args.make_answer_error("Error in JavaScript: " + e.args[0])
            vals = FieldValues.from_tim_format(processed_data)
    elif aalto_data:
        field_name_re = re.compile(r"(?P<num>\d+) (?P<type>Total|Ratio|Count)")
        fvals: DefaultDict[str, dict[str, str]] = defaultdict(dict)
        for d in aalto_data:
            user_vals = {}
            for k, v in d.items():
                m = field_name_re.match(k)
                if m and v != 0:
                    user_vals[m.group("type").lower() + m.group("num")] = str(v)
            fvals[d["StudentID"]] = user_vals
        vals = FieldValues(values=fvals)
    else:
        return args.make_answer_error(
            "sdata and aalto_data were both None, which should not happen"
        )
    did = int(args.taskID.split(".")[0])
    if isinstance(args.markup.docid, int):
        did = args.markup.docid
    rows = []
    id_prop = value_or_default(
        args.markup.joinProperty,
        "studentID(aalto.fi)" if args.markup.aplus else "username",
    )
    idents = [r for r in vals.values.keys()]

    m = re.fullmatch(r"studentID\((?P<org>[a-z.]+)\)", id_prop)
    users: dict[str, User]
    org = None
    if m:
        org = m.group("org")
        stmt = (
            select(User)
            .join(PersonalUniqueCode)
            .filter(PersonalUniqueCode.code.in_(idents))
            .join(HakaOrganization)
            .filter_by(name=org)
            .with_only_columns(PersonalUniqueCode.code, User)
        )
        users = {c: u for c, u in run_sql(stmt)}
    elif id_prop == "username":
        username_stmt = select(User).filter(User.name.in_(idents))
        users = {u.name: u for u in run_sql(username_stmt).scalars()}
    elif id_prop == "id":
        try:
            id_stmt = select(User).filter(User.id.in_([int(i) for i in idents]))
        except ValueError as e:
            return args.make_answer_error(f"User ids must be ints ({e})")
        users = {str(u.id): u for u in run_sql(id_stmt).scalars()}
    elif id_prop == "email":
        email_stmt = select(User).filter(User.email.in_(idents))
        users = {u.email: u for u in run_sql(email_stmt).scalars()}
    else:
        return args.make_answer_error(
            f"Invalid joinProperty: {args.markup.joinProperty}"
        )

    missing_users = {}
    for uident, vs in vals.values.items():
        u = users.get(uident)
        flds = {}
        for k, v in vs.items():
            if "." not in k:
                k = f"{did}.{k}"
            flds[k] = v
        if u:
            ur = {"user": u.id, "fields": flds}
            rows.append(ur)
        else:
            missing_users[uident] = flds

    wrong_count = len(vals.error_lines)
    wrongs = list(vals.list_errors())
    if wrong_count:
        if output:
            output += "\n\n"
        output += "Wrong lines: " + str(wrong_count) + "\n\n" + "\n".join(wrongs)

    mu = []
    if org:
        if aalto_data:
            student_id_email_map = {
                row["StudentID"]: row["Email"] for row in aalto_data
            }
            mu = [
                MissingUser(
                    user=UserInfo(
                        unique_codes=[
                            SchacPersonalUniqueCode(
                                code=sid, codetype="studentID", org=org
                            )
                        ],
                        email=student_id_email_map[sid],
                    ),
                    fields=fields,
                )
                for sid, fields in missing_users.items()
            ]
        elif missing_users:
            mu = [
                MissingUser(
                    user=UserInfo(
                        unique_codes=[
                            SchacPersonalUniqueCode(
                                code=sid, codetype="studentID", org=org
                            )
                        ],
                        username=f"imported_studentid_{sid}",
                    ),
                    fields=fields,
                )
                for sid, fields in missing_users.items()
            ]
    elif id_prop == "username":
        mu = [
            MissingUser(
                user=UserInfo(
                    username=sid,
                ),
                fields=fields,
            )
            for sid, fields in missing_users.items()
        ]
    elif id_prop == "email":
        mu = [
            MissingUser(
                user=UserInfo(
                    email=sid,
                ),
                fields=fields,
            )
            for sid, fields in missing_users.items()
        ]
    elif missing_users:
        return args.make_answer_error(
            'missingUsers not implemented when joinProperty is "id"'
        )
    if isinstance(args.input.createMissingUsers, bool):
        create_missing = args.input.createMissingUsers
    else:
        create_missing = args.markup.aplus is not missing
    jsonresp: ImportDataAnswerResp = {
        "ignoreMissing": args.markup.ignoreMissing,
        "allowMissing": args.markup.allowMissing,
        "savedata": rows,
        "createMissingUsers": create_missing,
        "missingUsers": mu,
        "web": {
            "result": f"{output}",
        },
    }

    save: dict[str, Any] = {}

    if args.input.url != args.markup.url or (
        args.state and args.state.url and args.state.url != args.input.url
    ):
        save["url"] = args.input.url

    if separator != defaultseparator or (
        args.state and args.state.separator and args.state.separator != separator
    ):
        save["separator"] = separator
    if not field_names:
        field_names = []
    if not args.markup.fields:
        args.markup.fields = []
    if field_names != args.markup.fields:
        save["fields"] = field_names
    if save:
        jsonresp["save"] = save
    return jsonresp


def reqs() -> PluginReqs:
    templates = [
        """
``` {#ImportData plugin="importData"}
buttonText: Import
```"""
    ]
    editor_tabs: list[EditorTab] = [
        {
            "text": "Fields",
            "items": [
                {
                    "text": "Save/Import",
                    "items": [
                        {
                            "data": templates[0].strip(),
                            "text": "Import data",
                            "expl": "Import data from text",
                        },
                    ],
                },
            ],
        },
    ]
    return {
        "js": ["importData"],
        "multihtml": True,
        "multimd": True,
        "editor_tabs": editor_tabs,
    }


importData_plugin = create_blueprint(
    __name__,
    "importData",
    ImportDataHtmlModel,
    ImportDataAnswerModel,
    answer,
    reqs,
    csrf,
)
MissingUserSchema = class_schema(MissingUser)
