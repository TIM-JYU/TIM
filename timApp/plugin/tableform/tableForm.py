"""
TIM example plugin: a tableFormndrome checker.
"""
import datetime
import io, json, re
from dataclasses import dataclass, asdict, field
from typing import Any, TypedDict, Sequence, Tuple
from zipfile import ZipFile, ZIP_DEFLATED

from flask import render_template_string, Response, send_file
from flask_babel import gettext
from marshmallow.utils import missing
from openpyxl import Workbook
from openpyxl.writer.excel import ExcelWriter
from sqlalchemy import select
from sqlalchemy.orm import selectinload
from webargs.flaskparser import use_args

from timApp.auth.accesshelper import get_doc_or_abort, AccessDenied
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docinfo import DocInfo
from timApp.document.hide_names import is_hide_names
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewRoute, ViewContext
from timApp.item.block import Block
from timApp.item.tag import Tag, TagType, GROUP_TAG_PREFIX
from timApp.plugin.jsrunner.jsrunner import jsrunner_run, JsRunnerParams, JsRunnerError
from timApp.plugin.plugin import (
    find_plugin_from_document,
    TaskNotFoundException,
    Plugin,
)
from timApp.plugin.tableform.comparatorFilter import RegexOrComparator
from timApp.plugin.taskid import TaskId
from timApp.sisu.parse_display_name import parse_sisu_group_display_name
from timApp.sisu.scimusergroup import ScimUserGroup
from timApp.sisu.sisu import get_potential_groups
from timApp.tim_app import csrf
from timApp.timdb.sqa import run_sql
from timApp.user.user import User, get_membership_end, get_membership_added
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import (
    RouteException,
    use_model,
    view_ctx_with_urlmacros,
    NotExist,
)
from timApp.util.flask.responsehelper import (
    csv_string,
    json_response,
    text_response,
)
from timApp.util.get_fields import (
    get_fields_and_users,
    MembershipFilter,
    UserFields,
    RequestedGroups,
    GetFieldsAccess,
)
from timApp.util.utils import fin_timezone
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    GenericAnswerModel,
    create_blueprint,
    value_or_default,
    PluginAnswerResp,
    PluginAnswerWeb,
    PluginReqs,
    EditorTab,
)
from tim_common.timjsonencoder import TimJsonEncoder
from tim_common.utils import Missing


@dataclass
class TableFormStateModel:
    """Model for the information that is stored in TIM database for each answer."""

    # TODO: Save user given table layouts like in timTable


@dataclass
class DataViewVirtualScrollingModel:
    enabled: bool | None = True
    verticalOverflow: int | Missing = missing
    horizontalOverflow: int | Missing = missing


@dataclass
class DataViewSettingsModel:
    virtual: DataViewVirtualScrollingModel | Missing | None = missing
    rowHeight: int | Missing = missing
    columnWidths: dict[str, int] | Missing = missing
    tableWidth: str | Missing = missing
    fixedColumns: int | Missing = missing


@dataclass
class RunScriptModel:
    script: str | None = None
    button: str | None = None
    all: bool | None = None
    update: bool | None = None
    onMemberAdd: bool | None = None
    interval: int | None = None


@dataclass
class TableFormMarkupModel(GenericMarkupModel):
    anonNames: bool | Missing = missing
    autosave: bool | Missing = missing
    autoUpdateFields: bool | Missing = True
    autoUpdateTables: bool | Missing = True
    cbColumn: bool | Missing | None = missing
    dataCollection: str | Missing | None = missing
    emails: bool | Missing = missing
    addedDates: bool | Missing = missing
    emailUsersButtonText: str | Missing | None = missing
    filterRow: int | bool | Missing | None = missing
    filters: list[dict[str | int, str | int]] | Missing = missing
    pasteTableChars: dict[str, list[str]] | Missing = missing
    fixedColor: str | Missing | None = missing
    fontSize: str | Missing | None = missing
    forceUpdateButtonText: str | Missing | None = missing
    groups: list[str] | Missing = missing
    hiddenColumns: list[int] | Missing | None = missing
    hiddenRows: list[int] | Missing | None = missing
    hide: dict[Any, Any] | Missing | None = missing
    hideButtonText: str | Missing | None = missing
    includeUsers: MembershipFilter = field(
        default=MembershipFilter.Current, metadata={"by_value": True}
    )
    locked: bool | Missing | None = missing
    lockedFields: list[str] | Missing = missing
    maxCols: str | Missing | None = missing
    maxRows: str | Missing | None = missing
    maxWidth: str | Missing = missing
    minWidth: str | Missing | None = missing
    nrColumn: bool | Missing | None = missing
    charRow: bool | Missing | None = missing
    open: bool | Missing = True
    openButtonText: str | Missing | None = missing
    realnames: bool | Missing = missing
    removeDocIds: bool | Missing = True
    removeUsersButtonText: str | Missing | None = missing
    report: bool | Missing = missing
    reportButton: str | Missing | None = missing
    reportFilter: str | Missing | None = missing
    downloadAsExcelFile: str | Missing | None = missing
    runScripts: list[str | RunScriptModel] | Missing = missing
    saveStyles: bool | Missing = True
    separator: str | Missing | None = missing
    showToolbar: bool | Missing | None = missing
    singleLine: bool | Missing | None = missing
    sisugroups: str | Missing = missing
    sortBy: str | Missing | None = missing
    table: bool | Missing = missing
    toolbarTemplates: list[dict[Any, Any]] | Missing = missing
    userListButtonText: str | Missing | None = missing
    usernames: bool | Missing = missing
    dataView: DataViewSettingsModel | Missing | None = missing
    replyToEmail: str | Missing | None = missing
    addUsersButton: str | Missing | None = missing
    notifyOnAdd: bool | Missing = False
    createMissingUsers: bool | Missing = False


TableFormMarkupSchema = class_schema(TableFormMarkupModel)


@dataclass
class TableFormInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""

    replyRows: dict[int, Any]
    nosave: bool | Missing = missing


def get_sisu_group_desc_for_table(g: UserGroup) -> str:
    p = parse_sisu_group_display_name(g.display_name)
    assert p is not None
    assert g.external_id is not None
    if g.external_id.is_studysubgroup:
        return p.desc
    # We want the most important groups to be at the top of the table.
    # The '(' at the beginning changes the sort order.
    return f"({p.desc})"


def get_sisugroups(user: User, sisu_id: str | None) -> "TableFormObj":
    gs = get_potential_groups(user, sisu_id)
    docs_with_course_tag = select(Tag.block_id).filter_by(type=TagType.CourseCode)
    tags = (
        run_sql(
            select(Tag)
            .filter(
                Tag.name.in_([GROUP_TAG_PREFIX + g.name for g in gs])
                & Tag.block_id.in_(docs_with_course_tag)
            )
            .options(selectinload(Tag.block).selectinload(Block.docentries))
        )
        .scalars()
        .all()
    )

    tag_map = {t.name[len(GROUP_TAG_PREFIX) :]: t for t in tags}

    def get_course_page(ug: UserGroup) -> str | None:
        t: Tag | None = tag_map.get(ug.name)
        if t:
            return f'<a href="{t.block.docentries[0].url_relative}">URL</a>'
        else:
            return None

    def get_ext_id(g: UserGroup) -> ScimUserGroup:
        assert g.external_id is not None
        return g.external_id

    def get_display_name(g: UserGroup) -> str:
        assert g.display_name is not None
        return g.display_name

    return TableFormObj(
        rows={
            get_ext_id(g).external_id: {
                "TIM-nimi": g.name,
                "URL": f'<a href="{g.admin_doc.docentries[0].url_relative}">URL</a>'
                if g.admin_doc
                else None,
                "Jäseniä": len(g.current_memberships),
                "Kurssisivu": get_course_page(g),
            }
            for g in gs
        },
        users={
            get_ext_id(g).external_id: TableFormUserInfo(
                real_name=get_sisu_group_desc_for_table(g)
                if sisu_id
                else get_display_name(g),
                # The rows are not supposed to match any real user when handling sisu groups,
                # so we try to use an id value that does not match anyone.
                id=-100000,
                email="",
            )
            for g in gs
        },
        fields=["Jäseniä", "TIM-nimi", "URL", "Kurssisivu"],
        aliases={
            "TIM-nimi": "TIM-nimi",
            "URL": "URL",
            "Jäseniä": "Jäseniä",
            "Kurssisivu": "Kurssisivu",
        },
        styles={get_ext_id(g).external_id: {} for g in gs},
        membership_add={},
        membership_end={},
    )


@dataclass
class TableFormHtmlModel(
    GenericHtmlModel[TableFormInputModel, TableFormMarkupModel, TableFormStateModel]
):
    def get_component_html_name(self) -> str:
        return "tableform-runner"

    def show_in_view_default(self) -> bool:
        return False

    def get_json_encoder(self) -> type[TimJsonEncoder]:
        return TimJsonEncoder

    def get_static_html(self) -> str:
        return render_static_table_form(self)

    def get_browser_json(self) -> dict:
        r = super().get_browser_json()
        if self.markup.open and self.markup.table is not False:
            doc_id = TaskId.parse_doc_id(self.taskID)
            d = get_doc_or_abort(doc_id)
            user = User.get_by_name(self.current_user_id)
            assert user is not None
            if isinstance(self.markup.sisugroups, str):
                f = get_sisugroups(user, self.markup.sisugroups)
            else:
                f = tableform_get_fields(
                    value_or_default(self.markup.fields, []),
                    value_or_default(self.markup.groups, []),
                    d,
                    user,
                    ViewContext(
                        ViewRoute.View if self.viewmode else ViewRoute.Teacher,
                        self.preview,
                    ),
                    value_or_default(self.markup.removeDocIds, True),
                    value_or_default(self.markup.showInView, False),
                    group_filter_type=self.markup.includeUsers,
                    anonymize_names=value_or_default(self.markup.anonNames, False)
                    or value_or_default(self.hide_names, False),
                )
            r = {**r, **f}
        return r


@dataclass
class TableFormAnswerModel(
    GenericAnswerModel[TableFormInputModel, TableFormMarkupModel, TableFormStateModel]
):
    pass


def render_static_table_form(m: TableFormHtmlModel) -> str:
    return render_template_string(
        """
<div class="tableform">
<button class="timButton">
    Avaa Taulukko/Raporttinäkymä
</button>
</div>
<br>
        """,
        **asdict(m.markup),
    )


@dataclass
class GenerateSpreadSheetModel:
    docId: int
    fields: list[str]
    groups: list[str]
    separator: str
    userFilter: list[str] = field(default_factory=list)
    usernames: bool | Missing = True
    realnames: bool | Missing = missing
    removeDocIds: bool | Missing = missing
    emails: bool | Missing = missing
    reportFilter: str | Missing = missing
    filterFields: list[str] = field(default_factory=list)
    filterValues: list[str] = field(default_factory=list)
    anonNames: bool = False
    downloadAsExcelFile: str | Missing = missing


GenerateSpreadSheetSchema = class_schema(GenerateSpreadSheetModel)


class TableformAnswerResp(PluginAnswerResp):
    savedata: list[dict[str, Any]]


def answer(args: TableFormAnswerModel) -> PluginAnswerResp:
    if args.markup.locked:
        raise RouteException("This table is not editable.")
    rows = args.input.replyRows
    save_rows = []
    for uid, r in rows.items():
        save_rows.append({"user": uid, "fields": r})
    web: PluginAnswerWeb = {}
    result: TableformAnswerResp = {"web": web, "savedata": save_rows}
    web["result"] = "saved"
    return result


def reqs() -> PluginReqs:
    templates = [
        """
``` {#tableForm_table plugin="tableForm"}    
# showInView: true # Add attribute  to show the plugin in normal view
groups: 
 # - "*"          # show all users who have some value on any of fields
 - Group Name     # Use Group Name here
fields:
 - d1=demo1       # List your fields here, = for alias
table: true
report: true
openButtonText: Avaa taulukko # text for open the table if closed as default
hideButtonText: Sulje taulukko # tex for closing the table
open: true        # use false if table is big and you do not want it open automatically
autosave: true    # save fields automatically
maxRows: 40em     # max height for the table before scrollbar 
realnames: true   # Show full name in 2nd column, true or false
usernames: false  # Show user name column
emails: false     # Show email column
addedDates: false # Show the date the user was added
#buttonText: Tallenna    # Name your save button here
cbColumn: true    # show checkboxes
nrColumn: true    # show numbers
# maxRows: 40em   # Hiw long is the table
# maxCols: fit-content # width of the table
# maxWidth: 30em  # max for column
filterRow: true   # show filters 
singleLine: true  # show every line as a single line
emailUsersButtonText: "Lähetä sähköpostia valituille" # if one wants to send email 
separator: ";"    # Define your value separator here, ";" as default
anonNames: false  # Whether to show anonymised names, true or false
reportButton: "Raportti"
userListButtonText: "Käyttäjälista"
showToolbar: true # toolbar for editing the table
# hiddenColumns: [0,1] # which columns are hidden

# forceUpdateButtonText: "Virkistä" # button for refreshing the table
#dataView:        # uncomment this if table is big or want to use special properties
#  tableWidth: 90vw
#  virtual:
#    enabled: true  # toggles virtual mode on or off; default true
#  fixedColumns: 1 # how many not scrolling columns in left
```"""
    ]
    editor_tabs: list[EditorTab] = [
        {
            "text": "Fields",
            "items": [
                {
                    "text": "Tables",
                    "items": [
                        {
                            "data": templates[0].strip(),
                            "text": "Table and report",
                            "expl": "Form a table for editing forms, that can be converted to report",
                        },
                    ],
                },
            ],
        },
    ]
    return {
        "js": ["tableForm"],
        "multihtml": True,
        "editor_tabs": editor_tabs,
    }


tableForm_plugin = create_blueprint(
    __name__,
    "tableForm",
    TableFormHtmlModel,
    TableFormAnswerModel,
    answer,
    reqs,
    csrf,
)


def check_field_filtering(
    r_filter: RegexOrComparator | None, target: str | float | None
) -> bool:
    if r_filter is None:
        return True
    return r_filter.is_match(target)


@tableForm_plugin.get("/generateCSV")
@use_args(GenerateSpreadSheetSchema())
def gen_csv_legacy(args: GenerateSpreadSheetModel) -> Response | str:
    """Legacy route for documents that have direct links to the route."""
    return gen_spreadsheet_impl(args)


@tableForm_plugin.get("/generateReport")
@use_args(GenerateSpreadSheetSchema())
def gen_spreadsheet(args: GenerateSpreadSheetModel) -> Response | str:
    """
    Generates a report defined by tableForm attributes
    # TODO: generic, move
    :return: SpreadSheet in CSV or .xlsx format containing header row and rows for users and values
    """
    return gen_spreadsheet_impl(args)


def gen_spreadsheet_impl(args: GenerateSpreadSheetModel) -> Response | str:
    curr_user = get_current_user_object()
    (
        docid,
        groups,
        separator,
        show_real_names,
        show_user_names,
        show_emails,
        remove_doc_ids,
        fields,
        user_filter,
        filter_fields,
        filter_values,
    ) = (
        args.docId,
        args.groups,
        args.separator,
        args.realnames,
        args.usernames,
        args.emails,
        args.removeDocIds,
        args.fields,
        args.userFilter,
        args.filterFields,
        args.filterValues,
    )

    if len(separator) > 1:
        # TODO: Add support >1 char strings like in Korppi
        return "Only 1-character string separators supported for now"

    doc = get_doc_or_abort(docid)
    if not isinstance(remove_doc_ids, bool):
        remove_doc_ids = True
    view_ctx = view_ctx_with_urlmacros(ViewRoute.Unknown)
    r = tableform_get_fields(
        fields,
        groups,
        doc,
        curr_user,
        view_ctx,
        remove_doc_ids,
        allow_non_teacher=True,
        user_filter=user_filter,
        anonymize_names=args.anonNames,
        # TODO: group_filter_type=self.markup.includeUsers,
    )
    data: list[list[str | float | None]] = [[]]
    if show_real_names:
        data[0].append(gettext("Real name"))
    if show_user_names:
        data[0].append(gettext("Username"))
    if show_emails:
        data[0].append(gettext("Email"))
    tmp: Sequence[str | float | None] = r["fields"]
    data[0] = data[0] + list(tmp)
    if len(filter_fields) != len(filter_values):
        raise RouteException("Filter targets and filter values do not match")
    # TODO: Check if filters could be easily integrated to original query in get_fields_and_users
    regs = {}
    # TODO: Create ComparatorFilters
    try:
        for i, f in enumerate(filter_fields):
            reg = RegexOrComparator(filter_values[i])
            if reg:
                regs[f] = reg
    except IndexError:
        raise RouteException("Too many filters")

    for rowkey, row in sorted(r["rows"].items()):
        row_data: list[str | float | None] = []
        u = r["users"].get(rowkey)
        if show_real_names:
            if u is None:
                continue
            val = u.get("real_name")
            row_data.append(val)
            if not check_field_filtering(regs.get("realname"), val):
                continue
        if show_user_names:
            val = rowkey
            row_data.append(val)
            if not check_field_filtering(regs.get("username"), val):
                continue
        if show_emails:
            if u is None:
                continue
            val = u.get("email")
            row_data.append(val)
            if not check_field_filtering(regs.get("email"), val):
                continue
        filter_this_row = False
        for i, _field in enumerate(r["fields"]):
            v = row.get(_field)
            row_data.append(v)
            if not check_field_filtering(regs.get(_field), v):
                filter_this_row = True
                break
        if filter_this_row:
            continue
        data.append(row_data)

    csv = csv_string(data, "excel", separator)
    csv, output = filter_csv_report(args.reportFilter, csv)

    if args.downloadAsExcelFile and isinstance(args.downloadAsExcelFile, str):
        file_name = args.downloadAsExcelFile
        file_ext = file_name.split(".")[-1]
        match file_ext:
            case "xlsx":
                num_re = re.compile(r"(^[\d,.\- ]+)")
                wb = Workbook()
                # ws = wb.active
                # grab the worksheet directly since wb.active seems to have some typing issues
                ws = wb.worksheets[0]

                # Only rudimentary value conversions are supported for now (see parse_row(rd, regex_filter))
                # TODO: implement configurable filtering/type-casting via JSRunner, like with CSVs
                for rowd in data:
                    rd = rowd.copy()
                    parse_row(rd, num_re)
                    ws.append(rd)

                fileio = save_workbook_to_memory(wb)
                return create_and_send_report(
                    file_name,
                    fileio,
                    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                )
            case "csv":
                output_content = output + csv
                return create_and_send_report(file_name, output_content, "text/csv")
            case _:
                return text_response(csv)

    return text_response(csv)

    # """
    #     # This did not work because if code is just return data; then it is not identical when returned
    #     if args.reportFilter:
    #         params = {'code': args.reportFilter, 'data': data}
    #         data, output = jsrunner_run(params)
    #     return csv_response(data, 'excel', separator)
    # """


def save_workbook_to_memory(wb: Workbook) -> io.BytesIO:
    """Saves a Workbook to memory instead of on disk.
    This is a convenience function so that we do not need to create temporary files on disk when exporting spreadsheets.
    """
    if wb.write_only and not wb.worksheets:
        wb.create_sheet()

    wb.properties.modified = datetime.datetime.now(tz=datetime.timezone.utc).replace(
        tzinfo=None
    )
    buffer = io.BytesIO()
    with ZipFile(buffer, "w", ZIP_DEFLATED, allowZip64=True) as archive:
        writer = ExcelWriter(wb, archive)
        writer.save()

    buffer.seek(0)
    return buffer


def parse_row(rd: list[str | float | None], regex_filter: re.Pattern) -> None:
    for i in range(len(rd)):
        m = regex_filter.match(str(rd[i])) if rd[i] is not None else None
        if m:
            val = m.group(0)
            val = val.replace(" ", "")
            # Determine whether the value should be negative. Only the presence of a minus sign matters,
            # an even number of minus signs preceding the number does not indicate a positive value.
            negative = "-" in val
            val = val.replace("-", "")

            if len(val) == 0:
                rd[i] = None
                continue
            dot, comma = val.find("."), val.find(",")
            decimal_sep = (
                "dot"
                if ((-1 < dot < comma) or (comma == -1 < dot))
                else "comma"
                if ((-1 < comma < dot) or (dot == -1 < comma))
                else None
            )
            match decimal_sep:
                case "dot":
                    val = val.replace(",", "")
                    val = val[: dot + 1] + (val[dot + 1 :].replace(".", ""))
                    if len(val) > 1:
                        rd[i] = float(val) if not negative else -float(val)
                    else:
                        rd[i] = None
                case "comma":
                    val = val.replace(".", "")
                    val = val[: comma + 1] + (val[comma + 1 :].replace(",", ""))
                    val = val.replace(",", ".")
                    if len(val) > 1:
                        rd[i] = float(val) if not negative else -float(val)
                    else:
                        rd[i] = None
                case _:
                    rd[i] = int(val) if not negative else -int(val)


def filter_csv_report(report_filter: str | Missing, content: str) -> Tuple[str, str]:
    csv = content
    output = ""
    if isinstance(report_filter, str) and report_filter:
        params = JsRunnerParams(code=report_filter, data=content)
        try:
            csv, output = jsrunner_run(params)
        except JsRunnerError as e:
            raise RouteException("Error in JavaScript: " + str(e)) from e
    return csv, output


def create_and_send_report(
    filename: str, content: str | io.BytesIO, mimetype: str
) -> Response:
    file_io = content
    if not isinstance(content, io.BytesIO):
        # Re-encode to UTF-8-BOM since that's what Excel opens by default
        file_io = io.BytesIO(content.encode("utf-8-sig"))
    return send_file(
        file_io,
        as_attachment=True,
        download_name=filename,
        mimetype=mimetype,
    )


@dataclass
class FetchTableDataModel:
    taskid: str


@tableForm_plugin.get("/fetchTableData")
@use_model(FetchTableDataModel)
def fetch_rows(m: FetchTableDataModel) -> Response:
    curr_user = get_current_user_object()
    tid = TaskId.parse(m.taskid, require_doc_id=True, allow_block_hint=False)
    assert tid.doc_id is not None
    doc = get_doc_or_abort(tid.doc_id)
    doc.document.insert_preamble_pars()
    view_ctx = view_ctx_with_urlmacros(ViewRoute.Unknown)
    try:
        plug = find_plugin_from_document(
            doc.document, tid, UserContext.from_one_user(curr_user), view_ctx
        )
    except TaskNotFoundException:
        raise NotExist(f"Table not found: {tid}")
    markup = load_tableform_markup(plug)
    include_users = markup.includeUsers
    fields = markup.fields
    if not isinstance(fields, list):
        fields = []
    groups = markup.groups
    if not isinstance(groups, list):
        groups = []
    r = tableform_get_fields(
        fields,
        groups,
        doc,
        curr_user,
        view_ctx,
        value_or_default(markup.removeDocIds, True),
        value_or_default(markup.showInView, False),
        group_filter_type=include_users,
        anonymize_names=value_or_default(markup.anonNames, False),
    )
    return json_response(r)


def load_tableform_markup(plug: Plugin) -> TableFormMarkupModel:
    model: TableFormMarkupModel = TableFormMarkupSchema().load(plug.values)
    return model


@dataclass
class FetchTableDataModelPreview(FetchTableDataModel):
    fields: list[str]
    groups: list[str]
    removeDocIds: bool = True
    anonNames: bool = False


@tableForm_plugin.get("/fetchTableDataPreview")
@use_model(FetchTableDataModelPreview)
def fetch_rows_preview(m: FetchTableDataModelPreview) -> Response:
    curr_user = get_current_user_object()
    tid = TaskId.parse(m.taskid, require_doc_id=False, allow_block_hint=False)
    assert tid.doc_id is not None
    doc = get_doc_or_abort(tid.doc_id)
    doc.document.insert_preamble_pars()
    # With this route we can't be certain about showInView so we just check for edit access
    # whoever can open the plugin in preview should have that right
    if not curr_user.has_edit_access(doc):
        raise AccessDenied(f"Missing edit access for document {doc.id}")
    view_ctx = view_ctx_with_urlmacros(ViewRoute.Unknown)
    r = tableform_get_fields(
        m.fields,
        m.groups,
        doc,
        curr_user,
        view_ctx,
        m.removeDocIds,
        allow_non_teacher=True,
        anonymize_names=m.anonNames,
        #  TODO: group_filter_type = plug.values.get("includeUsers"),
    )
    return json_response(r)


@dataclass
class UpdateFieldsModel:
    taskid: str
    fields: list[str]


@tableForm_plugin.get("/updateFields")
@use_model(UpdateFieldsModel)
def update_fields(m: UpdateFieldsModel) -> Response:
    r: dict[str, Any] = {}
    fields_to_update = m.fields
    taskid = m.taskid
    tid = TaskId.parse(taskid, require_doc_id=True, allow_block_hint=False)
    assert tid.doc_id is not None
    doc = get_doc_or_abort(tid.doc_id)
    curr_user = get_current_user_object()
    view_ctx = view_ctx_with_urlmacros(ViewRoute.Unknown)
    try:
        plug = find_plugin_from_document(
            doc.document, tid, UserContext.from_one_user(curr_user), view_ctx
        )
    except TaskNotFoundException:
        raise NotExist(f"Table not found: {tid}")
    markup = load_tableform_markup(plug)
    groupnames = markup.groups
    if not isinstance(groupnames, list):
        groupnames = []
    fielddata, _, field_names, _ = get_fields_and_users(
        fields_to_update,
        RequestedGroups.from_name_list(groupnames),
        doc,
        curr_user,
        view_ctx,
        value_or_default(markup.removeDocIds, True),
        add_missing_fields=True,
        access_option=GetFieldsAccess.from_bool(
            value_or_default(markup.showInView, False)
        ),
    )
    rows = {}
    styles = {}
    for f in fielddata:
        username = f["user"].name
        rows[username] = dict(f["fields"])
        for key, content in rows[username].items():
            if type(content) is dict:
                rows[username][key] = json.dumps(content)
        styles[username] = dict(f["styles"])
    r["rows"] = rows
    r["styles"] = styles
    r["fields"] = field_names
    return json_response(r)


class TableFormUserInfo(TypedDict):
    id: int
    real_name: str
    email: str


class TableFormObj(TypedDict):
    rows: dict[str, UserFields]
    users: dict[str, TableFormUserInfo]
    membership_add: dict[str, str | None]
    membership_end: dict[str, str | None]
    fields: list[str]
    aliases: dict[str, str]
    styles: dict[str, dict[str, str | None]]


def tableform_get_fields(
    flds: list[str],
    groupnames: list[str],
    doc: DocInfo,
    curr_user: User,
    view_ctx: ViewContext,
    remove_doc_ids: bool,
    allow_non_teacher: bool,
    group_filter_type: MembershipFilter = MembershipFilter.Current,
    user_filter: list[str] | None = None,
    anonymize_names: bool = False,
) -> TableFormObj:
    if is_hide_names():
        anonymize_names = True

    user_filter_q = None
    if user_filter is not None:
        user_ids: list[int] = []
        if anonymize_names:
            # Preprocess users to retrieve their ids
            # TODO: In future the ID is likely going to not be part of the name!
            for user in user_filter:
                if not user.startswith("user"):
                    user_ids = []
                    break
                try:
                    user_id = int(user[4:])
                    user_ids.append(user_id)
                except ValueError:
                    user_ids = []
                    break
        if user_ids:
            user_filter_q = User.id.in_(user_ids)
        elif user_filter:
            user_filter_q = User.name.in_(user_filter)

    fielddata, aliases, field_names, groups = get_fields_and_users(
        flds,
        RequestedGroups.from_name_list(groupnames),
        doc,
        curr_user,
        view_ctx,
        remove_doc_ids,
        add_missing_fields=True,
        access_option=GetFieldsAccess.from_bool(allow_non_teacher),
        member_filter_type=group_filter_type,
        user_filter=user_filter_q,
    )
    rows = {}
    users: dict[str, TableFormUserInfo] = {}
    styles = {}
    group_ids = {g.id for g in groups} if groups else None
    membership_add_map: dict[str, str | None] = {}
    membership_end_map: dict[str, str | None] = {}
    for f in fielddata:
        u: User = f["user"]
        u.is_name_hidden = anonymize_names
        user_info = u.to_json()
        username = user_info["name"]
        rn = user_info["real_name"]
        email = user_info["email"]
        rows[username] = dict(f["fields"])
        for key, content in rows[username].items():
            if type(content) is dict:
                rows[username][key] = json.dumps(content)
        users[username] = TableFormUserInfo(
            id=u.id,
            real_name=rn if rn is not None else "",
            email=email if email is not None else "",
        )
        styles[username] = dict(f["styles"])
        if group_ids:
            if group_filter_type != MembershipFilter.Current:
                membership_end = get_membership_end(u, group_ids)
                membership_end_map[username] = (
                    membership_end.astimezone(fin_timezone).strftime("%Y-%m-%d %H:%M")
                    if membership_end
                    else None
                )
            membership_added = get_membership_added(u, group_ids)
            membership_add_map[username] = (
                membership_added.astimezone(fin_timezone).strftime("%Y-%m-%d %H:%M")
                if membership_added
                else None
            )
    r = TableFormObj(
        rows=rows,
        users=users,
        membership_add=membership_add_map,
        membership_end=membership_end_map,
        fields=field_names,
        aliases=aliases,
        styles=styles,
    )
    return r
