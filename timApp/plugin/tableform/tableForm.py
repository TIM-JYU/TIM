"""
TIM example plugin: a tableFormndrome checker.
"""
import datetime
import json
from dataclasses import dataclass, asdict, field
from typing import Union, List, Optional, Dict, Any, TypedDict, Type, Sequence

from flask import render_template_string, abort, Response
from marshmallow.utils import missing
from sqlalchemy.orm import joinedload
from webargs.flaskparser import use_args

from markupmodels import GenericMarkupModel
from marshmallow_dataclass import class_schema
from pluginserver_flask import GenericHtmlModel, \
    GenericAnswerModel, create_blueprint, value_or_default, PluginAnswerResp, PluginAnswerWeb, PluginReqs, EditorTab
from timApp.auth.accesshelper import get_doc_or_abort
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docinfo import DocInfo
from timApp.document.timjsonencoder import TimJsonEncoder
from timApp.item.block import Block
from timApp.item.tag import Tag, TagType, GROUP_TAG_PREFIX
from timApp.plugin.jsrunner import jsrunner_run, JsRunnerParams, JsRunnerError
from timApp.plugin.plugin import find_plugin_from_document, TaskNotFoundException, Plugin
from timApp.plugin.tableform.comparatorFilter import RegexOrComparator
from timApp.plugin.taskid import TaskId
from timApp.sisu.parse_display_name import parse_sisu_group_display_name
from timApp.sisu.sisu import get_potential_groups
from timApp.tim_app import csrf
from timApp.user.user import User, get_membership_end
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import RouteException, use_model
from timApp.util.flask.responsehelper import csv_string, json_response, text_response
from timApp.util.get_fields import get_fields_and_users, MembershipFilter, UserFields, RequestedGroups, GetFieldsAccess
from timApp.util.utils import fin_timezone
from utils import Missing


@dataclass
class TableFormStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    # TODO: Save user given table layouts like in timTable


@dataclass
class DataViewVirtualScrollingModel:
    enabled: Optional[bool] = True
    verticalOverflow: Union[int, Missing] = missing
    horizontalOverflow: Union[int, Missing] = missing


@dataclass
class DataViewSettingsModel:
    virtual: Union[DataViewVirtualScrollingModel, Missing, None] = missing
    rowHeight: Union[int, Missing] = missing
    columnWidths: Union[Dict[str, int], Missing] = missing
    tableWidth: Union[str, Missing] = missing
    fixedColumns: Union[int, Missing] = missing


@dataclass
class TableFormMarkupModel(GenericMarkupModel):
    anonNames: Union[bool, Missing] = missing
    autosave: Union[bool, Missing] = missing
    autoUpdateFields: Union[bool, Missing] = True
    autoUpdateTables: Union[bool, Missing] = True
    cbColumn: Union[bool, Missing, None] = missing
    dataCollection: Union[str, Missing, None] = missing
    emails: Union[bool, Missing] = missing
    emailUsersButtonText: Union[str, Missing, None] = missing
    filterRow: Union[bool, Missing, None] = missing
    fixedColor: Union[str, Missing, None] = missing
    fontSize: Union[str, Missing, None] = missing
    forceUpdateButtonText: Union[str, Missing, None] = missing
    groups: Union[List[str], Missing] = missing
    hiddenColumns: Union[List[int], Missing, None] = missing
    hiddenRows: Union[List[int], Missing, None] = missing
    hide: Union[Dict[Any, Any], Missing, None] = missing
    hideButtonText: Union[str, Missing, None] = missing
    includeUsers: MembershipFilter = field(default=MembershipFilter.Current, metadata={'by_value': True})
    lockedFields: Union[List[str], Missing] = missing
    maxCols: Union[str, Missing, None] = missing
    maxRows: Union[str, Missing, None] = missing
    maxWidth: Union[str, Missing] = missing
    minWidth: Union[str, Missing, None] = missing
    nrColumn: Union[bool, Missing, None] = missing
    charRow: Union[bool, Missing, None] = missing
    open: Union[bool, Missing] = True
    openButtonText: Union[str, Missing, None] = missing
    realnames: Union[bool, Missing] = missing
    removeDocIds: Union[bool, Missing] = True
    removeUsersButtonText: Union[str, Missing, None] = missing
    report: Union[bool, Missing] = missing
    reportButton: Union[str, Missing, None] = missing
    reportFilter: Union[str, Missing, None] = missing
    runScripts: Union[List[str], Missing] = missing
    saveStyles: Union[bool, Missing] = True
    separator: Union[str, Missing, None] = missing
    showToolbar: Union[bool, Missing, None] = missing
    singleLine: Union[bool, Missing, None] = missing
    sisugroups: Union[str, Missing] = missing
    sortBy: Union[str, Missing, None] = missing
    table: Union[bool, Missing] = missing
    toolbarTemplates: Union[List[Dict[Any, Any]], Missing] = missing
    userListButtonText: Union[str, Missing, None] = missing
    usernames: Union[bool, Missing] = missing
    dataView: Union[DataViewSettingsModel, Missing, None] = missing


TableFormMarkupSchema = class_schema(TableFormMarkupModel)


@dataclass
class TableFormInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    replyRows: Dict[str, Any]
    nosave: Union[bool, Missing] = missing


def get_sisu_group_desc_for_table(g: UserGroup) -> str:
    p = parse_sisu_group_display_name(g.display_name)
    assert p is not None
    if g.external_id.is_studysubgroup:
        return p.desc
    # We want the most important groups to be at the top of the table.
    # The '(' at the beginning changes the sort order.
    return f'({p.desc})'


def get_sisugroups(user: User, sisu_id: Optional[str]) -> 'TableFormObj':
    gs = get_potential_groups(user, sisu_id)
    docs_with_course_tag = Tag.query.filter_by(type=TagType.CourseCode).with_entities(Tag.block_id).subquery()
    tags = (Tag.query
            .filter(Tag.name.in_([GROUP_TAG_PREFIX + g.name for g in gs]) & Tag.block_id.in_(docs_with_course_tag))
            .options(joinedload(Tag.block).joinedload(Block.docentries))
            .all())
    tag_map = {t.name[len(GROUP_TAG_PREFIX):]: t for t in tags}

    def get_course_page(ug: UserGroup) -> Optional[str]:
        t: Optional[Tag] = tag_map.get(ug.name)
        if t:
            return f'<a href="{t.block.docentries[0].url_relative}">URL</a>'
        else:
            return None

    return TableFormObj(
        rows={
            g.external_id.external_id: {
                'TIM-nimi': g.name,
                'URL': f'<a href="{g.admin_doc.docentries[0].url_relative}">URL</a>' if g.admin_doc else None,
                'Jäseniä': len(g.current_memberships),
                'Kurssisivu': get_course_page(g),
            } for g in gs
        },
        realnamemap={
            g.external_id.external_id: get_sisu_group_desc_for_table(g) if sisu_id else g.display_name for g in gs
        },
        emailmap={
            g.external_id.external_id: '' for g in gs
        },
        fields=['Jäseniä', 'TIM-nimi', 'URL', 'Kurssisivu'],
        aliases={
            'TIM-nimi': 'TIM-nimi',
            'URL': 'URL',
            'Jäseniä': 'Jäseniä',
            'Kurssisivu': 'Kurssisivu',
        },
        styles={
            g.external_id.external_id: {} for g in gs
        },
        membershipmap={},
    )


@dataclass
class TableFormHtmlModel(GenericHtmlModel[TableFormInputModel, TableFormMarkupModel, TableFormStateModel]):
    def get_component_html_name(self) -> str:
        return 'tableform-runner'

    def show_in_view_default(self) -> bool:
        return False

    def get_json_encoder(self) -> Type[TimJsonEncoder]:
        return TimJsonEncoder

    def get_static_html(self) -> str:
        return render_static_table_form(self)

    def get_browser_json(self) -> Dict:
        r = super().get_browser_json()
        if self.markup.open:
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
                    value_or_default(self.markup.removeDocIds, True),
                    value_or_default(self.markup.showInView, False),
                    group_filter_type=self.markup.includeUsers,
                )
            r = {**r, **f}
        return r


@dataclass
class TableFormAnswerModel(GenericAnswerModel[TableFormInputModel, TableFormMarkupModel, TableFormStateModel]):
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
class GenerateCSVModel:
    docId: int
    fields: List[str]
    groups: List[str]
    separator: str
    userFilter: List[str] = field(default_factory=list)
    usernames: Union[bool, Missing] = True
    realnames: Union[bool, Missing] = missing
    removeDocIds: Union[bool, Missing] = missing
    emails: Union[bool, Missing] = missing
    reportFilter: Union[str, Missing] = missing
    filterFields: List[str] = field(default_factory=list)
    filterValues: List[str] = field(default_factory=list)


GenerateCSVSchema = class_schema(GenerateCSVModel)


class TableformAnswerResp(PluginAnswerResp):
    savedata: List[Dict[str, Any]]


def answer(args: TableFormAnswerModel) -> PluginAnswerResp:
    rows = args.input.replyRows
    save_rows = []
    for u, r in rows.items():
        user = User.get_by_name(u)
        if not user:
            return args.make_answer_error(f'User not found: {u}')
        save_rows.append({'user': user.id, 'fields': r})

    web: PluginAnswerWeb = {}
    result: TableformAnswerResp = {'web': web, 'savedata': save_rows}
    web['result'] = "saved"
    return result


def reqs() -> PluginReqs:
    templates = ["""
``` {#tableForm_table plugin="tableForm"}    
# showInView: true # Add attribute  to show the plugin in normal view
groups: 
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
#buttonText: Tallenna    # Name your save button here
cbColumn: true    # show checkboxes
nrColumn: true    # show numbers
filterRow: true   # show filters 
singleLine: true  # show every line as a single line
emailUsersButtonText: "Lähetä sähköpostia valituille" # if one wants to send email 
separator: ";"    # Define your value separator here, ";" as default
anonNames: false  # To show or hide user (and full) names in report, true or false
reportButton: "Raportti"
userListButtonText: "Käyttäjälista"
showToolbar: true # toolbar for editing the table
# forceUpdateButtonText: "Virkistä" # button for refreshing the table
#dataView:        # uncomment this if table is big or want to use special properties
#   tableWidth: 90vw
#   virtual:
#    enabled: true  # toggles virtual mode on or off; default true
#   fixedColumns: 1 # how many not scrolling columns in left
```"""]
    editor_tabs: List[EditorTab] = [
        {
            'text': 'Fields',
            'items': [
                {
                    'text': 'Tables',
                    'items': [
                        {
                            'data': templates[0].strip(),
                            'text': 'Table and report',
                            'expl': 'Form a table for editing forms, that can be converted to report',
                        },
                    ],
                },
            ],
        },
    ]
    return {
        "js": ["tableForm"],
        "multihtml": True,
        'editor_tabs': editor_tabs,
    }


tableForm_plugin = create_blueprint(
    __name__,
    'tableForm',
    TableFormHtmlModel,
    TableFormAnswerModel,
    answer,
    reqs,
    csrf,
)


def check_field_filtering(r_filter: Optional[RegexOrComparator], target: Union[str, float, None]) -> bool:
    if r_filter is None:
        return True
    return r_filter.is_match(target)


@tableForm_plugin.route('/generateCSV')
@use_args(GenerateCSVSchema())
def gen_csv(args: GenerateCSVModel) -> Union[Response, str]:
    """
    Generates a report defined by tableForm attributes
    # TODO: generic, move
    :return: CSV containing headerrow and rows for users and values
    """
    curr_user = get_current_user_object()
    docid, groups, separator, show_real_names, show_user_names, show_emails, remove_doc_ids, fields, user_filter, \
        filter_fields, filter_values = args.docId, args.groups, args.separator, args.realnames, \
        args.usernames, args.emails, args.removeDocIds, args.fields, args.userFilter, \
        args.filterFields, args.filterValues
    if len(separator) > 1:
        # TODO: Add support >1 char strings like in Korppi
        return "Only 1-character string separators supported for now"
    doc = get_doc_or_abort(docid)
    if not isinstance(remove_doc_ids, bool):
        remove_doc_ids = True
    r = tableform_get_fields(
        fields,
        groups,
        doc,
        curr_user,
        remove_doc_ids,
        allow_non_teacher=True,
        user_filter=user_filter,
        # TODO: group_filter_type=self.markup.includeUsers,
    )
    data: List[List[Union[str, float, None]]] = [[]]
    if show_real_names:
        data[0].append("Real name")
    if show_user_names:
        data[0].append("Username")
    if show_emails:
        data[0].append("email")
    tmp: Sequence[Union[str, float, None]] = r['fields']
    data[0] = data[0] + list(tmp)
    if len(filter_fields) != len(filter_values):
        abort(400, "Filter targets and filter values do not match")
    # TODO: Check if filters could be easily integrated to original query in get_fields_and_users
    regs = {}
    # TODO: Create ComparatorFilters
    try:
        for i, f in enumerate(filter_fields):
            reg = RegexOrComparator(filter_values[i])
            if reg:
                regs[f] = reg
    except IndexError:
        abort(400, "Too many filters")

    for rowkey, row in sorted(r['rows'].items()):
        row_data: List[Union[str, float, None]] = []
        if show_real_names:
            val = r['realnamemap'].get(rowkey)
            row_data.append(val)
            if not check_field_filtering(regs.get("realname"), val):
                continue
        if show_user_names:
            val = rowkey
            row_data.append(val)
            if not check_field_filtering(regs.get("username"), val):
                continue
        if show_emails:
            val = r['emailmap'].get(rowkey)
            row_data.append(val)
            if not check_field_filtering(regs.get("email"), val):
                continue
        filter_this_row = False
        for i, _field in enumerate(r['fields']):
            v = row.get(_field)
            row_data.append(v)
            if not check_field_filtering(regs.get(_field), v):
                filter_this_row = True
                break
        if filter_this_row:
            continue
        data.append(row_data)

    csv = csv_string(data, 'excel', separator)
    output = ''
    if isinstance(args.reportFilter, str) and args.reportFilter:
        params = JsRunnerParams(code=args.reportFilter, data=csv)
        try:
            csv, output = jsrunner_run(params)
        except JsRunnerError as e:
            raise RouteException('Error in JavaScript: ' + str(e)) from e
    return text_response(output+csv)


"""
    # This did not work because if code is just return data; then it is not identical when returned
    if args.reportFilter:
        params = {'code': args.reportFilter, 'data': data}
        data, output = jsrunner_run(params)
    return csv_response(data, 'excel', separator)
"""


@dataclass
class FetchTableDataModel:
    taskid: str


@tableForm_plugin.route('/fetchTableData')
@use_model(FetchTableDataModel)
def fetch_rows(m: FetchTableDataModel) -> Response:
    curr_user = get_current_user_object()
    tid = TaskId.parse(m.taskid, require_doc_id=True, allow_block_hint=False)
    assert tid.doc_id is not None
    doc = get_doc_or_abort(tid.doc_id)
    doc.document.insert_preamble_pars()
    try:
        plug = find_plugin_from_document(doc.document, tid, curr_user)
    except TaskNotFoundException:
        return abort(404, f'Table not found: {tid}')
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
        value_or_default(markup.removeDocIds, True),
        value_or_default(markup.showInView, False),
        group_filter_type=include_users,
    )
    return json_response(r)


def load_tableform_markup(plug: Plugin) -> TableFormMarkupModel:
    model: TableFormMarkupModel = TableFormMarkupSchema().load(plug.values)
    return model


@dataclass
class FetchTableDataModelPreview(FetchTableDataModel):
    fields: List[str]
    groups: List[str]
    removeDocIds: bool = True


@tableForm_plugin.route('/fetchTableDataPreview')
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
        return abort(403, f'Missing edit access for document {doc.id}')
    r = tableform_get_fields(
        m.fields,
        m.groups,
        doc,
        curr_user,
        m.removeDocIds,
        allow_non_teacher=True
        #  TODO: group_filter_type = plug.values.get("includeUsers"),
    )
    return json_response(r)


@dataclass
class UpdateFieldsModel:
    taskid: str
    fields: List[str]


@tableForm_plugin.route('/updateFields')
@use_model(UpdateFieldsModel)
def update_fields(m: UpdateFieldsModel) -> Response:
    r: Dict[str, Any] = {}
    fields_to_update = m.fields
    taskid = m.taskid
    tid = TaskId.parse(taskid, require_doc_id=True, allow_block_hint=False)
    assert tid.doc_id is not None
    doc = get_doc_or_abort(tid.doc_id)
    curr_user = get_current_user_object()
    try:
        plug = find_plugin_from_document(doc.document, tid, curr_user)
    except TaskNotFoundException:
        return abort(404, f'Table not found: {tid}')
    markup = load_tableform_markup(plug)
    groupnames = markup.groups
    if not isinstance(groupnames, list):
        groupnames = []
    fielddata, _, field_names, _ = get_fields_and_users(
        fields_to_update,
        RequestedGroups.from_name_list(groupnames),
        doc,
        curr_user,
        value_or_default(markup.removeDocIds, True),
        add_missing_fields=True,
        access_option=GetFieldsAccess.from_bool(value_or_default(markup.showInView, False)),
    )
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
    return json_response(r)


class TableFormObj(TypedDict):
    rows: Dict[str, UserFields]
    realnamemap: Dict[str, str]
    emailmap: Dict[str, str]
    membershipmap: Dict[str, Union[datetime.datetime, None]]
    fields: List[str]
    aliases: Dict[str, str]
    styles: Dict[str, Dict[str, Union[str, None]]]


def tableform_get_fields(
        flds: List[str],
        groupnames: List[str],
        doc: DocInfo,
        curr_user: User,
        remove_doc_ids: bool,
        allow_non_teacher: bool,
        group_filter_type: MembershipFilter = MembershipFilter.Current,
        user_filter: Optional[List[str]] = None,
) -> TableFormObj:
    fielddata, aliases, field_names, groups = \
        get_fields_and_users(
            flds,
            RequestedGroups.from_name_list(groupnames),
            doc,
            curr_user,
            remove_doc_ids,
            add_missing_fields=True,
            access_option=GetFieldsAccess.from_bool(allow_non_teacher),
            member_filter_type=group_filter_type,
            user_filter=User.name.in_(user_filter) if user_filter else None,
        )
    rows = {}
    realnames: Dict[str, str] = {}
    emails = {}
    styles = {}
    group_ids = set(g.id for g in groups) if groups else None
    membershipmap = {}
    for f in fielddata:
        u: User = f['user']
        username = u.name
        rows[username] = dict(f['fields'])
        for key, content in rows[username].items():
            if type(content) is dict:
                rows[username][key] = json.dumps(content)
        rn = f['user'].real_name
        if rn is not None:
            realnames[username] = rn
        email = f['user'].email
        if email is not None:
            emails[username] = email
        styles[username] = dict(f['styles'])
        if group_ids and group_filter_type != MembershipFilter.Current:
            membership_end = get_membership_end(u, group_ids)
            if membership_end:
                membership_end = membership_end.astimezone(fin_timezone).strftime('%Y-%m-%d %H:%M')
            membershipmap[username] = membership_end
    r = TableFormObj(
        rows=rows,
        realnamemap=realnames,
        emailmap=emails,
        membershipmap=membershipmap,
        fields=field_names,
        aliases=aliases,
        styles=styles,
    )
    return r
