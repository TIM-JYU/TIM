"""
TIM example plugin: a tableFormndrome checker.
"""
import datetime
import json
from dataclasses import dataclass, asdict, field
from typing import Union, List, Optional, Dict, Any, TypedDict

from flask import jsonify, render_template_string, request, abort
from marshmallow.utils import missing
from sqlalchemy.orm import joinedload
from webargs.flaskparser import use_args

from markupmodels import GenericMarkupModel
from marshmallow_dataclass import class_schema
from pluginserver_flask import GenericHtmlModel, \
    GenericAnswerModel, create_blueprint
from timApp.auth.accesshelper import get_doc_or_abort
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docinfo import DocInfo
from timApp.document.timjsonencoder import TimJsonEncoder
from timApp.item.block import Block
from timApp.item.tag import Tag, TagType, GROUP_TAG_PREFIX
from timApp.plugin.jsrunner import jsrunner_run, JsRunnerParams, JsRunnerError
from timApp.plugin.plugin import find_plugin_from_document, TaskNotFoundException
from timApp.plugin.tableform.comparatorFilter import RegexOrComparator
from timApp.plugin.taskid import TaskId
from timApp.sisu.parse_display_name import parse_sisu_group_display_name
from timApp.sisu.sisu import get_potential_groups
from timApp.tim_app import csrf
from timApp.user.user import User, get_membership_end
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import csv_string, json_response, text_response
from timApp.util.get_fields import get_fields_and_users, MembershipFilter, UserFields, RequestedGroups
from timApp.util.utils import get_boolean, fin_timezone
from utils import Missing


@dataclass
class TableFormStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    # TODO: Save user given table layouts like in timTable


@dataclass
class DataViewVirtualScrollingModel(GenericMarkupModel):
    enabled: Optional[bool] = True
    verticalOverflow: Optional[int] = 1
    horizontalOverflow: Optional[int] = 1


@dataclass
class DataViewSettingsModel(GenericMarkupModel):
    virtual: Union[DataViewVirtualScrollingModel, Missing, None] = missing


@dataclass
class TableFormMarkupModel(GenericMarkupModel):
    anonNames: Union[bool, Missing] = missing
    autosave: Union[bool, Missing] = missing
    autoUpdateFields: Union[bool, Missing] = True
    autoUpdateTables: Union[bool, Missing] = True
    buttonText: Union[str, Missing, None] = missing
    cbColumn: Union[bool, Missing, None] = missing
    dataCollection: Union[str, Missing, None] = missing
    emails: Union[bool, Missing] = missing
    emailUsersButtonText: Union[str, Missing, None] = missing
    fields: Union[List[str], Missing] = missing
    filterRow: Union[bool, Missing, None] = missing
    fixedColor: Union[str, Missing, None] = missing
    fontSize: Union[str, Missing, None] = missing
    groups: Union[List[str], Missing] = missing
    hiddenColumns: Union[List[int], Missing, None] = missing
    hiddenRows: Union[List[int], Missing, None] = missing
    hide: Union[Dict[Any, Any], Missing, None] = missing
    hideButtonText: Union[str, Missing, None] = missing
    includeUsers: Union[MembershipFilter, Missing] = field(default=MembershipFilter.Current, metadata={'by_value': True})
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


@dataclass
class TableFormInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    replyRows: Union[Dict[Any, Any], Missing] = missing
    nosave: Union[bool, Missing] = missing


def get_sisu_group_desc_for_table(g: UserGroup):
    p = parse_sisu_group_display_name(g.display_name)
    if g.external_id.is_studysubgroup:
        return p.desc
    # We want the most important groups to be at the top of the table.
    # The '(' at the beginning changes the sort order.
    return f'({p.desc})'


def get_sisugroups(user: User, sisu_id: Optional[str]):
    gs = get_potential_groups(user, sisu_id)
    docs_with_course_tag = Tag.query.filter_by(type=TagType.CourseCode).with_entities(Tag.block_id).subquery()
    tags = (Tag.query
            .filter(Tag.name.in_([GROUP_TAG_PREFIX + g.name for g in gs]) & Tag.block_id.in_(docs_with_course_tag))
            .options(joinedload(Tag.block).joinedload(Block.docentries))
            .all())
    tag_map = {t.name[len(GROUP_TAG_PREFIX):]: t for t in tags}

    def get_course_page(ug: UserGroup):
        t: Tag = tag_map.get(ug.name)
        if t:
            return f'<a href="{t.block.docentries[0].url_relative}">URL</a>'
        else:
            return None

    return {
        'rows': {
            g.external_id.external_id: {
                'TIM-nimi': g.name,
                'URL': f'<a href="{g.admin_doc.docentries[0].url_relative}">URL</a>' if g.admin_doc else None,
                'Jäseniä': len(g.current_memberships),
                'Kurssisivu': get_course_page(g),
            } for g in gs
        },
        'realnamemap': {
            g.external_id.external_id: get_sisu_group_desc_for_table(g) if sisu_id else g.display_name for g in gs
        },
        'emailmap': {
            g.external_id.external_id: '' for g in gs
        },
        'fields': ['Jäseniä', 'TIM-nimi', 'URL', 'Kurssisivu'],
        'aliases': {
            'TIM-nimi': 'TIM-nimi',
            'URL': 'URL',
            'Jäseniä': 'Jäseniä',
            'Kurssisivu': 'Kurssisivu',
        },
        'styles': {
            g.external_id.external_id: {} for g in gs
        },
    }


@dataclass
class TableFormHtmlModel(GenericHtmlModel[TableFormInputModel, TableFormMarkupModel, TableFormStateModel]):
    def get_component_html_name(self) -> str:
        return 'tableform-runner'

    def show_in_view_default(self) -> bool:
        return False

    def get_json_encoder(self):
        return TimJsonEncoder

    def get_static_html(self) -> str:
        return render_static_table_form(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        if self.markup.open:
            tid = TaskId.parse(self.taskID)
            d = get_doc_or_abort(tid.doc_id)
            user = User.get_by_name(self.current_user_id)
            if self.markup.sisugroups:
                f = get_sisugroups(user, self.markup.sisugroups)
            else:
                f = tableform_get_fields(
                    self.markup.fields,
                    self.markup.groups,
                    d,
                    user,
                    self.markup.removeDocIds,
                    self.markup.showInView,
                    group_filter_type=self.markup.includeUsers,
                )
            r = {**r, **f}
        return r


@dataclass
class TableFormAnswerModel(GenericAnswerModel[TableFormInputModel, TableFormMarkupModel, TableFormStateModel]):
    pass


def render_static_table_form(m: TableFormHtmlModel):
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


TableFormHtmlSchema = class_schema(TableFormHtmlModel)
TableFormAnswerSchema = class_schema(TableFormAnswerModel)

tableForm_plugin = create_blueprint(__name__, 'tableForm', TableFormHtmlSchema, csrf)


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
    reportFilter: Union[str, Missing] = ""
    filterFields: List[str] = field(default_factory=list)
    filterValues: List[str] = field(default_factory=list)
    # filters: List[str] = field(default_factory=list)


GenerateCSVSchema = class_schema(GenerateCSVModel)


@tableForm_plugin.route('/generateCSV')
@use_args(GenerateCSVSchema())
def gen_csv(args: GenerateCSVModel):
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
    r = tableform_get_fields(fields, groups,
                             doc, curr_user, remove_doc_ids, allow_non_teacher=True, user_filter=user_filter
                             # TODO: group_filter_type=self.markup.includeUsers,
                             )
    rowkeys = list(r['rows'].keys())
    rowkeys.sort()
    data = [[]]
    if show_real_names:
        data[0].append("Real name")
    if show_user_names:
        data[0].append("Username")
    if show_emails:
        data[0].append("email")
    data[0] = data[0] + r['fields']
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

    def check_field_filtering(filter: RegexOrComparator, target: Union[str, float, None]) -> bool:
        if filter is None:
            return True
        return filter.is_match(target)

    for rowkey in rowkeys:
        row_data = []
        row = r['rows'].get(rowkey)
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
            val = row.get(_field)
            row_data.append(val)
            if not check_field_filtering(regs.get(_field), val):
                filter_this_row = True
                break
        if filter_this_row:
            continue
        data.append(row_data)

    csv = csv_string(data, 'excel', separator)
    output = ''
    if args.reportFilter:
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


@tableForm_plugin.route('/fetchTableData')
def fetch_rows():
    curr_user = get_current_user_object()
    taskid = request.args.get("taskid")
    tid = TaskId.parse(taskid, require_doc_id=False, allow_block_hint=False)
    doc = get_doc_or_abort(tid.doc_id)
    doc.document.insert_preamble_pars()
    try:
        plug = find_plugin_from_document(doc.document, tid, curr_user)
    except TaskNotFoundException:
        return abort(404, f'Table not found: {tid}')
    # debug = plug.values
    include_users = plug.values.get("includeUsers")
    r = tableform_get_fields(plug.values.get("fields",[]), plug.values.get("groups", []),
                             doc, curr_user, plug.values.get("removeDocIds", True),
                             plug.values.get("showInView"),
                             group_filter_type=MembershipFilter(include_users or 'current'),
                             )
    return json_response(r, headers={"No-Date-Conversion": "true"})


@tableForm_plugin.route('/fetchTableDataPreview')
def fetch_rows_preview():
    curr_user = get_current_user_object()
    taskid = request.args.get("taskid")
    tid = TaskId.parse(taskid, require_doc_id=False, allow_block_hint=False)
    doc = get_doc_or_abort(tid.doc_id)
    doc.document.insert_preamble_pars()
    # With this route we can't be certain about showInView so we just check for edit access
    # whoever can open the plugin in preview should have that right
    if not curr_user.has_edit_access(doc):
        return abort(403, f'Missing edit access for document {doc.id}')
    fields = request.args.getlist("fields")
    groups = request.args.getlist("groups")
    remove_doc_ids = get_boolean(request.args.get("removeDocIds"), True)
    r = tableform_get_fields(fields, groups,
                             doc, curr_user, remove_doc_ids, allow_non_teacher=True
                             #  TODO: group_filter_type = plug.values.get("includeUsers"),
                             )
    return json_response(r, headers={"No-Date-Conversion": "true"})


@tableForm_plugin.route('/updateFields')
def update_fields():
    r = {}
    fields_to_update = request.args.getlist("fields")
    taskid = request.args.get("taskid")
    tid = TaskId.parse(taskid, require_doc_id=False, allow_block_hint=False)
    doc = get_doc_or_abort(tid.doc_id)
    curr_user = get_current_user_object()
    try:
        plug = find_plugin_from_document(doc.document, tid, curr_user)
    except TaskNotFoundException:
        return abort(404, f'Table not found: {tid}')
    groupnames = plug.values.get("groups",[])
    fielddata, _, field_names, _ = get_fields_and_users(
        fields_to_update,
        RequestedGroups.from_name_list(groupnames),
        doc,
        curr_user,
        plug.values.get("removeDocIds", True),
        add_missing_fields=True,
        allow_non_teacher=plug.values.get("showInView"),
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
    return json_response(r, headers={"No-Date-Conversion": "true"})


class TableFormRow(TypedDict):
    pass


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
        group_filter_type = MembershipFilter.Current,
        user_filter: List[str] = None,
):
    fielddata, aliases, field_names, groups = \
        get_fields_and_users(
            flds,
            RequestedGroups.from_name_list(groupnames),
            doc,
            curr_user,
            remove_doc_ids,
            add_missing_fields=True,
            allow_non_teacher=allow_non_teacher,
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
        realnames[username] = f['user'].real_name
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


@tableForm_plugin.route('/answer', methods=['put'])
@csrf.exempt
@use_args(TableFormAnswerSchema(), locations=("json",))
def answer(args: TableFormAnswerModel):
    rows = args.input.replyRows
    save_rows = []
    for u, r in rows.items():
        user = User.get_by_name(u)
        save_rows.append({'user': user.id, 'fields': r})

    web = {}
    result = {'web': web}
    savedata = save_rows
    result["savedata"] = savedata
    web['result'] = "saved"
    return jsonify(result)


@tableForm_plugin.route('/reqs')
def reqs():
    templates = ["""
``` {#tableForm_table plugin="tableForm"}
# Add attribute 'showInView: true' to show the plugin in normal view
groups: 
 - Group Name   # Use Group Name here
fields:
 - d1=demo1     # List your fields here, = for alias
table: true
report: false
openButtonText: Avaa taulukko # text for open the table if closed as default
hideButtonText: Sulje taulukko # tex for closing the table
open: true 
maxRows: 40em     # max height for the table before scrollbar 
realnames: true   # Show full name in 2nd column, true or false
buttonText:       # Name your save button here
autosave: true    # autosave, true or false
cbColumn: true    # show checkboxes
nrColumn: true    # show numbers
filterRow: true   # show filters 
singleLine: true  #
emailUsersButtonText: "Lähetä sähköpostia valituille" # if one wants to send email
```""", """
``` {#tableForm_table_report plugin="tableForm"}
# Add attribute 'showInView: true' to show the plugin in normal view
groups: 
 - Group Name   # Use Group Name here
fields:
 - d1=demo1     # List your fields here, = for alias
table: true
report: true
openButtonText: Avaa taulukko # text for open the table if closed as default
hideButtonText: Sulje taulukko # tex for closing the table
open: true 
maxRows: 40em     # max height for the table before scrollbar 
realnames: true   # Show full name in 2nd column, true or false
buttonText:       # Name your save button here
autosave: true    # autosave, true or false
cbColumn: true    # show checkboxes
nrColumn: true    # show numbers
filterRow: true   # show filters 
singleLine: true  #
emailUsersButtonText: "Lähetä sähköpostia valituille" # if one wants to send email 
separator: ";"  # Define your value separator here, ";" as default
anonNames: false # To show or hide user (and full) names in report, true or false
reportButton: "Name your generate report button here"
```""", """
``` {#tableForm_report plugin="tableForm"}
# Add attribute 'showInView: true' to show the plugin in normal view
groups: 
 - Group Name   # Use Group Name here
fields:
 - d1=demo1     # List your fields here, = for alias
table: false
report: true
separator: ";"  # Define your value separator here, ";" as default
anonNames: false # To show or hide user (and full) names in report, true or false
reportButton: "Name your generate report button here"
```"""]
    editor_tabs = [
            {
                'text': 'Fields',
                'items': [
                    {
                        'text': 'Tables',
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
    return jsonify({
        "js": ["tableForm"],
        "multihtml": True,
        'editor_tabs': editor_tabs,
    },
    )
