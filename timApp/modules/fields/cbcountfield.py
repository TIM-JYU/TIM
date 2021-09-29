"""
TIM plugin: a checkbox field
"""
from dataclasses import dataclass, asdict
from typing import Union

from flask import render_template_string
from marshmallow.utils import missing

from timApp.document.docentry import DocEntry
from timApp.document.viewcontext import default_view_ctx
from timApp.modules.fields.textfield import TextfieldMarkupModel
from timApp.plugin.taskid import TaskId
from timApp.tim_app import csrf
from timApp.user.user import User
from timApp.util.get_fields import get_fields_and_users, RequestedGroups, GetFieldsAccess, FieldValue
from tim_common.common_schemas import TextfieldStateModel
from tim_common.pluginserver_flask import GenericHtmlModel, \
    create_blueprint, GenericAnswerModel, PluginAnswerWeb, PluginAnswerResp, PluginReqs
from tim_common.utils import Missing


@dataclass
class CbcountfieldMarkupModel(TextfieldMarkupModel):
    groups: Union[list[str], Missing] = missing
    limit: Union[int, Missing] = missing


@dataclass
class TextfieldInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    c: str
    nosave: Union[bool, Missing] = missing


@dataclass
class CbcountfieldHtmlModel(GenericHtmlModel[TextfieldInputModel, CbcountfieldMarkupModel, TextfieldStateModel]):
    def get_component_html_name(self) -> str:
        return 'cbcountfield-runner'

    def get_static_html(self) -> str:
        return render_static_cdfield(self)

    def get_browser_json(self) -> dict:
        r = super().get_browser_json()
        count, _ = get_checked_count(self.markup, self.taskID, self.current_user_id)
        r['count'] = count
        return r


@dataclass
class CbcountfieldAnswerModel(GenericAnswerModel[TextfieldInputModel, CbcountfieldMarkupModel, TextfieldStateModel]):
    pass


def render_static_cdfield(m: CbcountfieldHtmlModel) -> str:
    return render_template_string("""
<div>
<h4>{{ header or '' }}</h4>
<p class="stem">{{ stem or '' }}</p>
<div><label>{{ inputstem or '' }} <span>
<input type="checkbox"
class="xform-control"
placeholder="{{ inputplaceholder or '' }}"
size="{{ cols or '' }}"></span></label>
</div>
<a>{{ resetText or '' }}</a>
<p class="plgfooter">{{ '' }}</p>
</div>""".strip(),
        **asdict(m.markup),
    )


class CbAnswerWeb(PluginAnswerWeb, total=False):
    count: int
    new: int


class CbAnswerResp(PluginAnswerResp, total=False):
    pass


def cb_answer(args: CbcountfieldAnswerModel) -> CbAnswerResp:
    web: CbAnswerWeb = {}
    result = CbAnswerResp(web=web)
    c = args.input.c

    count, previous = get_checked_count(args.markup, args.taskID, args.info.primary_user)

    # Take the current answer into account.
    if previous is None:
        previous = '0'
    if previous != c:
        if c == '1':
            count += 1
        else:
            count -= 1
    nosave = args.input.nosave

    limit = args.markup.limit
    if isinstance(limit, int):
        limit = int(limit)  # not needed?
        if count > limit:
            web['result'] = "error"
            web['count'] = limit
            web['new'] = 0
            return result

    if not nosave:
        save = {"c": c}
        result["save"] = save
        web['result'] = "saved"
        web['count'] = count
        web['new'] = 1 if c == '1' else 0

    return result


def get_checked_count(markup: CbcountfieldMarkupModel, task_id: str, user_id: str) -> tuple[int, FieldValue]:
    groups = ['*']
    if isinstance(markup.groups, list):
        groups = markup.groups
    doc_id = TaskId.parse_doc_id(task_id)
    curr_user = User.get_by_name(user_id)
    assert curr_user is not None, f'Could not find user {user_id}'
    d = DocEntry.find_by_id(doc_id)
    if not d:
        return 0, None  # TODO handle error properly
    user_fields, _, _, _ = get_fields_and_users(
        [task_id],
        RequestedGroups.from_name_list(groups),
        d,
        curr_user,
        default_view_ctx,
        access_option=GetFieldsAccess.AllowAlwaysNonTeacher,
    )

    count = 0
    previous = None
    for u in user_fields:
        fs = u['fields']
        val = fs[task_id]
        if val == '1':
            count += 1
        if curr_user == u['user']:
            previous = val
    return count, previous


def cb_reqs() -> PluginReqs:
    return {
        "js": ["cbcountfield"],
        "css": ["/field/css/field.css"],
        "multihtml": True,
    }


cbcountfield_route = create_blueprint(
    __name__,
    'cbcountfield',
    CbcountfieldHtmlModel,
    CbcountfieldAnswerModel,
    cb_answer,
    cb_reqs,
    csrf,
)
