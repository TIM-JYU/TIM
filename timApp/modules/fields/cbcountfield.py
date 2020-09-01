"""
TIM plugin: a checkbox field
"""
from dataclasses import dataclass, asdict
from typing import Union, Dict, List

from flask import jsonify, render_template_string, request
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from common_schemas import TextfieldStateModel
from marshmallow_dataclass import class_schema
from pluginserver_flask import GenericHtmlModel, \
    render_multihtml, \
    create_blueprint, GenericAnswerModel, InfoModel
from timApp.document.docentry import DocEntry
from timApp.modules.fields.textfield import TextfieldMarkupModel
from timApp.plugin.taskid import TaskId
from timApp.tim_app import csrf
from timApp.user.user import User
from timApp.util.get_fields import get_fields_and_users, RequestedGroups, GetFieldsAccess
from utils import Missing


@dataclass
class CbcountfieldMarkupModel(TextfieldMarkupModel):
    groups: Union[List[str], Missing] = missing


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

    def get_browser_json(self) -> Dict:
        r = super().get_browser_json()
        if self.info:
            count, _ = get_checked_count(self.markup, self.taskID, self.info)
            r['count'] = count
        return r


@dataclass
class CbcountfieldAnswerModel(GenericAnswerModel[TextfieldInputModel, CbcountfieldMarkupModel, TextfieldStateModel]):
    pass



def render_static_cdfield(m: CbcountfieldHtmlModel):
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


CbcountfieldHtmlSchema = class_schema(CbcountfieldHtmlModel)
CbcountfieldAnswerSchema = class_schema(CbcountfieldAnswerModel)

cbcountfield_route = create_blueprint(__name__, 'cbcountfield', CbcountfieldHtmlSchema, csrf)

@cbcountfield_route.route('/multihtml', methods=['post'])
def cb_multihtml():
    ret = render_multihtml(request.get_json(), CbcountfieldHtmlSchema())
    return ret


@cbcountfield_route.route('/answer', methods=['put'])
@csrf.exempt
@use_args(CbcountfieldAnswerSchema(), locations=("json",))
def cb_answer(args: CbcountfieldAnswerModel):
    web = {}
    result = {'web': web}
    c = args.input.c

    count, previous = get_checked_count(args.markup, args.taskID, args.info)

    # Take the current answer into account.
    if previous is None:
        previous = '0'
    if previous != c:
        if c == '1':
            count += 1
        else:
            count -= 1
    nosave = args.input.nosave

    if not nosave:
        save = {"c": c}
        result["save"] = save
        web['result'] = "saved"
        web['count'] = count

    return jsonify(result)


def get_checked_count(markup: CbcountfieldMarkupModel, task_id: str, info: InfoModel):
    groups = ['*']
    if isinstance(markup.groups, list):
        groups = markup.groups
    tid = TaskId.parse(task_id)
    curr_user = User.get_by_name(info.user_id)
    user_fields, _, _, _ = get_fields_and_users(
        [task_id],
        RequestedGroups.from_name_list(groups),
        DocEntry.find_by_id(tid.doc_id),
        curr_user,
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


@cbcountfield_route.route('/reqs')
def cb_reqs():
    return jsonify({
        "js": ["cbcountfield"],
        "css": ["/field/css/field.css"],
        "multihtml": True,
    },
    )
