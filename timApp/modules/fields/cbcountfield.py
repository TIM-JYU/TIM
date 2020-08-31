"""
TIM plugin: a checkbox field
"""
from typing import Union, Dict

from dataclasses import dataclass, asdict
from flask import jsonify, render_template_string, Blueprint, request
from webargs.flaskparser import use_args

from common_schemas import TextfieldStateModel
from marshmallow_dataclass import class_schema
from marshmallow.utils import missing
from pluginserver_flask import GenericHtmlModel, \
    render_multihtml, \
    create_blueprint

from timApp.modules.fields.textfield import TextfieldAnswerModel, TextfieldAnswerSchema, TextfieldInputModel, \
    TextfieldMarkupModel

from timApp.tim_app import csrf

from utils import Missing


# cbcountfield_route = Blueprint('cb', __name__, url_prefix="/cbc")

@dataclass
class CbcountfieldMarkupModel(TextfieldMarkupModel):
    count: Union[int, Missing] = 12


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
        r['markup']['count'] = 11
        return r


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

cbcountfield_route = create_blueprint(__name__, 'cbcountfield', CbcountfieldHtmlSchema, csrf)

@cbcountfield_route.route('/multihtml', methods=['post'])
def cb_multihtml():
    ret = render_multihtml(request.get_json(), CbcountfieldHtmlSchema())
    return ret


@cbcountfield_route.route('/answer', methods=['put'])
@csrf.exempt
@use_args(TextfieldAnswerSchema(), locations=("json",))
def cb_answer(args: TextfieldAnswerModel):
    web = {}
    result = {'web': web}
    c = args.input.c

    count = 14
    nosave = args.input.nosave

    if not nosave:
        save = {"c": c}
        result["save"] = save
        web['result'] = "saved"
        web['count'] = count

    return jsonify(result)


@cbcountfield_route.route('/reqs')
def cb_reqs():
    return jsonify({
        "js": ["cbcountfield"],
        "css": ["/field/css/field.css"],
        "multihtml": True,
    },
    )
