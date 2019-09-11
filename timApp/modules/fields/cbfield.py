"""
TIM plugin: a checkbox field
"""
from dataclasses import dataclass, asdict
from flask import jsonify, render_template_string, Blueprint, request
from webargs.flaskparser import use_args

from common_schemas import TextfieldStateModel
from marshmallow_dataclass import class_schema
from pluginserver_flask import GenericHtmlModel, \
    render_multihtml
from textfield import TextfieldAnswerModel, TextfieldAnswerSchema, TextfieldInputModel, \
    TextfieldMarkupModel

cbfield_route = Blueprint('cb', __name__, url_prefix="/cb")


@dataclass
class CbfieldHtmlModel(GenericHtmlModel[TextfieldInputModel, TextfieldMarkupModel, TextfieldStateModel]):
    def get_component_html_name(self) -> str:
        return 'cbfield-runner'

    def get_static_html(self) -> str:
        return render_static_cdfield(self)


def render_static_cdfield(m: CbfieldHtmlModel):
    return render_template_string("""
<div>
<h4>{{ header or '' }}</h4>
<p class="stem">{{ stem or '' }}</p>
<div><label>{{ inputstem or '' }} <span>
<input type="checkbox"
class="form-control"
placeholder="{{ inputplaceholder or '' }}"
size="{{cols}}"></span></label>
</div>
<a>{{ resetText }}</a>
<p class="plgfooter">{{ '' }}</p>
</div>""".strip(),
        **asdict(m.markup),
    )


CbfieldHtmlSchema = class_schema(CbfieldHtmlModel)


@cbfield_route.route('/multihtml/', methods=['post'])
def cb_multihtml():
    ret = render_multihtml(request.get_json(), CbfieldHtmlSchema())
    return ret


@cbfield_route.route('/answer/', methods=['put'])
@use_args(TextfieldAnswerSchema(), locations=("json",))
def cb_answer(args: TextfieldAnswerModel):
    web = {}
    result = {'web': web}
    c = args.input.c

    nosave = args.input.nosave

    if not nosave:
        save = {"c": c}
        result["save"] = save
        web['result'] = "saved"

    return jsonify(result)


@cbfield_route.route('/reqs/')
@cbfield_route.route('/reqs')
def cb_reqs():
    """Introducing templates for cbfield plugin"""
    return jsonify({
        "js": ["/field/js/build/cbfield.js"],
        "css": ["/field/css/field.css"],
        "multihtml": True,
        'editor_tabs': [
            {
                'text': 'Fields',
                'items': [
                    {
                        'text': 'Check/Radio/Drop',
                        'items': [
                            {
                                'data': '#- {defaultplugin="cbfield" readonly="view" }\n',
                                'text': 'defaultplugin/cbfield',
                                'expl': 'Attribuutit kappaleelle jossa inline ruksi-kenttä (cbfield)',
                            },
                            {
                                'data': 'cbfiled',
                                'text': 'teksti: cbfield',
                                'expl': 'Pelkkä kentän tyyppi: cbfield',
                            },
                            {
                                'data': "{#cb1 #}",
                                'text': 'Checkbox (inline, autosave)',
                                'expl': 'Luo yhden ruksinkenttä',
                            }]
                    },
                ],
            },
        ],
    },
    )
