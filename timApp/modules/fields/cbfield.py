"""
TIM plugin: a checkbox field
"""
import attr
from flask import jsonify, render_template_string, Blueprint
from marshmallow import fields, post_load
from webargs.flaskparser import use_args

from common_schemas import TextfieldStateModel
from pluginserver_flask import GenericHtmlSchema, GenericHtmlModel, \
    InfoSchema, render_multihtml
from textfield import TextfieldAnswerModel, TextfieldAnswerSchema, TextfieldInputModel, \
    TextfieldMarkupModel, TextfieldAttrs

cbfield_route = Blueprint('cb', __name__, url_prefix="/cb")


@attr.s(auto_attribs=True)
class CbfieldHtmlModel(GenericHtmlModel[TextfieldInputModel, TextfieldMarkupModel, TextfieldStateModel]):
    def get_component_html_name(self) -> str:
        return 'cbfield-runner'

    def get_static_html(self) -> str:
        return render_static_cdfield(self)


class CbfieldHtmlSchema(TextfieldAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data, **kwargs):
        # noinspection PyArgumentList
        return CbfieldHtmlModel(**data)


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
        **attr.asdict(m.markup),
    )


CB_FIELD_HTML_SCHEMA = CbfieldHtmlSchema()

# @app.route('/cb/multihtml/', methods=['post'])
# @use_args(CbfieldHtmlSchema(many=True), locations=("json",))
@cbfield_route.route('/multihtml/', methods=['post'])
@use_args(GenericHtmlSchema(many=True), locations=("json",))
def cb_multihtml(args):  # args: List[GenericHtmlSchema]):
    ret = render_multihtml(CB_FIELD_HTML_SCHEMA, args)
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
