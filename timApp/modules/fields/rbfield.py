"""
TIM plugin: a radiobutton field
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

rbfield_route = Blueprint('rb', __name__, url_prefix="/rb")


@attr.s(auto_attribs=True)
class RbfieldHtmlModel(GenericHtmlModel[TextfieldInputModel, TextfieldMarkupModel, TextfieldStateModel]):
    def get_component_html_name(self) -> str:
        return 'rbfield-runner'

    def get_static_html(self) -> str:
        return render_static_rbfield(self)


class RbfieldHtmlSchema(TextfieldAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data, **_):
        # noinspection PyArgumentList
        return RbfieldHtmlModel(**data)


def render_static_rbfield(m: RbfieldHtmlModel):
    return render_template_string("""
<div>
<h4>{{ header or '' }}</h4>
<p class="stem">{{ stem or '' }}</p>
<div><label>{{ inputstem or '' }} <span>
<input type="radio"
class="form-control"
placeholder="{{ inputplaceholder or '' }}"
size="{{cols}}"></span></label>
</div>
<a>{{ resetText }}</a>
<p class="plgfooter">{{ '' }}</p>
</div>""".strip(),
        **attr.asdict(m.markup),
    )


# register_routes(app, CbfieldHtmlSchema(), '/cb')


RB_FIELD_HTML_SCHEMA = RbfieldHtmlSchema()


@rbfield_route.route('/multihtml/', methods=['post'])
@use_args(GenericHtmlSchema(many=True), locations=("json",))
def rb_multihtml(args):  # args: List[GenericHtmlSchema]):
    ret = render_multihtml(args)
    return ret


@rbfield_route.route('/answer/', methods=['put'])
@use_args(TextfieldAnswerSchema(), locations=("json",))
def rb_answer(args: TextfieldAnswerModel):
    web = {}
    result = {'web': web}
    c = args.input.c

    nosave = args.input.nosave

    if not nosave:
        save = {"c": c}
        result["save"] = save
        web['result'] = "saved"

    return jsonify(result)


@rbfield_route.route('/reqs/')
@rbfield_route.route('/reqs')
def rb_reqs():
    """Introducing templates for cbfield plugin"""
    return jsonify({
        "js": ["/field/js/build/rbfield.js"],
        "css": ["/field/css/field.css"],
        "multihtml": True,
        "multimd": True,
        'editor_tabs': [
            {
                'text': 'Fields',
                'items': [
                    {
                        'text': 'Check/Radio/Drop',
                        'items': [
                            {
                                'data': '#- {defaultplugin="rbfield" readonly="view" }\n',
                                'text': 'defaultplugin/rbfield',
                                'expl': 'Attribuutit kappaleelle jossa inline pallukka-kenttä (rbfield)',
                            },
                            {
                                'data': 'rbfield',
                                'text': 'teksti: rbfield',
                                'expl': 'Pelkkä kentän tyyppi: rbfield',
                            },
                            {
                                'data': "{#rb1 #}",
                                'text': 'Radiobutton (inline, autosave)',
                                'expl': 'Luo yhden pallukkakentän',
                            }]
                    },
                ],
            },
        ],
    },
    )
