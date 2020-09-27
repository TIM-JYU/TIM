"""
TIM plugin: a radiobutton field
"""
from dataclasses import dataclass, asdict

from flask import render_template_string

from common_schemas import TextfieldStateModel
from pluginserver_flask import GenericHtmlModel, \
    create_blueprint, PluginAnswerResp, PluginAnswerWeb, PluginReqs
from textfield import TextfieldAnswerModel, TextfieldInputModel, \
    TextfieldMarkupModel


@dataclass
class RbfieldHtmlModel(GenericHtmlModel[TextfieldInputModel, TextfieldMarkupModel, TextfieldStateModel]):
    def get_component_html_name(self) -> str:
        return 'rbfield-runner'

    def get_static_html(self) -> str:
        return render_static_rbfield(self)


def render_static_rbfield(m: RbfieldHtmlModel) -> str:
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
        **asdict(m.markup),
    )


def rb_answer(args: TextfieldAnswerModel) -> PluginAnswerResp:
    web: PluginAnswerWeb = {}
    result: PluginAnswerResp = {'web': web}
    c = args.input.c

    nosave = args.input.nosave

    if not nosave:
        save = {"c": c}
        result["save"] = save
        web['result'] = "saved"

    return result


def rb_reqs() -> PluginReqs:
    return {
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
                                'data': '#- {defaultplugin="rbfield" }\n',
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
    }


rbfield_route = create_blueprint(
    __name__,
    'rb',
    RbfieldHtmlModel,
    TextfieldAnswerModel,
    rb_answer,
    rb_reqs,
)
