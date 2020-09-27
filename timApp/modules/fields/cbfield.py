"""
TIM plugin: a checkbox field
"""
from dataclasses import dataclass, asdict

from flask import render_template_string

from common_schemas import TextfieldStateModel
from pluginserver_flask import GenericHtmlModel, \
    create_blueprint, PluginAnswerResp, PluginAnswerWeb, PluginReqs
from textfield import TextfieldAnswerModel, TextfieldInputModel, \
    TextfieldMarkupModel


@dataclass
class CbfieldHtmlModel(GenericHtmlModel[TextfieldInputModel, TextfieldMarkupModel, TextfieldStateModel]):
    def get_component_html_name(self) -> str:
        return 'cbfield-runner'

    def get_static_html(self) -> str:
        return render_static_cdfield(self)


def render_static_cdfield(m: CbfieldHtmlModel) -> str:
    return render_template_string("""
<div>
<h4>{{ header or '' }}</h4>
<p class="stem">{{ stem or '' }}</p>
<div><label>{{ inputstem or '' }} <span>
<input type="checkbox"
class="form-control"
placeholder="{{ inputplaceholder or '' }}"
size="{{cols or ''}}"></span></label>
</div>
<a>{{ resetText or ''}}</a>
<p class="plgfooter">{{ '' }}</p>
</div>""".strip(),
                                  **asdict(m.markup),
                                  )


def cb_answer(args: TextfieldAnswerModel) -> PluginAnswerResp:
    web: PluginAnswerWeb = {}
    result: PluginAnswerResp = {'web': web}
    c = args.input.c

    nosave = args.input.nosave

    if not nosave:
        save = {"c": c}
        result["save"] = save
        web['result'] = "saved"

    return result


def cb_reqs() -> PluginReqs:
    return {
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
                                'data': '#- {defaultplugin="cbfield"}\n',
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
    }


cbfield_route = create_blueprint(
    __name__,
    'cb',
    CbfieldHtmlModel,
    TextfieldAnswerModel,
    cb_answer,
    cb_reqs,
)
