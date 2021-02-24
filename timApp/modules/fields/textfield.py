"""
TIM plugin: a textfield
"""
from dataclasses import dataclass, asdict
from typing import Union, List, Any, Dict

from flask import render_template_string
from marshmallow.utils import missing

from tim_common.common_schemas import TextfieldStateModel
from tim_common.markupmodels import GenericMarkupModel
from tim_common.pluginserver_flask import GenericHtmlModel, \
    GenericAnswerModel, create_blueprint, PluginAnswerResp, PluginAnswerWeb, PluginReqs
from tim_common.utils import Missing


@dataclass
class TextfieldMarkupModel(GenericMarkupModel):
    autosave: Union[bool, Missing] = missing
    autogrow: Union[bool, Missing] = missing
    autoUpdateTables: Union[bool, Missing] = True
    clearstyles: Union[bool, Missing] = missing
    cols: Union[int, Missing] = missing
    errormessage: Union[str, Missing, None] = missing
    form: Union[bool, Missing] = missing
    rows: Union[int, Missing] = missing
    ignorestyles: Union[bool, Missing] = missing
    initword: Union[str, Missing, None] = missing
    inputplaceholder: Union[str, Missing, None] = missing
    inputstem: Union[str, Missing, None] = missing
    nosave: Union[bool, Missing] = missing
    points_array: Union[List[List[float]], Missing] = missing
    readOnlyStyle: Union[str, Missing, None] = missing
    showname: Union[int, Missing, None] = missing
    tag: Union[str, Missing, None] = missing
    textarea: Union[bool, Missing] = missing
    validinput: Union[str, Missing, None] = missing


@dataclass
class TextfieldInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    c: str
    nosave: Union[bool, Missing] = missing


@dataclass
class TextfieldHtmlModel(GenericHtmlModel[TextfieldInputModel, TextfieldMarkupModel, TextfieldStateModel]):
    def get_component_html_name(self) -> str:
        return 'textfield-runner'

    def get_static_html(self) -> str:
        return render_static_textfield(self)


@dataclass
class TextfieldAnswerModel(GenericAnswerModel[TextfieldInputModel, TextfieldMarkupModel, TextfieldStateModel]):
    pass


def render_static_textfield(m: TextfieldHtmlModel) -> str:
    return render_template_string("""
<div>
<h4>{{ header or '' }}</h4>
<p class="stem">{{ stem or '' }}</p>
<div><label>{{ inputstem or '' }} <span>
<input type="text"
class="form-control"
placeholder="{{ inputplaceholder or '' }}"
size="{{cols}}"></span></label>
</div>
<button class="timButton">
{{ buttonText or button or "Save" }}
</button>
<a>{{ resetText }}</a>
<p class="plgfooter">{{ '' }}</p>
</div>""".strip(),
        **asdict(m.markup),
    )


class TextfieldAnswerWeb(PluginAnswerWeb, total=False):
    clear: bool


def answer(args: TextfieldAnswerModel) -> PluginAnswerResp:
    web: TextfieldAnswerWeb = {}
    result: PluginAnswerResp = {'web': web}
    c = args.input.c


    nosave = args.input.nosave
    if args.markup.nosave:
        nosave = True

    if not nosave:
        save: Dict[str, Any] = {"c": c}
        if not args.markup.clearstyles and args.state is not None:
            if args.state.styles:
                save = {"c": c, "styles": args.state.styles}
        result["save"] = save
        web['result'] = "saved"
        if args.markup.clearstyles:
            web['clear'] = True

    return result


def reqs() -> PluginReqs:
    templates = [
"""``` {#PLUGINNAMEHERE plugin="textfield"}
header:          # otsikko, tyhjä = ei otsikkoa
stem:            # kysymys, tyhjä = ei kysymystä
inputstem:       # vastaus, tyhjä = ei vastausta
tag:        # seurantaid, tyhjä = ei seurantaid:tä
initword:        # alkuarvo, tyhjä = ei alkuarvoa
buttonText: Save # PAINIKKEEN NIMI, TYHJÄ = EI PAINIKETTA
cols: 7          # kentän koko, numeraalinen
autosave: false  # autosave, pois päältä
validinput: '^(hyv|hyl|[12345])$' # käyttäjäsyötteen rajoitin, tyhjä = ei rajoitusta
errormessage:    #inputcheckerin virheselite, tyhjä = selite on inputchecker
```""",
]
    return {
        "js": ["/field/js/build/textfield.js"],
        "multihtml": True,
        "css": ["/field/css/field.css"],
        'editor_tabs': [
            {
                'text': 'Fields',
                'items': [
                    {
                        'text': 'Text',
                        'items': [
                            {
                                'data': '#- {defaultplugin="textfield" readonly="view" .fieldCell}\n',
                                'text': 'defaultplugin/textfield',
                                'expl': 'Attribuutit kappaleelle jossa inline textfield',
                            },
                            {
                                'data': 'textfield',
                                'text': 'teksti: textfield',
                                'expl': 'Pelkkä kentän tyyppi: textfield',
                            },
                            {
                                'data': "%% 'd;dsum' | gfrange(1,5,'cols: 5') %%\n",
                                'text': 'Joukko numeroituja kenttiä',
                                'expl': 'Valmis joukko samannimisiä numeroituja kenttiä',
                            },
                            {
                                'data': "{#tf1#}",
                                'text': 'Tekstikenttä (inline, autosave)',
                                'expl': 'Luo kenttä jonka syöte on tekstiä',
                            },
                            {
                                'data': templates[0].strip(),
                                'text': 'Tekstikenttä (laajennettu)',
                                'expl': 'Luo kenttä jonka syöte on tekstiä',
                            },
                            {
                                'data': "{#tf2 readOnlyStyle: plaintext #}",
                                'text': 'Label kenttä (inline, read only)',
                                'expl': 'Luo kenttä jonka syötettä käyttäjä ei voi muokata',
                            },
                            {
                                'data': "{#username showname: 1, inputstem: 'Nimi: '#}",
                                'text': 'Käyttäjän nimi (inline)',
                                'expl': 'Kenttä joka näyttää käyttäjän nimen, tarvitsee lohkon alkuun defaultplugin',
                            },
                        ],
                    },
                ],
            },
        ],
    }


textfield_route = create_blueprint(
    __name__,
    'tf',
    TextfieldHtmlModel,
    TextfieldAnswerModel,
    answer,
    reqs,
)
