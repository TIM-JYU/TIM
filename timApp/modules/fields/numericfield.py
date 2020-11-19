"""
TIM plugin: a numericfield
"""

import re
from dataclasses import dataclass, asdict
from typing import Union, List, Any, Dict

from flask import render_template_string
from marshmallow.utils import missing

from common_schemas import TextfieldStateModel
from markupmodels import GenericMarkupModel
from pluginserver_flask import GenericHtmlModel, \
    GenericAnswerModel, create_blueprint, PluginAnswerResp, PluginAnswerWeb, PluginReqs
from utils import Missing

NumericfieldStateModel = TextfieldStateModel


@dataclass
class NumericfieldMarkupModel(GenericMarkupModel):
    arrows: Union[bool, Missing] = missing
    autosave: Union[bool, Missing] = missing
    autoUpdateTables: Union[bool, Missing] = True
    clearstyles: Union[bool, Missing] = missing
    cols: Union[int, Missing] = missing
    errormessage: Union[str, Missing, None] = missing
    form: Union[bool, Missing] = missing
    ignorestyles: Union[bool, Missing] = missing
    initnumber: Union[float, Missing, None] = missing
    inputplaceholder: Union[int, Missing, None] = missing
    inputstem: Union[str, Missing, None] = missing
    nosave: Union[bool, Missing] = missing
    points_array: Union[List[List[str]], Missing] = missing
    readOnlyStyle: Union[str, Missing, None] = missing
    save: Union[str, Missing, None] = missing  # TODO default 'double' or missing?
    step: Union[float, Missing, None] = missing
    tag: Union[str, Missing, None] = missing
    validinput: Union[str, Missing, None] = missing
    verticalkeys: Union[bool, Missing] = missing
    wheel: Union[bool, Missing] = missing


@dataclass
class NumericfieldInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    c: Union[str, Missing, None] = missing
    nosave: Union[bool, Missing] = missing


@dataclass
class NumericfieldHtmlModel(GenericHtmlModel[NumericfieldInputModel, NumericfieldMarkupModel, NumericfieldStateModel]):
    def get_component_html_name(self) -> str:
        return 'numericfield-runner'

    def get_static_html(self) -> str:
        return render_static_numericfield(self)

    def get_md(self) -> str:
        return "___"


@dataclass
class NumericfieldAnswerModel(GenericAnswerModel[NumericfieldInputModel,
                                                 NumericfieldMarkupModel, NumericfieldStateModel]):
    pass


def render_static_numericfield(m: NumericfieldHtmlModel) -> str:
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


REDOUBLE = re.compile(r"[^0-9,.e\-+]+")

def get_double(c: Union[float, int, str]) -> float:
    if isinstance(c, float):
        return c
    if isinstance(c, int):
        return c
    if isinstance(c, str):
        c = REDOUBLE.sub("", c)
        c = c.replace(",", ".")
        if c.startswith("e"):
            c = "1"+c
        return float(c)
    return 0


class NumericFieldAnswerWeb(PluginAnswerWeb, total=False):
    clear: bool
    value: Union[str, float, int]


def answer(args: NumericfieldAnswerModel) -> PluginAnswerResp:
    web: NumericFieldAnswerWeb = {}
    result: PluginAnswerResp = {'web': web}
    c_input = args.input.c

    nosave = args.input.nosave
    if args.markup.nosave:
        nosave = True

    if isinstance(c_input, Missing) or c_input is None:
        web['result'] = "unsaved"
        web['error'] = "Please enter a number"
        return result
    try:
        if c_input.strip() == "":
            c: Union[str, float, int] = ""
        elif args.markup.save == "double" or not args.markup.save:
            c = get_double(c_input)
        elif args.markup.save == "int":
            c = get_double(c_input)
            c = int(c)
        elif args.markup.save == "round":
            c = get_double(c_input)
            c = round(c, 0)
        else:
            raise ValueError(f'Unknown save attribute value: {args.markup.save}')
    except ValueError as e:
        error = str(e)
        web['result'] = "error"
        web['error'] = error + " " + c_input
        return result

    if not nosave:
        save: Dict[str, Any] = {"c": c}
        if not args.markup.clearstyles and args.state is not None:
            if args.state.styles:
                save = {"c": c, "styles": args.state.styles}
        result["save"] = save
        web['result'] = "saved"
        web['value'] = c
        if args.markup.clearstyles:
            web['clear'] = True

    return result


def reqs() -> PluginReqs:
    templates = ["""``` {#PLUGINNAMEHERE plugin="numericfield"}
header:          # otsikko, tyhjä = ei otsikkoa
stem:            # kysymys, tyhjä = ei kysymystä
step:            # numeraalinen askellus, tyhjä = oletus 1.0
inputstem:       # vastaus, tyhjä = ei vastausta
tag:        # seurantaid, tyhjä = ei seurantaid:tä
initnumber:      # alkuarvo, tyhjä = ei alkuarvoa
buttonText: Save # painikkeen nimi, tyhjä = ei painiketta
cols: 7          # kentän koko, numeraalinen
autosave: false  # autosave, pois päältä
validinput: '^\d{0,3}(\.\d{0,3})?$' # käyttäjäsyötteen rajoitin, tyhjä = ei rajoitusta
errormessage:    # inputcheckerin virheselite, tyhjä = selite on inputchecker
```""", """#- {defaultplugin="numericfield" readonly="view" .fieldCell}
%% 'd=;dsum=summa' | gfrange(1,5,'cols: 3') %%
```""", """#- {defaultplugin="numericfield" readonly="view" .fieldCell}
%% 'ht=Harhoitustyö;osa=Osasuoritus' |  gfields('cols: 3')  %%
""", ]
    return {
        "js": ["/field/js/build/numericfield.js"],
        "multihtml": True,
        "multimd": True,
        "css": ["/field/css/field.css"],
        'editor_tabs': [
            {
                'text': 'Fields',
                'items': [
                    {
                        'text': 'Numeric',
                        'items': [
                            {
                                'data': '#- {defaultplugin="numericfield" readonly="view" .fieldCell}\n',
                                'text': 'defaultplugin/numericfield',
                                'expl': 'Attribuutit kappaleelle jossa inline numericfield',
                            },
                            {
                                'data': 'numericfield',
                                'text': 'teksti: numericfield',
                                'expl': 'Pelkkä kentän tyyppi: numericfield',
                            },
                            {
                                'data': "%% 'd;dsum' | gfrange(1,5,'cols: 3') %%\n",
                                'text': 'Joukko kenttiä',
                                'expl': 'Valmis joukko samannimisiä kenttä',
                            },
                            {
                                'data': '{#nf1 #}',
                                'text': 'Numeerinen kenttä (inline, autosave)',
                                'expl': 'Luo kenttä jonka syötteet ovat vain numeroita',
                            },
                            {
                                'data': templates[0].strip(),
                                'text': 'Numeerinen kenttä (laajennettu)',
                                'expl': 'Luo kenttä jonka syöte ovat vain numero',
                            },
                            {
                                'data': "{#nf3 autosave: false, readOnlyStyle: plaintext #}",
                                'text': 'Label kenttä (read only)',
                                'expl': 'Luo kenttä jonka syötettä käyttäjä ei voi muokata',
                            },
                            {
                                'data': templates[1].strip() + '\n',
                                'text': 'Joukko numeroituja numeerisia kenttiä ja summa',
                                'expl': 'Lohko jossa joukko numeroituja numeerisia kenttiä ja niiden summa',
                            },
                            {
                                'data': templates[2].strip() + '\n',
                                'text': 'Joukko erikseen nimettyjä numeerisia kenttiä',
                                'expl': 'Lohko jossa eri tavoin nimettyjä numeerisia kenttiä',
                            },
                        ],
                    },
                    {
                        'text': 'Settings',
                        'items': [
                            {
                                'data': 'autosave: false,',
                                'text': 'autosave: false',
                                'expl': 'ei automaattista tallennusta',
                            },
                            {
                                'data': 'readonly="view"',
                                'text': 'readonly="view"',
                                'expl': 'kenttää ei voi muokata view-näkymässä, tämä lohkon otsikkoon',
                            },
                            {
                                'data': 'visible="%% False | isview%%" nocache="true"',
                                'text': 'lohko ei näy View-näkymässä',
                                'expl': 'tällä merkitään lohko, jonka ei haluta näkyvän View-näkymässä',
                            },
                            {
                                'data': 'cols: 4,',
                                'text': 'Sarakkeiden määrä',
                                'expl': 'Sarakkeiden määrä',
                            },
                        ],
                    },
                ],
            },
        ],
    }


numericfield_route = create_blueprint(
    __name__,
    'nf',
    NumericfieldHtmlModel,
    NumericfieldAnswerModel,
    answer,
    reqs,
)
