"""
TIM plugin: a numericfield
"""

import re
from dataclasses import dataclass, asdict
from typing import Union, Any

from flask import render_template_string
from marshmallow.utils import missing

from tim_common.common_schemas import TextfieldStateModel
from tim_common.markupmodels import GenericMarkupModel
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    GenericAnswerModel,
    create_blueprint,
    PluginAnswerResp,
    PluginAnswerWeb,
    PluginReqs,
)
from tim_common.utils import Missing

NumericfieldStateModel = TextfieldStateModel


@dataclass
class NumericfieldMarkupModel(GenericMarkupModel):
    arrows: bool | Missing = missing
    autosave: bool | Missing = missing
    autoUpdateTables: bool | Missing = True
    clearstyles: bool | Missing = missing
    cols: int | Missing = missing
    errormessage: str | Missing | None = missing
    ignorestyles: bool | Missing = missing
    initnumber: float | Missing | None = missing
    inputplaceholder: int | Missing | None = missing
    inputstem: str | Missing | None = missing
    nosave: bool | Missing = missing
    points_array: list[list[str]] | Missing = missing
    readOnlyStyle: str | Missing | None = missing
    save: str | Missing | None = missing  # TODO default 'double' or missing?
    step: float | Missing | None = missing
    tag: str | Missing | None = missing
    validinput: str | Missing | None = missing
    verticalkeys: bool | Missing = missing
    wheel: bool | Missing = missing


@dataclass
class NumericfieldInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""

    c: str | Missing | None = missing
    nosave: bool | Missing = missing


@dataclass
class NumericfieldHtmlModel(
    GenericHtmlModel[
        NumericfieldInputModel, NumericfieldMarkupModel, NumericfieldStateModel
    ]
):
    def get_component_html_name(self) -> str:
        return "numericfield-runner"

    def get_static_html(self) -> str:
        return render_static_numericfield(self)

    def get_md(self) -> str:
        return "___"


@dataclass
class NumericfieldAnswerModel(
    GenericAnswerModel[
        NumericfieldInputModel, NumericfieldMarkupModel, NumericfieldStateModel
    ]
):
    pass


def render_static_numericfield(m: NumericfieldHtmlModel) -> str:
    return render_template_string(
        """
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


def get_double(c: float | int | str) -> float:
    if isinstance(c, float):
        return c
    if isinstance(c, int):
        return c
    if isinstance(c, str):
        c = REDOUBLE.sub("", c)
        c = c.replace(",", ".")
        if c.startswith("e"):
            c = "1" + c
        return float(c)
    return 0


class NumericFieldAnswerWeb(PluginAnswerWeb, total=False):
    clear: bool
    value: str | float | int


def answer(args: NumericfieldAnswerModel) -> PluginAnswerResp:
    web: NumericFieldAnswerWeb = {}
    result: PluginAnswerResp = {"web": web}
    c_input = args.input.c

    nosave = args.input.nosave
    if args.markup.nosave:
        nosave = True

    if isinstance(c_input, Missing) or c_input is None:
        web["result"] = "unsaved"
        web["error"] = "Please enter a number"
        return result
    try:
        if c_input.strip() == "":
            c: str | float | int = ""
        elif args.markup.save == "double" or not args.markup.save:
            c = get_double(c_input)
        elif args.markup.save == "int":
            c = get_double(c_input)
            c = int(c)
        elif args.markup.save == "round":
            c = get_double(c_input)
            c = round(c, 0)
        else:
            raise ValueError(f"Unknown save attribute value: {args.markup.save}")
    except ValueError as e:
        error = str(e)
        web["result"] = "error"
        web["error"] = error + " " + c_input
        return result

    if not nosave:
        save: dict[str, Any] = {"c": c}
        if not args.markup.clearstyles and args.state is not None:
            if args.state.styles:
                save = {"c": c, "styles": args.state.styles}
        result["save"] = save
        web["result"] = "saved"
        web["value"] = c

    return result


def reqs() -> PluginReqs:
    templates = [
        r"""``` {#PLUGINNAMEHERE plugin="numericfield"}
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
```""",
        """#- {defaultplugin="numericfield" readonly="view" .fieldCell}
%% 'd=;dsum=summa' | gfrange(1,5,'cols: 3') %%
```""",
        """#- {defaultplugin="numericfield" readonly="view" .fieldCell}
%% 'ht=Harhoitustyö;osa=Osasuoritus' |  gfields('cols: 3')  %%
""",
    ]
    return {
        "js": ["/field/js/build/numericfield.js"],
        "multihtml": True,
        "multimd": True,
        "css": ["/field/css/field.css"],
        "editor_tabs": [
            {
                "text": "Fields",
                "items": [
                    {
                        "text": "Numeric",
                        "items": [
                            {
                                "data": '#- {defaultplugin="numericfield" readonly="view" .fieldCell}\n',
                                "text": "defaultplugin/numericfield",
                                "expl": "Attribuutit kappaleelle jossa inline numericfield",
                            },
                            {
                                "data": "numericfield",
                                "text": "teksti: numericfield",
                                "expl": "Pelkkä kentän tyyppi: numericfield",
                            },
                            {
                                "data": "%% 'd;dsum' | gfrange(1,5,'cols: 3') %%\n",
                                "text": "Joukko kenttiä",
                                "expl": "Valmis joukko samannimisiä kenttä",
                            },
                            {
                                "data": "{#nf1 #}",
                                "text": "Numeerinen kenttä (inline, autosave)",
                                "expl": "Luo kenttä jonka syötteet ovat vain numeroita",
                            },
                            {
                                "data": templates[0].strip(),
                                "text": "Numeerinen kenttä (laajennettu)",
                                "expl": "Luo kenttä jonka syöte ovat vain numero",
                            },
                            {
                                "data": "{#nf3 autosave: false, readOnlyStyle: plaintext #}",
                                "text": "Label kenttä (read only)",
                                "expl": "Luo kenttä jonka syötettä käyttäjä ei voi muokata",
                            },
                            {
                                "data": templates[1].strip() + "\n",
                                "text": "Joukko numeroituja numeerisia kenttiä ja summa",
                                "expl": "Lohko jossa joukko numeroituja numeerisia kenttiä ja niiden summa",
                            },
                            {
                                "data": templates[2].strip() + "\n",
                                "text": "Joukko erikseen nimettyjä numeerisia kenttiä",
                                "expl": "Lohko jossa eri tavoin nimettyjä numeerisia kenttiä",
                            },
                        ],
                    },
                    {
                        "text": "Settings",
                        "items": [
                            {
                                "data": "autosave: false,",
                                "text": "autosave: false",
                                "expl": "ei automaattista tallennusta",
                            },
                            {
                                "data": 'readonly="view"',
                                "text": 'readonly="view"',
                                "expl": "kenttää ei voi muokata view-näkymässä, tämä lohkon otsikkoon",
                            },
                            {
                                "data": 'visible="%% False | isview%%" nocache="true"',
                                "text": "lohko ei näy View-näkymässä",
                                "expl": "tällä merkitään lohko, jonka ei haluta näkyvän View-näkymässä",
                            },
                            {
                                "data": "cols: 4,",
                                "text": "Sarakkeiden määrä",
                                "expl": "Sarakkeiden määrä",
                            },
                        ],
                    },
                ],
            },
        ],
    }


numericfield_route = create_blueprint(
    __name__,
    "nf",
    NumericfieldHtmlModel,
    NumericfieldAnswerModel,
    answer,
    reqs,
)
