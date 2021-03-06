"""
TIM plugin: a textfield
"""
from dataclasses import dataclass, asdict
from typing import Any

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


@dataclass
class TextfieldMarkupModel(GenericMarkupModel):
    autosave: bool | Missing = missing
    autogrow: bool | Missing = missing
    autoUpdateTables: bool | Missing = True
    clearstyles: bool | Missing = missing
    cols: int | Missing = missing
    errormessage: str | Missing | None = missing
    form: bool | Missing = missing
    rows: int | Missing = missing
    ignorestyles: bool | Missing = missing
    initword: str | Missing | None = missing
    inputplaceholder: str | Missing | None = missing
    inputstem: str | Missing | None = missing
    nosave: bool | Missing = missing
    points_array: list[list[float]] | Missing = missing
    readOnlyStyle: str | Missing | None = missing
    showname: int | Missing | None = missing
    tag: str | Missing | None = missing
    textarea: bool | Missing = missing
    validinput: str | Missing | None = missing
    downloadButton: str | Missing = missing
    downloadButtonFile: str | Missing = missing


@dataclass
class TextfieldInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""

    c: str
    nosave: bool | Missing = missing


@dataclass
class TextfieldHtmlModel(
    GenericHtmlModel[TextfieldInputModel, TextfieldMarkupModel, TextfieldStateModel]
):
    def get_component_html_name(self) -> str:
        return "textfield-runner"

    def get_static_html(self) -> str:
        return render_static_textfield(self)

    # TODO: Expand this to support HTML and other formats of outputs
    def get_md(self) -> str:
        if not self.userPrint or not self.state or self.state.c is None:
            return ""
        if isinstance(self.state.c, str) and not self.state.c.strip():
            return ""
        return f"**{self.state.c}**"


@dataclass
class TextfieldAnswerModel(
    GenericAnswerModel[TextfieldInputModel, TextfieldMarkupModel, TextfieldStateModel]
):
    pass


def render_static_textfield(m: TextfieldHtmlModel) -> str:
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


class TextfieldAnswerWeb(PluginAnswerWeb, total=False):
    clear: bool


def answer(args: TextfieldAnswerModel) -> PluginAnswerResp:
    web: TextfieldAnswerWeb = {}
    result: PluginAnswerResp = {"web": web}
    c = args.input.c

    nosave = args.input.nosave
    if args.markup.nosave:
        nosave = True

    if not nosave:
        save: dict[str, Any] = {"c": c}
        if not args.markup.clearstyles and args.state is not None:
            if args.state.styles:
                save = {"c": c, "styles": args.state.styles}
        result["save"] = save
        web["result"] = "saved"
        if args.markup.clearstyles:
            web["clear"] = True

    return result


def reqs() -> PluginReqs:
    templates = [
        """``` {#PLUGINNAMEHERE plugin="textfield"}
header:          # otsikko, tyhj?? = ei otsikkoa
stem:            # kysymys, tyhj?? = ei kysymyst??
inputstem:       # vastaus, tyhj?? = ei vastausta
tag:        # seurantaid, tyhj?? = ei seurantaid:t??
initword:        # alkuarvo, tyhj?? = ei alkuarvoa
buttonText: Save # PAINIKKEEN NIMI, TYHJ?? = EI PAINIKETTA
cols: 7          # kent??n koko, numeraalinen
autosave: false  # autosave, pois p????lt??
validinput: '^(hyv|hyl|[12345])$' # k??ytt??j??sy??tteen rajoitin, tyhj?? = ei rajoitusta
errormessage:    #inputcheckerin virheselite, tyhj?? = selite on inputchecker
```""",
    ]
    return {
        "js": ["/field/js/build/textfield.js"],
        "multihtml": True,
        "multimd": True,
        "css": ["/field/css/field.css"],
        "editor_tabs": [
            {
                "text": "Fields",
                "items": [
                    {
                        "text": "Text",
                        "items": [
                            {
                                "data": '#- {defaultplugin="textfield" readonly="view" .fieldCell}\n',
                                "text": "defaultplugin/textfield",
                                "expl": "Attribuutit kappaleelle jossa inline textfield",
                            },
                            {
                                "data": "textfield",
                                "text": "teksti: textfield",
                                "expl": "Pelkk?? kent??n tyyppi: textfield",
                            },
                            {
                                "data": "%% 'd;dsum' | gfrange(1,5,'cols: 5') %%\n",
                                "text": "Joukko numeroituja kentti??",
                                "expl": "Valmis joukko samannimisi?? numeroituja kentti??",
                            },
                            {
                                "data": "{#tf1#}",
                                "text": "Tekstikentt?? (inline, autosave)",
                                "expl": "Luo kentt?? jonka sy??te on teksti??",
                            },
                            {
                                "data": templates[0].strip(),
                                "text": "Tekstikentt?? (laajennettu)",
                                "expl": "Luo kentt?? jonka sy??te on teksti??",
                            },
                            {
                                "data": "{#tf2 readOnlyStyle: plaintext #}",
                                "text": "Label kentt?? (inline, read only)",
                                "expl": "Luo kentt?? jonka sy??tett?? k??ytt??j?? ei voi muokata",
                            },
                            {
                                "data": "{#username showname: 1, inputstem: 'Nimi: '#}",
                                "text": "K??ytt??j??n nimi (inline)",
                                "expl": "Kentt?? joka n??ytt???? k??ytt??j??n nimen, tarvitsee lohkon alkuun defaultplugin",
                            },
                        ],
                    },
                ],
            },
        ],
    }


textfield_route = create_blueprint(
    __name__,
    "tf",
    TextfieldHtmlModel,
    TextfieldAnswerModel,
    answer,
    reqs,
)
