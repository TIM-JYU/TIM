"""
TIM plugin: a textfield
"""
from typing import Union, List

from dataclasses import dataclass, asdict
from flask import jsonify, render_template_string, Blueprint, request
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from common_schemas import TextfieldStateModel
from marshmallow_dataclass import class_schema
from pluginserver_flask import GenericHtmlModel, \
    GenericAnswerModel, render_multihtml
from markupmodels import GenericMarkupModel
from utils import Missing

textfield_route = Blueprint('tf', __name__, url_prefix="/tf")


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
    hasListeners: Union[bool, Missing] = missing


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


def render_static_textfield(m: TextfieldHtmlModel):
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


TextFieldHtmlSchema = class_schema(TextfieldHtmlModel)
TextfieldAnswerSchema = class_schema(TextfieldAnswerModel)

@textfield_route.route('/multihtml', methods=['post'])
def tf_multihtml():
    ret = render_multihtml(request.get_json(), TextFieldHtmlSchema())
    return ret


@textfield_route.route('/answer', methods=['put'])
@use_args(TextfieldAnswerSchema(), locations=("json",))
def answer(args: TextfieldAnswerModel):
    web = {}
    result = {'web': web}
    c = args.input.c


    nosave = args.input.nosave
    if args.markup.nosave:
        nosave = True

    if not nosave:
        save = {"c": c}
        if not args.markup.clearstyles and args.state is not None:
            if args.state.styles:
                save = {"c": c, "styles": args.state.styles}
        result["save"] = save
        web['result'] = "saved"
        if args.markup.clearstyles:
            web['clear'] = True

    return jsonify(result)


@textfield_route.route('/reqs')
def reqs():
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
    return jsonify({
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
    },
    )
