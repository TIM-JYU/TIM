"""
TIM plugin: a textfield
"""
from typing import Union

import attr
from flask import jsonify, render_template_string, Blueprint
from marshmallow import Schema, fields, post_load, validates
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from common_schemas import TextfieldStateModel, TextfieldStateSchema
from pluginserver_flask import GenericMarkupModel, GenericMarkupSchema, GenericHtmlSchema, GenericHtmlModel, \
    GenericAnswerSchema, GenericAnswerModel, Missing, \
    InfoSchema, render_multihtml

textfield_route = Blueprint('tf', __name__, url_prefix="/tf")


@attr.s(auto_attribs=True)
class TextfieldMarkupModel(GenericMarkupModel):
    points_array: Union[str, Missing] = missing
    inputstem: Union[str, Missing] = missing
    initword: Union[str, Missing] = missing
    cols: Union[int, Missing] = missing
    inputplaceholder: Union[str, Missing] = missing
    tag: Union[str, Missing] = missing
    autosave: Union[bool, Missing] = missing
    validinput: Union[str, Missing] = missing
    errormessage: Union[str, Missing] = missing
    readOnlyStyle: Union[str, Missing] = missing
    showname: Union[int, Missing] = missing
    nosave: Union[bool, Missing] = missing
    ignorestyles: Union[bool, Missing] = missing
    clearstyles: Union[bool, Missing] = missing
    autoUpdateTables: Union[bool, Missing] = True
    textarea: Union[bool, Missing] = missing


class TextfieldMarkupSchema(GenericMarkupSchema):
    points_array = fields.List(fields.List(fields.Number()))
    header = fields.String(allow_none=True)
    inputstem = fields.String(allow_none=True)
    stem = fields.String(allow_none=True)
    initword = fields.String(allow_none=True)
    cols = fields.Int()
    inputplaceholder = fields.Str(allow_none=True)
    tag = fields.String(allow_none=True)
    autosave = fields.Boolean()
    validinput = fields.String(allow_none=True)
    errormessage = fields.String(allow_none=True)
    readOnlyStyle = fields.String(allow_none=True)
    showname = fields.Int(allow_none=True)
    nosave = fields.Boolean()
    ignorestyles = fields.Boolean()
    clearstyles = fields.Boolean()
    autoUpdateTables = fields.Boolean(default=True)
    textarea = fields.Boolean()

    @post_load
    def make_obj(self, data):
        return TextfieldMarkupModel(**data)


@attr.s(auto_attribs=True)
class TextfieldInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    c: str
    nosave: bool = missing


class TextfieldInputSchema(Schema):
    c = fields.Str(required=True)
    nosave = fields.Bool()

    @validates('c')
    def validate_userword(self, word):
        pass

    @post_load
    def make_obj(self, data):
        return TextfieldInputModel(**data)


class TextfieldAttrs(Schema):
    """Common fields for HTML and answer routes."""
    markup = fields.Nested(TextfieldMarkupSchema)
    state = fields.Nested(TextfieldStateSchema, allow_none=True, required=True)


@attr.s(auto_attribs=True)
class TextfieldHtmlModel(GenericHtmlModel[TextfieldInputModel, TextfieldMarkupModel, TextfieldStateModel]):
    def get_component_html_name(self) -> str:
        return 'textfield-runner'

    def get_static_html(self) -> str:
        return render_static_textfield(self)


class TextfieldHtmlSchema(TextfieldAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return TextfieldHtmlModel(**data)


@attr.s(auto_attribs=True)
class TextfieldAnswerModel(GenericAnswerModel[TextfieldInputModel, TextfieldMarkupModel, TextfieldStateModel]):
    pass


class TextfieldAnswerSchema(TextfieldAttrs, GenericAnswerSchema):
    input = fields.Nested(TextfieldInputSchema, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return TextfieldAnswerModel(**data)


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
        **attr.asdict(m.markup),
    )


TEXT_FIELD_HTML_SCHEMA = TextfieldHtmlSchema()


@textfield_route.route('/multihtml/', methods=['post'])
@use_args(GenericHtmlSchema(many=True), locations=("json",))
def tf_multihtml(args):  # args: List[GenericHtmlSchema]):
    ret = render_multihtml(TEXT_FIELD_HTML_SCHEMA, args)
    return ret


@textfield_route.route('/answer/', methods=['put'])
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


@textfield_route.route('/reqs/')
@textfield_route.route('/reqs')
def reqs():
    """Introducing templates for textfield plugin"""
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
                                'text': 'Joukko kenttiä',
                                'expl': 'Valmis joukko samannimisiä kenttä',
                            },
                            {
                                'data': "{#tf1#}",
                                'text': 'Tekstikenttä (inline, autosave)',
                                'expl': 'Luo kenttä jonka syötteet ovat tekstiä',
                            },
                            {
                                'data': templates[0].strip(),
                                'text': 'Tekstikenttä (laajennettu)',
                                'expl': 'Luo kenttä jonka syötteet ovat tekstiä',
                            },
                            {
                                'data': "{#tf2 readOnlyStyle: plaintext #}",
                                'text': 'Label kenttä (inline, read only)',
                                'expl': 'Luo kenttä jonka syötteitä käyttäjä ei voi muokata',
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
