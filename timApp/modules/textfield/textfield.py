"""
TIM plugin: a textfield
"""
from typing import Union

import attr
from flask import jsonify, render_template_string
from marshmallow import Schema, fields, post_load, validates
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from common_schemas import TextfieldStateModel, TextfieldStateSchema
from pluginserver_flask import GenericMarkupModel, GenericMarkupSchema, GenericHtmlSchema, GenericHtmlModel, \
    GenericAnswerSchema, GenericAnswerModel, Missing, \
    InfoSchema, create_app


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


app = create_app(__name__, TextfieldHtmlSchema())


@app.route('/answer/', methods=['put'])
@use_args(TextfieldAnswerSchema(), locations=("json",))
def answer(args: TextfieldAnswerModel):
    web = {}
    result = {'web': web}
    c = args.input.c

    nosave = args.input.nosave

    if not nosave:
        save = {"c": c}
        result["save"] = save
        web['result'] = "saved"

    return jsonify(result)


@app.route('/reqs/')
@app.route('/reqs')
def reqs():
    """Introducing templates for textfield plugin"""
    templates = [
"""``` {#PLUGINNAMEHERE="textfield"}
header:          # otsikko, tyhjä = ei otsikkoa
stem:            # kysymys, tyhjä = ei kysymystä
inputstem:       # vastaus, tyhjä = ei vastausta
tag:        # seurantaid, tyhjä = ei seurantaid:tä
initword:        # alkuarvo, tyhjä = ei alkuarvoa
buttonText: Save # PAINIKKEEN NIMI, TYHJÄ = EI PAINIKETTA
cols: 1          # kentän koko, numeraalinen
autosave: false  # autosave, pois päältä
validinput: '^(hyv|hyl|[12345])$' #käyttäjäsyötteen rajoitin, tyhjä = ei rajoitusta
errormessage:    #inputcheckerin virheselite, tyhjä = selite on inputchecker
```""",
]
    return jsonify({
        "js": ["js/build/textfield.js"],
        "multihtml": True,
        "css": ["css/textfield.css"],
        'editor_tabs': [
            {
                'text': 'Plugins',
                'items': [
                    {
                        'text': 'Fields',
                        'items': [
                            {
                                'data': '#- {defaultplugin="textfield" readonly="view"}',
                                'text': 'defaultplugin/textfield',
                                'expl': 'Attribuutit kappaleelle jossa inline textfield',
                            },
                            {
                                'data': "{#tf1 autosave: true #}",
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


if __name__ == '__main__':
    app.run(
        host='0.0.0.0',
        port=5000,
        debug=False,  # for live reloading, this can be turned on
    )
