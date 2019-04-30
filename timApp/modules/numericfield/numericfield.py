"""
TIM plugin: a numericfield
"""

from typing import Union

import attr
from flask import jsonify, render_template_string
from marshmallow import Schema, fields, post_load, validates
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from pluginserver_flask import GenericMarkupModel, GenericMarkupSchema, GenericHtmlSchema, GenericHtmlModel, \
    GenericAnswerSchema, GenericAnswerModel, Missing, \
    InfoSchema, create_app


@attr.s(auto_attribs=True)
class NumericfieldStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    numericvalue: float


class NumericfieldStateSchema(Schema):
    numericvalue = fields.Number(required=True)

    @post_load
    def make_obj(self, data):
        return NumericfieldStateModel(**data)

    class Meta:
        strict = True

@attr.s(auto_attribs=True)
class NumericfieldMarkupModel(GenericMarkupModel):
    points_array: Union[str, Missing] = missing
    inputstem: Union[str, Missing] = missing
    initnumber: Union[int, Missing] = missing
    cols: Union[int, Missing] = missing
    inputplaceholder: Union[str, Missing] = missing
    followid: Union[str, Missing] = missing
    autosave: Union[bool, Missing] = missing

class NumericfieldMarkupSchema(GenericMarkupSchema):
    points_array = fields.List(fields.List(fields.Number()))
    header = fields.String(allow_none=True)
    inputstem = fields.String(allow_none=True)
    stem = fields.String(allow_none=True)
    initnumber = fields.Number(allow_none=True)
    cols = fields.Int()
    inputplaceholder: Union[int, Missing] = missing
    followid = fields.String(allow_none=True)
    autosave = fields.Boolean()

    @post_load
    def make_obj(self, data):
        return NumericfieldMarkupModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class NumericfieldInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    numericvalue: float
    nosave: bool = missing


class NumericfieldInputSchema(Schema):
    numericvalue = fields.Number(required=True)
    nosave = fields.Bool()

    @validates('numericvalue')
    def validate_numericvalue(self, number):
        pass
        # if not number:
        #     raise ValidationError('Syntax Error: Must be a number.')

    @post_load
    def make_obj(self, data):
        return NumericfieldInputModel(**data)


class NumericfieldAttrs(Schema):
    """Common fields for HTML and answer routes."""
    markup = fields.Nested(NumericfieldMarkupSchema)
    state = fields.Nested(NumericfieldStateSchema, allow_none=True, required=True)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class NumericfieldHtmlModel(GenericHtmlModel[NumericfieldInputModel, NumericfieldMarkupModel, NumericfieldStateModel]):
    def get_component_html_name(self) -> str:
        return 'numericfield-runner'

    def get_static_html(self) -> str:
        return render_static_numericfield(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        if self.state:
            r['numericvalue'] = self.state.numericvalue
        return r

    class Meta:
        strict = True


class NumericfieldHtmlSchema(NumericfieldAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return NumericfieldHtmlModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class NumericfieldAnswerModel(GenericAnswerModel[NumericfieldInputModel, NumericfieldMarkupModel, NumericfieldStateModel]):
    pass


class NumericfieldAnswerSchema(NumericfieldAttrs, GenericAnswerSchema):
    input = fields.Nested(NumericfieldInputSchema, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return NumericfieldAnswerModel(**data)

    class Meta:
        strict = True


def render_static_numericfield(m: NumericfieldHtmlModel):
    return render_template_string("""
<div class="csRunDiv no-popup-menu">
<h4>{{ header or '' }}</h4>
<p class="stem">{{ stem or '' }}</p>
<div><label>{{ inputstem or '' }} <span>
<input type="text"
        class="form-control"
        placeholder="{{ '' }}"
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


app = create_app(__name__, NumericfieldHtmlSchema())


@app.route('/answer/', methods=['put'])
@use_args(NumericfieldAnswerSchema(strict=True), locations=("json",))
def answer(args: NumericfieldAnswerModel):
    web = {}
    result = {'web': web}
    numericvalue = args.input.numericvalue

    nosave = args.input.nosave
    if not nosave:
        save = {"numericvalue": numericvalue}
        result["save"] = save
        web['result'] = "saved"

    return jsonify(result)


@app.route('/reqs/')
@app.route('/reqs')
def reqs():
    """Introducing templates for numericfield plugin"""
    templates = ["""
``` {#numericfield_normal plugin="numericfield"}
needed_len: 1 #MINIMIPITUUS, NUMERAALINEN
cols: 5 #KENTÄN KOKO, NUMERAALINEN
autosave: true #AUTOSAVE, PÄÄLLÄ
```""", """
``` {#numericfield_extended plugin="numericfield"}
header: #OTSIKKO, TYHJÄ = EI OTSIKKOA
stem: #KYSYMYS, TYHJÄ = EI KYSYMYSTÄ
inputstem: #VASTAUS, TYHJÄ = EI VASTAUSTA
followid: #SEURANTAID, TYHJÄ = EI SEURANTAID:tä
needed_len: 1 #MINIMIPITUUS, NUMERAALINEN 
initnumber: #ALKUARVO, TYHJÄ = EI ALKUARVOA
buttonText: Save #PAINIKKEEN NIMI, TYHJÄ = EI PAINIKETTA
cols: 5 #KENTÄN KOKO, NUMERAALINEN
autosave: false #AUTOSAVE, POIS PÄÄLTÄ
```""", """
``` {#label plugin="numericfield" readonly=view}
followid: #SEURANTAID, TYHJÄ = EI SEURANTAID:tä
needed_len: 1 #MINIMIPITUUS, NUMERAALINEN 
initnumber: #ALKUARVO, TYHJÄ = EI ALKUARVOA
cols: 5 #KENTÄN KOKO, NUMERAALINEN
autosave: false #AUTOSAVE, POIS PÄÄLTÄ
```"""
]
    return jsonify({
        "js": ["js/build/numericfield.js"],
        "multihtml": True,
        "css": ["css/numericfield.css"],
        'editor_tabs': [
            {
                'text': 'Plugins',
                'items': [
                    {
                        'text': 'Numericfield',
                        'items': [
                            {
                                'data': templates[0].strip(),
                                'text': 'Numeerinen kenttä (autosave)',
                                'expl': 'Luo kenttä jonka syötteet ovat vain numeroita',
                            },
                            {
                                'data': templates[1].strip(),
                                'text': 'Numeerinen kenttä (laajennettu)',
                                'expl': 'Luo kenttä jonka syötteet ovat vain numeroita',
                            },
                            {
                                'data': templates[2].strip(),
                                'text': 'Label kenttä (read only)',
                                'expl': 'Luo kenttä jonka syötteitä käyttäjä ei voi muokata',
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
