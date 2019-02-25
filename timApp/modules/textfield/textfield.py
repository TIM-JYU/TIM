"""
TIM example plugin: a textfieldndrome checker.
"""
import re
import sys
from typing import Union

import attr
from flask import jsonify, render_template_string
from marshmallow import Schema, fields, post_load, validates, ValidationError
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from pluginserver_flask import GenericMarkupModel, GenericMarkupSchema, GenericHtmlSchema, GenericHtmlModel, \
    GenericAnswerSchema, GenericAnswerModel, Missing, \
    InfoSchema, create_app

sys.path.insert(0, '/py')  # /py on mountattu docker kontissa /opt/tim/timApp/modules/py -hakemistoon

@attr.s(auto_attribs=True)
class TextfieldStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    demopisteet: float


class TextfieldStateSchema(Schema):
    demopisteet = fields.Number(required=True)

    @post_load
    def make_obj(self, data):
        return TextfieldStateModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class TextfieldMarkupModel(GenericMarkupModel):
    points_array: Union[str, Missing] = missing
    inputstem: Union[str, Missing] = missing
    needed_len: Union[int, Missing] = missing
    initword: Union[str, Missing] = missing
    cols: Union[int, Missing] = missing
    inputplaceholder: Union[str, Missing] = missing


class TextfieldMarkupSchema(GenericMarkupSchema):
    points_array = fields.List(fields.List(fields.Number()))
    inputstem2 = fields.Number()
    initword2 = fields.Number()
    cols = fields.Int()
    inputplaceholder2: Union[int, Missing] = missing

    @validates('points_array')
    def validate_points_array(self, value):
        if len(value) != 2 or not all(len(v) == 2 for v in value):
            raise ValidationError('Must be of size 2 x 2.')

    @post_load
    def make_obj(self, data):
        return TextfieldMarkupModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class TextfieldInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    demopisteet: float
    nosave: bool = missing


class TextfieldInputSchema(Schema):
    demopisteet = fields.Number(required=True)
    nosave = fields.Bool()

    @validates('demopisteet')
    def validate_demopisteet(self, number):
        if not number:
            raise ValidationError('Must be a number.')

    @post_load
    def make_obj(self, data):
        return TextfieldInputModel(**data)


class TextfieldAttrs(Schema):
    """Common fields for HTML and answer routes."""
    markup = fields.Nested(TextfieldMarkupSchema)
    state = fields.Nested(TextfieldStateSchema, allow_none=True, required=True)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class TextfieldHtmlModel(GenericHtmlModel[TextfieldInputModel, TextfieldMarkupModel, TextfieldStateModel]):
    def get_component_html_name(self) -> str:
        return 'textfield-runner'

    def get_static_html(self) -> str:
        return render_static_textfield(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        if self.state:
            r['demopisteet'] = self.state.demopisteet
        return r

    class Meta:
        strict = True


class TextfieldHtmlSchema(TextfieldAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return TextfieldHtmlModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class TextfieldAnswerModel(GenericAnswerModel[TextfieldInputModel, TextfieldMarkupModel, TextfieldStateModel]):
    pass


class TextfieldAnswerSchema(TextfieldAttrs, GenericAnswerSchema):
    input = fields.Nested(TextfieldInputSchema, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return TextfieldAnswerModel(**data)

    class Meta:
        strict = True


def render_static_textfield(m: TextfieldHtmlModel):
    return render_template_string(
        """
<div class="csRunDiv no-popup-menu">
    <h4>{{ header }}</h4>
    <p class="stem">{{ stem }}</p>
    <div><label>{{ inputstem or '' }} <span>
        <input type="text"
               class="form-control"
               placeholder="{{inputplaceholder}}"
               size="{{cols}}"></span></label>
    </div>
    <button class="timButton">
        {{ buttonText or button or "Save" }}
    </button>
    <a>{{ resetText }}</a>
    <p class="plgfooter">{{ footer }}</p>
</div>
        """,
        **attr.asdict(m.markup),
    )


app = create_app(__name__, TextfieldHtmlSchema())


@app.route('/answer/', methods=['put'])
@use_args(TextfieldAnswerSchema(strict=True), locations=("json",))
def answer(args: TextfieldAnswerModel):
    web = {}
    result = {'web': web}
    demopisteet = args.input.demopisteet

    return jsonify(result)


@app.route('/reqs/')
@app.route('/reqs')
def reqs():
    templates = ["""
``` {#ekatextfield plugin="textfield"}
header: Luo lomake
stem: Anna lomakkeen nimi ja kenttien lukumäärä.
-points_array: [[0, 0.1], [0.6, 1]]
inputstem: "Lomakkeen nimi:"
inputstem2: "Lomakkeen kenttien lukumäärä:"
needed_len: 1
initword: nimi
cols: 20
```""", """
``` {#tokatextfield plugin="textfield"}
header: Demopisteet
stem: Anna lomakkeen nimi ja kenttien lukumäärä.
-points_array: [[0, 0.1], [0.6, 1]]
inputstem2: 0
needed_len: 1
initword: nimi
cols: 20
```"""]
    return jsonify({
        "js": ["js/build/textfield.js"],
        "multihtml": True,
        "css": ["css/textfield.css"],
        'editor_tabs': [
            {
                'text': 'Plugins',
                'items': [
                    {
                        'text': 'Textfield:',
                        'items': [
                            {
                                'data': templates[0].strip(),
                                'text': 'Lomake ',
                                'expl': 'Luo lomake demokentille',
                            },
                            {
                                'data': templates[1].strip(),
                                'text': 'Lomake (laajempi) ',
                                'expl': 'Laajempi lomake demokentille',
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
