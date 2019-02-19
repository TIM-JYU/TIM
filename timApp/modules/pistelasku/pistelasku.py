"""
TIM example plugin: a pistelaskundrome checker.
"""
import re
from typing import Union

import attr
from flask import jsonify, render_template_string
from marshmallow import Schema, fields, post_load, validates, ValidationError
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from pluginserver_flask import GenericMarkupModel, GenericMarkupSchema, GenericHtmlSchema, GenericHtmlModel, \
    GenericAnswerSchema, GenericAnswerModel, Missing, \
    InfoSchema, create_app


@attr.s(auto_attribs=True)
class PistelaskuStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    userword: str


class PistelaskuStateSchema(Schema):
    userword = fields.Str(required=True)

    @post_load
    def make_obj(self, data):
        return PistelaskuStateModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class PistelaskuMarkupModel(GenericMarkupModel):
    points_array: Union[str, Missing] = missing
    inputstem: Union[str, Missing] = missing
    needed_len: Union[int, Missing] = missing
    initword: Union[str, Missing] = missing
    cols: Union[int, Missing] = missing
    inputplaceholder: Union[str, Missing] = missing


class PistelaskuMarkupSchema(GenericMarkupSchema):
    points_array = fields.List(fields.List(fields.Number()))
    inputstem = fields.Str()
    needed_len = fields.Int()
    initword = fields.Str()
    cols = fields.Int()
    inputplaceholder = fields.Str()

    @validates('points_array')
    def validate_points_array(self, value):
        if len(value) != 2 or not all(len(v) == 2 for v in value):
            raise ValidationError('Must be of size 2 x 2.')

    @post_load
    def make_obj(self, data):
        return PistelaskuMarkupModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class PistelaskuInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    userword: str
    pistelaskuOK: bool
    nosave: bool = missing


class PistelaskuInputSchema(Schema):
    userword = fields.Str(required=True)
    pistelaskuOK = fields.Bool(required=True)
    nosave = fields.Bool()

    @validates('userword')
    def validate_userword(self, word):
        if not word:
            raise ValidationError('Must not be empty.')

    @post_load
    def make_obj(self, data):
        return PistelaskuInputModel(**data)


class PistelaskuAttrs(Schema):
    """Common fields for HTML and answer routes."""
    markup = fields.Nested(PistelaskuMarkupSchema)
    state = fields.Nested(PistelaskuStateSchema, allow_none=True, required=True)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class PistelaskuHtmlModel(GenericHtmlModel[PistelaskuInputModel, PistelaskuMarkupModel, PistelaskuStateModel]):
    def get_component_html_name(self) -> str:
        return 'pistelasku-runner'

    def get_static_html(self) -> str:
        return render_static_pistelasku(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        if self.state:
            r['userword'] = self.state.userword
        return r

    class Meta:
        strict = True


class PistelaskuHtmlSchema(PistelaskuAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return PistelaskuHtmlModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class PistelaskuAnswerModel(GenericAnswerModel[PistelaskuInputModel, PistelaskuMarkupModel, PistelaskuStateModel]):
    pass


class PistelaskuAnswerSchema(PistelaskuAttrs, GenericAnswerSchema):
    input = fields.Nested(PistelaskuInputSchema, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return PistelaskuAnswerModel(**data)

    class Meta:
        strict = True


def render_static_pistelasku(m: PistelaskuHtmlModel):
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


app = create_app(__name__, PistelaskuHtmlSchema())


@app.route('/answer/', methods=['put'])
@use_args(PistelaskuAnswerSchema(strict=True), locations=("json",))
def answer(args: PistelaskuAnswerModel):
    web = {}
    result = {'web': web}
    needed_len = args.markup.needed_len
    userword = args.input.userword
    pistelasku_ok = args.input.pistelaskuOK
    len_ok = True
    if needed_len:
        len_ok = check_letters(userword, needed_len)
    if not len_ok:
        web['error'] = "Wrong length"
    if not needed_len and not pistelasku_ok:
        len_ok = False
    points_array = args.markup.points_array or [[0, 0.25], [0.5, 1]]
    points = points_array[pistelasku_ok][len_ok]

    # plugin can ask not to save the word
    nosave = args.input.nosave
    if not nosave:
        tim_info = {"points": points}
        save = {"userword": userword}
        result["save"] = save
        result["tim_info"] = tim_info
        web['result'] = "saved"

    return jsonify(result)


def check_letters(word: str, needed_len: int) -> bool:
    """Checks if word has needed amount of chars.

    :param word: word to check
    :param needed_len: how many letters needed
    :return: true if len match

    """
    s = word.upper()
    return len(re.sub("[^[A-ZÅÄÖ]", "", s)) == needed_len


@app.route('/reqs/')
@app.route('/reqs')
def reqs():
    templates = ["""
``` {#ekapistelasku plugin="pistelasku"}
header: Kirjoita pistelasku
stem: Kirjoita pistelasku, jossa on 5 kirjainta.
-points_array: [[0, 0.1], [0.6, 1]]
inputstem: "Pistelaskusi:"
needed_len: 5
answerLimit: 3
initword: muikku
cols: 20
```""", """
``` {#tokapistelasku plugin="pistelasku"}
header: Kirjoita pistelasku
stem: Kirjoita pistelasku, jossa on 7 kirjainta.
-points_array: [[0, 0.1], [0.6, 1]]
inputstem: "Pistelaskusi:"
needed_len: 7
answerLimit: 4
initword: muikku
cols: 20
```"""]
    return jsonify({
        "js": ["js/build/pistelasku.js"],
        "multihtml": True,
        "css": ["css/pistelasku.css"],
        'editor_tabs': [
            {
                'text': 'Plugins',
                'items': [
                    {
                        'text': 'Pistelasku',
                        'items': [
                            {
                                'data': templates[0].strip(),
                                'text': '5 letters',
                                'expl': 'Add a 5-letter pistelaskundrome task',
                            },
                            {
                                'data': templates[1].strip(),
                                'text': '7 letters',
                                'expl': 'Add a 7-letter pistelaskundrome task',
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
