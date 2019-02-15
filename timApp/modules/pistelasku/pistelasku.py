"""
Module for serving TIM example pistelaskundrome plugin.
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
    make_base64, InfoSchema, create_app


@attr.s(auto_attribs=True)
class PistelaskuStateModel:
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


class PistelaskuMarkupSchema(GenericMarkupSchema):
    points_array = fields.List(fields.List(fields.Number()))
    inputstem = fields.Str()
    needed_len = fields.Int()
    initword = fields.Str()
    cols = fields.Int()

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
    userword: str
    pistelaskuOK: bool
    nosave: bool = missing


class PistelaskuInputSchema(Schema):
    nosave = fields.Bool()
    pistelaskuOK = fields.Bool(required=True)
    userword = fields.Str(required=True)

    @validates('userword')
    def validate_userword(self, word):
        if not word:
            raise ValidationError('Must not be empty.')

    @post_load
    def make_obj(self, data):
        return PistelaskuInputModel(**data)


class PistelaskuAttrs(Schema):
    markup = fields.Nested(PistelaskuMarkupSchema)
    state = fields.Nested(PistelaskuStateSchema, allow_none=True, required=True)


@attr.s(auto_attribs=True)
class PistelaskuHtmlModel(GenericHtmlModel[PistelaskuInputModel, PistelaskuMarkupModel, PistelaskuStateModel]):
    def get_static_html(self) -> str:
        return render_static_pistelasku(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        if self.state:
            r['userword'] = self.state.userword
        return r

    def get_real_html(self):
        return render_template_string(
            """<pistelasku-runner json="{{data}}"></pistelasku-runner>""",
            data=make_base64(self.get_browser_json()),
        )

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
    return jsonify({
        "js": ["js/build/pistelasku.js"],
        "angularModule": ["pistelaskuApp"],
        "multihtml": True,
        "css": ["css/pistelasku.css"],
    })


if __name__ == '__main__':
    app.run(
        host='0.0.0.0',
        port=5000,
        debug=True,
    )
