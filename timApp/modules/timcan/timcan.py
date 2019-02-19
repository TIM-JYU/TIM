"""
Module for serving TIM example palindrome plugin.
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
class PaliStateModel:
    userword: str


class PaliStateSchema(Schema):
    userword = fields.Str(required=True)

    @post_load
    def make_obj(self, data):
        return PaliStateModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class PaliMarkupModel(GenericMarkupModel):
    points_array: Union[str, Missing] = missing
    inputstem: Union[str, Missing] = missing
    needed_len: Union[int, Missing] = missing
    initword: Union[str, Missing] = missing
    cols: Union[int, Missing] = missing


class PaliMarkupSchema(GenericMarkupSchema):
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
        return PaliMarkupModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class PaliInputModel:
    userword: str
    paliOK: bool
    nosave: bool = missing


class PaliInputSchema(Schema):
    nosave = fields.Bool()
    paliOK = fields.Bool(required=True)
    userword = fields.Str(required=True)

    @validates('userword')
    def validate_userword(self, word):
        if not word:
            raise ValidationError('Must not be empty.')

    @post_load
    def make_obj(self, data):
        return PaliInputModel(**data)


class PaliAttrs(Schema):
    markup = fields.Nested(PaliMarkupSchema)
    state = fields.Nested(PaliStateSchema, allow_none=True, required=True)


@attr.s(auto_attribs=True)
class PaliHtmlModel(GenericHtmlModel[PaliInputModel, PaliMarkupModel, PaliStateModel]):
    def get_static_html(self) -> str:
        return render_static_pali(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        if self.state:
            r['userword'] = self.state.userword
        return r

    def get_real_html(self):
        return render_template_string(
            """<pali-runner json="{{data}}"></pali-runner>""",
            data=make_base64(self.get_browser_json()),
        )

    class Meta:
        strict = True


class PaliHtmlSchema(PaliAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return PaliHtmlModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class PaliAnswerModel(GenericAnswerModel[PaliInputModel, PaliMarkupModel, PaliStateModel]):
    pass


class PaliAnswerSchema(PaliAttrs, GenericAnswerSchema):
    input = fields.Nested(PaliInputSchema, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return PaliAnswerModel(**data)

    class Meta:
        strict = True


def render_static_pali(m: PaliHtmlModel):
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


app = create_app(__name__, PaliHtmlSchema())


@app.route('/answer/', methods=['put'])
@use_args(PaliAnswerSchema(strict=True), locations=("json",))
def answer(args: PaliAnswerModel):
    web = {}
    result = {'web': web}
    needed_len = args.markup.needed_len
    userword = args.input.userword
    pali_ok = args.input.paliOK
    len_ok = True
    if needed_len:
        len_ok = check_letters(userword, needed_len)
    if not len_ok:
        web['error'] = "Wrong length"
    if not needed_len and not pali_ok:
        len_ok = False
    points_array = args.markup.points_array or [[0, 0.25], [0.5, 1]]
    points = points_array[pali_ok][len_ok]

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
``` {#ekapali plugin="timcan"}
header: Kirjoita palindromi
stem: Kirjoita palindromi, jossa on 5 kirjainta.
-points_array: [[0, 0.1], [0.6, 1]]
inputstem: "Palindromisi:"
needed_len: 5
answerLimit: 3
initword: muikku
cols: 20
```""", """
``` {#tokapali plugin="timcan"}
header: Kirjoita palindromi
stem: Kirjoita palindromi, jossa on 7 kirjainta.
-points_array: [[0, 0.1], [0.6, 1]]
inputstem: "Palindromisi:"
needed_len: 7
answerLimit: 4
initword: muikku
cols: 20
```"""]
    return jsonify({
        "js": ["js/build/timcan.js"],
        "multihtml": True,
        "css": ["css/timcan.css"],
        'editor_tabs': [
            {
                'text': 'Plugins',
                'items': [
                    {
                        'text': 'TIMCAN',
                        'items': [
                            {
                                'data': templates[0].strip(),
                                'text': '5 letters',
                                'expl': 'Add a 5-letter palindrome task',
                            },
                            {
                                'data': templates[1].strip(),
                                'text': '7 letters',
                                'expl': 'Add a 7-letter palindrome task',
                            },
                        ],
                    },
                ],
            },
        ],
    })


if __name__ == '__main__':
    app.run(
        host='0.0.0.0',
        port=5000,
        debug=True,
    )
