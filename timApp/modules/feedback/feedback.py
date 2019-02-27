"""
TIM feedback-plugin.
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
class FeedbackStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    userword: str


class FeedbackStateSchema(Schema):
    userword = fields.Str(required=True)

    @post_load
    def make_obj(self, data):
        return FeedbackStateModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class FeedbackMarkupModel(GenericMarkupModel):
    points_array: Union[str, Missing] = missing
    inputstem: Union[str, Missing] = missing
    needed_len: Union[int, Missing] = missing
    initword: Union[str, Missing] = missing
    cols: Union[int, Missing] = missing
    inputplaceholder: Union[str, Missing] = missing
    followid: Union[str, Missing] = missing
    field: Union[str, Missing] = missing


class FeedbackMarkupSchema(GenericMarkupSchema):
    points_array = fields.List(fields.List(fields.Number()))
    inputstem = fields.Str()
    needed_len = fields.Int()
    initword = fields.Str()
    cols = fields.Int()
    inputplaceholder = fields.Str()
    followid = fields.Str()
    field = fields.Str()

    @validates('points_array')
    def validate_points_array(self, value):
        if len(value) != 2 or not all(len(v) == 2 for v in value):
            raise ValidationError('Must be of size 2 x 2.')

    @post_load
    def make_obj(self, data):
        return FeedbackMarkupModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class FeedbackInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    userword: str
    paliOK: bool = missing
    nosave: bool = missing


class FeedbackInputSchema(Schema):
    userword = fields.Str(required=True)
    paliOK = fields.Bool()
    nosave = fields.Bool()

    @validates('userword')
    def validate_userword(self, word):
        if not word:
            raise ValidationError('Must not be empty.')

    @post_load
    def make_obj(self, data):
        return FeedbackInputModel(**data)


class FeedbackAttrs(Schema):
    """Common fields for HTML and answer routes."""
    markup = fields.Nested(FeedbackMarkupSchema)
    state = fields.Nested(FeedbackStateSchema, allow_none=True, required=True)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class FeedbackHtmlModel(GenericHtmlModel[FeedbackInputModel, FeedbackMarkupModel, FeedbackStateModel]):
    def get_component_html_name(self) -> str:
        return 'feedback-runner'

    def get_static_html(self) -> str:
        return render_static_feedback(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        if self.state:
            r['userword'] = self.state.userword
        return r

    class Meta:
        strict = True


class FeedbackHtmlSchema(FeedbackAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return FeedbackHtmlModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class FeedbackAnswerModel(GenericAnswerModel[FeedbackInputModel, FeedbackMarkupModel, FeedbackStateModel]):
    pass


class FeedbackAnswerSchema(FeedbackAttrs, GenericAnswerSchema):
    input = fields.Nested(FeedbackInputSchema, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return FeedbackAnswerModel(**data)

    class Meta:
        strict = True


def render_static_feedback(m: FeedbackHtmlModel):
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


app = create_app(__name__, FeedbackHtmlSchema())


@app.route('/answer/', methods=['put'])
@use_args(FeedbackAnswerSchema(strict=True), locations=("json",))
def answer(args: FeedbackAnswerModel):
    web = {}
    result = {'web': web}
    needed_len = args.markup.needed_len
    userword = args.input.userword
    pali_ok = args.input.paliOK or False
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
``` {#feedback1 plugin="feedback"}
header: Vastauksesi
inputstem: "Vastauksesi:"
cols: 20
field: item1
```""", """
``` {#tokapali plugin="feedback"}
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
        "js": ["js/build/feedback.js"],
        "multihtml": True,
        "css": ["css/feedback.css"],
        'editor_tabs': [
            {
                'text': 'Plugins',
                'items': [
                    {
                        'text': 'Feedback',
                        'items': [
                            {
                                'data': templates[0].strip(),
                                'text': 'One task',
                                'expl': 'Add a plugin to show feedback',
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
    },
    )


if __name__ == '__main__':
    app.run(
        host='0.0.0.0',
        port=5000,
        debug=False,  # for live reloading, this can be turned on
    )
