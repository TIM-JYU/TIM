"""
TIM example plugin: a omapluginndrome checker.
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
class omapluginStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    userword: str


class omapluginStateSchema(Schema):
    userword = fields.Str(required=True)

    @post_load
    def make_obj(self, data):
        return omapluginStateModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class omapluginMarkupModel(GenericMarkupModel):
    points_array: Union[str, Missing] = missing
    inputstem: Union[str, Missing] = missing
    inputstem2: Union[str, Missing] = missing
    needed_len: Union[int, Missing] = missing
    initword: Union[str, Missing] = missing
    cols: Union[int, Missing] = missing
    inputplaceholder: Union[str, Missing] = missing


class omapluginMarkupSchema(GenericMarkupSchema):
    points_array = fields.List(fields.List(fields.Number()))
    inputstem = fields.Str()
    inputstem2 = fields.Str()
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
        return omapluginMarkupModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class omapluginInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    userword: str
    omapluginOK: bool
    nosave: bool = missing


class omapluginInputSchema(Schema):
    userword = fields.Str(required=True)
    omapluginOK = fields.Bool(required=True)
    nosave = fields.Bool()

    @validates('userword')
    def validate_userword(self, word):
        if not word:
            raise ValidationError('Must not be empty.')

    @post_load
    def make_obj(self, data):
        return omapluginInputModel(**data)


class omapluginAttrs(Schema):
    """Common fields for HTML and answer routes."""
    markup = fields.Nested(omapluginMarkupSchema)
    state = fields.Nested(omapluginStateSchema, allow_none=True, required=True)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class omapluginHtmlModel(GenericHtmlModel[omapluginInputModel, omapluginMarkupModel, omapluginStateModel]):
    def get_component_html_name(self) -> str:
        return 'omaplugin-runner'

    def get_static_html(self) -> str:
        return render_static_omaplugin(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        if self.state:
            r['userword'] = self.state.userword
        return r

    class Meta:
        strict = True


class omapluginHtmlSchema(omapluginAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return omapluginHtmlModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class omapluginAnswerModel(GenericAnswerModel[omapluginInputModel, omapluginMarkupModel, omapluginStateModel]):
    pass


class omapluginAnswerSchema(omapluginAttrs, GenericAnswerSchema):
    input = fields.Nested(omapluginInputSchema, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return omapluginAnswerModel(**data)

    class Meta:
        strict = True


def render_static_omaplugin(m: omapluginHtmlModel):
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


app = create_app(__name__, omapluginHtmlSchema())


@app.route('/answer/', methods=['put'])
@use_args(omapluginAnswerSchema(strict=True), locations=("json",))
def answer(args: omapluginAnswerModel):
    web = {}
    result = {'web': web}
    needed_len = args.markup.needed_len
    userword = args.input.userword
    omaplugin_ok = args.input.omapluginOK
    len_ok = True
    if needed_len:
        len_ok = check_letters(userword, needed_len)
    if not len_ok:
        web['error'] = "aaa Wrong length asdf"
    if not needed_len and not omaplugin_ok:
        len_ok = False
    points_array = args.markup.points_array or [[0, 0.25], [0.5, 1]]
    points = points_array[omaplugin_ok][len_ok]

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
``` {#ekaomaplugin plugin="omaplugin"}
header: Kirjoita omapluginndromi
stem: Kirjoita omapluginndromi, jossa on 5 kirjainta.
-points_array: [[0, 0.1], [0.6, 1]]
inputstem: "omapluginndromisi:"
needed_len: 5
answerLimit: 3
initword: muikku
cols: 20
```""", """
``` {#tokaomaplugin plugin="omaplugin"}
header: Kirjoita omapluginndromi
stem: Kirjoita omapluginndromi, jossa on 7 kirjainta.
-points_array: [[0, 0.1], [0.6, 1]]
inputstem: "omapluginndromisi:"
needed_len: 7
answerLimit: 4
initword: muikku
cols: 20
```"""]
    return jsonify({
        "js": ["js/build/omaplugin.js"],
        "multihtml": True,
        "css": ["css/omaplugin.css"],
        'editor_tabs': [
            {
                'text': 'Plugins',
                'items': [
                    {
                        'text': 'omaplugin',
                        'items': [
                            {
                                'data': templates[0].strip(),
                                'text': '5 letters',
                                'expl': 'Add a 5-letter omapluginndrome task',
                            },
                            {
                                'data': templates[1].strip(),
                                'text': '7 letters',
                                'expl': 'Add a 7-letter omapluginndrome task',
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
