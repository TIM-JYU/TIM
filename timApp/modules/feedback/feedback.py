"""
TIM feedback-plugin.
"""
import re
from typing import Union, Any

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
    userAnswer: str


class FeedbackStateSchema(Schema):
    userAnswer = fields.Str(required=True)

    @post_load
    def make_obj(self, data):
        return FeedbackStateModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class FeedbackMarkupModel(GenericMarkupModel):
    inputstem: Union[str, Missing] = missing
    followid: Union[str, Missing] = missing
    field: Union[str, Missing] = missing
    questionItems: Union[Any, Missing] = missing
    choice: Union[Any, Missing] = missing
    matchElement: Union[Any, Missing] = missing


class FeedbackMarkupSchema(GenericMarkupSchema):
    inputstem = fields.Str()
    followid = fields.Str()
    field = fields.Str()
    questionItems = fields.Raw()
    choice= fields.Raw()
    matchElement= fields.Raw()

    @post_load
    def make_obj(self, data):
        return FeedbackMarkupModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class FeedbackInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    userword: str
    nosave: bool = missing


class FeedbackInputSchema(Schema):
    userword = fields.Str(required=True)
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

# TODO: Need to remake the saving for this plugin


@app.route('/answer/', methods=['put'])
@use_args(FeedbackAnswerSchema(strict=True), locations=("json",))
def answer(args: FeedbackAnswerModel):
    web = {}
    result = {'web': web}
    userword = args.input.userword

    # plugin can ask not to save the word
    nosave = args.input.nosave
    if not nosave:
        save = {"userword": userword}
        result["save"] = save
        web['result'] = "saved"

    return jsonify(result)


@app.route('/reqs/')
@app.route('/reqs')
def reqs():
    templates = ["""
``` {#feedback1 plugin="feedback"}
header: Your answer
inputstem: "Your choice:"
field: item1
```""","""
``` {#feedback2 plugin="feedback"}
header: Feedback
feedbackLevel: 3
toNextTaskRule: 2 correct answers in row
instructionID: InstructionsID
sampleItemID: sampleItemID
nextTask: linkki tähän
questionItems:
- pluginNames: [drop2, drop3]
  dropdownWords: [[is, do, are], [yesterday, today]]
  dragWords: [cat, mouse, dog, cat]
  correctAnswer: [is, today]
  correctAnswerFeedback: "Your answer was: [answer] which is correct."
  choices:
    - match:
        - index: 0
          answer: do
        - index: 2
          answer: asd
      levels:
        - vastasit [answer]. vastaus on hiukan väärin
        - vastasit [answer]. vastaus on hiukan väärin, mieti vielä
    - match: [are, yesterday]
      levels:
        - vastasit [answer]. vastaus on aivan pielessä
```
"""]
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
                                'text': 'Feedback without items',
                                'expl': 'Add a plugin to show feedback',
                            },
                            {
                                'data': templates[1].strip(),
                                'text': 'Feedback with question items',
                                'expl': 'Add a plugin to show feedback',
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
