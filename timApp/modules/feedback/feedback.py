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
    correct: bool
    feedback: str
    sentence: str
    id: int

class FeedbackStateSchema(Schema):
    correct = fields.Bool(required=True)
    feedback = fields.Str(required=True)
    sentence = fields.Str(required=True)
    id = fields.Int(required=True)

    @post_load
    def make_obj(self, data):
        return FeedbackStateModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class FeedbackMarkupModel(GenericMarkupModel):
    questionItems: Union[Any, Missing] = missing
    choice: Union[Any, Missing] = missing
    matchElement: Union[Any, Missing] = missing
    instructionID: Union[Any, Missing] = missing
    correctStreak: Union[Any, Missing] = missing
    teacherHide: Union[Any, Missing] = missing


class FeedbackMarkupSchema(GenericMarkupSchema):
    questionItems = fields.Raw()
    choice= fields.Raw()
    matchElement= fields.Raw()
    instructionID = fields.Str()
    correctStreak = fields.Int()
    teacherHide = fields.Bool()

    @post_load
    def make_obj(self, data):
        return FeedbackMarkupModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class FeedbackInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    correct: bool
    feedback: str
    sentence: str
    id: int
    nosave: bool = missing

class FeedbackInputSchema(Schema):
    correct = fields.Bool(required=True)
    feedback = fields.Str(required=True)
    sentence = fields.Str(required=True)
    id = fields.Int(required=True)
    nosave = fields.Bool()

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
    sentence = args.input.sentence
    feedback = args.input.feedback
    correct = args.input.correct
    id = args.input.id

    # plugin can ask not to save the word
    nosave = args.input.nosave
    if not nosave:
        save = {"sentence": sentence, "correct": correct, "feedback": feedback, "id": id}
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
feedbackLevel: 3
toNextTaskRule: 2 correct answers in row
nextTask: linkki tähän
questionItems:
- pluginNames: [drop1]
  words: [[is, do, are]]
  correctAnswer: [is]
  correctAnswerFeedback: vastasit [answer] vastaus on oikein, drop1 feedback
  choices:
    - match: [do]
      levels:
        - vastasit [answer]. vastaus on aivan pielessä, level1 drop1 feedback
        - vastasit [answer]. vastaus on aivan plääh, level2 drop1 feedback
        - "oikea vastaus: is, level3 drop1 feedback"
    - match: [are]
      levels:
        - vastasit [answer]. aika lähellä
        - vastasit [answer]. mietipä vielä hetki
        - "oikea vastaus: is, level2 drop1 feedback"
    - match: []
      levels:
        - default feedback for drop1
- pluginNames: [drop2, drop3]
  words: [[is, do, are], [yesterday, today]]
  correctAnswer: [is, today]
  correctAnswerFeedback: vastasit [answer] vastaus on oikein, drop2 feedback
  choices:
    - match: [are, yesterday]
      levels:
        - vastasit [answer]. vastaus on aivan pielessä, level1 drop2 feedback
        - vastasit [answer]. vastaus on aivan plääh, level2 drop2 feedback
        - "oikea vastaus: is, today, level3 drop2 feedback"
    - match: [is, yesterday]
      levels:
        - vastasit [answer]. aika lähellä, level1 drop2 feedback
        - vastasit [answer]. mietipä vielä hetki, level2 drop2 feedback
        - "oikea vastaus: is, today, level3 drop2 feedback"
    - match: [do, today]
      levels:
        - vastasit [answer]. virhe, level1 drop2 feedback
        - vastasit [answer]. töttöröö, level2 drop2 feedback
        - "oikea vastaus: is, today, level3 drop2 feedback"
    - match: []
      levels:
        - default feedback for drop2,drop3
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
