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
    answer: str
    correct: bool
    feedback: str
    sentence: str

class FeedbackStateSchema(Schema):
    answer = fields.Str(required=True)
    correct = fields.Bool(required=True)
    feedback = fields.Str(required=True)
    sentence = fields.Str(required=True)

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
    nextTask: Union[Any, Missing] = missing


class FeedbackMarkupSchema(GenericMarkupSchema):
    questionItems = fields.Raw()
    choice= fields.Raw()
    matchElement= fields.Raw()
    instructionID = fields.Str()
    correctStreak = fields.Int()
    teacherHide = fields.Bool()
    nextTask = fields.Str()

    @post_load
    def make_obj(self, data):
        return FeedbackMarkupModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class FeedbackInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    answer: str
    correct: bool
    feedback: str
    sentence: str
    nosave: bool = missing

class FeedbackInputSchema(Schema):
    answer = fields.Str(required=True)
    correct = fields.Bool(required=True)
    feedback = fields.Str(required=True)
    sentence = fields.Str(required=True)
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
    answer = args.input.answer

    # plugin can ask not to save the word
    nosave = args.input.nosave
    if not nosave:
        save = {"sentence": sentence, "correct": correct, "feedback": feedback, "answer": answer}
        result["save"] = save
        web['result'] = "saved"

    return jsonify(result)


@app.route('/reqs/')
@app.route('/reqs')
def reqs():
    templates = ["""
``` {#fb1⁞ plugin="feedback"}
correctStreak: 2
nextTask: linkki tähän
instructionID: instruction
teacherHide: false
questionItems:
- pluginNames: [drop1]
  words: [[is, do, are]]
  choices:
    - match: [is]
      correct: true
      levels:
        - "md: **Correct!** You answered: |answer|"
    - match: [do]
      levels:
        - "md: Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "md: Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "md: Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "md: Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about what the answer **is**. "
        - "md: Level 5 feedback: Please note the correct answer: 'What **is** love?'  
                               'What time **is** it?' 
                               'What **is** your favourite colour?'
                               'What **is** the purpose of all this?"
    - match: [are]
      levels:
        - "md: Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "md: Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "md: Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "md: Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about what the answer **is**. "
        - "md: Level 5 feedback: Please note the correct answer: 'What **is** love?'  
                               'What time **is** it?' 
                               'What **is** your favourite colour?'
                               'What **is** the purpose of all this?"
    - match: []
      levels:
        - "md: Level 1 feedback: default feedback for drop4"
        - "md: Level 2 feedback: default feedback for drop4"
        - "md: Level 3 feedback: default feedback for drop4"
        - "md: Level 4 feedback: default feedback for drop4"
        - "md: Level 5 feedback: Please note the correct answer: 'What **is** love?'  
                               'What time **is** it?' 
                               'What **is** your favourite colour?'
                               'What **is** the purpose of all this?"
- pluginNames: [drop2]
  words: [[is, do, are]]
  choices:
    - match: [is]
      correct: true
      levels:
        - "md: **Correct!** Great job! You answered: |answer|"
    - match: [do]
      levels:
        - "md: Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "md: Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "md: Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "md: Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about what the answer **is**. "
        - "md: Level 5 feedback: Please note the correct answer: 'What **is** love?'  
                               'What time **is** it?' 
                               'What **is** your favourite colour?'
                               'What **is** the purpose of all this?"
    - match: [are]
      levels:
        - "md: Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "md: Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "md: Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "md: Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about what the answer **is**. "
        - "md: Level 5 feedback: Please note the correct answer: 'What **is** love?'  
                               'What time **is** it?' 
                               'What **is** your favourite colour?'
                               'What **is** the purpose of all this?"
    - match: []
      levels:
        - "md: Level 1 feedback: default feedback for drop4"
        - "md: Level 2 feedback: default feedback for drop4"
        - "md: Level 3 feedback: default feedback for drop4"
        - "md: Level 4 feedback: default feedback for drop4"
        - "md: Level 5 feedback: Please note the correct answer: 'What **is** love?'  
                               'What time **is** it?' 
                               'What **is** your favourite colour?'
                               'What **is** the purpose of all this?"
- pluginNames: [drop3]
  words: [[is, do, are]]
  choices:
    - match: [is]
      correct: true
      levels:
        - "md: **Correct!** Good! You answered: |answer|"
    - match: [do]
      levels:
        - "md: Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "md: Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "md: Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "md: Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about what the answer **is**. "
        - "md: Level 5 feedback: Please note the correct answer: 'What **is** love?'  
                               'What time **is** it?' 
                               'What **is** your favourite colour?'
                               'What **is** the purpose of all this?"
    - match: [are]
      levels:
        - "md: Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "md: Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking a bit harder. "
        - "md: Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "md: Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about what the answer **is**. "
        - "md: Level 5 feedback: Please note the correct answer: 'What **is** love?'  
                               'What time **is** it?' 
                               'What **is** your favourite colour?'
                               'What **is** the purpose of all this?"
    - match: []
      levels:
        - "md: Level 1 feedback: default feedback for drop4"
        - "md: Level 2 feedback: default feedback for drop4"
        - "md: Level 3 feedback: default feedback for drop4"
        - "md: Level 4 feedback: default feedback for drop4"
        - "md: Level 5 feedback: Please note the correct answer: 'What **is** love?'  
                               'What time **is** it?' 
                               'What **is** your favourite colour?'
                               'What **is** the purpose of all this?"
- pluginNames: [drop4]
  words: [[is, do, are]]
  choices:
    - match: [is]
      correct: true
      levels:
        - "md: **Correct!** Good! You answered: |answer|"
    - match: [do]
      levels:
        - "md: Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "md: Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "md: Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "md: Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about what the answer **is**. "
        - "md: Level 5 feedback: Please note the correct answer: 'What **is** love?'  
                               'What time **is** it?' 
                               'What **is** your favourite colour?'
                               'What **is** the purpose of all this?"
    - match: [are]
      levels:
        - "md: Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "md: Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "md: Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "md: Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about what the answer **is**. "
        - "md: Level 5 feedback: Please note the correct answer: 'What **is** love?'  
                               'What time **is** it?' 
                               'What **is** your favourite colour?'
                               'What **is** the purpose of all this?"
    - match: []
      levels:
        - "md: Level 1 feedback: default feedback for drop4"
        - "md: Level 2 feedback: default feedback for drop4"
        - "md: Level 3 feedback: default feedback for drop4"
        - "md: Level 4 feedback: default feedback for drop4"
        - "md: Level 5 feedback: Please note the correct answer: 'What **is** love?'  
                               'What time **is** it?' 
                               'What **is** your favourite colour?'
                               'What **is** the purpose of all this?"
```
""","""
- pluginNames: [#PLUGINNAMEHERE]
  words: [[is, do, are]]
  choices:
    - match: [is]
      correct: true
      levels:
        - "md: **Correct!** You answered: |answer|"
    - match: [do]
      levels:
        - "md: Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "md: Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "md: Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "md: Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about what the answer **is**. "
        - "md: Level 5 feedback: Please note the correct answer: 'What **is** love?'  
                               'What time **is** it?' 
                               'What **is** your favourite colour?'
                               'What **is** the purpose of all this?"
    - match: [are]
      levels:
        - "md: Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "md: Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "md: Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "md: Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about what the answer **is**. "
        - "md: Level 5 feedback: Please note the correct answer: 'What **is** love?'  
                               'What time **is** it?' 
                               'What **is** your favourite colour?'
                               'What **is** the purpose of all this?"
    - match: []
      levels:
        - "md: Level 1 feedback: default feedback for drop4"
        - "md: Level 2 feedback: default feedback for drop4"
        - "md: Level 3 feedback: default feedback for drop4"
        - "md: Level 4 feedback: default feedback for drop4"
        - "md: Level 5 feedback: Please note the correct answer: 'What **is** love?'  
                               'What time **is** it?' 
                               'What **is** your favourite colour?'
                               'What **is** the purpose of all this?"
""","""
- match: [is⁞]
  correct: true
  levels:
    - "md: **Correct!** You answered: |answer|"
""","""
- match: [do⁞]
  levels:
    - "md: Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
    - "md: Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
    - "md: Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
    - "md: Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about what the answer **is**. "
    - "md: Level 5 feedback: Please note the correct answer: 'What **is** love?'  
                           'What time **is** it?' 
                           'What **is** your favourite colour?'
                           'What **is** the purpose of all this?"
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
                                'text': 'Example with 4 items',
                                'expl': 'Add a feedback-plugin with 4 items',
                            },
                            {
                                'data': templates[1].strip(),
                                'text': 'Question item',
                                'expl': 'Add a question item',
                            },
                            {
                                'data': templates[2].strip(),
                                'text': 'Correct match',
                                'expl': 'Add a correct match with feedback-levels',
                            },
                            {
                                'data': templates[3].strip(),
                                'text': 'Match',
                                'expl': 'Add a match with feedback-levels',
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
