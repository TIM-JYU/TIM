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
    points_array: Union[str, Missing] = missing
    questionItems: Union[Any, Missing] = missing
    choice: Union[Any, Missing] = missing
    matchElement: Union[Any, Missing] = missing
    instructionID: Union[Any, Missing] = missing
    correctStreak: Union[Any, Missing] = missing
    nextTask: Union[Any, Missing] = missing
    dragSource: Union[str, Missing] = missing
    shuffle: Union[bool, Missing] = missing
    area: Union[str, Missing] = missing
    showAnswers: Union[bool, Missing] = missing


class FeedbackMarkupSchema(GenericMarkupSchema):
    points_array = fields.List(fields.Number())
    questionItems = fields.Raw()
    choice = fields.Raw()
    matchElement = fields.Raw()
    instructionID = fields.Str()
    correctStreak = fields.Int()
    nextTask = fields.Str()
    dragSource = fields.Str()
    shuffle = fields.Bool()
    area = fields.Str()
    showAnswers = fields.Bool()

    @validates('points_array')
    def validate_points_array(self, value):
        #or not all(len(v) == 1 for v in value)
        if len(value) != 2:
            raise ValidationError('Must be of size 1 x 2.')

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
    points_array = args.markup.points_array or [0, 1]
    points = points_array[correct]

    # plugin can ask not to save the word
    nosave = args.input.nosave
    if not nosave:
        tim_info = {"points": points}
        save = {"sentence": sentence, "correct": correct, "feedback": feedback, "answer": answer}
        result["save"] = save
        result["tim_info"] = tim_info
        web['result'] = "saved"

    return jsonify(result)


@app.route('/reqs/')
@app.route('/reqs')
def reqs():
    templates = ["""#- {area="dropdowntask1" .task}

## Instructions {.instruction defaultplugin="dropdown"}

Welcome to the test. Read the question carefully. If you get the answer wrong, please read the feedback carefully.


Please try out a practice question:


I {#practice words: [will think, won't think, might think]} before answering.


## Item: {defaultplugin="dropdown"}

What {#dropdown1} on the stove?


## Item: {defaultplugin="dropdown"}

Who {#dropdown2} the cake?


## Item: {defaultplugin="dropdown"}

What {#dropdown3} on the roof?


## Item: {defaultplugin="dropdown"}

Who {#dropdown4} the 3 mile swim in the race?
 

``` {#fb1 plugin="feedback"}
correctStreak: 2
nextTask: nonexttaskdefined
questionItems:
- pluginNames: [dropdown1]
  words: [[is cooking, do cooking, are cooking]]
  choices:
    - match: [is cooking]
      correct: true
      levels: &ismatch
        - "**Correct!** You answered: |answer|"
    - match: [do cooking]
      levels: &domatch
        - "Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about what the answer **is**. "
        - "Level 5 feedback: Please note the correct answer: 'What **is** / Who **is**'"
    - match: [are cooking]
      levels: &arematch
        - "Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about what the answer **is**. "
        - "Level 5 feedback: Please note the correct answer: 'What **is** / Who **is**'"
    - match: []
      levels: &defaultmatch
        - "Level 1 feedback: default feedback for drop4"
        - "Level 2 feedback: default feedback for drop4"
        - "Level 3 feedback: default feedback for drop4"
        - "Level 4 feedback: default feedback for drop4"
        - "Level 5 feedback: Please note the correct answer: 'What **is** / Who **is**'"
- pluginNames: [dropdown2]
  words: [[is baking, do baking, are baking]]
  choices:
    - match: [is baking]
      correct: true
      levels: *ismatch
    - match: [do baking]
      levels: *domatch
    - match: [are baking]
      levels: *arematch
    - match: []
      levels: *defaultmatch
- pluginNames: [dropdown3]
  words: [[is jumping, do jumping, are jumping]]
  choices:
    - match: [is jumping]
      correct: true
      levels: *ismatch
    - match: [do jumping]
      levels: *domatch
    - match: [are jumping]
      levels: *arematch
    - match: []
      levels: *defaultmatch
- pluginNames: [dropdown4]
  words: [[is swimming, do swimming, are swimming]]
  choices:
    - match: [is swimming]
      correct: true
      levels: *ismatch
    - match: [do swimming]
      levels: *domatch
    - match: [are swimming]
      levels: *arematch
    - match: []
      levels: *defaultmatch
```
#- {area_end="dropdowntask1"}""", """#- {area="dragtask1" .task}

## Instructions {.instruction defaultplugin="drag"}

Welcome to the test. Read the question carefully. If you get the answer wrong, please read the feedback carefully.


Please try out a practice question:

Order the words correctly into the sentence below.

\
Drag from here: {#practice1 words: [I, before, will think, answering]}


To here: {#practice2}.


## Item {defaultplugin="drag"}
::: {.info}
Please order the words correctly into the sentence below.


\
{#drag1 words: [I, when, around, come]}

\
:::
You know where I'll be found {#drop1}.


## Item {defaultplugin="drag"}
::: {.info}
Please order the words correctly into the sentence below.


\
{#drag2 words: [I, if, a mile, run]}

\
:::
I will be quite tired {#drop2}.


## Item {defaultplugin="drag"}
::: {.info}
Please order the words correctly into the sentence below.


\
{#drag3 words: [I, who, at work, see]}

\
:::
I will tell you {#drop3}.


## Item {defaultplugin="drag"}
::: {.info}
Please order the words correctly into the sentence below.


\
{#drag4 words: [I, whether, a computer, had]}

\
:::
He wanted to know {#drop4}.



``` {#fb1 plugin="feedback"}

correctStreak: 2
nextTask: nonexttaskdefined
questionItems:
- pluginNames: [drop1]
  dragSource: drag1
  words: []
  choices:
    - match: [when I come around]
      correct: true
      levels: &rightmatch
        - "**Correct!** You answered: |answer|"
    - match: [when around I come]
      levels: &asvmatch
        - "Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about **what the answer is**. "
        - "Level 5 feedback: Please note the correct word order: 'conjuction subject verb the rest'"
    - match: [when come I around]
      levels: &vsamatch
        - "Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about **what the answer is** "
        - "Level 5 feedback: Please note the correct word order: 'conjuction subject verb the rest'"
    - match: []
      levels: &defaultmatch
        - "Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**."
        - "Level 2 feedback: You answered: *|answer|* Answer is wrong. Think about what comes first."
        - "Level 3 feedback: You answered: *|answer|* Answer is wrong. Think about what word comes first."
        - "Level 4 feedback: You answered: *|answer|* Answer is wrong. The conjunction should come first."
        - "Level 5 feedback: Please note the correct word order: 'conjuction subject verb the rest'"
- pluginNames: [drop2]
  dragSource: drag2
  words: []
  choices:
    - match: [if I run a mile]
      correct: true
      levels: *rightmatch
    - match: [if a mile I run]
      levels: *asvmatch
    - match: [if run I a mile]
      levels: *vsamatch
    - match: []
      levels: *defaultmatch
- pluginNames: [drop3]
  dragSource: drag3
  words: []
  choices:
    - match: [who I see at work]
      correct: true
      levels: *rightmatch
    - match: [who at work I see]
      levels: *asvmatch
    - match: [who see I at work]
      levels: *vsamatch
    - match: []
      levels: *defaultmatch
- pluginNames: [drop4]
  dragSource: drag4
  words: []
  choices:
    - match: [whether I had a computer]
      correct: true
      levels: *rightmatch
    - match: [whether a computer I had]
      levels: *asvmatch
    - match: [whether had I a computer]
      levels: *vsamatch
    - match: []
      levels: *defaultmatch

```
#- {area_end="dragtask1"}""", """## Instructions {.instruction defaultplugin="dropdown"}

Welcome to the test. Read the question carefully. If you get the answer wrong, please read the feedback carefully.


Please try out a practice question:


I {#practice words: [will think, won't think, might think]} before answering.

""", """## Instructions {.instruction defaultplugin="drag"}

Welcome to the test. Read the question carefully. If you get the answer wrong, please read the feedback carefully.


Please try out a practice question:

Order the words correctly into the sentence below.

\
Drag from here: {#practice1 words: [I, before, will think, answering]}


To here: {#practice2}.

""", """## Item: {defaultplugin="dropdown"}

What {#drop1} on the stove?

""", """## Item {defaultplugin="drag"}
::: {.info}
Please order the words correctly into the sentence below.


\
{#drag1 words: [I, when, around, come]}

\
:::
You know where I'll be found {#drop1}.
""", """``` {#fb1 plugin="feedback"}
correctStreak: 2
nextTask: nonexttaskdefined
questionItems:
- pluginNames: [drop1]
  words: [[is cooking, do cooking, are cooking]]
  choices:
    - match: [is cooking]
      correct: true
      levels: &ismatch
        - "**Correct!** You answered: |answer|"
    - match: [do cooking]
      levels: &domatch
        - "Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about what the answer **is**. "
        - "Level 5 feedback: Please note the correct answer: 'What **is** / Who **is**'"
    - match: [are cooking]
      levels: &arematch
        - "Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about what the answer **is**. "
        - "Level 5 feedback: Please note the correct answer: 'What **is** / Who **is**'"
    - match: []
      levels: &defaultmatch
        - "Level 1 feedback: default feedback for drop4"
        - "Level 2 feedback: default feedback for drop4"
        - "Level 3 feedback: default feedback for drop4"
        - "Level 4 feedback: default feedback for drop4"
        - "Level 5 feedback: Please note the correct answer: 'What **is** / Who **is**'"
- pluginNames: [drop2]
  words: [[is baking, do baking, are baking]]
  choices:
    - match: [is baking]
      correct: true
      levels: *ismatch
    - match: [do baking]
      levels: *domatch
    - match: [are baking]
      levels: *arematch
    - match: []
      levels: *defaultmatch
      
```""", """``` {#fb1 plugin="feedback"}

correctStreak: 2
nextTask: nonexttaskdefined
questionItems:
- pluginNames: [drop1]
  dragSource: drag1
  words: []
  choices:
    - match: [when I come around]
      correct: true
      levels: &rightmatch
        - "**Correct!** You answered: |answer|"
    - match: [when around I come]
      levels: &asvmatch
        - "Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about **what the answer is**. "
        - "Level 5 feedback: Please note the correct word order: 'conjuction subject verb the rest'"
    - match: [when come I around]
      levels: &vsamatch
        - "Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**. "
        - "Level 2 feedback: You answered: *|answer|* Answer is wrong. Try thinking *a bit* harder. "
        - "Level 3 feedback: You answered: *|answer|* Answer is wrong. Try thinking **much more** harder. "
        - "Level 4 feedback: You answered: *|answer|* Answer is wrong. Just think about **what the answer is** "
        - "Level 5 feedback: Please note the correct word order: 'conjuction subject verb the rest'"
    - match: []
      levels: &defaultmatch
        - "Level 1 feedback: You answered: *|answer|* Answer is wrong. Try **thinking**."
        - "Level 2 feedback: You answered: *|answer|* Answer is wrong. Think about what comes first."
        - "Level 3 feedback: You answered: *|answer|* Answer is wrong. Think about what word comes first."
        - "Level 4 feedback: You answered: *|answer|* Answer is wrong. The conjunction should come first."
        - "Level 5 feedback: Please note the correct word order: 'conjuction subject verb the rest'"
- pluginNames: [drop2]
  words: []
  choices:
    - match: [if I run a mile]
      correct: true
      levels: *rightmatch
    - match: [if a mile I run]
      levels: *asvmatch
    - match: [if run I a mile]
      levels: *vsamatch
    - match: []
      levels: *defaultmatch
      
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
                                'text': 'Task with dropdown questions',
                                'expl': 'Add a whole task with instructions, dropdown items and feedback',
                            },
                            {
                                'data': templates[1].strip(),
                                'text': 'Task with drag & drop questions',
                                'expl': 'Add a whole task with instructions, drag & drop items and feedback',
                            },
                            {
                                'data': templates[2].strip(),
                                'text': 'Instruction block for dropdown task',
                                'expl': 'Add an instruction block with a dropdown question item',
                            },
                            {
                                'data': templates[3].strip(),
                                'text': 'Instruction block for drag & drop task',
                                'expl': 'Add an instruction block with a drag & drop question item',
                            },
                            {
                                'data': templates[4].strip(),
                                'text': 'Question item block with dropdown ',
                                'expl': 'Add a question item block with a dropdown question',
                            },
                            {
                                'data': templates[5].strip(),
                                'text': 'Question item block with drag & drop',
                                'expl': 'Add a question item block with a drag & drop question',
                            },
                            {
                                'data': templates[6].strip(),
                                'text': 'Feedback block for dropdown',
                                'expl': 'Add a feedback block for dropdown task',
                            },
                            {
                                'data': templates[7].strip(),
                                'text': 'Feedback block for drag & drop ',
                                'expl': 'Add a feedback block for drag & drop task',
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
