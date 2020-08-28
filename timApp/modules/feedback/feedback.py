"""
TIM feedback-plugin.
"""
from dataclasses import dataclass, asdict
from typing import Union, Any, List, Dict

from flask import render_template_string
from marshmallow import validates, ValidationError
from marshmallow.utils import missing

from markupmodels import GenericMarkupModel
from pluginserver_flask import GenericHtmlModel, \
    GenericAnswerModel, register_plugin_app, launch_if_main
from utils import Missing


@dataclass
class FeedbackStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    user_answer: str
    correct: bool
    feedback: str
    correct_answer: str


@dataclass
class FeedbackMarkupModel(GenericMarkupModel):
    area: Union[str, Missing] = missing
    choice: Union[Any, Missing] = missing
    correctsInRow: Union[int, Missing] = missing
    dragSource: Union[str, Missing] = missing
    matchElement: Union[Any, Missing] = missing
    nextTask: Union[str, Missing] = missing
    points_array: Union[List[float], Missing] = missing
    practiceID: Union[str, Missing] = missing
    questionItems: Union[Any, Missing] = missing
    showAnswers: Union[bool, Missing] = missing
    shuffle: Union[bool, Missing] = missing

    @validates('points_array')
    def validate_points_array(self, value: List[float]) -> None:
        if len(value) != 2:
            raise ValidationError('Must be of size 1 x 2.')


@dataclass
class FeedbackInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    user_answer: str
    correct: bool
    feedback: str
    correct_answer: str
    nosave: Union[bool, Missing] = missing


@dataclass
class FeedbackHtmlModel(GenericHtmlModel[FeedbackInputModel, FeedbackMarkupModel, FeedbackStateModel]):
    def get_component_html_name(self) -> str:
        return 'feedback-runner'

    def get_static_html(self) -> str:
        return render_static_feedback(self)


@dataclass
class FeedbackAnswerModel(GenericAnswerModel[FeedbackInputModel, FeedbackMarkupModel, FeedbackStateModel]):
    pass


def render_static_feedback(m: FeedbackHtmlModel) -> str:
    return render_template_string(
        """
<div class="csRunDiv no-popup-menu">
    <h4>{{ header }}</h4>
    <p class="stem">{{ stem }}</p>
    <div><label>{{ inputstem or '' }}</label>
    </div>
    <button class="timButton">
        {{ buttonText or button or "Save" }}
    </button>
    <a>{{ resetText }}</a>
    <p class="plgfooter">{{ footer }}</p>
</div>
        """,
        **asdict(m.markup),
    )


def answer(args: FeedbackAnswerModel) -> Dict:
    web: Dict[str, Any] = {}
    result = {'web': web}
    correct_answer = args.input.correct_answer
    feedback = args.input.feedback
    correct = args.input.correct
    user_answer = args.input.user_answer
    points_array = args.markup.points_array
    if isinstance(points_array, Missing):
        points_array = [0, 1]
    points = points_array[1 if correct else 0]

    # Plugin can ask not to save the word.
    nosave = args.input.nosave
    if not nosave:
        tim_info = {"points": points}
        save = {"correct_answer": correct_answer, "correct": correct, "feedback": feedback, "user_answer": user_answer}
        result["save"] = save
        result["tim_info"] = tim_info
        web['result'] = "saved"

    return result


def reqs() -> Dict:
    templates = ["""#- {area="dropdowntask1" .task}

## Instruction header {.instruction defaultplugin="dropdown"}

Here you can write general instructions for the test. Pictures can also be inserted. The
"Instruction header" can also be changed, but do not change the `.instructions` in the
brackets.

The next is a practice question. It can be edited or deleted.

I {#practice words: ["will think", "won't think", "might think"]#} before answering.

## Item header {defaultplugin="dropdown"}
::: {.info}
Anything inside this `.info` section starting with `::: {.info}` and ending with `:::` 
will not be  a part of the actual question/answer. Here you can place extra instructions 
or you may delete the section.

The question ID `#dropdown1` should be unique to the task. It refers to the feedback and 
should be edited in both the question item and feedback plugins. 
:::

What {#dropdown1 shuffle: true#} on the stove?

## Item header {defaultplugin="dropdown"}
::: {.info}
This is the `.info` section.
:::

Who {#dropdown2 shuffle: true#} the cake?

## Item header {defaultplugin="dropdown"}
::: {.info}
This is the `.info` section.
:::

What {#dropdown3 shuffle: true#} on the roof?

## Item header {defaultplugin="dropdown"}
::: {.info}
This is the `.info` section.
:::

Who {#dropdown4 shuffle: true#} the 3 mile swim in the race?

``` {#fb1 plugin="feedback"}
# Quick reference for feedback options:
#  correctsInRow: number of correct answers in a row to advance to next task.
#  nextTask: the address of next task in the TIM file system.
#  shuffle: (true or false) whether question items are given in random order.
#  questionItems: contains the question items and the feedback for them.
#  - pluginNames: names of the target question item plugins.
#    words: list of words to be set as choices to the plugin(s) in pluginNames.
#    choices: defines the possible match choices and the feedback for each.
#     - match: defines a choice to match to trigger the defined feedback.
#       correct: true, indicates that the choice is the correct answer.
#       levels: defines the feedback levels for the matching choice.
#
# Using & + word (example: &match1) after "levels:" you can create a reference. 
# You can later refer to the defined reference levels with * + word (*match1).
#
correctsInRow: 2  
nextTask: "[Click here](next_task_document_name) to move to the next task."
shuffle: true
questionItems:
- pluginNames: ["dropdown1"]
  words: [["is cooking", "do cooking", "are cooking"]]
  choices:
    - match: ["is cooking"]
      correct: true
      levels: &rightmatch
        - "**Correct!** You answered: *|correct|*"
    - match: ["do cooking"]
      levels: &match1
        - "You can write the level 1 feedback for the match choice here."
        - "You can write the level 2 feedback for the match choice here."
        - "You can write the level 3 feedback for the match choice here."
        - "You can write the level 4 feedback for the match choice here."
    - match: ["are cooking"]
      levels: &match2
        - "Level 1: |answer| is a placeholder for the given answer."
        - "Level 2: |part[0]| refers to the 1st part of the answer."
        - "Level 3: |match[0]| is a placeholder for the current match choice."
        - "Level 4: |correct| is a placeholder for the correct answer."
    - match: []  # Empty brackets for default feedback.
      levels: &defaultmatch
        - "*Level 1 default feedback* in italics with *"
        - "**Level 2 default feedback** in bold with **."
        - "<u>Level 3 default feedback</u> now underlined."
        - "[Level 4 default feedback]{.red} now in red color."
- pluginNames: ["dropdown2"]
  words: [["is baking", "do baking", "are baking"]]
  choices:
    - match: ["is baking"]
      correct: true
      levels: *rightmatch
    - match: ["do baking"]
      levels: *match1
    - match: ["are baking"]
      levels: *match2
    - match: []
      levels: *defaultmatch
- pluginNames: ["dropdown3"]
  words: [["is jumping", "do jumping", "are jumping"]]
  choices:
    - match: ["is jumping"]
      correct: true
      levels: *rightmatch
    - match: ["do jumping"]
      levels: *match1
    - match: ["are jumping"]
      levels: *match2
    - match: []
      levels: *defaultmatch
- pluginNames: ["dropdown4"]
  words: [["is swimming", "do swimming", "are swimming"]]
  choices:
    - match: ["is swimming"]
      correct: true
      levels: *rightmatch
    - match: ["do swimming"]
      levels: *match1
    - match: ["are swimming"]
      levels: *match2
    - match: []
      levels: *defaultmatch

```

#- {area_end="dropdowntask1"}""", """#- {area="draganddroptask1" .task}

## Instruction header {.instruction defaultplugin="drag"}

Here you can write general instructions for the test. Pictures can also be inserted. The
"Instruction header" can also be changed, but do not change the `.instructions` in the
brackets.

The next is a practice question. It can be edited or deleted.

Drag from here: {#practicedrag1 words: ["I", "before", "will think", "answering"]#}

To here: {#practicedrop2#}.

## Item header {defaultplugin="drag"}
::: {.info}
Anything inside this `.info` section starting with `::: {.info}` and ending with `:::` 
will not be  a part of the actual question/answer. Here you can place extra instructions
and the draggable words in "drag1".

{#drag1 shuffle: true, words: ["I", "when", "around", "come"]#}

The question ID `#drop1` should be unique to the task. It refers to the feedback 
and should be edited in both the question item and feedback plugins. 
:::

You know where I'll be found {#drop1#}.

## Item header {defaultplugin="drag"}
::: {.info}
This is the `.info` section with draggable words.

{#drag2 shuffle: true, words: ["I", "if", "a mile", "run"]#}
:::

I will be quite tired {#drop2#}.

## Item header {defaultplugin="drag"}
::: {.info}
This is the `.info` section with draggable words.

{#drag3 shuffle: true, words: ["I", "who", "at work", "see"]#}
:::

I will tell you {#drop3#}.

## Item header {defaultplugin="drag"}
::: {.info}
This is the `.info` section with draggable words.

{#drag4 shuffle: true, words: ["I", "whether", "a computer", "had"]#}
:::

He wanted to know {#drop4#}.

``` {#fb1 plugin="feedback"}
# Quick reference for feedback options:
#  correctsInRow: number of correct answers in a row to advance to next task.
#  nextTask: the address of next task in the TIM file system.
#  shuffle: (true or false) whether question items are given in random order.
#  questionItems: contains the question items and the feedback for them.
#  - pluginNames: names of the target question item plugins.
#    dragSource: drag1, used only for case with one repeating question.
#    words: [] for drag and drop questions.
#    choices: defines the possible match choices and the feedback for each.
#     - match: defines a choice to match to trigger the defined feedback.
#       correct: true, indicates that the choice is the correct answer.
#       levels: defines the feedback levels for the matching choice.
#
# Using & + word (example: &match1) after "levels:" you can create a reference. 
# You can later refer to the defined reference levels with * + word (*match1).
#
correctsInRow: 2
nextTask: "[Click here](next_task_document_name) to move to the next task."
shuffle: true
questionItems:
- pluginNames: ["drop1"]
  words: []
  choices:
    - match: ["when I come around"]
      correct: true
      levels: &rightmatch
        - "**Correct!** You answered: *|correct|*"
    - match: ["when around I come"]
      levels: &match1
        - "You can write the level 1 feedback for the match choice here."
        - "You can write the level 2 feedback for the match choice here."
        - "You can write the level 3 feedback for the match choice here."
        - "You can write the level 4 feedback for the match choice here."
    - match: ["when come I around"]
      levels: &match2
        - "Level 1: |answer| is a placeholder for the given answer."
        - "Level 2: |part[0]| refers to the 1st part of the answer."
        - "Level 3: |match[0]| is a placeholder for the current match choice."
        - "Level 4: |correct| is a placeholder for the correct answer."
    - match: [] # Empty brackets for default feedback.
      levels: &defaultmatch
        - "*Level 1 default feedback* in italics with *"
        - "**Level 2 default feedback** in bold with **."
        - "<u>Level 3 default feedback</u> now underlined."
        - "[Level 4 default feedback]{.red} now in red color."
- pluginNames: ["drop2"]
  words: []
  choices:
    - match: ["if I run a mile"]
      correct: true
      levels: *rightmatch
    - match: ["if a mile I run"]
      levels: *match1
    - match: ["if run I a mile"]
      levels: *match2
    - match: []
      levels: *defaultmatch
- pluginNames: ["drop3"]
  words: []
  choices:
    - match: ["who I see at work"]
      correct: true
      levels: *rightmatch
    - match: ["who at work I see"]
      levels: *match1
    - match: ["who see I at work"]
      levels: *match2
    - match: []
      levels: *defaultmatch
- pluginNames: ["drop4"]
  words: []
  choices:
    - match: ["whether I had a computer"]
      correct: true
      levels: *rightmatch
    - match: ["whether a computer I had"]
      levels: *match1
    - match: ["whether had I a computer"]
      levels: *match2
    - match: []
      levels: *defaultmatch

```

#- {area_end="draganddroptask1"}""", """## Instruction header {.instruction defaultplugin="dropdown"}

Here you can write general instructions for the test. Pictures can also be inserted. The
"Instruction header" can also be changed, but do not change the `.instructions` in the
brackets.

The next is a practice question. It can be edited or deleted.

I {#practice words: ["will think", "won't think", "might think"]#} before answering.

""", """## Instruction header {.instruction defaultplugin="drag"}

Here you can write general instructions for the test. Pictures can also be inserted. The
"Instruction header" can also be changed, but do not change the `.instructions` in the
brackets.

The next is a practice question. It can be edited or deleted.

Drag from here: {#practicedrag1 words: ["I", "before", "will think", "answering"]#}

To here: {#practicedrop2#}.

""", """## Item header {defaultplugin="dropdown"}
::: {.info}
Anything inside this `.info` section starting with `::: {.info}` and ending with `:::` 
will not be  a part of the actual question/answer. Here you can place extra instructions 
or you may delete the section.

The question ID `#dropdown1` should be unique to the task. It refers to the feedback and 
should be edited in both the question item and feedback plugins. 
:::

What {#dropdown1 autosave: false, shuffle: true#} on the stove?

""", """## Item header {defaultplugin="drag"}
::: {.info}
Anything inside this `.info` section starting with `::: {.info}` and ending with `:::` 
will not be  a part of the actual question/answer. Here you can place extra instructions 
and the draggable words in "drag1".

{#drag1 shuffle: true , words: ["I", "when", "around", "come"]#}

The question ID `#drop1` should be unique to the task. It refers to the feedback and 
should be edited in both the question item and feedback plugins. 
:::

You know where I'll be found {#drop1#}.

""", """``` {#fb1 plugin="feedback"}
# Quick reference for feedback options:
#  correctsInRow: number of correct answers in a row to advance to next task.
#  nextTask: the address of next task in the TIM file system.
#  shuffle: (true or false) whether question items are given in random order.
#  questionItems: contains the question items and the feedback for them.
#  - pluginNames: names of the target question item plugins.
#    words: list of words to be set as choices to the plugin(s) in pluginNames.
#    choices: defines the possible match choices and the feedback for each.
#     - match: defines a choice to match to trigger the defined feedback.
#       correct: true, indicates that the choice is the correct answer.
#       levels: defines the feedback levels for the matching choice.
#
# Using & + word (example: &match1) after "levels:" you can create a reference. 
# You can later refer to the defined reference levels with * + word (*match1).
#
correctsInRow: 2  
nextTask: "[Click here](next_task_document_name) to move to the next task."
shuffle: true
questionItems:
- pluginNames: ["dropdown1"]
  words: [["is cooking", "do cooking", "are cooking"]]
  choices:
    - match: ["is cooking"]
      correct: true
      levels: &rightmatch
        - "**Correct!** You answered: *|correct|*"
    - match: ["do cooking"]
      levels: &match1
        - "You can write the level 1 feedback for the match choice here."
        - "You can write the level 2 feedback for the match choice here."
        - "You can write the level 3 feedback for the match choice here."
        - "You can write the level 4 feedback for the match choice here."
    - match: ["are cooking"]
      levels: &match2
        - "Level 1: |answer| is a placeholder for the given answer."
        - "Level 2: |part[0]| refers to the 1st part of the answer."
        - "Level 3: |match[0]| is a placeholder for the current match choice."
        - "Level 4: |correct| is a placeholder for the correct answer."
    - match: []  # Empty brackets for default feedback.
      levels: &defaultmatch
        - "*Level 1 default feedback* in italics with *"
        - "**Level 2 default feedback** in bold with **."
        - "<u>Level 3 default feedback</u> now underlined."
        - "[Level 4 default feedback]{.red} now in red color."
- pluginNames: ["dropdown2"]
  words: [["is baking", "do baking", "are baking"]]
  choices:
    - match: ["is baking"]
      correct: true
      levels: *rightmatch
    - match: ["do baking"]
      levels: *match1
    - match: ["are baking"]
      levels: *match2
    - match: []
      levels: *defaultmatch
- pluginNames: ["dropdown3"]
  words: [["is jumping", "do jumping", "are jumping"]]
  choices:
    - match: ["is jumping"]
      correct: true
      levels: *rightmatch
    - match: ["do jumping"]
      levels: *match1
    - match: ["are jumping"]
      levels: *match2
    - match: []
      levels: *defaultmatch
- pluginNames: ["dropdown4"]
  words: [["is swimming", "do swimming", "are swimming"]]
  choices:
    - match: ["is swimming"]
      correct: true
      levels: *rightmatch
    - match: ["do swimming"]
      levels: *match1
    - match: ["are swimming"]
      levels: *match2
    - match: []
      levels: *defaultmatch

```""", """``` {#fb1 plugin="feedback"}
# Quick reference for feedback options:
#  correctsInRow: number of correct answers in a row to advance to next task.
#  nextTask: the address of next task in the TIM file system.
#  shuffle: (true or false) whether question items are given in random order.
#  questionItems: contains the question items and the feedback for them.
#  - pluginNames: names of the target question item plugins.
#    dragSource: drag1, used only for case with one repeating question.
#    words: [] for drag and drop questions.
#    choices: defines the possible match choices and the feedback for each.
#     - match: defines a choice to match to trigger the defined feedback.
#       correct: true, indicates that the choice is the correct answer.
#       levels: defines the feedback levels for the matching choice.
#
# Using & + word (example: &match1) after "levels:" you can create a reference. 
# You can later refer to the defined reference levels with * + word (*match1).
#
correctsInRow: 2
nextTask: "[Click here](next_task_document_name) to move to the next task."
shuffle: true
questionItems:
- pluginNames: ["drop1"]
  words: []
  choices:
    - match: ["when I come around"]
      correct: true
      levels: &rightmatch
        - "**Correct!** You answered: *|correct|*"
    - match: ["when around I come"]
      levels: &match1
        - "You can write the level 1 feedback for the match choice here."
        - "You can write the level 2 feedback for the match choice here."
        - "You can write the level 3 feedback for the match choice here."
        - "You can write the level 4 feedback for the match choice here."
    - match: ["when come I around"]
      levels: &match2
        - "Level 1: |answer| is a placeholder for the given answer."
        - "Level 2: |part[0]| refers to the 1st part of the answer."
        - "Level 3: |match[0]| is a placeholder for the current match choice."
        - "Level 4: |correct| is a placeholder for the correct answer."
    - match: [] # Empty brackets for default feedback.
      levels: &defaultmatch
        - "*Level 1 default feedback* in italics with *"
        - "**Level 2 default feedback** in bold with **."
        - "<u>Level 3 default feedback</u> now underlined."
        - "[Level 4 default feedback]{.red} now in red color."
- pluginNames: ["drop2"]
  words: []
  choices:
    - match: ["if I run a mile"]
      correct: true
      levels: *rightmatch
    - match: ["if a mile I run"]
      levels: *match1
    - match: ["if run I a mile"]
      levels: *match2
    - match: []
      levels: *defaultmatch
- pluginNames: ["drop3"]
  words: []
  choices:
    - match: ["who I see at work"]
      correct: true
      levels: *rightmatch
    - match: ["who at work I see"]
      levels: *match1
    - match: ["who see I at work"]
      levels: *match2
    - match: []
      levels: *defaultmatch
- pluginNames: ["drop4"]
  words: []
  choices:
    - match: ["whether I had a computer"]
      correct: true
      levels: *rightmatch
    - match: ["whether a computer I had"]
      levels: *match1
    - match: ["whether had I a computer"]
      levels: *match2
    - match: []
      levels: *defaultmatch

```"""]
    return {
        "js": ["js/build/feedback.js"],
        "multihtml": True,
        "css": ["css/feedback.css"],
        'editor_tabs': [
            {
                'text': 'Fields',
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
    }


app = register_plugin_app(
    __name__,
    html_model=FeedbackHtmlModel,
    answer_model=FeedbackAnswerModel,
    answer_handler=answer,
    reqs_handler=reqs,
)


launch_if_main(__name__, app)
