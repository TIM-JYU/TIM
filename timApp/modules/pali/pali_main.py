"""
TIM example plugin: a palindrome checker.
"""
import os
import re
from dataclasses import dataclass, asdict
from typing import Union, List

from flask import render_template_string
from marshmallow import validates, ValidationError, missing

from tim_common.markupmodels import GenericMarkupModel
from tim_common.pluginserver_flask import GenericHtmlModel, \
    GenericAnswerModel, register_plugin_app, launch_if_main, PluginAnswerResp, PluginAnswerWeb, PluginReqs, EditorTab
from tim_common.utils import Missing


@dataclass
class PaliStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    userword: str


@dataclass
class PaliMarkupModel(GenericMarkupModel):
    points_array: Union[List[List[float]], Missing] = missing
    inputstem: Union[str, Missing] = missing
    needed_len: Union[int, Missing] = missing
    initword: Union[str, Missing] = missing
    cols: Union[int, Missing] = missing
    inputplaceholder: Union[str, Missing] = missing

    @validates('points_array')
    def validate_points_array(self, value: Union[List[List[float]], Missing]) -> None:
        if isinstance(value, list) and (len(value) != 2 or not all(len(v) == 2 for v in value)):
            raise ValidationError('Must be of size 2 x 2.')


@dataclass
class PaliInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    userword: str
    paliOK: Union[bool, Missing] = missing
    nosave: Union[bool, Missing] = missing

    @validates('userword')
    def validate_userword(self, word: str) -> None:
        if not word:
            raise ValidationError('Must not be empty.')


@dataclass
class PaliHtmlModel(GenericHtmlModel[PaliInputModel, PaliMarkupModel, PaliStateModel]):
    def get_component_html_name(self) -> str:
        return 'pali-runner'

    def get_static_html(self) -> str:
        return render_static_pali(self)


@dataclass
class PaliAnswerModel(GenericAnswerModel[PaliInputModel, PaliMarkupModel, PaliStateModel]):
    pass


def render_static_pali(m: PaliHtmlModel) -> str:
    return render_template_string(
        """
<div class="csRunDiv no-popup-menu">
{% if header %}<h4>{{ header }}</h4>{% endif %}
{% if stem %}<p class="stem">{{ stem }}</p>{% endif %}
<div><label>{{ inputstem or '' }}
<input type="text" class="form-control" placeholder="{{inputplaceholder or ''}}" value="{{userword or ''}}" size="{{cols or ''}}"></label>
</div>
<button class="timButton">
{{ buttonText or button or "Save" }}
</button>
{% if footer %}<p class="plgfooter">{{ footer }}</p>{% endif %}
</div>
        """.strip(),
        **asdict(m.markup),
        userword=m.state.userword if m.state else '',
    )


def answer(args: PaliAnswerModel) -> PluginAnswerResp:
    web: PluginAnswerWeb = {}
    result: PluginAnswerResp = {'web': web}
    needed_len = args.markup.needed_len
    userword = args.input.userword
    pali_ok = args.input.paliOK or False
    len_ok = True
    if not isinstance(needed_len, Missing):
        len_ok = check_letters(userword, needed_len)
    if not len_ok:
        web['error'] = "Wrong length"
    if not needed_len and not pali_ok:
        len_ok = False
    points_array = args.markup.points_array if isinstance(args.markup.points_array, list) else [[0, 0.25], [0.5, 1]]
    p_index = 1 if pali_ok else 0
    l_index = 1 if len_ok else 0
    points = points_array[p_index][l_index]

    # plugin can ask not to save the word
    nosave = args.input.nosave
    if not nosave:
        save = {"userword": userword}
        result["save"] = save
        result["tim_info"] = {"points": points}
        web['result'] = "saved"

    return result


def check_letters(word: str, needed_len: int) -> bool:
    """Checks if word has needed amount of chars.

    :param word: word to check
    :param needed_len: how many letters needed
    :return: true if len match

    """
    s = word.upper()
    return len(re.sub("[^[A-ZÅÄÖ]", "", s)) == needed_len


def reqs() -> PluginReqs:
    templates = ["""
``` {#ekapali plugin="pali"}
header: Kirjoita palindromi
stem: Kirjoita palindromi, jossa on 5 kirjainta.
-points_array: [[0, 0.1], [0.6, 1]]
inputstem: "Palindromisi 5 kirjainta inputstem:"
needed_len: 5
answerLimit: 3
initword: muikku
cols: 20
```""", """
``` {#tokapali plugin="pali"}
header: Kirjoita palindromi
stem: Kirjoita palindromi, jossa on 7 kirjainta.
-points_array: [[0, 0.1], [0.6, 1]]
inputstem: "Palindromisi:"
needed_len: 7
answerLimit: 4
initword: muikku
cols: 20
```"""]
    editor_tabs: List[EditorTab] = [
            {
                'text': 'Plugins',
                'items': [
                    {
                        'text': 'Pali',
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
        ]
    result: PluginReqs = {
        "js": ["js/build/pali.js"],
        "multihtml": True,
    }

    # Show pali templates only in development mode.
    if os.environ['COMPOSE_PROFILES'] == 'dev':
        result['editor_tabs'] = editor_tabs

    return result


app = register_plugin_app(
    __name__,
    html_model=PaliHtmlModel,
    answer_model=PaliAnswerModel,
    answer_handler=answer,
    reqs_handler=reqs,
)


launch_if_main(__name__, app)
