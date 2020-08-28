"""
Module for serving drag item-plugin.
"""
from dataclasses import dataclass, asdict
from typing import Union, List, Dict, Any

from flask import render_template_string
from marshmallow.utils import missing

from markupmodels import GenericMarkupModel
from pluginserver_flask import GenericHtmlModel, \
    GenericAnswerModel, register_plugin_app, launch_if_main
from utils import Missing


@dataclass
class DragStateModel:
    c: List[str]


@dataclass
class DragMarkupModel(GenericMarkupModel):
    cols: Union[int, Missing] = missing
    copy: Union[str, Missing] = missing
    followid: Union[str, Missing] = missing
    inputstem: Union[str, Missing] = missing
    max: Union[int, Missing] = missing
    needed_len: Union[int, Missing] = missing
    savebutton: Union[bool, Missing] = missing
    shuffle: Union[bool, Missing] = missing
    trash: Union[bool, Missing] = missing
    type: Union[str, Missing] = missing
    words: Union[List[str], Missing] = missing
    autoSave: Union[bool, Missing] = missing


@dataclass
class DragInputModel:
    words: List[str]

    copy: Union[str, Missing] = missing
    max: Union[int, Missing] = missing
    nosave: Union[bool, Missing] = missing
    savebutton: Union[bool, Missing] = missing
    shuffle: Union[bool, Missing] = missing
    trash: Union[bool, Missing] = missing
    type: Union[str, Missing] = missing


@dataclass
class DragHtmlModel(GenericHtmlModel[DragInputModel, DragMarkupModel, DragStateModel]):
    def get_component_html_name(self) -> str:
        return 'drag-runner'

    def get_static_html(self) -> str:
        return render_static_drag(self)


@dataclass
class DragAnswerModel(GenericAnswerModel[DragInputModel, DragMarkupModel, DragStateModel]):
    pass


def render_static_drag(m: DragHtmlModel) -> str:
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
        **asdict(m.markup),
    )


def answer(args: DragAnswerModel) -> Dict:
    web: Dict[str, Any] = {}
    result = {'web': web}
    words = args.input.words

    nosave = args.input.nosave
    if not nosave:
        save = {"c": words}
        result["save"] = save
        web['result'] = "saved"

    return result


def reqs() -> Dict:
    templates = ["""
#- {defaultplugin="drag"}
{#drag1 #}
""",
"""
#- {defaultplugin="drag"}
{#drag2 words: [weather, is, lovely, almost, always] #}
""","""
#- {defaultplugin="drag"}
{#dragtrash trash: true #}
"""]
    return {
        "js": ["js/build/drag.js"],
        "multihtml": True,
        'editor_tabs': [
            {
                'text': 'Fields',
                'items': [
                    {
                        'text': 'Drag',
                        'items': [
                            {
                                'data': templates[0].strip(),
                                'text': 'Drag container without words',
                                'expl': 'Add drag container without words'
                            },
                            {
                                'data': templates[1].strip(),
                                'text': 'Drag container with words',
                                'expl': 'Add drag container with words'
                            },
                            {
                                'data': templates[2].strip(),
                                'text': 'Drag Trashcontainer',
                                'expl': 'Add drag trashcontainer for deleting non-copyable words'
                            },
                        ],
                    },
                ],
            },
        ],
    }


app = register_plugin_app(
    __name__,
    html_model=DragHtmlModel,
    answer_model=DragAnswerModel,
    answer_handler=answer,
    reqs_handler=reqs,
)


launch_if_main(__name__, app)
