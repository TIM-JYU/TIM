"""
Module for serving drag item-plugin.
"""
from typing import Union, List

from dataclasses import dataclass, asdict
from flask import jsonify, render_template_string
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from marshmallow_dataclass import class_schema
from pluginserver_flask import GenericHtmlModel, \
    GenericAnswerModel, create_app
from markupmodels import GenericMarkupModel
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


def render_static_drag(m: DragHtmlModel):
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


DragHtmlSchema = class_schema(DragHtmlModel)
DragAnswerSchema = class_schema(DragAnswerModel)


app = create_app(__name__, DragHtmlSchema)


@app.route('/answer', methods=['put'])
@use_args(DragAnswerSchema(), locations=("json",))
def answer(args: DragAnswerModel):
    web = {}
    result = {'web': web}
    words = args.input.words

    nosave = args.input.nosave
    if not nosave:
        save = {"c": words}
        result["save"] = save
        web['result'] = "saved"

    return jsonify(result)

@app.route('/reqs')
def reqs():
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
    return jsonify({
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
    })


if __name__ == '__main__':
    app.run(
        host='0.0.0.0',
        port=5000,
        debug=False,
    )
