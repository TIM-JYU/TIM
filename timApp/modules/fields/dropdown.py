"""
Module for serving dropdown item-plugin.
"""
from typing import Union, List

from dataclasses import dataclass, asdict
from flask import jsonify, render_template_string, Blueprint, request
from marshmallow import validates, ValidationError
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from marshmallow_dataclass import class_schema
from pluginserver_flask import GenericHtmlModel, \
    GenericAnswerModel, make_base64, render_multihtml
from markupmodels import GenericMarkupModel
from utils import Missing

dropdown_route = Blueprint('dropdown', __name__, url_prefix="/dropdown")


@dataclass
class DropdownStateModel:
    c: str


@dataclass
class DropdownMarkupModel(GenericMarkupModel):
    words: Union[List[str], Missing] = missing
    instruction: Union[bool, Missing] = missing
    radio: Union[bool, Missing] = missing
    shuffle: Union[bool, Missing] = missing
    autosave: Union[bool, Missing] = missing
    answers: Union[bool, Missing] = missing
    tag: Union[str, Missing, None] = missing

@dataclass
class DropdownInputModel:
    selectedWord: str
    nosave: Union[bool, Missing] = missing
    shuffle: Union[bool, Missing] = missing

    @validates('selectedWord')
    def validate_selected_word(self, word):
        pass


@dataclass
class DropdownHtmlModel(GenericHtmlModel[DropdownInputModel, DropdownMarkupModel, DropdownStateModel]):
    def get_component_html_name(self) -> str:
        return 'dropdown-runner'

    def get_static_html(self) -> str:
        return render_static_dropdown(self)


@dataclass
class DropdownAnswerModel(GenericAnswerModel[DropdownInputModel, DropdownMarkupModel, DropdownStateModel]):
    pass


def render_static_dropdown(m: DropdownHtmlModel):
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


DropdownHtmlSchema = class_schema(DropdownHtmlModel)
DropdownAnswerSchema = class_schema(DropdownAnswerModel)


@dropdown_route.route('/multihtml', methods=['post'])
def dropdown_multihtml():
    ret = render_multihtml(request.get_json(), DropdownHtmlSchema())
    return ret


@dropdown_route.route('/answer', methods=['put'])
@use_args(DropdownAnswerSchema(), locations=("json",))
def answer(args: DropdownAnswerModel):
    web = {}
    result = {'web': web}
    selectedword = args.input.selectedWord
    # plugin can ask not to save the word
    nosave = args.input.nosave
    if not nosave:
        save = {"c": selectedword}
        result["save"] = save
        web['result'] = "saved"
    else:
        save = {"c": ""}
        result["save"] = save
        web['result'] = "saved"

    return jsonify(result)


@dropdown_route.route('/reqs')
def reqs():
    templates = ["""{#test:dropdown words: [option 1, option 2, option 3]#}""", """
#- {defaultplugin="dropdown"}

The weather {#drop1 words: [is,do,are⁞]#} nice today.
""", """
#- {defaultplugin="dropdown"}

The weather {#drop2 words: [is,do,are⁞]#} terrible {#drop3 words: [yesterday, today, tomorrow]#}, don't you think?
"""]
    return jsonify({
        "js": ["/field/js/build/dropdown.js"],
        "multihtml": True,
        "css": ["/field/css/field.css"],
        'editor_tabs': [
            {
                'text': 'Fields',
                'items': [
                    {
                        'text': 'Check/Radio/Drop',
                        'items': [
                            {
                                'data': templates[0].strip(),
                                'text': 'Dropdown plugin',
                                'expl': 'Add a single inline dropdown-plugin'
                            },
                            {
                                'data': templates[1].strip(),
                                'text': 'Block with one dropdown-plugin',
                                'expl': 'Add a block with one inline dropdown-plugin (defaultplugin="dropdown")'
                            },
                            {
                                'data': templates[2].strip(),
                                'text': 'Block with two dropdown-plugins',
                                'expl': 'Add a block with two inline dropdown-plugins (defaultplugin="dropdown")'
                            },
                        ],
                    },
                ],
            },
        ],
    })
