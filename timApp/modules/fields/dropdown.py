"""
Module for serving dropdown item-plugin.
"""
from dataclasses import dataclass, asdict
from typing import Union

from flask import render_template_string
from marshmallow import validates
from marshmallow.utils import missing

from tim_common.markupmodels import GenericMarkupModel
from tim_common.pluginserver_flask import GenericHtmlModel, \
    GenericAnswerModel, create_blueprint, PluginAnswerResp, PluginAnswerWeb, PluginReqs
from tim_common.utils import Missing


@dataclass
class DropdownStateModel:
    c: str


@dataclass
class DropdownMarkupModel(GenericMarkupModel):
    words: Union[list[str], Missing] = missing
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
    def validate_selected_word(self, word: str) -> None:
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


def render_static_dropdown(m: DropdownHtmlModel) -> str:
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


def answer(args: DropdownAnswerModel) -> PluginAnswerResp:
    web: PluginAnswerWeb = {}
    result: PluginAnswerResp = {'web': web}
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

    return result


def reqs() -> PluginReqs:
    templates = ["""{#test:dropdown words: [option 1, option 2, option 3]#}""", """
#- {defaultplugin="dropdown"}

The weather {#drop1 words: [is,do,are⁞]#} nice today.
""", """
#- {defaultplugin="dropdown"}

The weather {#drop2 words: [is,do,are⁞]#} terrible {#drop3 words: [yesterday, today, tomorrow]#}, don't you think?
"""]
    return {
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
    }


dropdown_route = create_blueprint(
    __name__,
    'dropdown',
    DropdownHtmlModel,
    DropdownAnswerModel,
    answer,
    reqs,
)
