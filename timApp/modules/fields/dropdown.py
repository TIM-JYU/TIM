"""
Module for serving dropdown item-plugin.
"""
from typing import Union, List

import attr
from flask import jsonify, render_template_string, Blueprint
from marshmallow import Schema, fields, post_load, validates, ValidationError
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from pluginserver_flask import GenericMarkupModel, GenericMarkupSchema, GenericHtmlSchema, GenericHtmlModel, \
    GenericAnswerSchema, GenericAnswerModel, Missing, \
    make_base64, InfoSchema, render_multihtml

dropdown_route = Blueprint('dropdown', __name__, url_prefix="/dropdown")


@attr.s(auto_attribs=True)
class DropdownStateModel:
    c: str


class DropdownStateSchema(Schema):
    c = fields.Str(required=True)

    @post_load
    def make_obj(self, data, **kwargs):
        return DropdownStateModel(**data)


@attr.s(auto_attribs=True)
class DropdownMarkupModel(GenericMarkupModel):
    words: Union[List[str], Missing] = missing
    instruction: Union[bool, Missing] = missing
    radio: Union[bool, Missing] = missing
    shuffle: Union[bool, Missing] = missing
    autosave: Union[bool, Missing] = missing
    answers: Union[bool, Missing] = missing


class DropdownMarkupSchema(GenericMarkupSchema):
    words = fields.List(fields.Str)
    instruction = fields.Bool()
    radio = fields.Bool()
    autosave = fields.Bool()
    answers = fields.Bool()
    shuffle = fields.Bool()

    @post_load
    def make_obj(self, data, **kwargs):
        return DropdownMarkupModel(**data)


@attr.s(auto_attribs=True)
class DropdownInputModel:
    selectedWord: str
    nosave: bool = missing
    shuffle: bool = missing


class DropdownInputSchema(Schema):
    selectedWord = fields.Str(required=True)
    nosave = fields.Bool()
    shuffle = fields.Bool()

    @validates('selectedWord')
    def validate_selected_word(self, word):
        if not word:
            raise ValidationError('Must not be empty.')

    @post_load
    def make_obj(self, data, **kwargs):
        return DropdownInputModel(**data)


class DropdownAttrs(Schema):
    markup = fields.Nested(DropdownMarkupSchema)
    state = fields.Nested(DropdownStateSchema, allow_none=True, required=True)


@attr.s(auto_attribs=True)
class DropdownHtmlModel(GenericHtmlModel[DropdownInputModel, DropdownMarkupModel, DropdownStateModel]):
    def get_component_html_name(self) -> str:
        return 'dropdown-runner'

    def get_static_html(self) -> str:
        return render_static_dropdown(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        if self.state:
            r['c'] = self.state.c
        return r

    def get_real_html(self):
        return render_template_string(
            """<dropdown-runner json="{{data}}"></dropdown-runner>""",
            data=make_base64(self.get_browser_json()),
        )


class DropdownHtmlSchema(DropdownAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data, **kwargs):
        # noinspection PyArgumentList
        return DropdownHtmlModel(**data)


@attr.s(auto_attribs=True)
class DropdownAnswerModel(GenericAnswerModel[DropdownInputModel, DropdownMarkupModel, DropdownStateModel]):
    pass


class DropdownAnswerSchema(DropdownAttrs, GenericAnswerSchema):
    input = fields.Nested(DropdownInputSchema, required=True)

    @post_load
    def make_obj(self, data, **kwargs):
        # noinspection PyArgumentList
        return DropdownAnswerModel(**data)


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
        **attr.asdict(m.markup),
    )


DROPDOWN_HTML_SCHEMA = DropdownHtmlSchema()


@dropdown_route.route('/multihtml/', methods=['post'])
@use_args(GenericHtmlSchema(many=True), locations=("json",))
def dropdown_multihtml(args):  # args: List[GenericHtmlSchema]):
    ret = render_multihtml(DROPDOWN_HTML_SCHEMA, args)
    return ret


@dropdown_route.route('/answer/', methods=['put'])
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


@dropdown_route.route('/reqs/')
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
