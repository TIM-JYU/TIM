"""
Module for serving dropdown item-plugin.
"""
import re
from typing import Union, List

import attr
from flask import jsonify, render_template_string
from marshmallow import Schema, fields, post_load, validates, ValidationError
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from pluginserver_flask import GenericMarkupModel, GenericMarkupSchema, GenericHtmlSchema, GenericHtmlModel, \
    GenericAnswerSchema, GenericAnswerModel, Missing, \
    make_base64, InfoSchema, create_app


@attr.s(auto_attribs=True)
class DropdownStateModel:
    selectedWord: str

class DropdownStateSchema(Schema):
    selectedWord = fields.Str(required=True)

    @post_load
    def make_obj(self, data):
        return DropdownStateModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class DropdownMarkupModel(GenericMarkupModel):
    words: Union[List[str], Missing] = missing
    instruction: Union[bool, Missing] = missing
    radio: Union[bool, Missing] = missing


class DropdownMarkupSchema(GenericMarkupSchema):
    words = fields.List(fields.Str)
    instruction = fields.Bool()
    radio = fields.Bool()

    @post_load
    def make_obj(self, data):
        return DropdownMarkupModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class DropdownInputModel:
    selectedWord: str
    nosave: bool = missing


class DropdownInputSchema(Schema):
    selectedWord = fields.Str(required=True)
    nosave = fields.Bool()

    @validates('selectedWord')
    def validate_selectedWord(self, word):
        if not word:
            raise ValidationError('Must not be empty.')

    @post_load
    def make_obj(self, data):
        return DropdownInputModel(**data)


class DropdownAttrs(Schema):
    markup = fields.Nested(DropdownMarkupSchema)
    state = fields.Nested(DropdownStateSchema, allow_none=True, required=True)


@attr.s(auto_attribs=True)
class DropdownHtmlModel(GenericHtmlModel[DropdownInputModel, DropdownMarkupModel, DropdownStateModel]):
    def get_static_html(self) -> str:
        return render_static_dropdown(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        if self.state:
            r['selectedWord'] = self.state.selectedWord
        return r

    def get_real_html(self):
        return render_template_string(
            """<dropdown-runner json="{{data}}"></dropdown-runner>""",
            data=make_base64(self.get_browser_json()),
        )

    class Meta:
        strict = True


class DropdownHtmlSchema(DropdownAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return DropdownHtmlModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class DropdownAnswerModel(GenericAnswerModel[DropdownInputModel, DropdownMarkupModel, DropdownStateModel]):
    pass


class DropdownAnswerSchema(DropdownAttrs, GenericAnswerSchema):
    input = fields.Nested(DropdownInputSchema, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return DropdownAnswerModel(**data)

    class Meta:
        strict = True


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


app = create_app(__name__, DropdownHtmlSchema())


@app.route('/answer/', methods=['put'])
@use_args(DropdownAnswerSchema(strict=True), locations=("json",))
def answer(args: DropdownAnswerModel):
    web = {}
    result = {'web': web}
    selectedword = args.input.selectedWord

    # plugin can ask not to save the word
    nosave = args.input.nosave
    if not nosave:
        save = {"selectedWord": selectedword}
        result["save"] = save
        web['result'] = "saved"
    else:
        save = {"selectedWord": ""}
        result["save"] = save
        web['result'] = "saved"

    return jsonify(result)

@app.route('/reqs/')
@app.route('/reqs')
def reqs():
    templates = ["""
#- {defaultplugin="dropdown"}
The weather {#drop1} nice today.
""","""
#- {defaultplugin="dropdown"}
The weather {#drop2} terrible {#drop3}, don't you think?
"""]
    return jsonify({
        "js": ["js/build/dropdown.js"],
        "multihtml": True,
        "css": ["css/dropdown.css"],
        'editor_tabs': [
            {
                'text': 'Plugins',
                'items': [
                    {
                        'text': 'Dropdown',
                        'items': [
                            {
                                'data': templates[0].strip(),
                                'text': 'One dropdown',
                                'expl': 'Add an inline dropdown-question'
                            },
                            {
                                'data': templates[1].strip(),
                                'text': 'Two dropdowns',
                                'expl': 'Add an inline dropdown-question with two questions'
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
        debug=True,
    )
