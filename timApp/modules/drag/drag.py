"""
Module for serving drag item-plugin.
"""
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
class DragStateModel:
    c: List[str]


class DragStateSchema(Schema):
    c = fields.List(fields.Str(required=True))

    @post_load
    def make_obj(self, data, **_):
        return DragStateModel(**data)


@attr.s(auto_attribs=True)
class DragMarkupModel(GenericMarkupModel):
    points_array: Union[str, Missing] = missing
    inputstem: Union[str, Missing] = missing
    needed_len: Union[int, Missing] = missing
    words: Union[List[str], Missing] = missing
    max: Union[int, Missing] = missing
    copy: Union[str, Missing] = missing
    type: Union[str, Missing] = missing
    trash: Union[bool, Missing] = missing
    savebutton: Union[bool, Missing] = missing
    shuffle: Union[bool, Missing] = missing
    followid: Union[str, Missing] = missing


class DragMarkupSchema(GenericMarkupSchema):
    points_array = fields.List(fields.List(fields.Number()))
    inputstem = fields.Str()
    needed_len = fields.Int()
    cols = fields.Int()
    words = fields.List(fields.Str())
    copy = fields.String()
    type = fields.String()
    max = fields.Int()
    trash = fields.Bool()
    savebutton = fields.Bool()
    shuffle = fields.Bool()
    followid = fields.String()

    @validates('points_array')
    def validate_points_array(self, value):
        if len(value) != 2 or not all(len(v) == 2 for v in value):
            raise ValidationError('Must be of size 2 x 2.')

    @post_load
    def make_obj(self, data, **_):
        return DragMarkupModel(**data)


@attr.s(auto_attribs=True)
class DragInputModel:
    words: List[str]
    copy: str = missing
    type: str = missing
    max: int = missing
    trash: bool = missing
    savebutton: bool = missing
    shuffle: bool = missing
    nosave: bool = missing


class DragInputSchema(Schema):
    nosave = fields.Bool()
    words = fields.List(fields.Str(required=True))
    copy = fields.String()
    type = fields.String()
    max = fields.Int()
    trash = fields.Bool()
    savebutton = fields.Bool()
    shuffle = fields.Bool()

    @post_load
    def make_obj(self, data, **_):
        return DragInputModel(**data)


class DragAttrs(Schema):
    markup = fields.Nested(DragMarkupSchema)
    state = fields.Nested(DragStateSchema, allow_none=True, required=True)


@attr.s(auto_attribs=True)
class DragHtmlModel(GenericHtmlModel[DragInputModel, DragMarkupModel, DragStateModel]):
    def get_static_html(self) -> str:
        return render_static_drag(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        if self.state:
            r['c'] = self.state.c
        return r

    def get_real_html(self):
        return render_template_string(
            """<drag-runner json="{{data}}"></drag-runner>""",
            data=make_base64(self.get_browser_json()),
        )


class DragHtmlSchema(DragAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data, **_):
        # noinspection PyArgumentList
        return DragHtmlModel(**data)


@attr.s(auto_attribs=True)
class DragAnswerModel(GenericAnswerModel[DragInputModel, DragMarkupModel, DragStateModel]):
    pass


class DragAnswerSchema(DragAttrs, GenericAnswerSchema):
    input = fields.Nested(DragInputSchema, required=True)

    @post_load
    def make_obj(self, data, **_):
        # noinspection PyArgumentList
        return DragAnswerModel(**data)


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
        **attr.asdict(m.markup),
    )


app = create_app(__name__, DragHtmlSchema)


@app.route('/answer/', methods=['put'])
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

@app.route('/reqs/')
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
        "css": ["css/drag.css"],
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
