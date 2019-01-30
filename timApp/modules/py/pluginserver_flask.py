import base64
import json
from typing import Optional, Set, Union, TypeVar, Generic, Dict, List

import attr
from flask import render_template_string, jsonify
from marshmallow import Schema, fields, post_load, missing, pre_load, ValidationError
# noinspection PyProtectedMember
from marshmallow.utils import _Missing as Missing


@attr.s(auto_attribs=True)
class InfoModel:
    current_user_id: str
    earlier_answers: int
    look_answer: bool
    max_answers: Optional[int]
    user_id: str
    valid: bool


class InfoSchema(Schema):
    current_user_id = fields.Str(required=True)
    earlier_answers = fields.Int(required=True)
    look_answer = fields.Bool(required=True)
    max_answers = fields.Int(required=True, allow_none=True)
    user_id = fields.Str(required=True)
    valid = fields.Bool(required=True)

    @post_load
    def make_obj(self, data):
        return InfoModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class GenericMarkupModel:
    hidden_keys: Set[str]
    header: Union[str, Missing] = missing
    footer: Union[str, Missing] = missing
    stem: Union[str, Missing] = missing
    lazy: Union[bool, Missing] = missing

    def get_visible_data(self):
        return {k: v for k, v in attr.asdict(self).items() if k not in self.hidden_keys and v is not missing}


class GenericMarkupSchema(Schema):
    lazy = fields.Bool()
    header = fields.Str()
    footer = fields.Str()
    hidden_keys = fields.List(fields.Str(), required=True)
    stem = fields.Str()

    @pre_load
    def process_minus(self, data):
        if isinstance(data, dict):
            hidden_keys = {k[1:] for k in data.keys() if isinstance(k, str) and k.startswith('-')}
            for k in hidden_keys:
                data[k] = data.pop(f'-{k}')
            data['hidden_keys'] = hidden_keys
        return data

    class Meta:
        strict = True


class GenericHtmlSchema(Schema):
    anonymous = fields.Bool(required=True)
    doLazy = fields.Bool(required=True)  # TODO this can also be string "NEVERLAZY" which now fails validation
    info = fields.Dict(allow_none=True, required=True)
    markup = fields.Dict(required=True)
    preview = fields.Bool(required=True)
    review = fields.Bool(required=True)
    state = fields.Field(allow_none=True, required=True)
    targetFormat = fields.Str(required=True)
    taskID = fields.Str(required=True)
    taskIDExt = fields.Str(required=True)
    user_id = fields.Str(required=True)
    userPrint = fields.Bool(required=True)

    class Meta:
        strict = True


class GenericAnswerSchema(Schema):
    info = fields.Nested(InfoSchema, required=True)
    input = fields.Dict(required=True)
    markup = fields.Dict(required=True)
    state = fields.Field(allow_none=True, required=True)
    taskID = fields.Str(required=True)

    class Meta:
        strict = True


PluginMarkup = TypeVar('PluginMarkup', bound=GenericMarkupModel)
PluginState = TypeVar('PluginState')
PluginInput = TypeVar('PluginInput')


@attr.s(auto_attribs=True)
class GenericRouteModel(Generic[PluginInput, PluginMarkup, PluginState]):
    info: Optional[InfoModel]
    markup: PluginMarkup
    state: Optional[PluginState]
    taskID: str


@attr.s(auto_attribs=True)
class GenericAnswerModel(GenericRouteModel[PluginInput, PluginMarkup, PluginState]):
    input: PluginInput


@attr.s(auto_attribs=True)
class GenericHtmlModel(GenericRouteModel[PluginInput, PluginMarkup, PluginState]):
    anonymous: bool
    doLazy: bool
    preview: bool
    review: bool
    targetFormat: str
    taskIDExt: str
    user_id: str
    userPrint: bool

    def get_browser_json(self) -> Dict:
        r = attr.asdict(self)
        r['markup'] = self.markup.get_visible_data()
        return r

    def get_static_html(self) -> str:
        raise NotImplementedError('Must be implemented by derived class.')

    def get_real_html(self) -> str:
        raise NotImplementedError('Must be implemented by derived class.')

    class Meta:
        strict = True


def render_validationerror(e: ValidationError):
    return render_template_string(
        """
        <div class="pluginError">
        The following fields have invalid values:
        <ul>
        {% for k, v in errors.items() %}
        <li>{{k}}: {{v[0]}}</li>
        {% endfor %}
        </ul>
        </div>
        """,
        errors=e.messages.get('markup', e.messages.get('input', e.messages)))


def render_plugin_with_login_request(m: GenericHtmlModel[PluginInput, PluginMarkup, PluginState]):
    return render_template_string(
        """
<!--nolazy-->
<p class="pluginError">
    Please <login-menu></login-menu> to interact with this component
</p>
{{ static_html }}""", static_html=m.get_static_html(),
    )


def make_base64(d: dict):
    return base64.b64encode(json.dumps(d).encode()).decode()


def is_lazy(q: GenericHtmlModel):
    if q.doLazy == 'NEVERLAZY':
        return False  # TODO currently unreachable because doLazy is validated as bool
    if q.markup.lazy:
        return True
    if q.markup.lazy is False:
        return False
    if q.doLazy:
        return True
    return False


def render_plugin_lazy(m: GenericHtmlModel[PluginInput, PluginMarkup, PluginState]):
    return render_template_string(
        """
<!--lazy {{ real_html|safe }} lazy-->
{{ static_html|safe }}""",
        static_html=m.get_static_html(),
        real_html=m.get_real_html(),
    )


def render_plugin_html(m: GenericHtmlModel[PluginInput, PluginMarkup, PluginState]):
    if m.user_id == "Anonymous":
        return render_plugin_with_login_request(m)
    if is_lazy(m):
        return render_plugin_lazy(m)
    return m.get_real_html()


def render_multihtml(schema: Schema, args: List[GenericHtmlSchema]):
    results = []
    for a in args:
        try:
            p = schema.load(a)
        except ValidationError as e:
            results.append(render_validationerror(e))
        else:
            results.append(render_plugin_html(p.data))
    return jsonify(results)
