"""Defines the skeleton for all TIM plugins.

TIM interacts with plugins through certain HTTP routes:

* multihtml: Renders the plugin instances as HTML.
             Called with a list of JSONs of the same plugin type.
             The route returns a list of corresponding HTMLs.

* answer:    Renders the plugin response when an answer is posted to the plugin.

The accepted data of each route is defined by Marshmallow Schemas. Each Schema
should have a corresponding Model to make code more type-safe. Upon successful validation, the JSON
is converted to the corresponding Model object that can be used in code.

If validation fails, the plugin returns an error with status code 422.

"""
import base64
import json
from typing import Optional, Set, Union, TypeVar, Generic, Dict, List

import attr
from flask import Blueprint
from flask import render_template_string, jsonify, Flask
from marshmallow import Schema, fields, post_load, missing, pre_load, ValidationError, validates
# noinspection PyProtectedMember
from marshmallow.utils import _Missing as Missing
from webargs.flaskparser import use_args
from werkzeug.exceptions import UnprocessableEntity


@attr.s(auto_attribs=True)
class InfoModel:
    """Model for the information that is given by TIM in an answer request."""
    current_user_id: str
    earlier_answers: int
    look_answer: bool
    max_answers: Optional[int]
    user_id: str
    valid: bool  # could be False e.g. if answering deadline has passed


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


def list_not_missing_fields(inst):
    return ((k, v) for k, v in attr.asdict(inst).items() if v is not missing)


@attr.s(auto_attribs=True)
class GenericMarkupModel:
    """Specifies which fields the editor can use in the plugin markup.
    This base class defines some fields that are applicable to all plugins.
    """

    hidden_keys: Set[str]
    """Meta field that keeps track which markup fields were hidden (that is, prefixed with "-").
    Hidden keys are never sent to browser.
    """

    header: Union[str, Missing] = missing
    footer: Union[str, Missing] = missing
    stem: Union[str, Missing] = missing
    lazy: Union[bool, Missing] = missing
    buttonText: Union[str, None, Missing] = missing

    def get_visible_data(self):
        return {k: v for k, v in list_not_missing_fields(self) if k not in self.hidden_keys}


class GenericMarkupSchema(Schema):
    lazy = fields.Bool()
    header = fields.Str()
    footer = fields.Str()
    hidden_keys = fields.List(fields.Str(), required=True)
    stem = fields.Str()
    buttonText = fields.Str(allow_none=True)

    @pre_load
    def process_minus(self, data):
        if isinstance(data, dict):
            hidden_keys = {k[1:] for k in data.keys() if isinstance(k, str) and k.startswith('-')}
            for k in hidden_keys:
                data[k] = data.pop(f'-{k}')
            data['hidden_keys'] = hidden_keys
        return data


class GenericHtmlSchema(Schema):
    access = fields.Str()
    anonymous = fields.Bool(required=True)
    doLazy = fields.Raw(required=True)  # boolean or "NEVERLAZY"
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

    @validates('doLazy')
    def validate_do_lazy(self, value):
        if value != 'NEVERLAZY' and not isinstance(value, bool):
            raise ValidationError('do_lazy must be bool or "NEVERLAZY"')


class GenericAnswerSchema(Schema):
    info = fields.Nested(InfoSchema, required=True)
    input = fields.Dict(required=True)
    markup = fields.Dict(required=True)
    state = fields.Field(allow_none=True, required=True)
    taskID = fields.Str(required=True)


PluginMarkup = TypeVar('PluginMarkup', bound=GenericMarkupModel)
PluginState = TypeVar('PluginState')
PluginInput = TypeVar('PluginInput')


@attr.s(auto_attribs=True)
class GenericRouteModel(Generic[PluginInput, PluginMarkup, PluginState]):
    """Base class for answer and HTML route models. Contains the fields that the routes have in common."""
    info: Optional[InfoModel]
    markup: PluginMarkup
    state: Optional[PluginState]
    taskID: str


@attr.s(auto_attribs=True)
class GenericAnswerModel(GenericRouteModel[PluginInput, PluginMarkup, PluginState]):
    """Generic base class for answer route models."""
    input: PluginInput


@attr.s(auto_attribs=True)
class GenericHtmlModel(GenericRouteModel[PluginInput, PluginMarkup, PluginState]):
    """Generic base class for HTML route models."""
    anonymous: bool
    doLazy: bool
    preview: bool
    review: bool
    targetFormat: str
    taskIDExt: str
    user_id: str
    userPrint: bool
    access: Union[str, Missing] = missing

    def get_browser_json(self) -> Dict:
        r = dict(list_not_missing_fields(self))
        r['markup'] = self.markup.get_visible_data()
        return r

    def get_static_html(self) -> str:
        """Renders a static version of the plugin.

        This is meant to be a static lightweight lookalike version of the plugin.
        """
        raise NotImplementedError('Must be implemented by a derived class.')

    def get_real_html(self) -> str:
        """Renders the plugin as HTML."""
        component = self.get_component_html_name()
        return render_template_string(
            """<{{component}} json="{{data}}"></{{component}}>""",
            data=make_base64(self.get_browser_json()),
            component=component,
        )

    def get_component_html_name(self) -> str:
        """Gets the name of the Angular component as it should be in HTML."""
        raise NotImplementedError('Must be implemented by a derived class.')


def render_validationerror(e: ValidationError):
    """Renders a validation error as HTML indicating which fields were erroneous."""
    return render_template_string(
        """
<div class="pluginError">
The following fields have invalid values:
<ul>
{%- for k, v in errors.items() -%}
<li>{{k}}: {{v[0]}}</li>
{%- endfor -%}
</ul>
</div>
        """.strip(),
        errors=e.messages.get('markup', e.messages.get('input', e.messages)))


def render_plugin_with_login_request(m: GenericHtmlModel[PluginInput, PluginMarkup, PluginState]):
    """Renders a static version of the plugin as HTML along with a request to log in."""
    return render_template_string(
        """
<!--nolazy-->
<p class="pluginError">
    Please <login-menu></login-menu> to interact with this component.
</p>
{{ static_html|safe }}""", static_html=m.get_static_html(),
    )


def make_base64(d: dict):
    """Converts the given dict to a base64-encoded JSON string."""
    return base64.b64encode(json.dumps(d, sort_keys=True).encode()).decode()


def is_lazy(q: GenericHtmlModel):
    """Determines if the server should render a lazy version of the plugin."""
    if q.doLazy == 'NEVERLAZY':
        return False
    if q.markup.lazy:
        return True
    if q.markup.lazy is False:
        return False
    if q.doLazy:
        return True
    return False


def render_plugin_lazy(m: GenericHtmlModel[PluginInput, PluginMarkup, PluginState]):
    """Renders lazy HTML for a plugin.

    The lazy HTML displays the static version of the plugin and has the real HTML in an attribute
    that will be activated on mouse hover.

    The end "<!--lazy" is needed because it is the old style of storing the real HTML and TIM does not recognize the
    new style yet, so otherwise TIM would wrap this inside <!--lazy ... lazy-->.

    :param m: The plugin HTML schema.
    :return: HTML.
    """
    return render_template_string(
        """
<div class="lazy" data-html="{{ real_html }}"></div>
{{ static_html|safe }} <!--lazy -->""".strip(),
        static_html=m.get_static_html(),
        real_html=m.get_real_html(),
    )


def render_plugin_html(m: GenericHtmlModel[PluginInput, PluginMarkup, PluginState]):
    """Renders HTML for a plugin.

    :param m: The plugin HTML schema.
    :return: HTML.
    """
    if m.user_id == "Anonymous":
        return render_plugin_with_login_request(m)
    if is_lazy(m):
        return render_plugin_lazy(m)
    return m.get_real_html()


def render_multihtml(schema: GenericHtmlSchema, args: List[GenericHtmlSchema]):
    """Renders HTMLs according to the given Schema.

    :param schema: The plugin HTML schema.
    :param args: Partially validated HTML arguments.
    :return: List of HTMLs.
    """
    results = []
    for a in args:
        try:
            p = schema.load(a)
        except ValidationError as e:
            results.append(render_validationerror(e))
        else:
            results.append(render_plugin_html(p))
    return jsonify(results)


def create_app(name: str, html_schema: GenericHtmlSchema):
    """Creates the Flask app for the plugin server.

    :param name: Name of import. Usually __name__ should be passed.
    :param html_schema: Schema for the plugin HTML route.
    :return: The app.
    """
    app = Flask(name, static_folder=".", static_url_path="")
    register_routes(app, html_schema)
    return app


def register_routes(app, html_schema: GenericHtmlSchema, csrf=None):
    @app.errorhandler(422)
    def handle_invalid_request(error: UnprocessableEntity):
        return jsonify({'web': {'error': render_validationerror(ValidationError(message=error.data['messages']))}})

    @app.route('/multihtml/', methods=['post'])
    @use_args(GenericHtmlSchema(many=True), locations=("json",))
    def multihtml(args: List[GenericHtmlSchema]):
        return render_multihtml(html_schema, args)

    if csrf:
        csrf.exempt(multihtml)

    @app.before_request
    def print_rq():
        pass
        # pprint(request.get_json(silent=True))

    return app


def create_blueprint(name: str, plugin_name: str, html_schema: GenericHtmlSchema, csrf=None):
    bp = Blueprint(f'{plugin_name}_plugin',
                   name,
                   url_prefix=f'/{plugin_name}')
    register_routes(bp, html_schema, csrf)
    return bp
