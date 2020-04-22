"""Defines the skeleton for all TIM plugins.

TIM interacts with plugins through certain HTTP routes:

* multihtml: Renders the plugin instances as HTML.
             Called with a list of JSONs of the same plugin type.
             The route returns a list of corresponding HTMLs.

* answer:    Renders the plugin response when an answer is posted to the plugin.

The accepted data of each route is defined by a dataclass. Upon successful validation, the JSON
is converted to the corresponding Model object that can be used in code.

If validation fails, the plugin returns an error with status code 422.

"""
import base64
import json
from enum import Enum
from typing import Optional, Union, TypeVar, Generic, Dict, Type

from dataclasses import dataclass, field
from flask import Blueprint, request
from flask import render_template_string, jsonify, Flask
from marshmallow import ValidationError, Schema
from marshmallow.utils import missing
from werkzeug.exceptions import UnprocessableEntity

from marshmallow_dataclass import class_schema
from markupmodels import list_not_missing_fields, GenericMarkupModel
from utils import Missing


class PluginJsonEncoder(json.JSONEncoder):
    def default(self, o):
        tojson = getattr(o, 'to_json', None)
        if tojson:
            return tojson()


@dataclass
class InfoModel:
    """Model for the information that is given by TIM in an answer request."""
    earlier_answers: int
    look_answer: bool
    max_answers: Optional[int]
    user_id: str
    valid: bool  # could be False e.g. if answering deadline has passed


InfoSchema = class_schema(InfoModel)

PluginMarkup = TypeVar('PluginMarkup', bound=GenericMarkupModel)
PluginState = TypeVar('PluginState')
PluginInput = TypeVar('PluginInput')


@dataclass
class GenericRouteModel(Generic[PluginInput, PluginMarkup, PluginState]):
    """Base class for answer and HTML route models. Contains the fields that the routes have in common."""
    info: Optional[InfoModel]
    markup: PluginMarkup
    state: Optional[PluginState]
    taskID: str


@dataclass
class GenericAnswerModel(GenericRouteModel[PluginInput, PluginMarkup, PluginState]):
    """Generic base class for answer route models."""
    input: PluginInput

    def make_answer_error(self, msg: str):
        return jsonify({'web': {'error': msg}})


class Laziness(Enum):
    No = False
    Yes = True
    Never = 'NEVERLAZY'

    def to_json(self):
        return self.value

    def __bool__(self):
        raise Exception("Please use Laziness.{Yes,No,Never} explicitly.")


@dataclass
class GenericHtmlModel(GenericRouteModel[PluginInput, PluginMarkup, PluginState]):
    """Generic base class for HTML route models."""
    anonymous: bool
    current_user_id: str
    doLazy: Laziness = field(metadata={'by_value': True})
    preview: bool
    review: bool
    targetFormat: str
    taskIDExt: str
    user_id: str
    userPrint: bool
    viewmode: bool
    access: Union[str, Missing] = missing

    def requires_login(self) -> bool:
        """
        Whether the login prompt is shown to anonymous users.
        """
        return True

    def get_browser_json(self) -> Dict:
        r = dict(list_not_missing_fields(self))
        r['markup'] = self.markup.get_visible_data()
        return r

    def get_maybe_empty_static_html(self) -> str:
        """Renders a static version of the plugin.
           When plugin is not shown in view mode, return empty string
        """
        if self.viewmode and not self.show_in_view():
            return render_template_string("")
        return self.get_static_html()

    def get_static_html(self) -> str:
        """Renders a static version of the plugin.

        This is meant to be a static lightweight lookalike version of the plugin.
        """
        raise NotImplementedError('Must be implemented by a derived class.')

    def get_review(self):
        # TODO: This "if" should be in field classes, not here.
        if self.state and self.state.c:
            return f"<pre>{json.dumps(self.state.c)}</pre>"

        return "<pre>review</pre>"

    def get_real_html(self) -> str:
        """Renders the plugin as HTML."""
        component = self.get_component_html_name()
        if self.viewmode and not self.show_in_view():
            return render_template_string("")

        if self.review:
            try:
                return self.get_review()
            except:
                return "<pre>review?</pre>"

        return render_template_string(
            """<{{component}} json="{{data}}"></{{component}}>""",
            data=make_base64(self.get_browser_json(), json_encoder=self.get_json_encoder()),
            component=component,
        )

    def get_md(self) -> str:
        return "Not implemented"

    def get_real_md(self) -> str:
        """Renders the plugin as HTML."""
        component = self.get_component_html_name()
        if self.viewmode and not self.show_in_view():
            return render_template_string("")

        return self.get_md()

    def get_json_encoder(self):
        """Set JSON-encoder."""
        return PluginJsonEncoder

    def get_component_html_name(self) -> str:
        """Gets the name of the Angular component as it should be in HTML."""
        raise NotImplementedError('Must be implemented by a derived class.')

    def show_in_view_default(self) -> bool:
        """
        If component is not show in view mode return False.
        This method is for to be overridden in child classes if they should not be shown on View-mode
        :return: is component shown in view as deafault
        """
        return True

    def show_in_view(self) -> bool:
        """
        If component is not show in view mode return False
        :return: is component shown in view mode
        """
        if self.markup.showInView != missing:
            return self.markup.showInView
        return self.show_in_view_default()


def render_validationerror(e: ValidationError):
    """Renders a validation error as HTML indicating which fields were erroneous."""
    return render_template_string(
        """
<div class="pluginError">
The following fields have invalid values:
<ul>
{%- for k, v in errors.items() -%}
<li>{{k}}: {{v[0] if v[0] is string else v[0][0]}}</li>
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
    Please <tim-login-menu></tim-login-menu> to interact with this component.
</p>
{{ static_html|safe }}""", static_html=m.get_maybe_empty_static_html(),
    )


def make_base64(d: dict, json_encoder=None):
    """Converts the given dict to a base64-encoded JSON string."""
    return base64.b64encode(json.dumps(d, sort_keys=True, cls=json_encoder).encode()).decode()


def is_lazy(q: GenericHtmlModel):
    """Determines if the server should render a lazy version of the plugin."""
    if q.doLazy == Laziness.Never:
        return False
    if q.markup.lazy:
        return True
    if q.markup.lazy is False:
        return False
    if q.doLazy == Laziness.Yes:
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
        static_html=m.get_maybe_empty_static_html(),
        real_html=m.get_real_html(),
    )


def render_plugin_html(m: GenericHtmlModel[PluginInput, PluginMarkup, PluginState]):
    """Renders HTML for a plugin.

    :param m: The plugin HTML schema.
    :return: HTML.
    """
    if m.user_id == "Anonymous" and m.requires_login():
        return render_plugin_with_login_request(m)
    if is_lazy(m):
        return render_plugin_lazy(m)
    return m.get_real_html()


def render_multihtml(args, schema: Schema):
    """Renders HTMLs according to the given Schema.

    :param schema: The marshmallow schema to use for validating the plugin data.
    :param args: Unvalidated HTML arguments.
    :return: List of HTMLs.
    """
    results = []
    for a in args:
        try:
            p = schema.load(a)
        except ValidationError as e:
            results.append(render_validationerror(e))
        except Exception as e:
            results.append(render_validationerror(e))
        else:
            results.append(render_plugin_html(p))
    return jsonify(results)


def render_plugin_md(m: GenericHtmlModel[PluginInput, PluginMarkup, PluginState]):
    """Renders HTML for a plugin.

    :param m: The plugin HTML schema.
    :return: HTML.
    """
    if m.user_id == "Anonymous" and m.requires_login():
        return render_plugin_with_login_request(m)
    return m.get_real_md()


def render_multimd(args, schema):
    """Renders HTMLs according to the given Schema.

    :param args: Partially validated HTML arguments.
    :return: List of HTMLs.
    """
    results = []
    for a in args:
        try:
            p = schema.load(a)
        except ValidationError as e:
            results.append(render_validationerror(e))
        except Exception as e:
            results.append(render_validationerror(e))
        else:
            results.append(render_plugin_md(p))
    return jsonify(results)


def create_app(name: str, html_schema: Type[Schema]):
    """Creates the Flask app for the plugin server.

    :param name: Name of import. Usually __name__ should be passed.
    :param html_schema: Schema for the plugin HTML route.
    :return: The app.
    """
    app = Flask(name, static_folder=".", static_url_path="")
    register_routes(app, html_schema)
    return app


def register_routes(app, html_schema: Type[Schema], csrf=None, pre=""):
    @app.errorhandler(422)
    def handle_invalid_request(error: UnprocessableEntity):
        return jsonify({'web': {'error': render_validationerror(ValidationError(message=error.data['messages']))}})

    @app.route(pre+'/multihtml', methods=['post'])
    def multihtml():
        ret = render_multihtml(request.get_json(), html_schema())
        return ret

    @app.route(pre+'/multimd', methods=['post'])
    def multimd():
        ret = render_multimd(request.get_json(), html_schema())
        return ret

    if csrf:
        csrf.exempt(multihtml)
        csrf.exempt(multimd)

    @app.before_request
    def print_rq():
        pass
        # pprint(request.get_json(silent=True))
        # print(request.get_json(silent=True))

    return app


def create_blueprint(name: str, plugin_name: str, html_schema: Type[Schema], csrf=None):
    bp = Blueprint(f'{plugin_name}_plugin',
                   name,
                   url_prefix=f'/{plugin_name}')
    register_routes(bp, html_schema, csrf)
    return bp
