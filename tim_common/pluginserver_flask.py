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
from abc import ABC
from dataclasses import dataclass, field
from enum import Enum
from json import JSONEncoder
from typing import (
    TypeVar,
    Generic,
    Any,
    Callable,
    TypedDict,
)

from flask import Blueprint, request, Response, current_app
from flask import render_template_string, jsonify, Flask
from flask_wtf import CSRFProtect
from marshmallow import ValidationError, Schema
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from tim_common.markupmodels import list_not_missing_fields, GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.timjsonencoder import TimJsonEncoder
from tim_common.utils import Missing


class PluginAnswerWeb(TypedDict, total=False):
    result: str
    error: str


class TimInfo(TypedDict, total=False):
    points: float
    # TODO add others when needed


class PluginAnswerResp(TypedDict, total=False):
    web: PluginAnswerWeb
    save: dict[str, Any]
    tim_info: TimInfo


class EditorMenuItem(TypedDict):
    data: str
    expl: str
    text: str


class EditorMenu(TypedDict):
    text: str
    items: list[EditorMenuItem]


class EditorTab(TypedDict):
    text: str
    items: list[EditorMenu]


class PluginReqs(TypedDict, total=False):
    js: list[str]
    multihtml: bool
    multimd: bool
    editor_tabs: list[EditorTab]
    css: list[str]


@dataclass
class InfoModel:
    """Model for the information that is given by TIM in an answer request."""

    earlier_answers: int
    look_answer: bool
    max_answers: int | None
    user_id: str
    valid: bool  # could be False e.g. if answering deadline has passed

    @property
    def primary_user(self) -> str:
        users = self.user_id.split(";")
        return users[0]


InfoSchema = class_schema(InfoModel)

PluginMarkup = TypeVar("PluginMarkup", bound=GenericMarkupModel)
PluginState = TypeVar("PluginState")
PluginInput = TypeVar("PluginInput")


@dataclass
class GenericRouteModel(Generic[PluginInput, PluginMarkup, PluginState]):
    """Base class for answer and HTML route models. Contains the fields that the routes have in common."""

    info: InfoModel | None
    markup: PluginMarkup
    state: PluginState | None
    taskID: str


@dataclass
class GenericAnswerModel(GenericRouteModel[PluginInput, PluginMarkup, PluginState]):
    """Generic base class for answer route models."""

    input: PluginInput
    info: InfoModel

    def make_answer_error(self, msg: str) -> PluginAnswerResp:
        return {"web": {"error": msg}}


class Laziness(Enum):
    No = False
    Yes = True
    Never = "NEVERLAZY"

    def to_json(self) -> str | bool:
        return self.value

    def __bool__(self) -> bool:
        raise Exception("Please use Laziness.{Yes,No,Never} explicitly.")


@dataclass
class GenericHtmlModel(GenericRouteModel[PluginInput, PluginMarkup, PluginState]):
    """Generic base class for HTML route models."""

    anonymous: bool
    current_user_id: str
    doLazy: Laziness = field(metadata={"by_value": True})
    preview: bool
    review: bool
    targetFormat: str
    taskIDExt: str
    user_id: str
    userPrint: bool
    viewmode: bool
    access: str | Missing = missing
    hide_names: bool | Missing = missing
    temporary_save: bool | Missing = missing
    previous_save: PluginState | Missing = missing

    def requires_login(self) -> bool:
        """
        Whether the login prompt is shown to anonymous users.
        """
        return not self.markup.anonymous

    def get_browser_json(self) -> dict:
        r = dict(list_not_missing_fields(self))
        r["markup"] = self.markup.get_visible_data()
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
        raise NotImplementedError("Must be implemented by a derived class.")

    def get_review(self) -> str:
        # TODO: This should be in field classes, not here.
        c = getattr(self.state, "c", None)
        if c:
            return f"<pre>{json.dumps(c)}</pre>"

        return "<pre>review</pre>"

    def get_real_html(self) -> str:
        """Renders the plugin as HTML."""
        component = self.get_component_html_name()
        if self.viewmode and not self.show_in_view():
            return ""

        if self.review:
            return self.get_review()

        return current_app.jinja_env.from_string(
            """<{{component}} json="{{data}}"></{{component}}>""",
        ).render(
            data=make_base64(
                self.get_browser_json(), json_encoder=self.get_json_encoder()
            ),
            component=component,
        )

    def get_md(self) -> str:
        return "Not implemented"

    def get_real_md(self) -> str:
        if self.viewmode and not self.show_in_view():
            return ""

        return self.get_md()

    def get_json_encoder(self) -> type[json.JSONEncoder]:
        return TimJsonEncoder

    def get_component_html_name(self) -> str:
        """Gets the name of the Angular component as it should be in HTML."""
        raise NotImplementedError("Must be implemented by a derived class.")

    def show_in_view_default(self) -> bool:
        """
        If component is not show in view mode return False.
        This method is for to be overridden in child classes if they should not be shown on View-mode
        :return: is component shown in view as default
        """
        return True

    def show_in_view(self) -> bool:
        """
        If component is not show in view mode return False
        :return: is component shown in view mode
        """
        if isinstance(self.markup.showInView, bool):
            return self.markup.showInView
        return self.show_in_view_default()


PluginContext = TypeVar("PluginContext")


@dataclass
class GenericHtmlModelWithContext(
    Generic[PluginInput, PluginMarkup, PluginState, PluginContext],
    GenericHtmlModel[PluginInput, PluginMarkup, PluginState],
    ABC,
):
    ctx: PluginContext | None = field(
        init=False, default=None, metadata={"missing": True}
    )


def render_validationerror(e: ValidationError) -> str:
    """Renders a validation error as HTML indicating which fields were erroneous."""
    if isinstance(e.messages, dict):
        msgs = e.messages
    else:
        msgs = {}  # TODO check if this is ever reached in practice
    return render_template_string(
        """
<div class="pluginError">
The following fields have invalid values:
<ul>
{%- for k, val in errors.items() recursive -%}
<li>{{k}}
    {%- if val is mapping -%}
    <ul>
        {{ loop(val.items()) }}
    </ul>
    {%- elif val is iterable -%}
        : {% for item in val -%}{{item}}{% if loop.nextitem %}, {% endif %}{% endfor -%}
    {%- else -%}
        : {{ val }}
    {%- endif -%}
</li>
{%- endfor -%}
</ul>
</div>
        """.strip(),
        errors=msgs.get("markup", msgs.get("input", e.messages)),
    )


def render_plugin_with_login_request(
    m: GenericHtmlModel[PluginInput, PluginMarkup, PluginState]
) -> str:
    """Renders a static version of the plugin as HTML along with a request to log in."""
    return render_template_string(
        """
<!--nolazy-->
<p class="pluginError">
    Please <tim-login-menu></tim-login-menu> to interact with this component.
</p>
{{ static_html|safe }}""",
        static_html=m.get_maybe_empty_static_html(),
    )


def make_base64(d: dict, json_encoder: type[JSONEncoder] | None = None) -> str:
    """Converts the given dict to a base64-encoded JSON string."""
    return base64.b64encode(
        json.dumps(d, sort_keys=True, cls=json_encoder).encode()
    ).decode()


def is_lazy(q: GenericHtmlModel) -> bool:
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


def render_plugin_lazy(
    m: GenericHtmlModel[PluginInput, PluginMarkup, PluginState]
) -> str:
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


def render_plugin_html(
    m: GenericHtmlModel[PluginInput, PluginMarkup, PluginState]
) -> str:
    """Renders HTML for a plugin.

    :param m: The plugin HTML schema.
    :return: HTML.
    """
    if m.user_id == "Anonymous" and m.requires_login():
        return render_plugin_with_login_request(m)
    if is_lazy(m):
        return render_plugin_lazy(m)
    return m.get_real_html()


def render_plugin_md(
    m: GenericHtmlModel[PluginInput, PluginMarkup, PluginState]
) -> str:
    """Renders HTML for a plugin.

    :param m: The plugin HTML schema.
    :return: HTML.
    """
    if m.user_id == "Anonymous" and m.requires_login():
        return render_plugin_with_login_request(m)
    return m.get_real_md()


def render_multimd(args: list[dict], schema: Schema) -> Response:
    """Renders HTMLs according to the given Schema.

    :param args: Partially validated HTML arguments.
    :param schema: The marshmallow schema to use for validating the plugin data.
    :return: List of HTMLs.
    """
    results = []
    for a in args:
        try:
            p = schema.load(a)
        except ValidationError as e:
            results.append(render_validationerror(e))
        else:
            results.append(render_plugin_md(p))
    return jsonify(results)


def create_app(name: str) -> Flask:
    """Creates the Flask app for the plugin server.

    :param name: Name of import. Usually __name__ should be passed.
    :return: The app.
    """
    return Flask(name, static_folder=".", static_url_path="")


HtmlModel = TypeVar("HtmlModel", bound=GenericHtmlModel)
AnswerModel = TypeVar("AnswerModel", bound=GenericAnswerModel)


def register_html_routes(
    app: Flask | Blueprint,
    html_schema: type[Schema],
    reqs_handler: Callable[[], PluginReqs],
    csrf: CSRFProtect | None = None,
    multihtml_preprocessor: Callable[[list[HtmlModel]], None] | None = None,
) -> None:
    @app.errorhandler(422)
    def handle_invalid_request(error: Any) -> Response:
        return jsonify(
            {
                "web": {
                    "error": render_validationerror(
                        ValidationError(message=error.data["messages"])
                    )
                }
            }
        )

    @app.post("/multihtml")
    def multihtml() -> Response:
        args = request.get_json(silent=True)
        if not isinstance(args, list):
            args = [args]
        ret = render_multihtml(args, html_schema())
        return ret

    @app.post("/multimd")
    def multimd() -> Response:
        args = request.get_json(silent=True)
        if not isinstance(args, list):
            args = [args]
        ret = render_multimd(args, html_schema())
        return ret

    @app.get("/reqs")
    def reqs() -> Response:
        return jsonify(reqs_handler())

    if csrf:
        csrf.exempt(multihtml)
        csrf.exempt(multimd)

    @app.before_request
    def print_rq() -> None:
        pass
        # pprint(request.get_json(silent=True))
        # print(request.get_json(silent=True))

    def render_multihtml(args: list[dict], schema: Schema) -> Response:
        """Renders HTMLs according to the given Schema.

        :param schema: The marshmallow schema to use for validating the plugin data.
        :param args: Unvalidated HTML arguments.
        :return: List of HTMLs.
        """
        results: list[str | None] = []
        result_data = []
        for a in args:
            try:
                p = schema.load(a)
            except ValidationError as e:
                results.append(render_validationerror(e))
            else:
                result_data.append(p)
                # Leave empty space for the preprocessed data
                results.append(None)

        if multihtml_preprocessor:
            multihtml_preprocessor(result_data)

        # Fill in Nones with preprocessed multihtml
        j = 0
        for i, d in enumerate(results):
            if d is None:
                results[i] = render_plugin_html(result_data[j])
                j += 1

        return jsonify(results)


T = TypeVar("T")


def value_or_default(val: T | None | Missing, default: T) -> T:
    if val is None or isinstance(val, Missing):
        return default
    return val


def create_blueprint(
    name: str,
    plugin_name: str,
    html_model: type[HtmlModel],
    answer_model: type[AnswerModel],
    answer_handler: Callable[[AnswerModel], PluginAnswerResp],
    reqs_handler: Callable[[], PluginReqs],
    csrf: CSRFProtect | None = None,
    multihtml_preprocessor: Callable[[list[HtmlModel]], None] | None = None,
) -> Blueprint:
    bp = create_nontask_blueprint(
        name, plugin_name, html_model, reqs_handler, csrf, multihtml_preprocessor
    )
    register_answer_route(bp, answer_model, answer_handler, csrf)
    return bp


def create_nontask_blueprint(
    name: str,
    plugin_name: str,
    html_model: type[HtmlModel],
    reqs_handler: Callable[[], PluginReqs],
    csrf: CSRFProtect | None = None,
    multihtml_preprocessor: Callable[[list[HtmlModel]], None] | None = None,
) -> Blueprint:
    bp = Blueprint(plugin_name, name, url_prefix=f"/{plugin_name}")
    register_html_routes(
        bp, class_schema(html_model), reqs_handler, csrf, multihtml_preprocessor
    )

    return bp


def register_answer_route(
    app: Flask | Blueprint,
    answer_model: type[AnswerModel],
    answer_handler: Callable[[AnswerModel], PluginAnswerResp],
    csrf: CSRFProtect | None = None,
) -> None:
    @app.put("/answer")
    @use_args(class_schema(answer_model)(), locations=("json",))
    def ans(m: AnswerModel) -> Response:
        return jsonify(answer_handler(m))

    if csrf:
        csrf.exempt(ans)

    return ans


def register_plugin_app(
    name: str,
    html_model: type[HtmlModel],
    answer_model: type[AnswerModel],
    answer_handler: Callable[[AnswerModel], PluginAnswerResp],
    reqs_handler: Callable[[], PluginReqs],
) -> Flask:
    app = create_app(name)
    register_html_routes(app, class_schema(html_model), reqs_handler)
    register_answer_route(app, answer_model, answer_handler)
    return app


def launch_if_main(name: str, app: Flask) -> None:
    if name == "__main__":
        app.run(
            host="0.0.0.0",
            port=5000,
            debug=False,  # for live reloading, this can be turned on
        )
