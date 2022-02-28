from dataclasses import dataclass, asdict

from timApp.util.flask.typedblueprint import TypedBlueprint
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    PluginReqs,
    register_html_routes,
)
from tim_common.utils import DurationSchema

todo_plugin = TypedBlueprint("todo_plugin", __name__, url_prefix="/todoList")


@dataclass
class TodoItem:
    text: str
    done: bool = False

    def to_json(self) -> dict:
        return asdict(self)


@dataclass
class TodoListMarkup(GenericMarkupModel):
    """Class that defines plugin markup (the YAML settings and their types)"""

    defaultTodos: list[TodoItem] | None = None


@dataclass
class TodoListStateModel:
    """Class that defines plugin state (the JSON answer data of a task)"""

    pass


@dataclass
class TodoListInputModel:
    """Class that defines data that browser sends to the server"""

    pass


@dataclass
class TodoListHtmlModel(
    GenericHtmlModel[TodoListInputModel, TodoListMarkup, TodoListStateModel]
):
    """Common logic of the plugin. See GenericHtmlModel for methods that you can overload."""

    def get_component_html_name(self) -> str:
        return "tim-todo-list"

    def get_static_html(self) -> str:
        return """
            <div>Todo list</div>
        """


def reqs_handler() -> PluginReqs:
    """Return plugins' dependencies and info on how to render it"""
    return {
        "js": [
            "todoList"
        ],  # What modules to load (see staticDynamicImport.ts for list of modules)
        "multihtml": True,
    }


# Register generic plugin routes
# Note: there is also create_nontask_blueprint that does this + creates the blueprint,
# but for now it's better to create the blueprint manually
register_html_routes(
    todo_plugin,
    class_schema(TodoListHtmlModel, base_schema=DurationSchema),
    reqs_handler,
)
