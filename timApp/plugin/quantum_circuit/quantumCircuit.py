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

quantum_circuit_plugin = TypedBlueprint(
    "quantum_circuit_plugin", __name__, url_prefix="/quantumCircuit"
)


@dataclass
class GateType:
    name: str
    target: int
    time: int
    controls: list[int] | None = None

    def to_json(self) -> dict:
        return asdict(self)


@dataclass
class QuantumCircuitMarkup(GenericMarkupModel):
    """Class that defines plugin markup (the YAML settings and their types)"""

    initialCircuit: list[GateType] | None = None
    nQubits: int | None = None
    nMoments: int | None = None


@dataclass
class QuantumCircuitStateModel:
    """Class that defines plugin state (the JSON answer data of a task)"""

    pass


@dataclass
class QuantumCircuitInputModel:
    """Class that defines data that browser sends to the server"""

    pass


@dataclass
class QuantumCircuitHtmlModel(
    GenericHtmlModel[
        QuantumCircuitInputModel, QuantumCircuitMarkup, QuantumCircuitStateModel
    ]
):
    """Common logic of the plugin. See GenericHtmlModel for methods that you can overload."""

    def get_component_html_name(self) -> str:
        return "tim-quantum-circuit"

    def get_static_html(self) -> str:
        return """
            <div>Quantum circuit</div>
        """


def reqs_handler() -> PluginReqs:
    """Return plugins' dependencies and info on how to render it"""

    return {"js": ["quantumCircuit"], "multihtml": True}


# Register generic plugin routes
# Note: there is also create_nontask_blueprint that does this + creates the blueprint,
# but for now it's better to create the blueprint manually
register_html_routes(
    quantum_circuit_plugin,
    class_schema(QuantumCircuitHtmlModel, base_schema=DurationSchema),
    reqs_handler,
)
