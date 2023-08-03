from dataclasses import dataclass, asdict
from typing import Union

from flask import render_template_string

from qulacs import Observable, QuantumCircuit, QuantumState
from qulacs.gate import Y, CNOT, merge

from timApp.tim_app import csrf
from tim_common.markupmodels import GenericMarkupModel
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    PluginReqs,
    create_blueprint,
    GenericAnswerModel,
    PluginAnswerResp,
)
from tim_common.utils import Missing


@dataclass
class SingleOrMultiQubitGateInfo:
    name: str
    time: int
    target: int
    editable: bool | Missing | None = True

    def to_json(self) -> dict:
        return asdict(self)


@dataclass
class SwapGateInfo:
    time: int
    swap1: int
    swap2: int
    editable: bool | Missing | None = True

    def to_json(self) -> dict:
        return asdict(self)


@dataclass
class ControlGateInfo:
    name: str
    time: int
    target: int
    controls: list[int]
    editable: bool | Missing | None = True

    def to_json(self) -> dict:
        return asdict(self)


GateInfo = Union[SingleOrMultiQubitGateInfo, SwapGateInfo, ControlGateInfo]


@dataclass
class CustomGateInfo:
    name: str
    matrix: str
    description: str

    def to_json(self) -> dict:
        return asdict(self)


@dataclass
class QuantumCircuitMarkup(GenericMarkupModel):
    """Class that defines plugin markup (the YAML settings and their types)"""

    nQubits: int | None = None
    nMoments: int | None = None
    qubitNotation: str | None = None
    showChart: bool | None = None
    showPrintField: bool | None = None
    showOutputBits: bool | None = None
    samplingMode: str | None = None
    nSamples: int | None = None
    modelCircuit: list[GateInfo] | None = None
    modelInput: list[int] | None = None
    qubitNames: list[str] | None = None
    outputNames: list[str] | None = None

    initialCircuit: list[GateInfo] | None = None
    customGates: list[CustomGateInfo] | None = None
    gates: list[str] | None = None


@dataclass
class QuantumCircuitStateModel:
    """Class that defines plugin state (the JSON answer data of a task)"""

    userCircuit: list[GateInfo] | None = None
    userInput: list[int] | None = None


@dataclass
class QuantumCircuitInputModel:
    """Class that defines data that browser sends to the server"""

    userCircuit: list[GateInfo] | None = None
    userInput: list[int] | None = None


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
        return render_static_quantum_circuit(self)


def render_static_quantum_circuit(m: QuantumCircuitHtmlModel) -> str:
    return render_template_string(
        """
        <div class="panel panel-default">
            <div class="panel-heading" style="background-color: #004494; color: white;">
                {% if header %}
                    <h4>{{header}}</h4>
                {% endif %}
            </div>
            <div class="panel-body">
                {% if stem %}
                    <p>{{ stem }}</p>
                {% endif %}
                <p class="alert alert-info">Laajenna viemällä hiiri tehtävän päälle tai klikkaamalla.</p>
            </div>
        </div>
    """.strip(),
        **asdict(m.markup)
    )


@dataclass
class QuantumCircuitAnswerModel(
    GenericAnswerModel[
        QuantumCircuitInputModel, QuantumCircuitMarkup, QuantumCircuitStateModel
    ]
):
    pass


def answer(args: QuantumCircuitAnswerModel) -> PluginAnswerResp:
    # initial_circuit = args.markup.initialCircuit

    state = QuantumState(3)
    state.set_Haar_random_state()

    circuit = QuantumCircuit(3)
    circuit.add_X_gate(0)
    merged_gate = merge(CNOT(0, 1), Y(1))
    circuit.add_gate(merged_gate)
    circuit.add_RX_gate(1, 0.5)
    circuit.update_quantum_state(state)

    observable = Observable(3)
    observable.add_operator(2.0, "X 2 Y 1 Z 0")
    observable.add_operator(-3.0, "Z 2")
    value = observable.get_expectation_value(state)
    print(value)

    model_circuit = args.markup.modelCircuit
    model_input = args.markup.modelInput

    if model_circuit is None:
        return args.make_answer_error("Missing modelCircuit")
    if model_input is None:
        return args.make_answer_error("Missing modelInput")

    user_circuit = args.input.userCircuit
    user_input = args.input.userInput

    return {
        "save": {"userCircuit": user_circuit, "userInput": user_input},
        "tim_info": {"points": 1.0},
        "web": {"result": "oikein"},
    }


def reqs_handler() -> PluginReqs:
    """Return plugins' dependencies and info on how to render it"""

    return {"js": ["quantumCircuit"], "multihtml": True}


quantum_circuit_plugin = create_blueprint(
    __name__,
    "quantumCircuit",
    QuantumCircuitHtmlModel,
    QuantumCircuitAnswerModel,
    answer,
    reqs_handler,
    csrf,
)
