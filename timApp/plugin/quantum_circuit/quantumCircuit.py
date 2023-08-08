from dataclasses import dataclass, asdict
from typing import Union
import json

from flask import render_template_string, request, jsonify, Response

from qulacs import QuantumCircuit, QuantumState, QuantumGateMatrix
from qulacs.gate import H, X, Y, Z, S, T, to_matrix_gate, DenseMatrix
import numpy as np

from timApp.document.yamlblock import YamlBlock
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
class AnswerCustomGateInfo:
    name: str
    matrix: str

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
    customGates: list[AnswerCustomGateInfo] | None = None


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
        **asdict(m.markup),
    )


@dataclass
class QuantumCircuitAnswerModel(
    GenericAnswerModel[
        QuantumCircuitInputModel, QuantumCircuitMarkup, QuantumCircuitStateModel
    ]
):
    pass


def get_gate_matrix(
    name: str, target: int, custom_gates: dict[str, np.ndarray]
) -> QuantumGateMatrix | None:
    gate_mapping = {"H": H, "X": X, "Y": Y, "Z": Z, "S": S, "T": T}
    gate_constructor = gate_mapping.get(name, None)
    if gate_constructor is not None:
        return to_matrix_gate(gate_constructor(target))
    gate_matrix = custom_gates.get(name, None)
    if gate_matrix is not None:
        return DenseMatrix(target, gate_matrix)
    return None


def input_to_int(input_list: list[int]) -> int:
    """
    [0,0,1] -> "001" -> "100" -> 0b100 -> 4
    :param input_list: list of 0 and 1
    :return: input with bits interpreted as int
    """
    return int("".join(map(str, input_list))[::-1], 2)


def add_gates_to_circuit(
    gates: list[GateInfo],
    circuit: QuantumCircuit,
    custom_gates: dict[str, np.ndarray],
) -> None:
    # qulacs has implicit sense of time so gates need to added in correct order
    for gate_def in sorted(gates, key=lambda x: x.time):
        if isinstance(gate_def, SingleOrMultiQubitGateInfo):
            gate = get_gate_matrix(gate_def.name, gate_def.target, custom_gates)
            if gate:
                circuit.add_gate(gate)
        elif isinstance(gate_def, SwapGateInfo):
            circuit.add_SWAP_gate(gate_def.swap1, gate_def.swap2)
        elif isinstance(gate_def, ControlGateInfo):
            mat = get_gate_matrix(gate_def.name, gate_def.target, custom_gates)
            if mat:
                for c in gate_def.controls:
                    mat.add_control_qubit(c, 1)
                circuit.add_gate(mat)
        else:
            print(f"undefined type {gate_def}")


def parse_matrix(m_str: str) -> np.ndarray:
    """
    Parse n x n array from string. The values are formatted as complex numbers.
    :param m_str:
    :return:
    """
    arr = json.loads(m_str)
    if isinstance(arr, list):
        m = []
        for row in arr:
            m_row = []
            if isinstance(row, list):
                for cell in row:
                    value = complex(cell)
                    m_row.append(value)
            m.append(m_row)
        return np.array(m, dtype=complex)
    raise ValueError("failed to parse matrix from input: " + m_str)


def parse_custom_gates(
    gates: list[AnswerCustomGateInfo] | None,
) -> dict[str, np.ndarray]:
    custom_gates: dict[str, np.ndarray] = {}
    if not gates:
        return custom_gates
    for gate in gates:
        m = parse_matrix(gate.matrix)
        custom_gates[gate.name] = m
    return custom_gates


def run_simulation(
    gates: list[GateInfo],
    input_list: list[int],
    n_qubits: int,
    custom_gates: dict[str, np.ndarray],
) -> np.ndarray:
    """
    Runs simulator.
    :param gates: gates that are in the circuit
    :param input_list: input bits as a list of 0s and 1s [q0 value, q1 value,...]
    :param n_qubits: how many qubits the circuit has
    :param custom_gates: custom gates defined in YAML
    :return: state vector after simulation
    """
    state = QuantumState(n_qubits)
    initial_state = input_to_int(input_list)
    state.set_computational_basis(initial_state)

    circuit = QuantumCircuit(n_qubits)

    circuit.update_quantum_state(state)

    add_gates_to_circuit(gates, circuit, custom_gates)

    circuit.update_quantum_state(state)

    return state.get_vector()


def check_answer(user_result: np.ndarray, model_result: np.ndarray) -> bool:
    """
    Checks whether the arrays have same values.
    :param user_result: the state of user's circuit after simulation
    :param model_result: the state after simulation of the model circuit
    :return: whether the array are same up to some small error margin
    """
    return np.allclose(user_result, model_result)


def answer(args: QuantumCircuitAnswerModel) -> PluginAnswerResp:
    # initial_circuit = args.markup.initialCircuit

    model_circuit = args.markup.modelCircuit
    model_input = args.markup.modelInput

    user_circuit = args.input.userCircuit
    user_input = args.input.userInput

    n_qubits = args.markup.nQubits

    points = 1.0
    result = "tallennettu"
    error = ""
    if (
        model_circuit is not None
        and model_input is not None
        and user_circuit is not None
        and user_input is not None
        and n_qubits is not None
    ):
        custom_gates = parse_custom_gates(args.input.customGates)
        user_result = run_simulation(user_circuit, user_input, n_qubits, custom_gates)
        model_result = run_simulation(
            model_circuit, model_input, n_qubits, custom_gates
        )

        correct = check_answer(user_result, model_result)
        if correct:
            points = 1.0
            result = "Oikein"
        else:
            points = 0.0
            result = ""
            error = "Väärä vastaus"

    return {
        "save": {"userCircuit": user_circuit, "userInput": user_input},
        "tim_info": {"points": points},
        "web": {"result": result, "error": error},
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


@quantum_circuit_plugin.post("/circuitToYaml")
def quantum_circuit_circuit_to_yaml() -> Response:
    """
    Formats circuit as YAML text.
    :return: YAML string
    """
    data = request.get_json()

    md = YamlBlock(data).to_markdown()
    return jsonify({"web": md})
