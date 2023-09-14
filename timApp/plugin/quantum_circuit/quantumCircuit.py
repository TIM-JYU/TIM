from dataclasses import dataclass, asdict
from typing import Union
import json
import re
from collections import defaultdict
import math
from threading import Thread

from flask import render_template_string, request, jsonify, Response
import yaml

from qulacs import QuantumCircuit, QuantumState, QuantumGateMatrix
from qulacs.gate import H, X, Y, Z, S, T, to_matrix_gate, DenseMatrix
import numpy as np

from timApp.auth.accesshelper import verify_logged_in
from timApp.tim_app import csrf
from timApp.util.flask.requesthelper import use_model
from tim_common.markupmodels import GenericMarkupModel
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    PluginReqs,
    create_blueprint,
    GenericAnswerModel,
    PluginAnswerResp,
    EditorTab,
)
from tim_common.utils import Missing
from timApp.util.logger import log_warning


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
    info: str
    color: str | None = None
    textColor: str | None = None

    def to_json(self) -> dict:
        return asdict(self)


@dataclass
class NumericCustomGateInfo:
    name: str
    matrix: str

    def to_json(self) -> dict:
        return asdict(self)


@dataclass
class QubitInfo:
    name: str | None = None
    value: int | None = None
    editable: bool | None = None

    def to_json(self) -> dict:
        return asdict(self)


@dataclass
class ConditionsNotSatisfiedError:
    condition: str
    values: str
    errorType: str = "condition-not-satisfied"


@dataclass
class ConditionNotInterpretableError:
    condition: str
    errorType: str = "condition-not-interpretable"


@dataclass
class ConditionInvalidError:
    condition: str
    errorType: str = "condition-invalid"


@dataclass
class AnswerIncorrectError:
    bitstring: str
    expected: list[float]
    actual: list[float]
    errorType: str = "answer-incorrect"


@dataclass
class MatrixIncorrectError:
    matrix: str
    errorType: str = "matrix-incorrect"


@dataclass
class TooManyQubitsError:
    qubits: int
    maxQubits: int
    errorType: str = "too-many-qubits"


@dataclass
class TooManyMomentsError:
    moments: int
    maxMoments: int
    errorType: str = "too-many-moments"


@dataclass
class RegexInvalidError:
    regex: str
    errorType: str = "regex-invalid"


@dataclass
class SimulationTimedOutError:
    errorType: str = "simulation-timed-out"


@dataclass
class TooLongTimeoutError:
    timeout: int
    maxTimeout: int
    errorType: str = "too-long-timeout"


@dataclass
class CircuitUnInterpretableError:
    message: str
    errorType: str = "circuit-uninterpretable"


ErrorType = Union[
    ConditionsNotSatisfiedError,
    ConditionNotInterpretableError,
    ConditionInvalidError,
    AnswerIncorrectError,
    MatrixIncorrectError,
    TooManyQubitsError,
    TooManyMomentsError,
    RegexInvalidError,
    SimulationTimedOutError,
    TooLongTimeoutError,
    CircuitUnInterpretableError,
]


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
    modelInput: list[str] | None = None
    modelConditions: list[str] | None = None
    qubits: list[QubitInfo] | None = None
    outputNames: list[str] | None = None
    maxRunTimeout: int | None = None

    initialCircuit: list[GateInfo] | None = None
    customGates: list[CustomGateInfo] | None = None
    gates: list[str] | None = None
    simulate: str | None = None

    leftAxisLabel: str | None = None
    rightAxisLabel: str | None = None
    timeAxisLabel: str | None = None


@dataclass
class QuantumCircuitStateModel:
    """Class that defines plugin state (the JSON answer data of a task)"""

    userCircuit: list[GateInfo] | None = None
    userInput: list[int] | None = None
    measurements: int | None = None


@dataclass
class QuantumCircuitInputModel:
    """Class that defines data that browser sends to the server"""

    userCircuit: list[GateInfo] | None = None
    userInput: list[int] | None = None
    customGates: list[NumericCustomGateInfo] | None = None
    measurements: int | None = None


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
        matrix_size = math.floor(math.log2(gate_matrix.shape[0]))
        if matrix_size > 1:
            # all concurrent indices of target are qubits it affects
            return DenseMatrix([target + i for i in range(matrix_size)], gate_matrix)
        return DenseMatrix(target, gate_matrix)
    return None


def get_all_gate_names(custom_gates: dict[str, np.ndarray]) -> set[str]:
    """
    Get all available names of gates including some default gate names and
    names from custom gate definitions.
    :param custom_gates:
    :return:
    """
    custom_names = custom_gates.keys()
    def_names = ["H", "X", "Y", "Z", "S", "T"]

    all_names: set[str] = set()
    all_names.update(custom_names)
    all_names.update(def_names)

    return all_names


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
) -> None | ErrorType:
    # qulacs has implicit sense of time so gates need to added in correct order
    for gate_def in sorted(gates, key=lambda x: x.time):
        # check nMoments limit here. nQubits limit doesn't need to be checked because
        # circuit knows it and throws error if gate_def.target violates it
        if gate_def.time >= 50:
            return TooManyMomentsError(gate_def.time, 50)
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
            log_warning(f"quantum: undefined gate type {gate_def}")

    return None


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
    gates: list[NumericCustomGateInfo] | None,
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
) -> tuple[bool, ErrorType | np.ndarray]:
    """
    Runs simulator.
    :param gates: gates that are in the circuit
    :param input_list: input bits as a list of 0s and 1s [q0 value, q1 value,...]
    :param n_qubits: how many qubits the circuit has
    :param custom_gates: custom gates defined in YAML
    :return: tuple of success status, error or array of probabilities of each state after simulation
    """
    state = QuantumState(n_qubits)
    initial_state = input_to_int(input_list)
    state.set_computational_basis(initial_state)

    circuit = QuantumCircuit(n_qubits)

    circuit.update_quantum_state(state)

    add_err = add_gates_to_circuit(gates, circuit, custom_gates)
    if add_err is not None:
        return False, add_err

    circuit.update_quantum_state(state)

    return True, np.power(np.abs(state.get_vector()), 2)


def check_answer(user_result: np.ndarray, model_result: np.ndarray) -> bool:
    """
    Checks whether the arrays have same values.
    :param user_result: the probabilities of states of user's circuit after simulation
    :param model_result: the probabilities of states of the model circuit after simulation
    :return: whether the array are same up to some small error margin
    """
    return np.allclose(user_result, model_result)


def check_input(
    bitstring: str, patterns: list[str] | None
) -> tuple[bool, ErrorType | None]:
    """
    Check if simulator should be run with given bitstring input.
    :param bitstring: input qubit values
    :param patterns: regex like bitstring patterns to match bitstring against
    :return: True if bitstring matches any pattern else False
    """
    if patterns is None:
        return True, None

    for pattern in patterns:
        try:
            if re.fullmatch(pattern, bitstring):
                return True, None
        except re.error:
            return False, RegexInvalidError(pattern)

    return False, None


@dataclass
class ThreadedSimParams:
    needs_to_be_stopped: bool
    result: tuple[bool, ErrorType | None]


def run_all_simulations(
    model_circuit: list[GateInfo],
    user_circuit: list[GateInfo],
    n_qubits: int,
    custom_gates: dict[str, np.ndarray],
    model_input: list[str] | None,
    threaded_sim_params: ThreadedSimParams,
) -> None:
    """
    Runs simulator with all inputs comparing model_circuit to user_circuit.
    :param model_circuit: correct circuit that user_circuit is compared to
    :param user_circuit: circuit that was created by user
    :param n_qubits: how many qubits are in circuit
    :param custom_gates: custom gates available to simulator
    :param model_input: pattern defining which inputs are used out of all permutations
    :param threaded_sim_params: Used to pass info to and from thread
    :return: Nothing. return value is passed through threaded_sim_params
    """
    if n_qubits > 20:
        threaded_sim_params.result = False, TooManyQubitsError(n_qubits, 20)
        return

    for i in range(2**n_qubits):
        if threaded_sim_params.needs_to_be_stopped:
            threaded_sim_params.result = False, None
            return
        bitstring = "{0:b}".format(i).rjust(n_qubits, "0")
        bitstring_reversed = "".join(reversed(bitstring))
        check_input_valid, check_input_error = check_input(
            bitstring_reversed, model_input
        )
        if not check_input_valid and check_input_error is not None:
            threaded_sim_params.result = False, check_input_error
            return
        if not check_input_valid:
            continue
        input_list = [int(d) for d in bitstring]
        try:
            success1, expected = run_simulation(
                model_circuit, input_list, n_qubits, custom_gates
            )
            if not success1 and not isinstance(expected, np.ndarray):
                threaded_sim_params.result = False, expected
                return
            success2, actual = run_simulation(
                user_circuit, input_list, n_qubits, custom_gates
            )
            if not success2 and not isinstance(actual, np.ndarray):
                threaded_sim_params.result = False, actual
                return
        except (TypeError, RuntimeError) as e:
            # remove function name from error message. These come from qulacs.
            error_message = re.sub(r"Error:.+:", "", str(e))
            threaded_sim_params.result = False, CircuitUnInterpretableError(
                error_message
            )
            return

        if (
            isinstance(actual, np.ndarray)
            and isinstance(expected, np.ndarray)
            and not check_answer(actual, expected)
        ):
            threaded_sim_params.result = False, AnswerIncorrectError(
                bitstring_reversed, list(expected), list(actual)
            )
            return

    threaded_sim_params.result = True, None


def run_all_simulations_threaded(
    model_circuit: list[GateInfo],
    user_circuit: list[GateInfo],
    n_qubits: int,
    custom_gates: dict[str, np.ndarray],
    model_input: list[str] | None,
    max_run_timeout: int | None,
) -> tuple[bool, ErrorType | None]:

    sim_params = ThreadedSimParams(False, (True, None))

    # allow at max 25 seconds of simulation time
    if max_run_timeout is not None and max_run_timeout > 25:
        return False, TooLongTimeoutError(max_run_timeout, 25)

    if max_run_timeout:
        max_run_time = max_run_timeout
    else:
        max_run_time = 10

    t = Thread(
        target=run_all_simulations,
        args=(
            model_circuit,
            user_circuit,
            n_qubits,
            custom_gates,
            model_input,
            sim_params,
        ),
    )
    t.start()
    t.join(max_run_time)
    if not t.is_alive():
        return sim_params.result

    # signal thread to stop and wait at max 5 seconds before throwing exception
    sim_params.needs_to_be_stopped = True
    t.join(5)
    if t.is_alive():
        raise Exception("Quantum simulator didn't exit properly")
    return False, SimulationTimedOutError()


def get_gate_counts(
    circuit: list[GateInfo], custom_gates: dict[str, np.ndarray]
) -> defaultdict:
    counts: defaultdict[str, int] = defaultdict(int)
    circuit_names: set[str] = set()
    for gate_def in circuit:
        if isinstance(gate_def, SingleOrMultiQubitGateInfo):
            counts[gate_def.name] += 1
            circuit_names.add(gate_def.name)
        elif isinstance(gate_def, SwapGateInfo):
            counts["swap"] += 1
            circuit_names.add("swap")
        elif isinstance(gate_def, ControlGateInfo):
            counts[gate_def.name] += 1
            circuit_names.add(gate_def.name)
        else:
            log_warning(f"quantum: undefined gate type {gate_def}")

    # also add names that are not in circuit but could be in conditions
    for name in get_all_gate_names(custom_gates):
        if name not in circuit_names:
            counts[name] = 0

    return counts


def check_valid_characters(condition: str) -> bool:
    valid_chars = r"(and|or|\d|[=<>() ])+"
    return re.fullmatch(valid_chars, condition) is not None


def evaluate_condition(
    condition: str, var_counts: defaultdict[str, int]
) -> tuple[bool, ErrorType | None]:
    eval_condition = condition
    # sort by name length descending so that e.g. X in SX doesn't get replaced by the count of X
    for name, count in sorted(
        var_counts.items(), key=lambda x: len(x[0]), reverse=True
    ):
        eval_condition = eval_condition.replace(name, str(count))

    eval_condition = eval_condition.replace("&&", " and ")
    eval_condition = eval_condition.replace("||", " or ")

    if not check_valid_characters(eval_condition):
        return False, ConditionInvalidError(condition)

    try:
        result = eval(eval_condition, {"__builtins__": None})
    except SyntaxError:
        return False, ConditionNotInterpretableError(condition)

    if not result:
        values = ", ".join(map(lambda kv: f"{kv[0]}={kv[1]}", var_counts.items()))
        return False, ConditionsNotSatisfiedError(condition, values)

    return result, None


def check_conditions(
    conditions: list[str] | None,
    circuit: list[GateInfo] | None,
    n_measurements: int | None,
    custom_gates: dict[str, np.ndarray],
) -> tuple[bool, ErrorType | None]:
    if conditions is None or circuit is None:
        return True, None
    counts = get_gate_counts(circuit, custom_gates)
    if n_measurements is not None:
        counts["measurements"] = n_measurements
    else:
        counts["measurements"] = 0

    for condition in conditions:
        is_valid, error = evaluate_condition(condition, counts)
        if not is_valid:
            return is_valid, error

    return True, None


def answer(args: QuantumCircuitAnswerModel) -> PluginAnswerResp:
    model_circuit = args.markup.modelCircuit
    model_input = args.markup.modelInput
    model_conditions = args.markup.modelConditions

    user_circuit = args.input.userCircuit
    user_input = args.input.userInput

    n_qubits = args.markup.nQubits

    n_measurements = args.input.measurements

    max_run_timeout = args.markup.maxRunTimeout

    points = 1.0
    result = "saved"
    error = None

    custom_gates = parse_custom_gates(args.input.customGates)

    valid_conditions = True
    if model_conditions is not None:
        is_valid, message = check_conditions(
            model_conditions, user_circuit, n_measurements, custom_gates
        )
        if not is_valid:
            valid_conditions = False
            error = asdict(message)
            points = 0.0
            result = ""
    if (
        valid_conditions
        and model_circuit is not None
        and user_circuit is not None
        and n_qubits is not None
    ):
        ok, sim_error = run_all_simulations_threaded(
            model_circuit,
            user_circuit,
            n_qubits,
            custom_gates,
            model_input,
            max_run_timeout,
        )

        if ok:
            points = 1.0
            result = "correct"
        else:
            points = 0.0
            result = ""
            error = asdict(sim_error)

    return {
        "save": {"userCircuit": user_circuit, "userInput": user_input},
        "tim_info": {"points": points},
        "web": {
            "result": result,
            "error": json.dumps(error, ensure_ascii=False, indent=4),
        },
    }


def reqs_handler() -> PluginReqs:
    """Return plugins' dependencies and info on how to render it"""

    template_full = """
``` {#tehtava1 plugin="quantumCircuit"}
header: "Tehtävän otsikko"
stem: "Tehtävänanto"
# footer: "alateksti"
# starttime: '2023-07-25 15:00:00'
# deadline: '2023-07-28 23:59:00'
nQubits: 4
nMoments: 8
lazy: false
# gates: ["H", "X", "Y", "Z", "S", "T", "swap", "control", "SX"]
qubitNotation: "bit"
showChart: true
showOutputBits: true
showPrintField: true
samplingMode: matrix
nSamples: 100
# simulate: server
initialCircuit:
  -
    name: H
    target: 0
    time: 0
  - 
    name: X
    target: 1
    time: 1
    controls: [0]
  -
    swap1: 2
    swap2: 3
    time: 2
  -
    name: I2
    target: 1
    time: 3
    editable: false
customGates:
  -
    name: Id
    matrix: "[[1,0],[0,1]]"
    info: identiteettiportti
    color: "#484848"
    textColor: white
  -
    name: I2
    matrix: "[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]"
    info: "identiteetti portti"
    color: "rgb(100, 100, 100)"
  -
    name: SX
    matrix: "(0.5)*[[1+i,1-i],[1-i,1+i]]"
    info: "X:n neliöjuuri"
```

    """

    editor_tabs: list[EditorTab] = [
        {
            "text": "plugins",
            "items": [
                {
                    "text": "QuantumCircuit",
                    "items": [
                        {
                            "data": template_full.strip(),
                            "text": "Quantum Circuit",
                            "expl": "A full example of the quantum circuit plugin",
                        }
                    ],
                }
            ],
        }
    ]

    return {"js": ["quantumCircuit"], "multihtml": True, "editor_tabs": editor_tabs}


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
def quantum_circuit_to_yaml() -> Response:
    """
    Formats circuit as YAML text.
    :return: YAML string
    """
    list_of_gates = request.get_json()
    yaml_str = yaml.dump(list_of_gates, default_flow_style=False, allow_unicode=True)

    return jsonify({"web": yaml_str})


@dataclass
class SimulationArgs:
    gates: list[GateInfo]
    inputList: list[int]
    nQubits: int
    customGates: list[NumericCustomGateInfo] | None = None


@quantum_circuit_plugin.post("/simulate")
@use_model(SimulationArgs)
def quantum_circuit_simulate(args: SimulationArgs) -> Response:
    verify_logged_in()
    custom_gates = parse_custom_gates(args.customGates)
    if args.nQubits > 20:
        err_str = json.dumps(
            asdict(TooManyQubitsError(args.nQubits, 20)), ensure_ascii=False, indent=4
        )
        return jsonify({"web": {"result": "", "error": err_str}})

    success, result = run_simulation(
        args.gates, args.inputList, args.nQubits, custom_gates
    )
    if success and isinstance(result, np.ndarray):
        return jsonify({"web": {"result": list(result), "error": ""}})

    return jsonify(
        {
            "web": {
                "result": [],
                "error": json.dumps(asdict(result), ensure_ascii=False, indent=4),
            }
        }
    )
