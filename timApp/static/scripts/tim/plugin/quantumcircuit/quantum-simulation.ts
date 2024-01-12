import type {
    ICustomGateInfo,
    INumericCustomGateInfo,
    IServerError,
    Measurement,
    SimulationArgs,
} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import {ServerError} from "tim/plugin/quantumcircuit/quantum-circuit.component";

import type {Matrix, FormatOptions} from "mathjs";
import {range} from "mathjs";
import {format} from "mathjs";
import {dotPow, abs, transpose, multiply, kron, identity, index} from "mathjs";
import type {QuantumChartData} from "tim/plugin/quantumcircuit/quantum-stats.component";
import type {Cell, QuantumBoard} from "tim/plugin/quantumcircuit/quantum-board";
import {
    Control,
    Gate,
    MultiQubitGate,
    Swap,
} from "tim/plugin/quantumcircuit/quantum-board";
import type {GateService} from "tim/plugin/quantumcircuit/gate.service";
import type {Qubit} from "tim/plugin/quantumcircuit/qubit";
import type {SerializerService} from "tim/plugin/quantumcircuit/serializer.service";
import type {Result} from "tim/util/utils";
import {timeout, toPromise} from "tim/util/utils";
import type {HttpClient} from "@angular/common/http";
import {isRight} from "fp-ts/Either";

export abstract class QuantumCircuitSimulator {
    board: QuantumBoard;

    // array of probabilities of each output
    result?: number[];
    // array of imaginary numbers
    stateVector?: string[];

    protected constructor(
        protected gateService: GateService,
        board: QuantumBoard
    ) {
        this.board = board;
    }

    /**
     * Runs simulator. The implementations should put output probability vector in this.result
     * and final state vector in this.stateVector as array of imaginary numbers.
     * method return information about success of simulation in Promise.
     *
     * the bit order in arrays is e.g. with two qubits as follows:
     * 00 (q[0]=0, q[1]=0)
     * 01 (q[0]=0, q[1]=1)
     * 10 (q[0]=1, q[1]=0)
     * 11 (q[0]=1, q[1]=1)
     *
     * @param qubits initial state to use for simulation. [q[0], q[1],...]
     */
    abstract run(
        qubits: Qubit[]
    ): Promise<Result<string, string | IServerError>>;

    setBoard(board: QuantumBoard) {
        this.board = board;
    }

    /**
     * Returns a random integer in range [0, weights.length-1]
     * Relative probability of each number is based on weights.
     * @param weights probabilities associated to each choice
     */
    private randomChoice(weights: number[]) {
        const randomNum = Math.random();

        // choose one possible outcome based on probabilities
        let cumulativeProbability = 0;
        for (let i = 0; i < weights.length; i++) {
            cumulativeProbability += weights[i];
            if (randomNum < cumulativeProbability) {
                return i;
            }
        }
    }

    static indexToBitstring(i: number, nQubits: number) {
        return i.toString(2).padStart(nQubits, "0");
    }

    /**
     * Bits in result should be in reverse order in terms of qubits.
     * probability(001) -> probability(100)
     * @param result result with qubit order changed
     */
    static reverseResultQubitOrder<T>(result: T[]): T[] {
        const rev = [];
        const nQubits = Math.floor(Math.log2(result.length));

        for (let i = 0; i < 2 ** nQubits; i++) {
            const bitString = QuantumCircuitSimulator.indexToBitstring(
                i,
                nQubits
            );
            const bitStringReversed = bitString.split("").reverse().join("");
            const j = parseInt(bitStringReversed, 2);
            rev.push(result[j]);
        }
        return rev;
    }

    /**
     * Get one measurement chosen randomly from possible outcomes and their relative probabilities.
     */
    sample(qubits: Qubit[]): Measurement | undefined {
        const input = qubits.map((q) => q.value).join("");
        if (!this.result) {
            console.error("run simulator before sampling");
            return undefined;
        }

        const output = this.randomChoice(this.result);
        if (output !== undefined) {
            return {
                input: input,
                output: QuantumCircuitSimulator.indexToBitstring(
                    output,
                    this.board.length
                ),
                value: output,
            };
        }
        return undefined;
    }

    /**
     * Get probabilities of each output having value 1.
     */
    getOutputProbabilities() {
        if (!this.result) {
            console.error("run simulator before getting output probabilities");
            return undefined;
        }
        const probabilities: number[] = Array(this.board.nQubits).fill(0);
        const data = this.getProbabilities();

        // see where there are 1's and increase the respective probabilities
        for (let i = 0; i < data.probabilities.length; i++) {
            const label = data.labels[i];
            const probability = data.probabilities[i];
            for (let j = 0; j < label.length; j++) {
                if (label.charAt(j) === "1") {
                    probabilities[j] += probability;
                }
            }
        }

        return probabilities;
    }

    private computeProbabilitiesFromMeasurements(
        measurements: Measurement[],
        n: number
    ) {
        // draw samples and keep track of how many times they occur
        const counter = Array(n).fill(0);
        for (const measurement of measurements) {
            counter[measurement.value]++;
        }

        return counter.map((v) => (v / measurements.length) * 100.0);
    }

    private computeProbabilitiesBySampling(
        result: number[],
        sampleSize: number
    ) {
        // draw samples and keep track of how many times they occur
        const counter = Array(result.length).fill(0);
        for (let i = 0; i < sampleSize; i++) {
            const sample = this.randomChoice(result);

            if (sample !== undefined) {
                counter[sample]++;
            }
        }
        return counter.map((v) => (v / sampleSize) * 100.0);
    }

    /**
     * Gets probabilities and labels for each output.
     * @param sampleSize if given then use this many samples to compute probabilities for outputs
     * @param measurements if give then use them to compute probabilities for outputs
     */
    getProbabilities(
        sampleSize: number | undefined = undefined,
        measurements: Measurement[] | undefined = undefined
    ): QuantumChartData {
        if (!this.result) {
            console.error("run simulator before getting probabilities");
            return {probabilities: [], labels: []};
        }

        const N = this.result.length;
        let probabilities: number[] = [];
        let dataIndex = 0;
        if (sampleSize !== undefined) {
            probabilities = this.computeProbabilitiesBySampling(
                this.result,
                sampleSize
            );
        }
        if (measurements !== undefined) {
            probabilities = this.computeProbabilitiesFromMeasurements(
                measurements,
                this.result.length
            );
        }
        if (probabilities.length === 0) {
            probabilities = Array(N);
        }
        const labels: string[] = Array(N);
        this.result.forEach((value, i) => {
            if (sampleSize === undefined && measurements === undefined) {
                probabilities[dataIndex] = value * 100;
            }
            labels[dataIndex] = QuantumCircuitSimulator.indexToBitstring(
                i,
                this.board.length
            );
            dataIndex++;
        });
        return {
            probabilities: probabilities,
            labels: labels,
        };
    }
}

/**
 * Quantum circuit simulator where the computation in done in the browser.
 */
export class BrowserQuantumCircuitSimulator extends QuantumCircuitSimulator {
    constructor(gateService: GateService, board: QuantumBoard) {
        super(gateService, board);
    }

    private getCellMatrix(cell: Cell) {
        if (
            cell instanceof Gate ||
            cell instanceof MultiQubitGate ||
            cell instanceof Swap
        ) {
            const name = cell instanceof Swap ? "swap" : cell.name;
            const firstGateMatrix = this.gateService.getMatrix(name);
            if (firstGateMatrix) {
                return firstGateMatrix;
            }
        }
        return this.gateService.identityMatrix;
    }

    /**
     * Builds a controlled gate matrix using gate at specified cell.
     * @param target qubit index to build gate for
     * @param time time
     * @param controls control qubits for this gate
     */
    private buildControlledGate(
        target: number,
        time: number,
        controls: number[]
    ) {
        const cell = this.board.get(target, time);
        const cellMatrix = this.getCellMatrix(cell);
        const matrixSize = this.gateService.getMatrixSize(cellMatrix);
        const size = 2 ** (controls.length + matrixSize);

        const res = identity(size) as Matrix;
        const [rows, cols] = cellMatrix.size();
        return res.subset(
            index(range(size - rows, size), range(size - cols, size)),
            cellMatrix
        );
    }

    /**
     * Get list of controls for each qubit at given time.
     * @param colI time
     */
    private getGateControls(colI: number) {
        // initialize so that each gate has no controls
        const gateControls: number[][] = [];
        for (const _ of this.board.board) {
            gateControls.push([]);
        }
        // add controls to gates
        for (let i = 0; i < this.board.length; i++) {
            const cell = this.board.get(i, colI);
            if (cell instanceof Control) {
                const controlled = cell.target;
                gateControls[controlled].push(i);
            }
        }
        return gateControls;
    }

    /**
     * Takes all uncontrolled single and multi-qubit gates in column and makes a matrix out of them.
     * @param gateControls controls for each qubit at given time
     * @param colI time
     */
    private buildColumnFromAdjacentGates(
        gateControls: number[][],
        colI: number
    ) {
        let columnMatrix;
        let i = 0;
        while (i < gateControls.length) {
            const cell = this.board.get(i, colI);
            let gateMatrix = this.gateService.identityMatrix;

            if (cell instanceof Gate && gateControls[i].length === 0) {
                gateMatrix = this.getCellMatrix(cell);
                i++;
            } else if (
                cell instanceof MultiQubitGate &&
                gateControls[i].length === 0
            ) {
                gateMatrix = this.getCellMatrix(cell);
                i += cell.size;
            } else {
                i++;
            }

            if (columnMatrix) {
                columnMatrix = kron(columnMatrix, gateMatrix);
            } else {
                columnMatrix = gateMatrix;
            }
        }

        return columnMatrix;
    }

    /**
     * Expand matrix to be 2^nQubits dimensional.
     * @param mat matrix to expand
     * @param startI the relative position of matrix on board
     * @param matSize number of qubits this matrix takes
     */
    private padMatrix(mat: Matrix, startI: number, matSize: number) {
        let res = mat;
        if (startI > 0) {
            const before = identity(2 ** startI) as Matrix;
            res = kron(before, mat);
        }

        const afterSize = this.board.nQubits - (startI + matSize);
        if (afterSize > 0) {
            const after = identity(2 ** afterSize) as Matrix;
            res = kron(res, after);
        }
        return res;
    }

    /**
     * Apply swaps to move qubit from position to another.
     * @param currI current index of qubit
     * @param destI destination index of qubit
     * @param input matrix to apply to
     * @param basis relative positions of qubits
     */
    private applyPositionChange(
        currI: number,
        destI: number,
        input: Matrix,
        basis: number[]
    ) {
        let result = input;
        function applySwap(i: number, swap: Matrix) {
            result = multiply(swap, result);
            // swap basis indices
            const temp = basis[i];
            basis[i] = basis[i + 1];
            basis[i + 1] = temp;
        }
        if (currI < destI) {
            for (let i = currI; i < destI; i++) {
                const swap = this.padMatrix(this.gateService.swapMatrix, i, 2);
                applySwap(i, swap);
            }
        } else {
            for (let i = currI - 1; i >= destI; i--) {
                const swap = this.padMatrix(this.gateService.swapMatrix, i, 2);
                applySwap(i, swap);
            }
        }
        return result;
    }

    /**
     * Applies multi-qubit gate to input with specific qubits
     * @param qubits qubit indices that this gate is applied to
     * @param gate gate to apply
     * @param input current state of circuit at this time
     */
    private applyMultiQubitGate(qubits: number[], gate: Matrix, input: Matrix) {
        const basis = [];

        for (let i = 0; i < this.board.nQubits; i++) {
            basis.push(i);
        }
        // keep track of moves to do them backwards afterward

        const moves = [];
        // apply swap gates to move controls to their correct positions
        let result = input;
        for (let i = 0; i < qubits.length; i++) {
            // the positions of qubits changed so find where it is now
            const currentI = basis.indexOf(qubits[i]);
            moves.push([currentI, i]);
            result = this.applyPositionChange(currentI, i, result, basis);
        }

        // apply gate
        gate = this.padMatrix(gate, 0, qubits.length);
        result = multiply(gate, result);

        // apply swaps to move control qubits back to where they were
        for (let i = moves.length - 1; i >= 0; i--) {
            result = this.applyPositionChange(
                moves[i][1],
                moves[i][0],
                result,
                basis
            );
        }

        return result;
    }

    /**
     * Build matrix associated to given column and multiplies it with output.
     * @param colI column to get matrix for
     * @param output vector to do matrix multiplication with
     */
    private applyColumnMatrix(colI: number, output: Matrix) {
        const gateControls = this.getGateControls(colI);
        // apply all non controlled gates
        let result = this.buildColumnFromAdjacentGates(gateControls, colI);
        if (!result) {
            return undefined;
        }
        result = multiply(result, output);

        // apply all controlled gates and uncontrolled swap gates
        for (let i = 0; i < gateControls.length; i++) {
            const cell = this.board.get(i, colI);

            if (gateControls[i].length > 0) {
                // add qubits that the gate occupies (controls and gate qubits)
                const qubits = [...gateControls[i]];
                if (cell instanceof MultiQubitGate) {
                    for (let mi = i; mi < i + cell.size; mi++) {
                        qubits.push(mi);
                    }
                } else if (cell instanceof Swap) {
                    // (cell.target <= i) is added so that swap is not applied twice
                    if (cell.target <= i) {
                        continue;
                    }
                    qubits.push(cell.target);
                    qubits.push(i);
                } else {
                    qubits.push(i);
                }

                const gate = this.buildControlledGate(i, colI, gateControls[i]);
                result = this.applyMultiQubitGate(qubits, gate, result);
            } else if (
                cell instanceof Swap &&
                gateControls[i].length === 0 &&
                cell.target > i
            ) {
                const qubits = [i, cell.target];
                result = this.applyMultiQubitGate(
                    qubits,
                    this.gateService.swapMatrix,
                    result
                );
            }
        }
        return result;
    }

    /**
     * Run simulation on current circuit and qubits.
     */
    async run(qubits: Qubit[]): Promise<Result<string, string | IServerError>> {
        if (qubits.length > 10) {
            return Promise.resolve({
                ok: false,
                result: $localize`Too many qubits for browser simulator. Try changing simulate attribute to "server".`,
            });
        }

        const inputQubits = qubits.map((q) => q.asVector());

        let input = inputQubits[0];
        for (let i = 1; i < inputQubits.length; i++) {
            input = kron(input, inputQubits[i]);
        }

        let output = transpose(input);

        for (let colI = 0; colI < this.board.nMoments; colI++) {
            const res = this.applyColumnMatrix(colI, output);
            if (!res) {
                return Promise.resolve({
                    ok: false,
                    result: "undefined column matrix",
                });
            }
            output = res;
            await timeout(0);
        }

        const resMatrix = dotPow(abs(output), 2) as Matrix;
        const res = [];
        const state = [];
        const formatOptions: FormatOptions = {
            precision: 2,
        };
        for (let i = 0; i < 2 ** this.board.length; i++) {
            // when nQubits === 1 then this.result is 1d array
            if (resMatrix.size().length > 1) {
                res.push(resMatrix.get([i, 0]));
                state.push(
                    format(output.get([i, 0]), formatOptions).replace(/\s/g, "")
                );
            } else {
                res.push(resMatrix.get([i]));
                state.push(
                    format(output.get([i]), formatOptions).replace(/\s/g, "")
                );
            }
        }
        this.result = res;
        this.stateVector = state;

        return Promise.resolve({
            ok: true,
            result: "",
        });
    }
}

/**
 * Quantum circuit simulator where the computation is done on the server.
 */
export class ServerQuantumCircuitSimulator extends QuantumCircuitSimulator {
    serializerService: SerializerService;
    customGates: ICustomGateInfo[] | null | undefined;

    constructor(
        private http: HttpClient,
        gateService: GateService,
        board: QuantumBoard,
        serializerService: SerializerService,
        customGates: ICustomGateInfo[] | null | undefined
    ) {
        super(gateService, board);
        this.serializerService = serializerService;
        this.customGates = customGates;
    }

    parseError(e: string) {
        const errInfo = ServerError.decode(JSON.parse(e));
        if (isRight(errInfo)) {
            return errInfo.right;
        }
        return e;
    }

    async run(qubits: Qubit[]): Promise<Result<string, string | IServerError>> {
        const inputList = qubits.map((q) => q.value);
        let customGates: INumericCustomGateInfo[] = [];
        if (this.customGates) {
            customGates = this.serializerService.serializeCustomGates(
                this.customGates,
                this.gateService
            );
        }

        const params: SimulationArgs = {
            gates: this.serializerService.serializeUserCircuit(this.board),
            nQubits: this.board.length,
            inputList: inputList,
            customGates: customGates,
        };

        const url = "/quantumCircuit/simulate";
        const r = await toPromise(
            this.http.post<{
                web: {
                    result?: number[];
                    error?: string;
                    stateVector?: string[];
                };
            }>(url, params)
        );
        if (r.ok) {
            const e = r.result.web.error;
            if (e) {
                const err = this.parseError(e);
                return Promise.resolve({ok: false, result: err});
            }
            const res = r.result.web.result;
            const stateVector = r.result.web.stateVector;
            if (res && stateVector) {
                this.result =
                    QuantumCircuitSimulator.reverseResultQubitOrder(res);
                this.stateVector =
                    QuantumCircuitSimulator.reverseResultQubitOrder(
                        stateVector
                    );
            }
        } else {
            return Promise.resolve({ok: false, result: r.result.error.error});
        }
        return Promise.resolve({ok: true, result: ""});
    }
}
