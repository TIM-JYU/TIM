import type {
    Measurement,
    Qubit,
} from "tim/plugin/quantumcircuit/quantum-circuit.component";

import type {Matrix} from "mathjs";
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

export class QuantumCircuitSimulator {
    board: QuantumBoard;
    qubits: Qubit[];
    result?: Matrix;

    constructor(
        private gateService: GateService,
        board: QuantumBoard,
        qubits: Qubit[]
    ) {
        this.board = board;
        this.qubits = qubits;
    }

    private getCellMatrix(cell: Cell) {
        if (cell instanceof Gate || cell instanceof MultiQubitGate) {
            const firstGateMatrix = this.gateService.getMatrix(cell.name);
            if (firstGateMatrix) {
                return firstGateMatrix;
            }
        }
        return this.gateService.identityMatrix;
    }

    /**
     * Builds a controlled matrix using gate at specified cell with as many controls as
     * that gate has.
     * @param target qubit index to build gate for
     * @param time time
     * @param controls controllers of this gate
     */
    private buildControlledGate(
        target: number,
        time: number,
        controls: number[]
    ) {
        const cell = this.board.get(target, time);
        const cellMatrix = this.getCellMatrix(cell);
        const size = 2 ** (controls.length + 1);
        const res = identity(size) as Matrix;
        const [rows, cols] = cellMatrix.size();
        return res.subset(
            index([size - rows, size - 1], [size - cols, size - 1]),
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
     * Takes all single and multi-qubit gates in column and makes a matrix out of them.
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
            } else if (cell instanceof MultiQubitGate) {
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
    applyPositionChange(
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
        let result = this.buildColumnFromAdjacentGates(gateControls, colI);
        if (!result) {
            return undefined;
        }
        result = multiply(result, output);
        for (let i = 0; i < gateControls.length; i++) {
            const cell = this.board.get(i, colI);
            // controlled gate
            if (cell instanceof Gate && gateControls[i].length > 0) {
                const qubits = [...gateControls[i], i];
                const gate = this.buildControlledGate(i, colI, gateControls[i]);
                result = this.applyMultiQubitGate(qubits, gate, result);
            }
            // (cell.target > i) is added so that swap is not applied twice
            if (cell instanceof Swap && cell.target > i) {
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
    run() {
        const inputQubits = this.qubits.map((q) => q.asVector());

        let input = inputQubits[0];
        for (let i = 1; i < inputQubits.length; i++) {
            input = kron(input, inputQubits[i]);
        }

        let output = transpose(input);

        for (let colI = 0; colI < this.board.nMoments; colI++) {
            const res = this.applyColumnMatrix(colI, output);
            if (!res) {
                console.log("undefined column matrix");
                return;
            }
            output = res;
        }

        this.result = dotPow(abs(output), 2) as Matrix;
    }

    private getNumber(i: number | number[]) {
        if (i instanceof Array) {
            return i[0];
        }
        return i;
    }

    private indexToBitstring(i: number) {
        return i.toString(2).padStart(this.qubits.length, "0");
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
        console.log("got here");
    }

    /**
     * Get one measurement chosen randomly from possible outcomes and their relative probabilities.
     */
    sample(): Measurement | undefined {
        const input = this.qubits.map((q) => q.value).join("");

        if (!this.result) {
            return undefined;
        }

        const probabilities: number[] = [];
        this.result.forEach((probability) => {
            probabilities.push(probability);
        });

        const output = this.randomChoice(probabilities);
        if (output !== undefined) {
            return {
                input: input,
                output: this.indexToBitstring(output),
                value: output,
            };
        }
        return undefined;
    }

    private computeProbabilitiesBySampling(result: Matrix, sampleSize: number) {
        const probabilities: number[] = [];
        result.forEach((probability) => {
            probabilities.push(probability);
        });

        // draw samples and keep track of how many times they occur
        const counter = Array(probabilities.length).fill(0);
        for (let i = 0; i < sampleSize; i++) {
            const sample = this.randomChoice(probabilities);

            if (sample !== undefined) {
                counter[sample]++;
            }
        }
        return counter.map((v) => (v / sampleSize) * 100.0);
    }

    computeProbabilitiesFromMeasurements(
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
            console.log("run simulator first");
            return {probabilities: [], labels: []};
        }

        let probabilities: number[] = [];
        if (sampleSize !== undefined) {
            probabilities = this.computeProbabilitiesBySampling(
                this.result,
                sampleSize
            );
        }
        if (measurements !== undefined) {
            probabilities = this.computeProbabilitiesFromMeasurements(
                measurements,
                this.result.size()[0]
            );
        }
        const labels: string[] = [];
        this.result.forEach((value, i) => {
            if (sampleSize === undefined && measurements === undefined) {
                probabilities.push(value * 100);
            }
            // index should be number[] but it seems to be typed incorrectly as number
            const label: number = this.getNumber(i);
            const bitString: string = this.indexToBitstring(label);
            labels.push(bitString);
        });
        return {
            probabilities: probabilities,
            labels: labels,
        };
    }

    setBoard(board: QuantumBoard) {
        this.board = board;
    }
}
