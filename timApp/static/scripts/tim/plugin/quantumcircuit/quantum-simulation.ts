import type {
    Measurement,
    Qubit,
} from "tim/plugin/quantumcircuit/quantum-circuit.component";

import type {Complex, Matrix} from "mathjs";
import {
    divide,
    pi,
    e,
    pow,
    dotPow,
    abs,
    transpose,
    complex,
    multiply,
    kron,
    matrix,
    identity,
    index,
} from "mathjs";
import type {QuantumChartData} from "tim/plugin/quantumcircuit/quantum-stats.component";
import type {Cell, QuantumBoard} from "tim/plugin/quantumcircuit/quantum-board";
import {Control, Gate} from "tim/plugin/quantumcircuit/quantum-board";

export class QuantumCircuitSimulator {
    board: QuantumBoard;
    qubits: Qubit[];
    result?: Matrix;
    gateNameToMatrix: Map<string, Matrix> = new Map();

    identityMatrix = matrix([
        [1, 0],
        [0, 1],
    ]);

    swapMatrix = matrix([
        [1, 0, 0, 0],
        [0, 0, 1, 0],
        [0, 1, 0, 0],
        [0, 0, 0, 1],
    ]);

    constructor(board: QuantumBoard, qubits: Qubit[]) {
        this.board = board;
        this.qubits = qubits;
        const H = multiply(
            matrix([
                [1, 1],
                [1, -1],
            ]),
            1 / Math.sqrt(2)
        );
        const X = matrix([
            [0, 1],
            [1, 0],
        ]);
        const Y = matrix([
            [0, complex(0, -1)],
            [complex(0, 1), 0],
        ]);
        const Z = matrix([
            [1, 0],
            [0, -1],
        ]);
        const S = matrix([
            [1, 0],
            [0, complex(0, 1)],
        ]);
        const tValue = pow(
            e,
            divide(multiply(complex(0, 1), pi) as Complex, 4) as Complex
        ) as Complex;
        const T = matrix([
            [1, 0],
            [0, tValue],
        ]);

        this.gateNameToMatrix.set("H", H);
        this.gateNameToMatrix.set("X", X);
        this.gateNameToMatrix.set("Y", Y);
        this.gateNameToMatrix.set("Z", Z);
        this.gateNameToMatrix.set("S", S);
        this.gateNameToMatrix.set("T", T);
    }

    private getCellMatrix(cell: Cell) {
        if (cell instanceof Gate) {
            const firstGateMatrix = this.gateNameToMatrix.get(cell.name);
            if (firstGateMatrix) {
                return firstGateMatrix;
            }
        }
        return this.identityMatrix;
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
        for (const _ of this.board) {
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
     * Takes all 2x2 gates in row and makes a matrix out of them.
     * @param gateControls controls for each qubit at given time
     * @param colI time
     */
    private buildColumnFromSingleGates(gateControls: number[][], colI: number) {
        let columnMatrix;
        for (let i = 0; i < gateControls.length; i++) {
            // replace controls and gates with identity matrix
            if (
                gateControls[i].length > 0 ||
                this.board.get(i, colI) instanceof Control
            ) {
                const gateMatrix = this.identityMatrix;
                if (columnMatrix) {
                    columnMatrix = kron(columnMatrix, gateMatrix);
                } else {
                    columnMatrix = gateMatrix;
                }
            } else {
                // use 2x2 gate if one exists
                const gateMatrix = this.getCellMatrix(this.board.get(i, colI));
                if (columnMatrix) {
                    columnMatrix = kron(columnMatrix, gateMatrix);
                } else {
                    columnMatrix = gateMatrix;
                }
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
                const swap = this.padMatrix(this.swapMatrix, i, 2);
                applySwap(i, swap);
            }
        } else {
            for (let i = currI - 1; i >= destI; i--) {
                const swap = this.padMatrix(this.swapMatrix, i, 2);
                applySwap(i, swap);
            }
        }
        return result;
    }

    /**
     * Build controlled gate matrix.
     * @param controls qubit indices that control this qubit
     * @param target qubit index to build matrix for
     * @param time time
     * @param input current state of circuit at this time
     */
    private applyColumnMatrixFromControlledGate(
        controls: number[],
        target: number,
        time: number,
        input: Matrix
    ) {
        // create the controlled gate
        let gate = this.buildControlledGate(target, time, controls);
        gate = this.padMatrix(gate, 0, controls.length + 1);

        const basis = [];
        for (let i = 0; i < this.board.nQubits; i++) {
            basis.push(i);
        }

        // keep track of moves to do them backwards afterward
        const moves = [];
        // apply swap gates to move controls to their correct positions
        let result = input;
        for (let i = 0; i < controls.length; i++) {
            const currI = controls[i];
            moves.push([basis[currI], i]);
            result = this.applyPositionChange(basis[currI], i, result, basis);
        }
        // apply swaps also for gate target qubit to move it to correct position (after control qubits)
        moves.push([basis[target], controls.length]);
        result = this.applyPositionChange(
            basis[target],
            controls.length,
            result,
            basis
        );

        // apply gate
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

        console.log(basis);
        return result;
    }

    /**
     * Build matrix associated to given column and multiplies it with output.
     * @param colI column to get matrix for
     * @param output vector to do matrix multiplication with
     */
    private applyColumnMatrix(colI: number, output: Matrix) {
        const gateControls = this.getGateControls(colI);
        let result = this.buildColumnFromSingleGates(gateControls, colI);
        if (!result) {
            return undefined;
        }
        result = multiply(result, output);
        for (let i = 0; i < gateControls.length; i++) {
            const cell = this.board.get(i, colI);
            // controlled gate
            if (cell instanceof Gate && gateControls[i].length > 0) {
                result = this.applyColumnMatrixFromControlledGate(
                    gateControls[i],
                    i,
                    colI,
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
        const inputQubits = this.qubits.map((q) => this.bitToVector(q.value));

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

    /**
     * Transforms bit into corresponding qubit state.
     * @param value bit value either 0 or 1
     */
    private bitToVector(value: number) {
        if (value === 0) {
            return matrix([1, 0]);
        }
        return matrix([0, 1]);
    }

    private indexToBitstring(i: number) {
        return i.toString(2).padStart(this.qubits.length, "0");
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

        const randomNum = Math.random();

        // choose one possible outcome based on probabilities
        let cumulativeProbability = 0;
        for (let i = 0; i < probabilities.length; i++) {
            cumulativeProbability += probabilities[i];
            if (randomNum < cumulativeProbability) {
                return {
                    input: input,
                    output: this.indexToBitstring(i),
                };
            }
        }

        return undefined;
    }

    /**
     * Gets probabilities and labels for each output.
     */
    getProbabilities(): QuantumChartData {
        if (!this.result) {
            console.log("run simulator first");
            return {probabilities: [], labels: []};
        }
        const probabilities: number[] = [];
        const labels: string[] = [];
        this.result.forEach((value, i) => {
            probabilities.push(value * 100);
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
}
