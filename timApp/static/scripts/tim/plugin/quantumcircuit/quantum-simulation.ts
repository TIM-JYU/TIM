import type {
    Board,
    Cell,
    Measurement,
    Qubit,
} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import {Gate} from "tim/plugin/quantumcircuit/quantum-circuit.component";

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
} from "mathjs";
import type {QuantumChartData} from "tim/plugin/quantumcircuit/quantum-stats.component";

export class QuantumCircuitSimulator {
    board: Board;
    qubits: Qubit[];
    result?: Matrix;
    gateNameToMatrix: Map<string, Matrix> = new Map();

    identityMatrix = matrix([
        [1, 0],
        [0, 1],
    ]);

    constructor(board: Board, qubits: Qubit[]) {
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
     * Run simulation on current circuit and qubits.
     */
    run() {
        const inputQubits = this.qubits.map((q) => this.bitToVector(q.value));

        let input = inputQubits[0];
        for (let i = 1; i < inputQubits.length; i++) {
            input = kron(input, inputQubits[i]);
        }

        this.result = input;

        const colMatrices = [];

        for (let colI = 0; colI < this.board[0].length; colI++) {
            let colMatrix = this.getCellMatrix(this.board[0][colI]);

            for (let rowI = 1; rowI < this.board.length; rowI++) {
                colMatrix = kron(
                    colMatrix,
                    this.getCellMatrix(this.board[rowI][colI])
                );
            }
            colMatrices.push(colMatrix);
        }

        let circuitMatrix = colMatrices[0];
        for (let i = 1; i < colMatrices.length; i++) {
            circuitMatrix = multiply(circuitMatrix, colMatrices[i]);
        }
        this.result = dotPow(
            abs(multiply(circuitMatrix, transpose(input))),
            2
        ) as Matrix;
    }

    private getNumber(index: number | number[]) {
        if (index instanceof Array) {
            return index[0];
        }
        return index;
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

    private indexToBitstring(index: number) {
        return index.toString(2).padStart(this.qubits.length, "0");
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
        this.result.forEach((value, index) => {
            probabilities.push(value * 100);
            // index should be number[] but it seems to be typed incorrectly as number
            const label: number = this.getNumber(index);
            const bitString: string = this.indexToBitstring(label);
            labels.push(bitString);
        });
        return {
            probabilities: probabilities,
            labels: labels,
        };
    }
}
