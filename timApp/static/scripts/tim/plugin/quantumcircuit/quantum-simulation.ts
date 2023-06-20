import type {
    Board,
    Measurement,
    Qubit,
} from "tim/plugin/quantumcircuit/quantum-circuit.component";

import type {Complex, Matrix} from "mathjs";
import {divide} from "mathjs";
import {pi} from "mathjs";
import {e} from "mathjs";
import {pow} from "mathjs";
import {dotPow} from "mathjs";
import {abs} from "mathjs";
import {transpose} from "mathjs";
import {complex} from "mathjs";
import {multiply} from "mathjs";
import {kron, matrix} from "mathjs";
import type {QuantumChartData} from "tim/plugin/quantumcircuit/quantum-stats.component";

export class QuantumCircuitSimulator {
    board: Board;
    qubits: Qubit[];
    result?: Matrix;
    gateNameToMatrix: Map<string, Matrix> = new Map();

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

        const I = matrix([
            [1, 0],
            [0, 1],
        ]);
        this.gateNameToMatrix.set("H", H);
        this.gateNameToMatrix.set("X", X);
        this.gateNameToMatrix.set("Y", Y);
        this.gateNameToMatrix.set("Z", Z);
        this.gateNameToMatrix.set("S", S);
        this.gateNameToMatrix.set("T", T);
        this.gateNameToMatrix.set("", I);
    }

    run() {
        const inputQubits = this.qubits.map((q) => this.bitToVector(q.value));

        let input = inputQubits[0];
        for (let i = 1; i < inputQubits.length; i++) {
            input = kron(input, inputQubits[i]);
        }

        this.result = input;

        const identityMatrix = matrix([
            [1, 0],
            [0, 1],
        ]);

        const colMatrices = [];
        for (let colI = 0; colI < this.board[0].length; colI++) {
            const firstGateName = this.board[0][colI]?.name;
            let colMatrix = identityMatrix;
            const firstGateMatrix = this.gateNameToMatrix.get(
                firstGateName ?? ""
            );
            if (firstGateMatrix) {
                colMatrix = firstGateMatrix;
            }
            for (let rowI = 1; rowI < this.board.length; rowI++) {
                const gateName = this.board[rowI][colI]?.name;
                if (gateName === undefined) {
                    colMatrix = kron(colMatrix, identityMatrix);
                } else {
                    const gateMatrix = this.gateNameToMatrix.get(gateName);
                    if (!gateMatrix) {
                        console.log("missing matrix for gate", gateName);
                    } else {
                        colMatrix = kron(colMatrix, gateMatrix);
                    }
                }
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

    getNumber(index: number | number[]) {
        if (index instanceof Array) {
            return index[0];
        }
        return index;
    }

    bitToVector(value: number) {
        if (value === 0) {
            return matrix([1, 0]);
        }
        return matrix([0, 1]);
    }

    indexToBitstring(index: number) {
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
