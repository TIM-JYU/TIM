import type {
    Board,
    Measurement,
    Qubit,
} from "tim/plugin/quantumcircuit/quantum-circuit.component";

import type {Matrix} from "mathjs";
import {kron, matrix} from "mathjs";
import type {QuantumChartData} from "tim/plugin/quantumcircuit/quantum-stats.component";

export class QuantumCircuitSimulator {
    board: Board;
    qubits: Qubit[];
    result?: Matrix;
    constructor(board: Board, qubits: Qubit[]) {
        this.board = board;
        this.qubits = qubits;
    }

    bitToVector(value: number) {
        if (value === 0) {
            return matrix([1, 0]);
        }
        return matrix([0, 1]);
    }

    run() {
        const inputQubits = this.qubits.map((q) => this.bitToVector(q.value));

        let input = inputQubits[0];
        for (let i = 1; i < inputQubits.length; i++) {
            input = kron(input, inputQubits[i]);
        }

        this.result = input;

        // for (let i = 0; i < this.board.length; i++) {
        //     const colMatrices =
        //     for (let j = 0; j < this.board[0].length; j++) {
        //         const gate = this.board[]
        //         const M = identity(2);
        //     }
        // }
    }

    getNumber(index: number | number[]) {
        if (index instanceof Array) {
            return index[1];
        }
        return index;
    }

    /**
     * One sample from possible outputs
     */
    sample(): Measurement {
        const input = this.qubits.map((q) => q.value).join("");

        const output = "";

        if (!this.result) {
            return {
                input: "",
                output: "",
            };
        }

        const probabilities: [number, number][] = [];
        this.result.forEach((probability, index) => {
            probabilities.push([this.getNumber(index), probability]);
        });

        // sort by probability
        probabilities.sort((a, b) => b[1] - a[1]);

        const randomNum = Math.random();

        // choose one possible outcome based on probabilities
        let i = 0;
        let cumulativeProbability = 0;
        while (i < probabilities.length) {
            cumulativeProbability += probabilities[i][1];
            if (randomNum <= cumulativeProbability) {
                return {
                    input: input,
                    output: probabilities[i][0]
                        .toString(2)
                        .padStart(this.qubits.length, "0"),
                };
            }
            i++;
        }

        return {
            input: input,
            output: output,
        };
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
            const bitString: string = label
                .toString(2)
                .padStart(this.qubits.length, "0");
            labels.push(bitString);
        });
        return {
            probabilities: probabilities,
            labels: labels,
        };
    }
}
