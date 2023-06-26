import type {
    Board,
    Cell,
    Measurement,
    Qubit,
} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import {
    Control,
    Gate,
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

export class QuantumCircuitSimulator {
    board: Board;
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

    private buildControlledGate(
        target: number,
        time: number,
        controls: number[]
    ) {
        const cell = this.board[target][time];
        const cellMatrix = this.getCellMatrix(cell);
        const size = 2 ** (controls.length + 1);
        const res = identity(size) as Matrix;
        const [rows, cols] = cellMatrix.size();
        return res.subset(
            index([size - rows, size - 1], [size - cols, size - 1]),
            cellMatrix
        );
    }

    private getGateControls(colI: number) {
        // initialize so that each gate has no controls
        const gateControls: number[][] = [];
        this.board.forEach(() => {
            gateControls.push([]);
        });
        // add controls to gates
        for (let i = 0; i < this.board.length; i++) {
            const cell = this.board[i][colI];
            if (cell instanceof Control) {
                const controlled = cell.target;
                gateControls[controlled].push(i);
            }
        }
        return gateControls;
    }

    private buildColumnFromSingleGates(gateControls: number[][], colI: number) {
        let columnMatrix;
        for (let i = 0; i < gateControls.length; i++) {
            if (
                gateControls[i].length > 0 ||
                this.board[i][colI] instanceof Control
            ) {
                const gateMatrix = this.identityMatrix;
                if (columnMatrix) {
                    columnMatrix = kron(columnMatrix, gateMatrix);
                } else {
                    columnMatrix = gateMatrix;
                }
            } else {
                const gateMatrix = this.getCellMatrix(this.board[i][colI]);
                if (columnMatrix) {
                    columnMatrix = kron(columnMatrix, gateMatrix);
                } else {
                    columnMatrix = gateMatrix;
                }
            }
        }
        return columnMatrix;
    }

    private buildColumnWithoutPadding(
        controls: number[],
        target: number,
        time: number
    ) {
        const firstControlI = controls[0];
        let distance = target - firstControlI - 1;
        let gate = this.buildControlledGate(target, time, controls);

        if (firstControlI > target) {
            distance = firstControlI - target - 1;
            gate = multiply(multiply(this.swapMatrix, gate), this.swapMatrix);
        }

        // move gate and control next to each other if they aren't next to each other
        if (distance > 0) {
            // move qubits using swap gates
            let swapsBeforeGate = kron(
                identity(2 ** distance) as Matrix,
                this.swapMatrix
            );
            for (let i = 1; i < distance; i++) {
                const paddingBefore = identity(2 ** (distance - i)) as Matrix;
                const paddingAfter = identity(
                    2 ** (distance - (distance - i))
                ) as Matrix;
                const swapLayer = kron(
                    kron(paddingBefore, this.swapMatrix),
                    paddingAfter
                );
                swapsBeforeGate = multiply(swapsBeforeGate, swapLayer);
            }

            // pad gate with identity matrix to span the same distance as controls and target do
            // tensor product of identity matrix should also be identity matrix so just create it directly with right size
            const gateWithPadding = kron(
                gate,
                identity(2 ** distance) as Matrix
            );

            let result = multiply(swapsBeforeGate, gateWithPadding);

            // reverse the effect of moving qubits by moving them back
            for (let i = 1; i < distance; i++) {
                const paddingBefore = identity(
                    2 ** (distance - (distance - i))
                ) as Matrix;
                const paddingAfter = identity(2 ** (distance - i)) as Matrix;
                const swapLayer = kron(
                    kron(paddingBefore, this.swapMatrix),
                    paddingAfter
                );
                result = multiply(result, swapLayer);
            }
            result = multiply(
                result,
                kron(identity(2 ** distance) as Matrix, this.swapMatrix)
            );
            return result;
        }
        return gate;
    }

    private buildColumnMatrixFromControlledGate(
        controls: number[],
        target: number,
        time: number
    ) {
        let mat = this.buildColumnWithoutPadding(controls, target, time);
        // pad the size to match the total qubit amount with identity matrices
        if (controls[0] < target) {
            if (controls[0] !== 0) {
                mat = kron(identity(2 ** controls[0]) as Matrix, mat);
            }
            if (target !== this.board.length - 1) {
                mat = kron(
                    mat,
                    identity(2 ** (this.board.length - target - 1)) as Matrix
                );
            }
        } else {
            if (target !== 0) {
                mat = kron(identity(2 ** target) as Matrix, mat);
            }
            if (controls[controls.length - 1] !== this.board.length - 1) {
                mat = kron(
                    mat,
                    identity(
                        2 **
                            (this.board.length -
                                controls[controls.length - 1] -
                                1)
                    ) as Matrix
                );
            }
        }

        return mat;
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
            const cell = this.board[i][colI];
            // controlled gate
            if (cell instanceof Gate && gateControls[i].length > 0) {
                const gateMatrix = this.buildColumnMatrixFromControlledGate(
                    gateControls[i],
                    i,
                    colI
                );
                result = multiply(gateMatrix, result);
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

        for (let colI = 0; colI < this.board[0].length; colI++) {
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
