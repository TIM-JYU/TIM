import {Injectable} from "@angular/core";
import type {Complex} from "mathjs";
import {
    complex,
    divide,
    e,
    evaluate,
    matrix,
    multiply,
    pi,
    pow,
    Matrix,
} from "mathjs";
import {of} from "rxjs";

export interface Gate {
    name: string;
    matrix: Matrix;
}

@Injectable({
    providedIn: "root",
})
export class GateService {
    gates: Gate[] = [];
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

    constructor() {
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

        this.gates = [
            {
                name: "H",
                matrix: H,
            },
            {
                name: "X",
                matrix: X,
            },
            {
                name: "Y",
                matrix: Y,
            },
            {
                name: "Z",
                matrix: Z,
            },
            {
                name: "S",
                matrix: S,
            },
            {
                name: "T",
                matrix: T,
            },
            {
                name: "swap",
                matrix: this.swapMatrix,
            },
            {
                name: "control",
                matrix: this.identityMatrix,
            },
        ];

        for (const gate of this.gates) {
            this.gateNameToMatrix.set(gate.name, gate.matrix);
        }
    }

    getGates() {
        return of(this.gates);
    }

    getMatrix(name: string) {
        return this.gateNameToMatrix.get(name);
    }

    /**
     * Replaces default set of gates with user defined ones.
     * @param gateNames names of gates to use from default set of gates
     * @param customGates gates defined by user
     */
    registerUserDefinedGates(
        gateNames: string[],
        customGates: {name: string; matrix: string}[]
    ) {
        // don't override default ones if user didn't provide their owns
        if (gateNames.length === 0) {
            return;
        }

        this.gates = [];

        for (const gateName of gateNames) {
            const m = this.gateNameToMatrix.get(gateName);
            if (m) {
                this.gates.push({
                    name: gateName,
                    matrix: m,
                });
            }
        }

        for (const customGate of customGates) {
            const parsedCustomGate = this.parseCustomGate(
                customGate.name,
                customGate.matrix
            );
            if (parsedCustomGate) {
                this.gates.push(parsedCustomGate);
                this.gateNameToMatrix.set(
                    parsedCustomGate.name,
                    parsedCustomGate.matrix
                );
            }
        }

        this.gateNameToMatrix = new Map<string, Matrix>();
        for (const gate of this.gates) {
            this.gateNameToMatrix.set(gate.name, gate.matrix);
        }
    }

    /**
     * Parses a new gate with given name and parses
     * its matrix from string.
     * Matrix is parsed using syntax specified in
     * https://mathjs.org/docs/expressions/syntax.html
     * @param name name of gate. Should be unique
     * @param matrixStr string to parse matrix from
     */
    private parseCustomGate(name: string, matrixStr: string) {
        try {
            const customMatrix = evaluate(matrixStr);
            if (!(customMatrix instanceof Matrix)) {
                console.error("invalid custom matrix value", name, matrixStr);
                return undefined;
            }
            return {
                name: name,
                matrix: customMatrix,
            };
        } catch (error) {
            console.error("invalid custom matrix value", name, matrixStr);
        }
    }
}
