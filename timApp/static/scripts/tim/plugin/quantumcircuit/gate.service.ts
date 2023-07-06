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
import type {ICustomGateInfo} from "tim/plugin/quantumcircuit/quantum-circuit.component";

export interface Gate {
    name: string;
    matrix: Matrix;
    hidden: boolean;
}

@Injectable()
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
                hidden: false,
            },
            {
                name: "X",
                matrix: X,
                hidden: false,
            },
            {
                name: "Y",
                matrix: Y,
                hidden: false,
            },
            {
                name: "Z",
                matrix: Z,
                hidden: false,
            },
            {
                name: "S",
                matrix: S,
                hidden: false,
            },
            {
                name: "T",
                matrix: T,
                hidden: false,
            },
            {
                name: "swap",
                matrix: this.swapMatrix,
                hidden: false,
            },
            {
                name: "control",
                matrix: this.identityMatrix,
                hidden: false,
            },
        ];

        for (const gate of this.gates) {
            this.gateNameToMatrix.set(gate.name, gate.matrix);
        }
    }

    /**
     * Get gates to be shown in menu to user.
     */
    getMenuGates() {
        return of(this.gates.filter((g) => !g.hidden));
    }

    getMatrix(name: string) {
        return this.gateNameToMatrix.get(name);
    }

    /**
     * Get number of qubits this gate takes.
     * @param name name of gate to get size for
     */
    getGateSize(name: string) {
        const gate = this.gateNameToMatrix.get(name);
        if (!gate) {
            return 1;
        }
        const sideLength = gate.size()[0];
        return Math.floor(Math.log2(sideLength));
    }

    private registerCustomGates(customGates: ICustomGateInfo[]) {
        for (const customGate of customGates) {
            const parsedCustomGate = this.parseCustomGate(
                customGate.name,
                customGate.matrix
            );
            if (parsedCustomGate) {
                this.gates.push({
                    name: parsedCustomGate.name,
                    matrix: parsedCustomGate.matrix,
                    hidden: false,
                });
                this.gateNameToMatrix.set(
                    parsedCustomGate.name,
                    parsedCustomGate.matrix
                );
            }
        }
    }

    /**
     * Adds user defined gates.
     * @param gateNames names of gates to show in menu
     * @param customGates self defined gates
     */
    registerUserDefinedGates(
        gateNames: string[] | null | undefined,
        customGates: ICustomGateInfo[] | null | undefined
    ) {
        if (customGates) {
            this.registerCustomGates(customGates);
        }

        // set which ones are visible in menu
        if (gateNames !== undefined && gateNames !== null) {
            const menuSet = new Set(gateNames);
            for (const gate of this.gates) {
                const visibleInMenu = menuSet.has(gate.name);
                gate.hidden = !visibleInMenu;
            }
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
