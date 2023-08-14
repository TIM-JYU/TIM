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

export interface ServiceGate {
    name: string;
    matrix: Matrix;
    hidden: boolean;
    group: "basic" | "phase" | "swap" | "control" | "custom";
    description: string;
}

@Injectable()
export class GateService {
    gates: ServiceGate[] = [];
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
                group: "basic",
                description: "Hadamard",
            },
            {
                name: "X",
                matrix: X,
                hidden: false,
                group: "basic",
                description: "Pauli-X",
            },
            {
                name: "Y",
                matrix: Y,
                hidden: false,
                group: "basic",
                description: "Pauli-Y",
            },
            {
                name: "Z",
                matrix: Z,
                hidden: false,
                group: "basic",
                description: "Pauli-Z",
            },
            {
                name: "S",
                matrix: S,
                hidden: false,
                group: "phase",
                description: "Phase S",
            },
            {
                name: "T",
                matrix: T,
                hidden: false,
                group: "phase",
                description: "Phase T",
            },
            {
                name: "swap",
                matrix: this.swapMatrix,
                hidden: false,
                group: "swap",
                description: "Swap",
            },
            {
                name: "control",
                matrix: this.identityMatrix,
                hidden: false,
                group: "control",
                description: "Control",
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
     * Check if gate with given name exists and is in gate menu
     * @param name
     */
    isMenuGate(name: string) {
        for (const gate of this.gates) {
            if (!gate.hidden && gate.name === name) {
                return true;
            }
        }
        return false;
    }

    getGate(name: string) {
        return this.gates.find((g) => g.name === name);
    }

    getGateGroup(name: string) {
        return this.getGate(name)?.group;
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

    getGateNames() {
        return this.gates.map((g) => g.name);
    }

    /**
     * Uses canvas.measureText to compute and return the width of the given text of given font in pixels.
     *
     * @param {String} text The text to be rendered.
     * @param {String} font The css font descriptor that text is to be rendered with (e.g. "bold 14px verdana").
     *
     * @see https://stackoverflow.com/questions/118241/calculate-text-width-with-javascript/21015393#21015393
     */
    getTextWidth(
        text: string,
        font: string = "normal 16px Verdana,Arial,sans-serif"
    ) {
        // re-use canvas object for better performance
        const canvas = document.createElement("canvas");
        const context = canvas.getContext("2d");
        if (!context) {
            return text.length * 16;
        }
        context.font = font;
        const metrics = context.measureText(text);
        return metrics.width;
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
                    group: "custom",
                    description: customGate.description,
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
     * @throws Error if matrix can't be parsed
     */
    private parseCustomGate(name: string, matrixStr: string) {
        try {
            const customMatrix = evaluate(matrixStr);
            if (customMatrix instanceof Matrix) {
                return {
                    name: name,
                    matrix: customMatrix,
                };
            }
        } catch (error) {
            throw new Error(`Invalid custom matrix value ${name} ${matrixStr}`);
        }
        throw new Error(`Invalid custom matrix value ${name} ${matrixStr}`);
    }
}
