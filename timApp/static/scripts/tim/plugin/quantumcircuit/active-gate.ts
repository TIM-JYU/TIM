import {format} from "mathjs";
import type {FormatOptions, Matrix} from "mathjs";
import type {Qubit} from "tim/plugin/quantumcircuit/qubit";

/**
 * Content to show in table cell as rounded and longer version
 */
export interface TableCellData {
    rounded: string;
    long: string;
}

/**
 * Information about gate.
 */
export class ActiveGateInfo {
    name: string;
    description: string;
    matrix: Matrix;
    /**
     * @param name the name of gate
     * @param description more detailed info about gate than its name
     * @param mat actual matrix presentation of the gate
     */
    constructor(name: string, description: string, mat: Matrix) {
        this.name = name;
        this.description = description;
        this.matrix = mat;
    }

    /**
     * Formats the matrix into an array with elements as strings
     * to be used in html table.
     */
    formatMatrixAsTable() {
        const arr = this.matrix.toArray();
        const formatOptions: FormatOptions = {
            precision: 2,
        };
        const res: TableCellData[][] = [];
        arr.forEach((row) => {
            if (Array.isArray(row)) {
                const rowValues = row.map((v) => ({
                    rounded: format(v, formatOptions).replace(/\s/g, ""),
                    long: format(v).replace(/\s/g, ""),
                }));
                res.push(rowValues);
            }
        });
        return res;
    }
}

/**
 * Information about gate that is placed on board.
 */
export class CircuitActiveGateInfo extends ActiveGateInfo {
    target: number;
    time: number;
    controls: number[];
    qubits: Qubit[];
    editable: boolean;
    swap?: [number, number];

    /**
     * @param target the index of qubit related to this gate
     * @param time the time moment related to this gate
     * @param name the name of gate
     * @param mat actual matrix presentation of the gate
     * @param controls indices of qubits that control this gate if any or just an empty array
     * @param qubits qubit objects used to get names of qubits
     * @param description more detailed info about gate than its name
     * @param swap pair of qubit indices for swap gate
     * @param editable whether this gate can be edited or not
     */
    constructor(
        target: number,
        time: number,
        name: string,
        mat: Matrix,
        controls: number[],
        qubits: Qubit[],
        description: string,
        editable: boolean,
        swap?: [number, number]
    ) {
        super(name, description, mat);
        this.target = target;
        this.time = time;
        this.qubits = qubits;
        this.controls = controls;
        this.editable = editable;
        this.swap = swap;
    }

    formatControlsAsString() {
        return this.controls.map((ci) => this.qubits[ci].name).join(", ");
    }

    formatSwapAsString() {
        if (!this.swap) {
            return "";
        }
        return (
            this.qubits[this.swap[0]].name +
            ", " +
            this.qubits[this.swap[1]].name
        );
    }

    formatTimeAsString() {
        return this.time.toString();
    }

    formatQubitAsString() {
        return this.qubits[this.target].name;
    }
}
