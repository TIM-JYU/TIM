import type {CircuitOptions} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import {matrix} from "mathjs";

/**
 * Information about qubit.
 * Value is the qubit state presented as 0 or 1 (could be [a,b] where a,b are complex numbers).
 * Name is shows on screen.
 * text is used to represent the value of qubit e.g. |0> or 0.
 * editable implies if the value can be changed
 */
export class Qubit {
    value: number;
    name: string;
    text: string;
    editable: boolean;
    circuitOptions: CircuitOptions;

    constructor(
        value: number,
        name: string,
        editable: boolean,
        circuitOptions: CircuitOptions
    ) {
        this.value = value;
        this.name = name;
        this.circuitOptions = circuitOptions;
        this.text = this.getQubitText();
        this.editable = editable;
    }

    updateNotation(circuitOptions: CircuitOptions) {
        this.circuitOptions = circuitOptions;
        this.text = this.getQubitText();
    }

    /**
     * Gets the text for qubit in correct format.
     */
    private getQubitText() {
        const rightAngleChar = "\u27E9";
        if (this.circuitOptions.useBraket) {
            return `|${this.value}${rightAngleChar}`;
        }
        return this.value.toString();
    }

    /**
     * Transforms a bit into corresponding qubit state.
     */
    asVector() {
        if (this.value === 0) {
            return matrix([1, 0]);
        }
        return matrix([0, 1]);
    }

    toggled() {
        if (this.editable) {
            const newValue = this.value === 0 ? 1 : 0;
            return new Qubit(
                newValue,
                this.name,
                this.editable,
                this.circuitOptions
            );
        }
        return new Qubit(
            this.value,
            this.name,
            this.editable,
            this.circuitOptions
        );
    }
}
