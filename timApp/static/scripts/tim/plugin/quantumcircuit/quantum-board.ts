import type {GatePos} from "tim/plugin/quantumcircuit/quantum-circuit-board.component";

export class Gate {
    name: string;
    constructor(name: string) {
        this.name = name;
    }

    toString() {
        return this.name;
    }
}

export class Control {
    target: number;

    constructor(target: number) {
        this.target = target;
    }

    toString() {
        return "";
    }
}

export class Swap {
    target: number;

    constructor(target: number) {
        this.target = target;
    }
}

export type Cell = Gate | Control | Swap | undefined;

/**
 * Quatum circuit board containing information about gates in circuit.
 */
export class QuantumBoard {
    board: Cell[][];
    nQubits: number;
    nMoments: number;

    constructor(nQubits: number, nMoments: number) {
        this.board = [];
        this.nQubits = nQubits;
        this.nMoments = nMoments;

        for (let i = 0; i < nQubits; i++) {
            const row: (Gate | undefined)[] = [];
            for (let j = 0; j < nMoments; j++) {
                row.push(undefined);
            }
            this.board.push(row);
        }
    }

    private isInvalidPosition(target: number, time: number) {
        return (
            target < 0 ||
            target >= this.nQubits ||
            time < 0 ||
            time >= this.nMoments
        );
    }
    /**
     * Sets cell in board.
     * @param target qubit associated with cell
     * @param time time of cell
     * @param value new value for cell
     */
    set(target: number, time: number, value: Cell) {
        if (this.isInvalidPosition(target, time)) {
            console.log("invalid indices", target, time);
        }
        this.board[target][time] = value;
    }

    /**
     * Gets cell in board
     * @param target qubit associated with cell
     * @param time time of cell
     */
    get(target: number, time: number) {
        if (this.isInvalidPosition(target, time)) {
            console.log("Invalid indices", target, time);
        }
        return this.board[target][time];
    }

    get length() {
        return this.board.length;
    }

    /**
     * Removes gate from board and controls or swap pair associated with it.
     * @param gate gate to remove
     */
    remove(gate: GatePos) {
        this.set(gate.target, gate.time, undefined);

        for (let i = 0; i < this.board.length; i++) {
            if (i === gate.target) {
                continue;
            }
            const cell = this.get(i, gate.time);
            if (cell instanceof Control && cell.target === gate.target) {
                this.set(i, gate.time, undefined);
            }
            if (cell instanceof Swap && cell.target === gate.target) {
                this.set(i, gate.time, undefined);
            }
        }
    }

    /**
     * Iterator for board. Returns rows of board.
     */
    *[Symbol.iterator]() {
        for (const row of this.board) {
            yield row;
        }
    }
}
