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

export type Cell = Gate | Control | undefined;

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
     * Iterator for board. Returns rows of board.
     */
    *[Symbol.iterator]() {
        for (const row of this.board) {
            yield row;
        }
    }
}
