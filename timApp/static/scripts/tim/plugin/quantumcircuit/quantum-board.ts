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
     * Adds new gate on board. Removes gates that are connected to previous gate at that location.
     * @param pos position to put gate in
     * @param gate gate to put add
     */
    addGate(pos: GatePos, gate: Gate) {
        this.set(pos.target, pos.time, gate);
    }

    /**
     * Find gate to control.
     * @param pos position of control
     */
    private findControllable(pos: GatePos) {
        for (let i = pos.target + 1; i < this.board.length; i++) {
            const cell = this.board[i][pos.time];
            if (cell instanceof Gate) {
                return i;
            }
        }
        for (let i = pos.target - 1; i >= 0; i--) {
            const cell = this.board[i][pos.time];
            if (cell instanceof Gate) {
                return i;
            }
        }

        return undefined;
    }

    private getControls(pos: GatePos) {
        const controls = [];
        for (let i = 0; i < this.board.length; i++) {
            const cell = this.get(i, pos.time);
            if (cell instanceof Control && cell.target === pos.target) {
                controls.push(i);
            }
        }
        return controls;
    }

    /**
     * Moves gate from position to another.
     * If times doesn't change then try to reconnect controls.
     * @param oldPos position to move from
     * @param newPos position to move to
     * @param gate gate to move
     */
    private moveGate(oldPos: GatePos, newPos: GatePos, gate: Gate) {
        const controls = this.getControls(oldPos);
        this.remove(oldPos.target, oldPos.time);
        this.set(newPos.target, newPos.time, gate);
        if (oldPos.time !== newPos.time) {
            return;
        }
        // reconnect controls
        for (const c of controls) {
            if (c !== newPos.target) {
                this.set(c, newPos.time, new Control(newPos.target));
            }
        }
    }

    /**
     * Moves control gate. Don't move if new position is the one controlled.
     * If time is same then use same target as new target or target if given and given target is not new position.
     * If time changes then use target or choose automatically.
     * @param oldPos previous position of control
     * @param newPos new position of control
     * @param control control to move
     * @param targetPos possible target to control
     */
    private moveControl(
        oldPos: GatePos,
        newPos: GatePos,
        control: Control,
        targetPos: GatePos | null
    ) {
        if (oldPos.time === newPos.time) {
            if (
                targetPos &&
                targetPos.time === newPos.time &&
                targetPos.target !== newPos.target &&
                this.get(targetPos.target, targetPos.time) instanceof Gate
            ) {
                this.remove(oldPos.target, oldPos.time);
                this.set(
                    newPos.target,
                    newPos.time,
                    new Control(targetPos.target)
                );
            } else {
                if (newPos.target !== control.target) {
                    this.remove(oldPos.target, oldPos.time);

                    this.set(
                        newPos.target,
                        newPos.time,
                        new Control(control.target)
                    );
                }
            }
        } else {
            if (
                targetPos &&
                targetPos.time === newPos.time &&
                targetPos.target !== newPos.target &&
                this.get(targetPos.target, targetPos.time) instanceof Gate
            ) {
                this.remove(oldPos.target, oldPos.time);
                this.set(
                    newPos.target,
                    newPos.time,
                    new Control(targetPos.target)
                );
            } else {
                const target = this.findControllable(newPos);
                if (target !== undefined) {
                    this.remove(oldPos.target, oldPos.time);
                    this.set(newPos.target, newPos.time, new Control(target));
                }
            }
        }
    }

    /**
     * Moves swap from one position to another.
     * Reconnects swap pair if possible or finds new one if time changes.
     * If new position is swap pair then do nothing.
     * @param oldPos position to move the swap from
     * @param newPos position to move the swap to
     * @param swap gate to move
     */
    private moveSwap(oldPos: GatePos, newPos: GatePos, swap: Swap) {
        if (oldPos.time === newPos.time) {
            if (newPos.target === swap.target) {
                return;
            }
            this.remove(oldPos.target, oldPos.time);
            this.remove(newPos.target, newPos.time);
            this.remove(swap.target, newPos.time);
            this.board[newPos.target][newPos.time] = new Swap(swap.target);
            this.board[swap.target][newPos.time] = new Swap(newPos.target);
        } else {
            const pair = this.findSwapPair(newPos);
            if (pair !== undefined) {
                this.remove(oldPos.target, oldPos.time);
                this.remove(newPos.target, newPos.time);
                this.remove(pair, newPos.time);
                this.board[newPos.target][newPos.time] = new Swap(pair);
                this.board[pair][newPos.time] = new Swap(newPos.target);
            }
        }
    }

    /**
     * Moves cell from position to another.
     * @param oldPos
     * @param newPos
     * @param target chosen gate to possibly use as target for control gate
     */
    moveCell(oldPos: GatePos, newPos: GatePos, target: GatePos | null) {
        if (oldPos.target === newPos.target && oldPos.time === newPos.time) {
            return;
        }
        const cell = this.get(oldPos.target, oldPos.time);
        if (cell instanceof Gate) {
            this.moveGate(oldPos, newPos, cell);
        } else if (cell instanceof Control) {
            this.moveControl(oldPos, newPos, cell, target);
        } else if (cell instanceof Swap) {
            this.moveSwap(oldPos, newPos, cell);
        }
    }

    /**
     * Adds control gate. Target is gate at gatePos if that is a gate.
     * If gatePos is not given then target is chosen automatically from
     * available gates at same time slot.
     * @param pos position to put a control gate
     * @param gatePos position to set as target for the control
     */
    addControl(pos: GatePos, gatePos: GatePos | null) {
        if (gatePos) {
            if (gatePos.time !== pos.time || gatePos.target === pos.target) {
                return;
            }
            const cell = this.get(gatePos.target, gatePos.time);
            if (cell instanceof Gate) {
                this.set(pos.target, pos.time, new Control(gatePos.target));
            }
        } else if (!gatePos) {
            const target = this.findControllable(pos);
            if (target !== undefined) {
                this.set(pos.target, pos.time, new Control(target));
            }
        }
    }

    /**
     * Find empty cell in the time slot as given position that
     * isn't the position given.
     * @param pos position to look swap pair for
     */
    private findSwapPair(pos: GatePos) {
        // Prefer positions after current position
        for (let i = pos.target + 1; i < this.board.length; i++) {
            if (!this.board[i][pos.time]) {
                return i;
            }
        }
        // then before in reverse order
        for (let i = pos.target - 1; i >= 0; i--) {
            if (!this.board[i][pos.time]) {
                return i;
            }
        }
        return undefined;
    }

    /**
     * Adds swap gate between two positions.
     * Automatically selects pair for it from available empty cells
     * if pos2 is not given.
     * Removes connections to gate at position.
     * @param pos position to add swap to
     * @param pos2 position of the pair of the swap gate
     */
    addSwap(pos: GatePos, pos2: GatePos | null = null) {
        if (pos2 && pos.target === pos2.time && pos.target !== pos2.target) {
            this.remove(pos.target, pos.time);
            this.remove(pos2.target, pos2.time);
            this.board[pos.target][pos.time] = new Swap(pos2.target);
            this.board[pos2.target][pos2.time] = new Swap(pos.target);
        } else {
            const pair = this.findSwapPair(pos);
            if (pair !== undefined) {
                this.remove(pos.target, pos.time);
                this.remove(pair, pos.time);
                this.board[pos.target][pos.time] = new Swap(pair);
                this.board[pair][pos.time] = new Swap(pos.target);
            }
        }
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
        this.remove(target, time);

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
     * @param target target of gate to remove
     * @param time time of gate to remove
     */
    remove(target: number, time: number) {
        this.board[target][time] = undefined;

        // remove connected gates in same column
        for (let i = 0; i < this.board.length; i++) {
            if (i === target) {
                continue;
            }
            const cell = this.get(i, time);
            if (cell instanceof Control && cell.target === target) {
                this.board[i][time] = undefined;
            }
            if (cell instanceof Swap && cell.target === target) {
                this.board[i][time] = undefined;
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
