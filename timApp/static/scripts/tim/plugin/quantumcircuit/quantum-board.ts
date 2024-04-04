import type {GatePos} from "tim/plugin/quantumcircuit/quantum-circuit-board.component";

export class Gate {
    name: string;
    editable: boolean;
    constructor(name: string, editable: boolean) {
        this.name = name;
        this.editable = editable;
    }

    toString() {
        return this.name;
    }
}

export class MultiQubitGate {
    name: string;
    size: number;
    editable: boolean;
    constructor(name: string, size: number, editable: boolean) {
        this.name = name;
        this.size = size;
        this.editable = editable;
    }

    toString() {
        return this.name;
    }
}

export class MultiQubitGateCell {
    target: number;
    editable: boolean;

    constructor(target: number, editable: boolean) {
        this.target = target;
        this.editable = editable;
    }

    toString() {
        return "";
    }
}

export class Control {
    target: number;
    editable: boolean;
    anti: boolean;

    constructor(target: number, editable: boolean, anti: boolean) {
        this.target = target;
        this.editable = editable;
        this.anti = anti;
    }

    toString() {
        return "";
    }
}

export class Swap {
    target: number;
    editable: boolean;

    constructor(target: number, editable: boolean) {
        this.target = target;
        this.editable = editable;
    }
}

export type Cell =
    | Gate
    | Control
    | Swap
    | MultiQubitGate
    | MultiQubitGateCell
    | undefined;

/**
 * Quantum circuit board containing information about gates in circuit.
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

    clone() {
        const copy = new QuantumBoard(this.nQubits, this.nMoments);
        const board: Cell[][] = [];
        for (const row of this.board) {
            const bRow: Cell[] = [];
            for (const cell of row) {
                bRow.push(cell);
            }
            board.push(bRow);
        }
        copy.board = board;
        return copy;
    }

    /**
     * Adds new gate on board. Also removes gates that are connected to previous gate at that location.
     * @param pos position to put gate in
     * @param gate gate to put add
     */
    addGate(pos: GatePos, gate: Gate) {
        if (this.get(pos.target, pos.time)?.editable === false) {
            return;
        }
        this.set(pos.target, pos.time, gate);
    }

    /**
     * Adds new multi-qubit gate on board. Removes gates that are connected to previous gate at that
     * location or ones under it that will be also occupied by this gate.
     * @param pos position of first qubit this gate should occupy
     * @param gate gate to add
     */
    addMultiQubitGate(pos: GatePos, gate: MultiQubitGate) {
        // would go out of bounds
        if (pos.target + gate.size > this.board.length) {
            return;
        }

        // only add if all cells it occupies are editable
        if (this.get(pos.target, pos.time)?.editable === false) {
            return;
        }
        for (let i = 0; i < gate.size - 1; i++) {
            if (this.get(pos.target + i + 1, pos.time)?.editable === false) {
                return;
            }
        }

        this.set(pos.target, pos.time, gate);
        for (let i = 0; i < gate.size - 1; i++) {
            this.set(
                pos.target + i + 1,
                pos.time,
                new MultiQubitGateCell(pos.target, gate.editable)
            );
        }
    }

    /**
     * Determine if cell contains a gate that can be controlled.
     * @param cell cell on board to check
     */
    isControllable(cell: Cell) {
        return (
            cell instanceof Gate ||
            cell instanceof MultiQubitGate ||
            cell instanceof Swap
        );
    }

    /**
     * Find gate to control.
     * @param pos position of control
     */
    private findControllable(pos: GatePos) {
        for (let i = pos.target + 1; i < this.board.length; i++) {
            const cell = this.board[i][pos.time];
            if (this.isControllable(cell)) {
                // when controlling swap the target should be the upper one to make drawing easier
                if (cell instanceof Swap) {
                    return Math.min(cell.target, i);
                }
                return i;
            }
        }
        for (let i = pos.target - 1; i >= 0; i--) {
            const cell = this.board[i][pos.time];
            if (this.isControllable(cell)) {
                // when controlling swap the target should be the upper one to make drawing easier
                if (cell instanceof Swap) {
                    return Math.min(cell.target, i);
                }
                return i;
            }
        }

        return undefined;
    }

    /**
     * Get controls for a gate.
     * @param pos position of the gate
     * @param anti return anti controls if true else normal controls
     */
    getControls(pos: GatePos, anti: boolean) {
        const controls = [];
        for (let i = 0; i < this.board.length; i++) {
            const cell = this.get(i, pos.time);
            if (
                cell instanceof Control &&
                cell.anti === anti &&
                cell.target === pos.target &&
                i !== pos.target
            ) {
                controls.push(i);
            }
        }
        return controls;
    }

    /**
     * Moves gate from position to another.
     * If time doesn't change then try to reconnect controls.
     * @param oldPos position to move from
     * @param newPos position to move to
     * @param gate gate to move
     */
    private moveGate(oldPos: GatePos, newPos: GatePos, gate: Gate) {
        const controls = this.getControls(oldPos, false);
        const antiControls = this.getControls(oldPos, true);
        this.remove(oldPos.target, oldPos.time);
        this.set(newPos.target, newPos.time, gate);
        if (oldPos.time !== newPos.time) {
            return;
        }
        // reconnect controls
        for (const c of controls) {
            if (c !== newPos.target) {
                this.set(
                    c,
                    newPos.time,
                    new Control(newPos.target, true, false)
                );
            }
        }

        // reconnect anti controls
        for (const c of antiControls) {
            if (c !== newPos.target) {
                this.set(
                    c,
                    newPos.time,
                    new Control(newPos.target, true, true)
                );
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
                    new Control(targetPos.target, true, control.anti)
                );
            } else {
                if (newPos.target !== control.target) {
                    this.remove(oldPos.target, oldPos.time);

                    this.set(
                        newPos.target,
                        newPos.time,
                        new Control(control.target, true, control.anti)
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
                    new Control(targetPos.target, true, control.anti)
                );
            } else {
                const target = this.findControllable(newPos);
                if (target !== undefined) {
                    this.remove(oldPos.target, oldPos.time);
                    this.set(
                        newPos.target,
                        newPos.time,
                        new Control(target, true, control.anti)
                    );
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
            const controls = this.getControls(oldPos, false);
            const antiControls = this.getControls(oldPos, true);
            if (newPos.target === swap.target) {
                return;
            }
            this.remove(oldPos.target, oldPos.time);
            this.remove(newPos.target, newPos.time);
            this.remove(swap.target, newPos.time);
            this.board[newPos.target][newPos.time] = new Swap(
                swap.target,
                swap.editable
            );
            this.board[swap.target][newPos.time] = new Swap(
                newPos.target,
                swap.editable
            );
            const cTarget = Math.min(newPos.target, swap.target);
            for (const ci of controls) {
                if (ci !== newPos.target && ci !== swap.target) {
                    this.set(
                        ci,
                        newPos.time,
                        new Control(cTarget, true, false)
                    );
                }
            }
            for (const ci of antiControls) {
                if (ci !== newPos.target && ci !== swap.target) {
                    this.set(ci, newPos.time, new Control(cTarget, true, true));
                }
            }
        } else {
            const pair = this.findSwapPair(newPos);
            if (pair !== undefined) {
                this.remove(oldPos.target, oldPos.time);
                this.remove(newPos.target, newPos.time);
                this.remove(pair, newPos.time);
                this.board[newPos.target][newPos.time] = new Swap(pair, true);
                this.board[pair][newPos.time] = new Swap(newPos.target, true);
            }
        }
    }

    /**
     * Moves multi-qubit gate if there's room for its full size.
     * @param oldPos position to move the gate from
     * @param newPos position to move the gate to
     * @param gate the multi-qubit gate
     */
    private moveMultiQubitGate(
        oldPos: GatePos,
        newPos: GatePos,
        gate: MultiQubitGate
    ) {
        // would go out of bounds
        if (newPos.target + gate.size > this.board.length) {
            return;
        }
        this.remove(oldPos.target, oldPos.time);
        this.set(newPos.target, newPos.time, gate);
        for (let i = newPos.target + 1; i < newPos.target + gate.size; i++) {
            this.set(
                i,
                newPos.time,
                new MultiQubitGateCell(newPos.target, true)
            );
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

        if (cell?.editable === false) {
            return;
        }
        const newPosCell = this.get(newPos.target, newPos.time);
        if (newPosCell?.editable === false) {
            return;
        }
        if (target !== null) {
            const targetCell = this.get(target.target, target.time);
            if (targetCell?.editable === false) {
                return;
            }
        }

        if (cell instanceof Gate) {
            this.moveGate(oldPos, newPos, cell);
        } else if (cell instanceof Control) {
            this.moveControl(oldPos, newPos, cell, target);
        } else if (cell instanceof Swap) {
            this.moveSwap(oldPos, newPos, cell);
        } else if (cell instanceof MultiQubitGate) {
            this.moveMultiQubitGate(oldPos, newPos, cell);
        } else {
            console.log("unhandled move case", cell);
        }
    }

    /**
     * Adds control gate. Target is gate at gatePos if that is a gate.
     * If gatePos is not given then target is chosen automatically from
     * available gates at same time slot.
     * Don't put control if the cell isn't empty.
     * @param pos position to put a control gate
     * @param gatePos position to set as target for the control
     * @param anti is the control an anti control
     */
    addControl(pos: GatePos, gatePos: GatePos | null, anti: boolean) {
        if (this.get(pos.target, pos.time)?.editable === false) {
            return;
        }
        if (this.get(pos.target, pos.time) !== undefined) {
            return;
        }
        if (gatePos) {
            if (gatePos.time !== pos.time || gatePos.target === pos.target) {
                return;
            }
            const cell = this.get(gatePos.target, gatePos.time);
            if (this.isControllable(cell)) {
                this.set(
                    pos.target,
                    pos.time,
                    new Control(gatePos.target, true, anti)
                );
            }
        } else if (!gatePos) {
            const target = this.findControllable(pos);
            if (target !== undefined) {
                this.set(pos.target, pos.time, new Control(target, true, anti));
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
     * @param editable whether the swap gate is editable
     * @param controls controls for swap gate
     * @param antiControls anti controls for swap gate
     */
    addSwap(
        pos: GatePos,
        pos2: GatePos | null = null,
        editable: boolean,
        controls?: number[],
        antiControls?: number[]
    ) {
        if (this.get(pos.target, pos.time)?.editable === false) {
            return;
        }
        if (
            pos2 !== null &&
            this.get(pos2.target, pos2.time)?.editable === false
        ) {
            return;
        }
        if (pos.target === pos2?.target) {
            return;
        }
        if (pos2 && pos.time === pos2.time) {
            this.remove(pos.target, pos.time);
            this.remove(pos2.target, pos2.time);
            this.board[pos.target][pos.time] = new Swap(pos2.target, editable);
            this.board[pos2.target][pos2.time] = new Swap(pos.target, editable);
        } else {
            const pair = this.findSwapPair(pos);
            if (pair !== undefined) {
                this.remove(pos.target, pos.time);
                this.remove(pair, pos.time);
                this.board[pos.target][pos.time] = new Swap(pair, editable);
                this.board[pair][pos.time] = new Swap(pos.target, editable);
            }
        }

        if (controls) {
            for (const ci of controls) {
                let cTarget = pos.target;
                if (pos2?.target !== undefined && pos2.target < pos.target) {
                    cTarget = pos2.target;
                }
                this.board[ci][pos.time] = new Control(
                    cTarget,
                    editable,
                    false
                );
            }
        }

        if (antiControls) {
            for (const ci of antiControls) {
                let cTarget = pos.target;
                if (pos2?.target !== undefined && pos2.target < pos.target) {
                    cTarget = pos2.target;
                }
                this.board[ci][pos.time] = new Control(cTarget, editable, true);
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
            return;
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
            return undefined;
        }
        return this.board[target][time];
    }

    get length() {
        return this.board.length;
    }

    /**
     * Removes gate from board and cells connected to it.
     * @param target target of gate to remove
     * @param time time of gate to remove
     */
    remove(target: number, time: number) {
        let multiQubitTarget;
        let swapTarget;

        const targetCell = this.board[target][time];
        if (targetCell instanceof MultiQubitGateCell) {
            multiQubitTarget = targetCell.target;
        } else if (targetCell instanceof Swap) {
            swapTarget = Math.min(targetCell.target, target);
        }

        this.board[target][time] = undefined;

        // remove connected gates in same column
        for (let i = 0; i < this.board.length; i++) {
            if (i === target) {
                continue;
            }
            const cell = this.get(i, time);
            if (
                cell instanceof Control &&
                (cell.target === target ||
                    cell.target === multiQubitTarget ||
                    cell.target === swapTarget)
            ) {
                this.board[i][time] = undefined;
            }
            if (cell instanceof Swap && cell.target === target) {
                this.board[i][time] = undefined;
            }
            if (cell instanceof MultiQubitGateCell && cell.target === target) {
                this.board[i][time] = undefined;
            }
            if (multiQubitTarget !== undefined && i === multiQubitTarget) {
                this.board[i][time] = undefined;
            }
            if (
                multiQubitTarget !== undefined &&
                cell instanceof MultiQubitGateCell &&
                cell.target === multiQubitTarget
            ) {
                this.board[i][time] = undefined;
            }
        }
    }
}
