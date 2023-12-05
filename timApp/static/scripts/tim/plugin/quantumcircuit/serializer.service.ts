import {Injectable} from "@angular/core";
import type {QuantumBoard} from "tim/plugin/quantumcircuit/quantum-board";
import {
    Gate,
    MultiQubitGate,
    Swap,
} from "tim/plugin/quantumcircuit/quantum-board";
import type {
    ICircuit,
    ICustomGateInfo,
} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import {format} from "mathjs";
import type {INumericCustomGateInfo} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import type {GateService} from "tim/plugin/quantumcircuit/gate.service";

@Injectable()
export class SerializerService {
    serializeUserCircuit(board: QuantumBoard): ICircuit {
        const userCircuit: ICircuit = [];

        for (let targetI = 0; targetI < board.length; targetI++) {
            for (let timeI = 0; timeI < board.board[targetI].length; timeI++) {
                const cell = board.board[targetI][timeI];
                if (cell instanceof Gate || cell instanceof MultiQubitGate) {
                    const controls = board.getControls({
                        target: targetI,
                        time: timeI,
                    });

                    userCircuit.push({
                        name: cell.name,
                        editable: cell.editable,
                        target: targetI,
                        time: timeI,
                        controls: controls,
                    });
                } else if (cell instanceof Swap && targetI < cell.target) {
                    const controls = board.getControls({
                        target: targetI,
                        time: timeI,
                    });
                    userCircuit.push({
                        swap1: targetI,
                        swap2: cell.target,
                        time: timeI,
                        editable: cell.editable,
                        controls: controls,
                    });
                }
            }
        }
        return userCircuit;
    }

    /**
     * Turns matrix into json format that can be loaded in Python.
     * @param name name of matrix
     * @param gateService gate service to use to get matrices
     */
    customMatrixDefToPythonJsonStr(name: string, gateService: GateService) {
        const m = gateService.getMatrix(name);
        if (!m) {
            return undefined;
        }
        const [n] = m.size();
        const res: string[][] = [];
        for (let i = 0; i < n; i++) {
            const resRow: string[] = [];
            for (let j = 0; j < n; j++) {
                const d = m.get([i, j]);
                // format number or complex number,
                // remove whitespace and replace i with j as complex number marker.
                resRow.push(format(d).replace(/\s/g, "").replace(/i/g, "j"));
            }
            res.push(resRow);
        }
        return JSON.stringify(res);
    }

    serializeCustomGates(
        customGates: ICustomGateInfo[],
        gateService: GateService
    ) {
        const numericCustomGates: INumericCustomGateInfo[] = [];

        for (const g of customGates) {
            const m = this.customMatrixDefToPythonJsonStr(g.name, gateService);
            if (m) {
                numericCustomGates.push({
                    name: g.name,
                    matrix: m,
                });
            } else {
                console.error("failed to serialize gate", g);
            }
        }
        return numericCustomGates;
    }
}
