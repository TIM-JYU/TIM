import type {IGateCountInfo} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import type {
    GateService,
    ServiceGate,
} from "tim/plugin/quantumcircuit/gate.service";
import type {GateDrop} from "tim/plugin/quantumcircuit/quantum-circuit-board.component";

/**
 * Exercise type where specified amount of each gate can be used
 * Keep track of how many gates are in gate menu and circuit
 */
export class GateCounts {
    menu: Map<string, number> = new Map<string, number>();
    circuit: Map<string, number> = new Map<string, number>();

    /**
     * Sets gate counts
     * @param gateCountMarkUp names and counts of available gates
     * @param boardGateCounts gates that are in board
     */
    setGateCounts(
        gateCountMarkUp: IGateCountInfo[] | null | undefined,
        boardGateCounts: Map<string, number>
    ) {
        this.menu = new Map<string, number>();
        this.circuit = boardGateCounts;

        if (gateCountMarkUp) {
            for (const gate of gateCountMarkUp) {
                this.menu.set(gate.name, gate.count);
            }

            for (const [name, count] of this.menu) {
                const val = boardGateCounts.get(name);
                if (val) {
                    this.menu.set(name, count - val);
                    if (count - val < 0) {
                        console.log(
                            "board gate counts don't match the gate count condition",
                            name,
                            count
                        );
                    }
                }
            }
        }
    }

    /**
     * Gates that are available in menu after subtracting ones in circuit
     * @param gateService
     */
    getMenuGates(gateService: GateService): ServiceGate[] {
        const menuGates = [];
        for (const gate of gateService.getMenuGatesArray()) {
            const count = this.menu.get(gate.name);
            if (count !== undefined && count > 0) {
                menuGates.push(gate);
            }
        }
        return menuGates;
    }

    /**
     * Check if moving gate from menu to circuit board would brake gateCounts condition
     * @param gate
     */
    checkAddToBoard(gate: GateDrop) {
        const menuCount = this.menu.get(gate.name);
        return menuCount !== undefined && menuCount > 0;
    }
}
