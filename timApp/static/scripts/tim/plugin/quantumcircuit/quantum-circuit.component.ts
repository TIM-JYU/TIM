import * as t from "io-ts";
import type {
    AfterViewInit,
    ApplicationRef,
    DoBootstrap,
    OnInit,
} from "@angular/core";
import {Component, NgModule, ViewChild, ElementRef} from "@angular/core";
import {HttpClientModule} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {QuantumGateMenuComponent} from "tim/plugin/quantumcircuit/quantum-gate-menu.component";
import {QuantumToolboxComponent} from "tim/plugin/quantumcircuit/quantum-toolbox.component";
import type {
    GateDrop,
    GateMove,
    GatePos,
} from "tim/plugin/quantumcircuit/quantum-circuit-board.component";
import {
    InstanceofPipe,
    QuantumCircuitBoardComponent,
    RangePipe,
} from "tim/plugin/quantumcircuit/quantum-circuit-board.component";
import type {QuantumChartData} from "tim/plugin/quantumcircuit/quantum-stats.component";
import {
    MeasurementsPipe,
    QuantumStatsComponent,
} from "tim/plugin/quantumcircuit/quantum-stats.component";
import {NgChartsModule} from "ng2-charts";
import {QuantumCircuitSimulator} from "tim/plugin/quantumcircuit/quantum-simulation";
import {
    Control,
    Gate,
    QuantumBoard,
} from "tim/plugin/quantumcircuit/quantum-board";
import {TimUtilityModule} from "tim/ui/tim-utility.module";

export interface Qubit {
    value: number;
    name: string;
}

export interface QubitOutput {
    value: number;
}

/**
 * One input output pair measured from circuit
 */
export interface Measurement {
    input: string;
    output: string;
}

/**
 * Name, target, time and optionally a list of controls.
 */
const GateInfo = t.type({
    name: t.string,
    target: t.number,
    time: t.number,
    controls: nullable(t.array(t.number)),
});

// All settings that are defined in the plugin markup YAML
const QuantumCircuitMarkup = t.intersection([
    t.partial({
        initialCircuit: nullable(t.array(GateInfo)),
    }),
    GenericPluginMarkup,
    t.type({
        nQubits: withDefault(t.number, 1),
        nMoments: withDefault(t.number, 5),
    }),
]);

// All data that plugin receives from the server (Markup + any extra state data)
const QuantumCircuitFields = t.intersection([
    getTopLevelFields(QuantumCircuitMarkup),
    t.type({}),
]);

export interface Colors {
    dark: string;
    medium: string;
    light: string;
}

/**
 * Styling related options for circuit.
 */
export interface CircuitStyleOptions {
    colors: Colors;
    // relative size of circuit cells (side length) and other elements
    baseSize: number;
    // size of gates (side length)
    gateSize: number;
    // whether to use braket notation for input qubits or just (0,1)
    useBraket: boolean;
    // timeAxisHeight: height of first row showing time steps 0,1,2...
    timeAxisHeight: number;
    // border radius value for gates (rounded corners)
    gateBorderRadius: number;
}

@Component({
    selector: "tim-quantum-circuit",
    template: `
        <div #qcContainer class="circuit-container">
            <div class="top-menu">
                <tim-quantum-gate-menu [circuitStyleOptions]="circuitStyleOptions"></tim-quantum-gate-menu>
                <tim-quantum-toolbox></tim-quantum-toolbox>
            </div>

            <div class="circuit">
                <tim-quantum-circuit-board
                        [board]="board" 
                        [selectedGate]="selectedGate"
                        [qubits]="qubits"
                        [qubitOutputs]="qubitOutputs"
                        [circuitStyleOptions]="circuitStyleOptions"
                        (qubitChange)="handleQubitChange($event)"
                        (gateDrop)="handleGateDrop($event)"
                        (gateMove)="handleGateMove($event)"
                        (gateRemove)="handleGateRemove($event)"
                        (gateSelect)="handleGateSelect($event)"
                ></tim-quantum-circuit-board>
            </div>

            <div class="stats">
                <tim-quantum-stats [measurements]="measurements"
                                   [quantumChartData]="quantumChartData"
                                   [nQubits]="nQubits"
                                   (clear)="handleClearMeasurements()"
                                   (measure)="handleMeasure()">
                </tim-quantum-stats>
            </div>
        </div>
    `,
    styleUrls: ["./quantum-circuit.component.scss"],
})
export class QuantumCircuitComponent
    extends AngularPluginBase<
        t.TypeOf<typeof QuantumCircuitMarkup>,
        t.TypeOf<typeof QuantumCircuitFields>,
        typeof QuantumCircuitFields
    >
    implements OnInit, AfterViewInit
{
    timeoutId: number = -1;

    @ViewChild("qcContainer")
    qcContainer!: ElementRef<HTMLElement>;

    qubits: Qubit[] = [];
    qubitOutputs: QubitOutput[] = [];

    simulator!: QuantumCircuitSimulator;

    quantumChartData!: QuantumChartData;

    circuitStyleOptions: CircuitStyleOptions = {
        baseSize: 60,
        gateSize: 40,
        colors: {
            dark: "black",
            medium: "grey",
            light: "white",
        },
        useBraket: false,
        timeAxisHeight: 30,
        gateBorderRadius: 2,
    };

    board!: QuantumBoard;

    selectedGate?: GatePos;

    measurements: Measurement[] = [];

    get nQubits() {
        return this.markup.nQubits;
    }

    get nMoments() {
        return this.markup.nMoments;
    }

    /**
     * Runs simulator and updates statistics.
     * @param delay whether to run simulator instantly
     */
    runSimulation(delay: boolean = true) {
        if (this.timeoutId !== -1) {
            window.clearTimeout(this.timeoutId);
            this.timeoutId = -1;
        }
        if (delay) {
            this.timeoutId = window.setTimeout(() => {
                const startTime = new Date();
                console.log("started simulating", startTime);
                this.simulator.run();
                const endTime = new Date();

                const timeDiff = endTime.getTime() - startTime.getTime();
                console.log(`simulation ended in: ${timeDiff}ms`);

                this.quantumChartData = this.simulator.getProbabilities();
                this.setQubitOutputs();
                this.timeoutId = -1;
            }, 0);
        } else {
            this.simulator.run();
            this.quantumChartData = this.simulator.getProbabilities();
            this.setQubitOutputs();
        }
    }

    /**
     * Toggle qubits initial state between 0 and 1
     */
    handleQubitChange(qubitId: number) {
        if (qubitId < 0 || qubitId >= this.qubits.length) {
            console.log("non-existing qubitId", qubitId);
            return;
        }
        if (this.qubits[qubitId].value === 0) {
            this.qubits[qubitId].value = 1;
        } else {
            this.qubits[qubitId].value = 0;
        }

        this.runSimulation();
    }

    findPossibleTargetForControl(time: number, target: number) {
        for (let i = 0; i < this.board.length; i++) {
            if (i === target) {
                continue;
            }
            const cell = this.board.get(i, time);
            if (cell instanceof Gate) {
                return i;
            }
        }
        return undefined;
    }

    /**
     * Replaces cell of the board with gate.
     * @param gate gate to put in cell
     */
    handleGateDrop(gate: GateDrop) {
        const {time, target} = gate;

        if (gate.name === "control") {
            // Gate hasn't being selected as target so select one automatically
            if (!this.selectedGate) {
                const controlTarget = this.findPossibleTargetForControl(
                    time,
                    target
                );
                if (controlTarget !== undefined) {
                    this.board.set(target, time, new Control(controlTarget));
                }
            } else {
                // use selected gate as target
                this.board.set(
                    target,
                    time,
                    new Control(this.selectedGate.target)
                );
            }
        } else {
            this.board.set(target, time, new Gate(gate.name));
        }

        this.selectedGate = undefined;

        this.runSimulation();
    }

    /**
     * Get controls for gate
     * @param gate position of gate
     */
    getControlling(gate: GatePos): GatePos[] {
        const controls = [];
        for (let i = 0; i < this.board.length; i++) {
            if (i !== gate.target) {
                const c = this.board.get(i, gate.time);
                if (c instanceof Control && c.target === gate.target) {
                    controls.push({target: i, time: gate.time});
                }
            }
        }
        return controls;
    }

    /**
     * Move gate or control from one cell to another.
     * @param gateMove object describing gate movement
     */
    handleGateMove(gateMove: GateMove) {
        const {
            from: {target: target1, time: time1},
            to: {target: target2, time: time2},
        } = gateMove;
        const fromCell = this.board.get(target1, time1);
        if (fromCell instanceof Gate) {
            const name = fromCell.name;
            this.board.set(target2, time2, new Gate(name));
            this.board.set(target1, time1, undefined);
            const controlling = this.getControlling(gateMove.from);
            if (controlling.length > 0) {
                // time didn't change so wires can be reconnected to their target
                if (time1 === time2) {
                    for (const control of controlling) {
                        this.board.set(
                            control.target,
                            control.time,
                            new Control(target2)
                        );
                    }
                } else {
                    // time changed so remove all connected wires
                    for (const control of controlling) {
                        this.board.set(control.target, control.time, undefined);
                    }
                }
            }
        } else if (fromCell instanceof Control) {
            if (time1 === time2) {
                this.board.set(target2, time2, new Control(fromCell.target));
                this.board.set(target1, time1, undefined);
            }
        }
        this.selectedGate = undefined;
        this.runSimulation();
    }

    /**
     * Removes gate from board and controls associated to it.
     * @param gate gate to remove
     */
    handleGateRemove(gate: GatePos) {
        this.board.set(gate.target, gate.time, undefined);

        for (let i = 0; i < this.board.length; i++) {
            if (i === gate.target) {
                continue;
            }
            const cell = this.board.get(i, gate.time);
            if (cell instanceof Control && cell.target === gate.target) {
                this.board.set(i, gate.time, undefined);
            }
        }

        this.selectedGate = undefined;

        this.runSimulation();
    }

    /**
     * Marks gate as selected.
     * @param gate position of gate on board
     */
    handleGateSelect(gate: GatePos) {
        const cell = this.board.get(gate.target, gate.time);
        if (cell instanceof Gate) {
            // same was selected so unselect it
            if (
                this.selectedGate &&
                this.selectedGate.time === gate.time &&
                this.selectedGate.target === gate.target
            ) {
                this.selectedGate = undefined;
            } else {
                this.selectedGate = gate;
            }
        }
    }

    handleMeasure() {
        const measurement = this.simulator.sample();
        if (!measurement) {
            return;
        }
        this.measurements = [measurement, ...this.measurements];
    }

    /**
     * Deletes all measurements.
     */
    handleClearMeasurements() {
        this.measurements = [];
    }

    /**
     * Add gates from initialCircuit to board.
     */
    addInitialGates() {
        if (!this.markup.initialCircuit) {
            return;
        }
        for (const gateData of this.markup.initialCircuit) {
            const gate = new Gate(gateData.name);
            this.board.set(gateData.target, gateData.time, gate);
            if (!gateData.controls) {
                continue;
            }
            for (const controlData of gateData.controls) {
                const control = new Control(gateData.target);
                this.board.set(controlData, gateData.time, control);
            }
        }
    }

    /**
     * Initializes board, qubits and outputs.
     */
    initializeBoard() {
        this.qubits = [];
        for (let i = 0; i < this.nQubits; i++) {
            this.qubits.push({
                name: `q[${i}]`,
                value: 0,
            });
        }
        this.board = new QuantumBoard(this.nQubits, this.nMoments);

        this.addInitialGates();

        this.qubitOutputs = [];
        for (let i = 0; i < this.nQubits; i++) {
            this.qubitOutputs.push({
                value: 0,
            });
        }
    }

    /**
     * Set qubit output value based on random sample from output's distribution.
     */
    setQubitOutputs() {
        this.qubitOutputs = [];
        const vals = this.simulator.sample();
        if (!vals) {
            return;
        }
        this.qubitOutputs = Array.from(vals.output).map((bit) => ({
            value: parseInt(bit, 10),
        }));
    }

    initializeSimulator() {
        this.simulator = new QuantumCircuitSimulator(this.board, this.qubits);

        this.runSimulation(false);
    }

    /**
     * Compute dimensions for elements.
     */
    ngAfterViewInit() {
        // Compute sizes for board cells based on available space and number of cells
        const baseSize =
            this.qcContainer.nativeElement.offsetWidth / (this.nMoments + 4);
        const gateSize = (2 / 3) * baseSize;

        this.circuitStyleOptions = {
            baseSize: baseSize,
            gateSize: gateSize,
            colors: {
                dark: "black",
                medium: "grey",
                light: "white",
            },
            useBraket: false,
            timeAxisHeight: 30,
            gateBorderRadius: 2,
        };
    }

    ngOnInit(): void {
        super.ngOnInit();

        this.initializeBoard();

        this.initializeSimulator();
    }

    getAttributeType() {
        return QuantumCircuitFields;
    }

    getDefaultMarkup() {
        return {};
    }
}

@NgModule({
    declarations: [
        QuantumCircuitComponent,
        QuantumGateMenuComponent,
        QuantumToolboxComponent,
        QuantumCircuitBoardComponent,
        QuantumStatsComponent,
        MeasurementsPipe,
        InstanceofPipe,
        RangePipe,
    ],
    exports: [QuantumCircuitComponent],
    imports: [CommonModule, HttpClientModule, NgChartsModule, TimUtilityModule],
})
export class QuantumCircuitModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin(
    "tim-quantum-circuit",
    QuantumCircuitModule,
    QuantumCircuitComponent
);
