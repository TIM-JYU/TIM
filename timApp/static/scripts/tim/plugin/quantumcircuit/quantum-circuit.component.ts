import * as t from "io-ts";
import type {
    AfterViewInit,
    ApplicationRef,
    DoBootstrap,
    OnInit,
} from "@angular/core";
import {Component, NgModule, ViewChild, ElementRef} from "@angular/core";
import {HttpClient, HttpClientModule} from "@angular/common/http";
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
import {QuantumStatsComponent} from "tim/plugin/quantumcircuit/quantum-stats.component";
import {NgChartsModule} from "ng2-charts";
import {QuantumCircuitSimulator} from "tim/plugin/quantumcircuit/quantum-simulation";
import {
    Control,
    Gate,
    MultiQubitGate,
    QuantumBoard,
} from "tim/plugin/quantumcircuit/quantum-board";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {timeout} from "tim/util/utils";
import {FormsModule} from "@angular/forms";
import {GateService} from "tim/plugin/quantumcircuit/gate.service";
import {DomSanitizer} from "@angular/platform-browser";

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
    value: number;
    input: string;
    output: string;
}

const ControlGateInfo = t.type({
    name: t.string,
    time: t.number,
    target: t.number,
    controls: t.array(t.number),
});

/**
 * Assume that multi-qubit gate uses qubits starting from target until it's size is matched.
 */
const SingleOrMultiQubitGateInfo = t.type({
    name: t.string,
    time: t.number,
    target: t.number,
});

const SwapGateInfo = t.type({
    time: t.number,
    swap1: t.number,
    swap2: t.number,
});

const GateInfo = t.union([
    SingleOrMultiQubitGateInfo,
    SwapGateInfo,
    ControlGateInfo,
]);

type IGateInfo = t.TypeOf<typeof GateInfo>;
type ISingleOrMultiQubitGateInfo = t.TypeOf<typeof SingleOrMultiQubitGateInfo>;
type ISwapGateInfo = t.TypeOf<typeof SwapGateInfo>;
type IControlGateInfo = t.TypeOf<typeof ControlGateInfo>;

const CustomGateInfo = t.type({
    name: t.string,
    matrix: t.string,
});

export type ICustomGateInfo = t.TypeOf<typeof CustomGateInfo>;

// All settings that are defined in the plugin markup YAML
const QuantumCircuitMarkup = t.intersection([
    t.partial({
        initialCircuit: nullable(t.array(GateInfo)),
        customGates: nullable(t.array(CustomGateInfo)),
        gates: nullable(t.array(t.string)),
    }),
    GenericPluginMarkup,
    t.type({
        nQubits: withDefault(t.number, 1),
        nMoments: withDefault(t.number, 5),
        qubitNotation: withDefault(t.keyof({braket: null, bit: null}), "bit"),
        showChart: withDefault(t.boolean, true),
        showPrintField: withDefault(t.boolean, true),
        showOutputBits: withDefault(t.boolean, true),
        samplingMode: withDefault(
            t.keyof({sample: null, autoSample: null, matrix: null}),
            "matrix"
        ),
        nSamples: withDefault(t.number, 100),
    }),
]);

// All data that plugin receives from the server (Markup + any extra state data)
const QuantumCircuitFields = t.intersection([
    getTopLevelFields(QuantumCircuitMarkup),
    t.type({}),
]);

/**
 * Colors to use for UI elements.
 */
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
                        [showOutputBits]="showOutputBits"
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
                                   [showChart]="showChart"
                                   [showPrintField]="showPrintField"
                                   (clear)="handleClearMeasurements()"
                                   (measure)="handleMeasure()">
                </tim-quantum-stats>
            </div>
        </div>
    `,
    styleUrls: ["./quantum-circuit.component.scss"],
    providers: [GateService],
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

    selectedGate: GatePos | null = null;

    measurements: Measurement[] = [];

    constructor(
        private gateService: GateService,
        el: ElementRef,
        http: HttpClient,
        domSanitizer: DomSanitizer
    ) {
        super(el, http, domSanitizer);
    }

    get nQubits() {
        return this.markup.nQubits;
    }

    get nMoments() {
        return this.markup.nMoments;
    }

    get showChart() {
        return this.markup.showChart;
    }

    get showPrintField() {
        return this.markup.showPrintField;
    }

    get showOutputBits() {
        return this.markup.showOutputBits;
    }

    /**
     * Collects statistics from simulator.
     */
    collectStats() {
        const mode = this.markup.samplingMode;
        if (mode === "matrix") {
            this.quantumChartData = this.simulator.getProbabilities();
        } else if (mode === "autoSample") {
            this.quantumChartData = this.simulator.getProbabilities(
                this.markup.nSamples
            );
        } else {
            this.quantumChartData = this.simulator.getProbabilities(
                undefined,
                this.measurements
            );
        }

        this.setQubitOutputs();
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

                this.collectStats();

                this.timeoutId = -1;
            }, 0);
        } else {
            this.simulator.run();
            this.collectStats();
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

    /**
     * Replaces cell of the board with gate.
     * @param gate gate to put in cell
     */
    handleGateDrop(gate: GateDrop) {
        if (gate.name === "control") {
            this.board.addControl(gate, this.selectedGate);
        } else if (gate.name === "swap") {
            this.board.addSwap(gate);
        } else {
            const size = this.gateService.getGateSize(gate.name);
            if (size > 1) {
                this.board.addMultiQubitGate(
                    gate,
                    new MultiQubitGate(gate.name, size)
                );
            } else {
                this.board.addGate(gate, new Gate(gate.name));
            }
        }

        this.selectedGate = null;

        this.runSimulation();
    }

    /**
     * Move gate or control from one cell to another.
     * @param gateMove object describing gate movement
     */
    handleGateMove(gateMove: GateMove) {
        this.board.moveCell(gateMove.from, gateMove.to, this.selectedGate);
        this.selectedGate = null;
        this.runSimulation();
    }

    /**
     * Removes gate from board and controls or swap pair associated with it.
     * @param gate gate to remove
     */
    handleGateRemove(gate: GatePos) {
        this.board.remove(gate.target, gate.time);

        this.selectedGate = null;

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
                this.selectedGate = null;
            } else {
                this.selectedGate = gate;
            }
        }
    }

    /**
     * Sample and add a new measurement to beginning of measurements.
     */
    handleMeasure() {
        const measurement = this.simulator.sample();
        if (!measurement) {
            return;
        }
        this.measurements = [measurement, ...this.measurements];

        if (this.markup.samplingMode === "sample") {
            this.quantumChartData = this.simulator.getProbabilities(
                undefined,
                this.measurements
            );
        }
    }

    /**
     * Deletes all measurements.
     */
    handleClearMeasurements() {
        this.measurements = [];
        this.collectStats();
    }

    isSwap(gate: IGateInfo): gate is ISwapGateInfo {
        return "swap1" in gate;
    }

    isControl(gate: IGateInfo): gate is IControlGateInfo {
        return "controls" in gate;
    }

    isSingleOrMultiQubit(gate: IGateInfo): gate is ISingleOrMultiQubitGateInfo {
        return !this.isSwap(gate) && !this.isControl(gate);
    }

    /**
     * Add gates from initialCircuit to board.
     */
    addInitialGates() {
        if (!this.markup.initialCircuit) {
            return;
        }

        for (const gateData of this.markup.initialCircuit) {
            if (this.isSwap(gateData)) {
                this.board.addSwap(
                    {target: gateData.swap1, time: gateData.time},
                    {
                        target: gateData.swap2,
                        time: gateData.time,
                    }
                );
            } else if (this.isControl(gateData)) {
                const gate = new Gate(gateData.name);
                this.board.set(gateData.target, gateData.time, gate);
                for (const controlTarget of gateData.controls) {
                    const control = new Control(gateData.target);
                    this.board.set(controlTarget, gateData.time, control);
                }
            } else if (this.isSingleOrMultiQubit(gateData)) {
                const size = this.gateService.getGateSize(gateData.name);
                if (size > 1) {
                    const gate = new MultiQubitGate(gateData.name, size);
                    this.board.addMultiQubitGate(
                        {target: gateData.target, time: gateData.time},
                        gate
                    );
                } else {
                    const gate = new Gate(gateData.name);
                    this.board.addGate(
                        {target: gateData.target, time: gateData.time},
                        gate
                    );
                }
            } else {
                console.log("missing type", gateData);
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

        this.gateService.registerUserDefinedGates(
            this.markup.gates,
            this.markup.customGates
        );

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
        this.simulator = new QuantumCircuitSimulator(
            this.gateService,
            this.board,
            this.qubits
        );

        this.runSimulation(false);
    }

    /**
     * Compute dimensions for elements.
     */
    async ngAfterViewInit() {
        // when entering paragraph editor, qcContainer width is zero for some reason and timeout fixes that
        await timeout();
        // Compute sizes for board cells based on available space and number of cells
        const baseSize =
            this.qcContainer.nativeElement.clientWidth / (this.nMoments + 4);
        const gateSize = (2 / 3) * baseSize;

        const useBraket = this.markup.qubitNotation === "braket";

        this.circuitStyleOptions = {
            baseSize: baseSize,
            gateSize: gateSize,
            colors: {
                dark: "black",
                medium: "grey",
                light: "white",
            },
            useBraket: useBraket,
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
        InstanceofPipe,
        RangePipe,
    ],
    exports: [QuantumCircuitComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        NgChartsModule,
        TimUtilityModule,
        FormsModule,
    ],
})
export class QuantumCircuitModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin(
    "tim-quantum-circuit",
    QuantumCircuitModule,
    QuantumCircuitComponent
);
