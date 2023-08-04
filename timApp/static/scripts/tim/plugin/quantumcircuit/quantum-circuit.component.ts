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
    MultiQubitGateCell,
    QuantumBoard,
    Swap,
} from "tim/plugin/quantumcircuit/quantum-board";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {timeout} from "tim/util/utils";
import {FormsModule} from "@angular/forms";
import {GateService} from "tim/plugin/quantumcircuit/gate.service";
import {DomSanitizer} from "@angular/platform-browser";
import {SvgCellComponent} from "tim/plugin/quantumcircuit/svg-cell.component";
import {PurifyModule} from "tim/util/purify.module";
import {Qubit} from "tim/plugin/quantumcircuit/qubit";
import {
    ActiveGateInfo,
    CircuitActiveGateInfo,
} from "tim/plugin/quantumcircuit/active-gate";

export interface QubitOutput {
    value: number;
    name?: string;
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
    editable: withDefault(t.boolean, true),
});

/**
 * Assume that multi-qubit gate uses qubits starting from target until it's size is matched.
 */
const SingleOrMultiQubitGateInfo = t.type({
    name: t.string,
    time: t.number,
    target: t.number,
    editable: withDefault(t.boolean, true),
});

const SwapGateInfo = t.type({
    time: t.number,
    swap1: t.number,
    swap2: t.number,
    editable: withDefault(t.boolean, true),
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
    description: t.string,
});

export type ICustomGateInfo = t.TypeOf<typeof CustomGateInfo>;

const CircuitType = nullable(t.array(GateInfo));

type ICircuit = t.TypeOf<typeof CircuitType>;

const InputBitsType = nullable(t.array(t.number));
type IUserInput = t.TypeOf<typeof InputBitsType>;

// All settings that are defined in the plugin markup YAML
const QuantumCircuitMarkup = t.intersection([
    t.partial({
        initialCircuit: nullable(t.array(GateInfo)),
        customGates: nullable(t.array(CustomGateInfo)),
        gates: nullable(t.array(t.string)),
        modelCircuit: nullable(t.array(GateInfo)),
        modelInput: nullable(t.array(t.number)),
        qubitNames: nullable(t.array(t.string)),
        outputNames: nullable(t.array(t.string)),
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
    t.type({
        state: nullable(
            t.type({
                userCircuit: nullable(t.array(GateInfo)),
                userInput: nullable(t.array(t.number)),
            })
        ),
    }),
]);

/**
 * Colors to use for UI elements.
 */
export interface Colors {
    dark: string;
    medium: string;
    light: string;
}

export interface Color {
    text: string;
    fill: string;
    selection: string;
}

/**
 * Styling related options for circuit.
 */
export interface CircuitStyleOptions {
    colors: Colors;
    gateColors: Map<string, Color>;
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
    // width of the input holding qubit name
    inputWidth: number;
    // width of the input holding output name
    outputWidth: number;
}

@Component({
    selector: "tim-quantum-circuit",
    template: `
        <tim-plugin-frame [markupError]="markupError">
            <tim-plugin-header *ngIf="header" header>
                <span [innerHTML]="header | purify"></span>
            </tim-plugin-header>
            <p stem *ngIf="stem" [innerHTML]="stem | purify"></p>
            <ng-container body>
                <div #qcContainer class="circuit-container" (window:resize)="handleResize()">
                    <div class="top-menu">
                        <tim-quantum-gate-menu
                                [circuitStyleOptions]="circuitStyleOptions"
                                (select)="handleMenuGateSelect($event)">
                        </tim-quantum-gate-menu>
                        <tim-quantum-toolbox [activeGateInfo]="activeGateInfo"
                                             (close)="handleActiveGateHide()"></tim-quantum-toolbox>
                    </div>

                    <tim-quantum-circuit-board
                            class="circuit"
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

                    <tim-quantum-stats class="starts"
                                       [measurements]="measurements"
                                       [quantumChartData]="quantumChartData"
                                       [showChart]="showChart"
                                       [showPrintField]="showPrintField"
                                       [samplingMode]="samplingMode"
                                       [nSamples]="nSamples"
                                       (clear)="handleClearMeasurements()"
                                       (measure)="handleMeasure()">
                    </tim-quantum-stats>

                    <div class="buttons">
                        <button class="timButton"
                                (click)="save()">
                            Tallenna
                        </button>
                        <button class="btn btn-default btn-xs"
                                (click)="reset()">Palauta
                        </button>
                    </div>
                </div>

                <div>{{result}}</div>
                <div>{{error}}</div>
            </ng-container>
            <p footer *ngIf="footer" [innerHTML]="footer | purify"></p>

        </tim-plugin-frame>

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
        gateColors: new Map<string, Color>([
            [
                "basic",
                {
                    fill: "#0098e9",
                    text: "#000000",
                    selection: "#000000",
                },
            ],
            [
                "phase",
                {
                    fill: "#fca534",
                    text: "#000000",
                    selection: "#000000",
                },
            ],
            [
                "custom",
                {
                    fill: "#a9875a",
                    text: "#000000",
                    selection: "#000000",
                },
            ],
            [
                "swap",
                {
                    fill: "#000000",
                    text: "#ffffff",
                    selection: "#00ff00",
                },
            ],
            [
                "control",
                {
                    fill: "#000000",
                    text: "#ffffff",
                    selection: "#00ff00",
                },
            ],
        ]),
        useBraket: false,
        timeAxisHeight: 40,
        gateBorderRadius: 2,
        inputWidth: 120,
        outputWidth: 120,
    };

    board!: QuantumBoard;

    selectedGate: GatePos | null = null;

    activeGateInfo?: ActiveGateInfo;

    measurements: Measurement[] = [];

    result: string = "";
    error: string = "";

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

    get samplingMode() {
        return this.markup.samplingMode;
    }

    get nSamples() {
        return this.markup.nSamples;
    }

    serializeUserCircuit(): ICircuit {
        const userCircuit: ICircuit = [];
        const board = this.board.board;
        for (let targetI = 0; targetI < board.length; targetI++) {
            for (let timeI = 0; timeI < board[targetI].length; timeI++) {
                const cell = board[targetI][timeI];
                if (cell instanceof Gate) {
                    const controls = this.board.getControls({
                        target: targetI,
                        time: timeI,
                    });

                    if (controls.length > 0) {
                        userCircuit.push({
                            name: cell.name,
                            editable: cell.editable,
                            target: targetI,
                            time: timeI,
                            controls: controls,
                        });
                    } else {
                        userCircuit.push({
                            name: cell.name,
                            target: targetI,
                            time: timeI,
                            editable: cell.editable,
                        });
                    }
                } else if (cell instanceof MultiQubitGate) {
                    userCircuit.push({
                        name: cell.name,
                        time: timeI,
                        target: targetI,
                        editable: cell.editable,
                    });
                } else if (cell instanceof Swap && targetI < cell.target) {
                    userCircuit.push({
                        swap1: targetI,
                        swap2: cell.target,
                        time: timeI,
                        editable: cell.editable,
                    });
                }
            }
        }
        return userCircuit;
    }

    /**
     * Save answer.
     */
    async save() {
        const userCircuit = this.serializeUserCircuit();

        const userInput: IUserInput = this.qubits.map((q) => q.value);

        const params = {
            input: {
                userCircuit: userCircuit,
                userInput: userInput,
            },
        };
        const r = await this.postAnswer<{
            web: {result?: string; error?: string};
        }>(params);
        if (r.ok) {
            this.result = r.result.web.result ?? "";
            this.error = r.result.web.error ?? "";
            console.log(this.result, this.error);
        } else {
            this.result = "";
            this.error = r.result.error.error;
        }
    }

    /**
     * Reset circuit to its initial state.
     */
    reset() {
        this.initializeBoard(true);
        this.runSimulation(true);
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
     * Toggle state of qubit between basis states.
     * @param qubitId qubit to toggle
     */
    handleQubitChange(qubitId: number) {
        if (qubitId < 0 || qubitId >= this.qubits.length) {
            console.log("non-existing qubitId", qubitId);
            return;
        }
        this.qubits[qubitId] = this.qubits[qubitId].toggled();

        this.runSimulation();
    }

    /**
     * Override reference to board for change detection to work.
     */
    updateBoard() {
        this.board = this.board.clone();
        this.simulator.setBoard(this.board);
    }

    /**
     * Replaces cell of the board with gate.
     * @param gate gate to put in cell
     */
    handleGateDrop(gate: GateDrop) {
        if (!this.gateService.isMenuGate(gate.name)) {
            return;
        }

        if (gate.name === "control") {
            this.board.addControl(gate, this.selectedGate);
        } else if (gate.name === "swap") {
            this.board.addSwap(gate, null, true);
        } else {
            const size = this.gateService.getGateSize(gate.name);
            if (size > 1) {
                this.board.addMultiQubitGate(
                    gate,
                    new MultiQubitGate(gate.name, size, true)
                );
            } else {
                this.board.addGate(gate, new Gate(gate.name, true));
            }
        }

        this.updateBoard();

        this.handleActiveGateHide();

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
        this.updateBoard();
        this.handleActiveGateHide();
        this.runSimulation();
    }

    /**
     * Removes gate from board and controls or swap pair associated with it.
     * @param gate gate to remove
     */
    handleGateRemove(gate: GatePos) {
        if (this.board.get(gate.target, gate.time)?.editable !== false) {
            this.board.remove(gate.target, gate.time);
        }
        this.selectedGate = null;
        this.updateBoard();
        this.handleActiveGateHide();
        this.runSimulation();
    }

    /**
     * Gets the name of the gate resolving controls and multi-qubit gate cells
     * to their target gate.
     * @param pos position of gate to get name for
     */
    getGateName(pos: GatePos) {
        const cell = this.board.get(pos.target, pos.time);
        if (cell instanceof Gate || cell instanceof MultiQubitGate) {
            return cell.name;
        }
        if (cell instanceof Control) {
            const targetGate = this.board.get(cell.target, pos.time);
            if (targetGate instanceof Gate) {
                return targetGate.name;
            }
        }
        if (cell instanceof Swap) {
            return "swap";
        }
        if (cell instanceof MultiQubitGateCell) {
            const targetGate = this.board.get(cell.target, pos.time);
            if (targetGate instanceof MultiQubitGate) {
                return targetGate.name;
            }
        }
    }

    /**
     * Sets active gate based on chosen cell.
     * @param gate position of gate
     */
    updateActiveGate(gate: GatePos) {
        if (this.board.get(gate.target, gate.time) === undefined) {
            this.handleActiveGateHide();
            return;
        }
        const name = this.getGateName(gate);
        if (name === undefined) {
            return;
        }
        const gateInfo = this.gateService.getGate(name);
        if (!gateInfo) {
            return;
        }
        let swapInfo: [number, number] | undefined;
        if (name === "swap") {
            const cell = this.board.get(gate.target, gate.time);
            if (cell instanceof Swap) {
                swapInfo = [gate.target, cell.target];
            }
        }
        const controls = this.board.getControls(gate);

        const editable =
            this.board.get(gate.target, gate.time)?.editable === true;

        this.activeGateInfo = new CircuitActiveGateInfo(
            gate.target,
            gate.time,
            name,
            gateInfo.matrix,
            controls,
            this.qubits,
            gateInfo.description,
            editable,
            swapInfo
        );
    }

    handleActiveGateHide() {
        this.activeGateInfo = undefined;
    }

    handleMenuGateSelect(gateName: string) {
        const gateInfo = this.gateService.getGate(gateName);
        if (gateInfo && gateInfo.name === "control") {
            this.activeGateInfo = undefined;
        } else if (gateInfo) {
            this.activeGateInfo = new ActiveGateInfo(
                gateInfo.name,
                gateInfo.description,
                gateInfo.matrix
            );
        }
    }

    /**
     * Marks gate as selected.
     * @param gate position of gate on board
     */
    handleGateSelect(gate: GatePos) {
        const cell = this.board.get(gate.target, gate.time);

        this.updateActiveGate(gate);

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
        this.updateBoard();
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
     * @param circuit gates to add to board
     */
    addInitialGates(circuit: ICircuit) {
        if (!circuit) {
            return;
        }

        for (const gateData of circuit) {
            if (this.isSwap(gateData)) {
                this.board.addSwap(
                    {target: gateData.swap1, time: gateData.time},
                    {
                        target: gateData.swap2,
                        time: gateData.time,
                    },
                    gateData.editable
                );
            } else if (this.isControl(gateData)) {
                const gate = new Gate(gateData.name, gateData.editable);
                this.board.set(gateData.target, gateData.time, gate);
                for (const controlTarget of gateData.controls) {
                    if (controlTarget === gateData.target) {
                        continue;
                    }
                    const control = new Control(
                        gateData.target,
                        gateData.editable
                    );
                    this.board.set(controlTarget, gateData.time, control);
                }
            } else if (this.isSingleOrMultiQubit(gateData)) {
                const size = this.gateService.getGateSize(gateData.name);
                if (size > 1) {
                    const gate = new MultiQubitGate(
                        gateData.name,
                        size,
                        gateData.editable
                    );
                    this.board.addMultiQubitGate(
                        {target: gateData.target, time: gateData.time},
                        gate
                    );
                } else {
                    const gate = new Gate(gateData.name, gateData.editable);
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
     * @param reset whether this call is to reset board to initial state
     */
    initializeBoard(reset: boolean) {
        this.qubits = [];
        const defaultValue = 0;
        const userInput = this.attrsall.state?.userInput;

        if (userInput && !reset) {
            for (let i = 0; i < this.nQubits; i++) {
                const value = userInput[i];
                this.qubits.push(
                    new Qubit(value, `q[${i}]`, this.circuitStyleOptions)
                );
            }
        } else {
            for (let i = 0; i < this.nQubits; i++) {
                this.qubits.push(
                    new Qubit(defaultValue, `q[${i}]`, this.circuitStyleOptions)
                );
            }
        }

        if (this.markup.qubitNames) {
            for (let i = 0; i < this.qubits.length; i++) {
                this.qubits[i].name = this.markup.qubitNames[i];
            }
        }

        this.board = new QuantumBoard(this.nQubits, this.nMoments);

        this.gateService.registerUserDefinedGates(
            this.markup.gates,
            this.markup.customGates
        );

        const userCircuit = this.attrsall.state?.userCircuit;

        if (userCircuit && !reset) {
            this.addInitialGates(userCircuit);
        } else if (this.markup.initialCircuit) {
            this.addInitialGates(this.markup.initialCircuit);
        }

        this.qubitOutputs = [];
        for (let i = 0; i < this.nQubits; i++) {
            this.qubitOutputs.push({
                value: 0,
            });
        }
        if (this.markup.outputNames) {
            for (let i = 0; i < this.qubitOutputs.length; i++) {
                this.qubitOutputs[i].name = this.markup.outputNames[i];
            }
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

        if (this.markup.outputNames) {
            for (let i = 0; i < this.qubitOutputs.length; i++) {
                this.qubitOutputs[i].name = this.markup.outputNames[i];
            }
        }
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
     * Computes width of text.
     * @param text text to get width for
     */
    textWidth(text: string) {
        const canvas = document.createElement("canvas");
        const ctx = canvas.getContext("2d");
        if (!ctx) {
            return text.length * 14;
        }
        ctx.font = "normal 14px Verdana";
        return ctx.measureText(text).width;
    }

    /**
     * Compute sizes for board cells based on available space and number of cells.
     */
    setSizes() {
        // qubit name | qubit bit value | nMoments * space for gate | measure-logo | output bit | maybe output name
        const extraSpaces = this.markup.outputNames ? 5 : 4;
        // try to fit everything on screen
        let baseSize =
            this.qcContainer.nativeElement.clientWidth /
            (this.nMoments + extraSpaces);
        // don't make gates excessively large
        baseSize = Math.min(50, baseSize);
        // don't make gates too small
        baseSize = Math.max(baseSize, 30);

        // set left hand side width as longest qubit text width
        let inputWidth = 0;
        for (const qubit of this.qubits) {
            inputWidth = Math.max(inputWidth, this.textWidth(qubit.name));
        }
        // a little bit of extra is needed for some reason
        inputWidth += 14;

        // set right hand side width as longest output text width
        let outputWidth = 0;
        for (const out of this.qubitOutputs) {
            outputWidth = Math.max(outputWidth, this.textWidth(out.name ?? ""));
        }
        outputWidth += 14;

        const gateSize = 0.8 * baseSize;

        const useBraket = this.markup.qubitNotation === "braket";

        this.circuitStyleOptions = {
            baseSize: baseSize,
            gateSize: gateSize,
            colors: {
                dark: "black",
                medium: "grey",
                light: "#fca534",
            },
            gateColors: this.circuitStyleOptions.gateColors,
            useBraket: useBraket,
            timeAxisHeight: this.circuitStyleOptions.timeAxisHeight,
            gateBorderRadius: this.circuitStyleOptions.gateBorderRadius,
            inputWidth: inputWidth,
            outputWidth: outputWidth,
        };
    }

    /**
     * Resize elements.
     */
    handleResize() {
        this.setSizes();
    }

    /**
     * Compute dimensions for elements.
     */
    async ngAfterViewInit() {
        // when entering paragraph editor, qcContainer width is zero for some reason and timeout fixes that
        await timeout();

        this.setSizes();

        for (const qubit of this.qubits) {
            qubit.updateNotation(this.circuitStyleOptions);
        }
    }

    ngOnInit(): void {
        super.ngOnInit();

        this.initializeBoard(false);

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
        SvgCellComponent,
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
        PurifyModule,
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
