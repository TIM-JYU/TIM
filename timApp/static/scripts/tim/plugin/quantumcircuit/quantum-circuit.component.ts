import * as t from "io-ts";
import type {TypeOf} from "io-ts";
import type {AfterViewInit, DoBootstrap, OnInit} from "@angular/core";
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
import {QuantumCircuitBoardComponent} from "tim/plugin/quantumcircuit/quantum-circuit-board.component";
import type {QuantumChartData} from "tim/plugin/quantumcircuit/quantum-stats.component";
import {QuantumStatsComponent} from "tim/plugin/quantumcircuit/quantum-stats.component";
import {NgChartsModule} from "ng2-charts";
import type {QuantumCircuitSimulator} from "tim/plugin/quantumcircuit/quantum-simulation";
import {
    BrowserQuantumCircuitSimulator,
    ServerQuantumCircuitSimulator,
} from "tim/plugin/quantumcircuit/quantum-simulation";
import {
    Control,
    Gate,
    MultiQubitGate,
    MultiQubitGateCell,
    QuantumBoard,
    Swap,
} from "tim/plugin/quantumcircuit/quantum-board";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {copyToClipboard, isIOS, timeout} from "tim/util/utils";
import {FormsModule} from "@angular/forms";
import {GateService} from "tim/plugin/quantumcircuit/gate.service";
import {DomSanitizer} from "@angular/platform-browser";
import {QuantumCellComponent} from "tim/plugin/quantumcircuit/quantum-cell.component";
import {PurifyModule} from "tim/util/purify.module";
import {Qubit} from "tim/plugin/quantumcircuit/qubit";
import {
    ActiveGateInfo,
    CircuitActiveGateInfo,
} from "tim/plugin/quantumcircuit/active-gate";
import {genericglobals} from "tim/util/globals";
import {SerializerService} from "tim/plugin/quantumcircuit/serializer.service";
import {QuantumErrorComponent} from "tim/plugin/quantumcircuit/quantum-error.component";
import {isRight} from "fp-ts/Either";
import {Users} from "tim/user/userService";
import {InstanceofModule} from "tim/util/instanceof.module";
import {polyfill} from "mobile-drag-drop";
import {scrollBehaviourDragImageTranslateOverride} from "mobile-drag-drop/scroll-behaviour";
import type {ITimComponent} from "tim/document/viewctrl";

export interface QubitOutput {
    value: string;
    probability: number;
    probabilityText: string;
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

const NormalGateInfo = t.type({
    name: t.string,
    time: t.number,
    target: t.number,
    controls: withDefault(t.array(t.number), []),
    antiControls: withDefault(t.array(t.number), []),
    editable: withDefault(t.boolean, true),
});

const SwapGateInfo = t.type({
    time: t.number,
    swap1: t.number,
    swap2: t.number,
    controls: withDefault(t.array(t.number), []),
    antiControls: withDefault(t.array(t.number), []),
    editable: withDefault(t.boolean, true),
});

const GateInfo = t.union([NormalGateInfo, SwapGateInfo]);

type IGateInfo = t.TypeOf<typeof GateInfo>;
type ISwapGateInfo = t.TypeOf<typeof SwapGateInfo>;

const CustomGateInfo = t.intersection([
    t.type({
        name: t.string,
        matrix: t.string,
        info: t.string,
    }),
    t.partial({color: nullable(t.string), textColor: nullable(t.string)}),
]);

export type ICustomGateInfo = t.TypeOf<typeof CustomGateInfo>;

const NumericCustomGateInfo = t.type({
    name: t.string,
    matrix: t.string,
});

export type INumericCustomGateInfo = t.TypeOf<typeof NumericCustomGateInfo>;

const CircuitType = nullable(t.array(GateInfo));

export type ICircuit = t.TypeOf<typeof CircuitType>;

const InputBitsType = nullable(t.array(t.number));
type IUserInput = t.TypeOf<typeof InputBitsType>;

export interface SimulationArgs {
    gates: ICircuit;
    inputList: number[];
    nQubits: number;
    customGates: INumericCustomGateInfo[];
}

const QubitInfo = t.partial({
    name: nullable(t.string),
    value: nullable(t.number),
    editable: nullable(t.boolean),
});

// All settings that are defined in the plugin markup YAML
const QuantumCircuitMarkup = t.intersection([
    t.partial({
        initialCircuit: nullable(t.array(GateInfo)),
        customGates: nullable(t.array(CustomGateInfo)),
        gates: nullable(t.array(t.string)),
        modelCircuit: nullable(t.array(GateInfo)),
        modelInput: nullable(t.array(t.string)),
        modelConditions: nullable(t.array(t.string)),
        qubits: nullable(t.array(QubitInfo)),
        outputNames: nullable(t.array(t.string)),
        maxRunTimeout: nullable(t.number),
        hideGateInfo: nullable(t.array(t.string)),
        feedbackText: nullable(
            t.partial({
                correct: nullable(t.string),
                wrong: nullable(t.string),
                conditionWrong: nullable(t.string),
            })
        ),
        lazyBody: nullable(t.string),
    }),
    GenericPluginMarkup,
    t.type({
        nQubits: withDefault(t.number, 1),
        nMoments: withDefault(t.number, 5),
        qubitNotation: withDefault(t.keyof({braket: null, bit: null}), "bit"),
        showChart: withDefault(t.boolean, true),
        showPrintField: withDefault(t.boolean, true),
        showOutputBits: withDefault(t.boolean, true),
        showExport: withDefault(t.boolean, false),
        samplingMode: withDefault(
            t.keyof({sample: null, autoSample: null, matrix: null}),
            "matrix"
        ),
        nSamples: withDefault(t.number, 100),
        simulate: withDefault(
            t.keyof({browser: null, server: null}),
            "browser"
        ),
        leftAxisLabel: withDefault(t.string, "Qubit"),
        middleAxisLabel: withDefault(t.string, "Step"),
        rightAxisLabel: withDefault(t.string, "Output"),
        feedbackShowTable: withDefault(t.boolean, true),
        answerExactMatch: withDefault(t.boolean, false),
        removeControls: withDefault(t.boolean, false),
        touchOffset: withDefault(t.number, 40),
    }),
]);

export type QuantumCircuitMarkupType = TypeOf<typeof QuantumCircuitMarkup>;

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
 * Different types of errors that can be received from server.
 * errorType values need to match the ones in quantumCircuit.py.
 */
export const ServerError = t.union([
    t.type({
        condition: t.string,
        values: t.string,
        errorType: t.literal("condition-not-satisfied"),
    }),
    t.type({
        condition: t.string,
        errorType: t.literal("condition-not-interpretable"),
    }),
    t.type({condition: t.string, errorType: t.literal("condition-invalid")}),
    t.type({
        bitstring: t.string,
        expected: t.array(t.number),
        actual: t.array(t.number),
        errorType: t.literal("answer-incorrect"),
    }),
    t.type({
        errorType: t.literal("answer-incorrect-exact"),
    }),
    t.type({
        matrix: t.string,
        errorType: t.literal("matrix-incorrect"),
    }),
    t.type({
        qubits: t.number,
        maxQubits: t.number,
        errorType: t.literal("too-many-qubits"),
    }),
    t.type({
        moments: t.number,
        maxMoments: t.number,
        errorType: t.literal("too-many-moments"),
    }),
    t.type({
        regex: t.string,
        errorType: t.literal("regex-invalid"),
    }),
    t.type({
        errorType: t.literal("simulation-timed-out"),
    }),
    t.type({
        timeout: t.number,
        maxTimeout: t.number,
        errorType: t.literal("too-long-timeout"),
    }),
    t.type({
        message: t.string,
        errorType: t.literal("circuit-uninterpretable"),
    }),
]);

export type IServerError = t.TypeOf<typeof ServerError>;

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
export interface CircuitOptions {
    colors: Colors;
    gateColors: Map<string, Color>;
    // relative size of circuit cells (side length) and other elements
    baseSize: number;
    gateWidth: number;
    baseWidth: number;
    // size of gates (side length)
    gateSize: number;
    // whether to use braket notation for input qubits or just (0,1)
    useBraket: boolean;
    // timeAxisHeight: height of first row showing time steps 0,1,2...
    timeAxisHeight: number;
    // border radius value for gates (rounded corners)
    gateBorderRadius: number;
    // width of each column, should be wide enough to fit the longest gate name
    columnWidths?: number[];
    // total width of circuit area
    columnWidthsSum?: number;
    // prefix sum of columnWidths, these are used to calculate where each column is located in x-direction
    columnWidthSums?: number[];
}

@Component({
    selector: "tim-quantum-circuit",
    template: `
        <tim-plugin-frame [markupError]="markupError">
            <tim-plugin-header *ngIf="header" header>
                <span [innerHTML]="header | purify"></span>
            </tim-plugin-header>
            <p stem class="stem" *ngIf="stem" [innerHTML]="stem | purify"></p>
            <ng-container body>
                <div #qcContainer class="circuit-container" (window:resize)="handleResize()">
                    <div class="top-menu">
                        <tim-quantum-gate-menu
                                [circuitOptions]="circuitOptions"
                                [isToolboxStaticHeight]="isToolboxStaticHeight"
                                (select)="handleMenuGateSelect($event)">
                        </tim-quantum-gate-menu>
                        <tim-quantum-toolbox [activeGateInfo]="activeGateInfo"
                                             [isToolboxStaticHeight]="isToolboxStaticHeight"
                                             (close)="handleActiveGateHide()"></tim-quantum-toolbox>
                    </div>

                    <tim-quantum-circuit-board
                            *ngIf="board"
                            class="circuit"
                            [board]="board"
                            [selectedGate]="selectedGate"
                            [qubits]="qubits"
                            [qubitOutputs]="qubitOutputs"
                            [circuitOptions]="circuitOptions"
                            [showOutputBits]="showOutputBits"
                            [leftAxisLabel]="leftAxisLabel"
                            [rightAxisLabel]="rightAxisLabel"
                            [middleAxisLabel]="middleAxisLabel"
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
                                       [showExport]="showExport"
                                       [samplingMode]="samplingMode"
                                       [nSamples]="nSamples"
                                       [isSimulatorRunning]="isSimulatorRunning"
                                       (clear)="handleClearMeasurements()"
                                       (measure)="handleMeasure()"
                                       (export)="handleExport($event)">
                    </tim-quantum-stats>

                    <div class="buttons">
                        <button class="timButton"
                                (click)="save()">
                            <ng-container i18n>Save</ng-container>
                        </button>
                        <button class="btn btn-default btn-xs"
                                (click)="reset()">
                            <ng-container i18n>Reset</ng-container>
                        </button>
                        <button class="btn btn-default btn-xs"
                                *ngIf="hasEditRights"
                                (click)="copyCircuit()">
                            <ng-container i18n>Copy circuit to clipboard</ng-container>
                        </button>
                        <div *ngIf="notification">{{notification}}</div>
                    </div>
                </div>

                <tim-loading *ngIf="isResultCheckingRunning"></tim-loading>
                <tim-quantum-error *ngIf="error" [error]="error" [feedbackShowTable]="feedbackShowTable"></tim-quantum-error>
                <pre class="circuit-error" *ngIf="errorString" [innerHTML]="errorString | purify"></pre>
                <pre *ngIf="result" [innerHTML]="result | purify"></pre>
            </ng-container>
            <p footer *ngIf="footer" [innerHTML]="footer | purify"></p>

        </tim-plugin-frame>

    `,
    styleUrls: ["./quantum-circuit.component.scss"],
    providers: [GateService, SerializerService],
})
export class QuantumCircuitComponent
    extends AngularPluginBase<
        t.TypeOf<typeof QuantumCircuitMarkup>,
        t.TypeOf<typeof QuantumCircuitFields>,
        typeof QuantumCircuitFields
    >
    implements OnInit, AfterViewInit, ITimComponent
{
    private static dndPolyfillApplied = false;

    @ViewChild("qcContainer")
    qcContainer!: ElementRef<HTMLElement>;

    qubits: Qubit[] = [];
    qubitOutputs: QubitOutput[] = [];

    simulator!: QuantumCircuitSimulator;

    quantumChartData!: QuantumChartData;

    circuitOptions: CircuitOptions = {
        baseSize: 60,
        baseWidth: 60,
        gateSize: 40,
        gateWidth: 40,
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
            [
                "antiControl",
                {
                    fill: "#000000",
                    text: "#ffffff",
                    selection: "#00ff00",
                },
            ],
        ]),
        useBraket: false,
        timeAxisHeight: 40,
        gateBorderRadius: 3,
    };

    board?: QuantumBoard;

    selectedGate: GatePos | null = null;

    activeGateInfo?: ActiveGateInfo;

    measurements: Measurement[] = [];

    result: string = "";
    error?: IServerError;
    errorString: string = "";
    notification: string = "";

    hasEditRights: boolean = false;

    isSimulatorRunning: boolean = false;

    isResultCheckingRunning: boolean = false;

    isToolboxStaticHeight: boolean = true;

    constructor(
        private gateService: GateService,
        private serializerService: SerializerService,
        el: ElementRef,
        http: HttpClient,
        domSanitizer: DomSanitizer
    ) {
        super(el, http, domSanitizer);
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

    get showExport() {
        return this.markup.showExport;
    }

    get samplingMode() {
        return this.markup.samplingMode;
    }

    get nSamples() {
        return this.markup.nSamples;
    }

    get leftAxisLabel() {
        return this.markup.leftAxisLabel;
    }

    get rightAxisLabel() {
        return this.markup.rightAxisLabel;
    }

    get middleAxisLabel() {
        return this.markup.middleAxisLabel;
    }

    get feedbackShowTable() {
        return this.markup.feedbackShowTable;
    }

    parseError(e: string) {
        const errInfo = ServerError.decode(JSON.parse(e));
        if (isRight(errInfo)) {
            return errInfo.right;
        } else {
            throw errInfo;
        }
    }

    /**
     * Save answer.
     */
    async save() {
        if (!this.board) {
            return {saved: false, message: undefined};
        }
        const userCircuit = this.serializerService.serializeUserCircuit(
            this.board
        );

        const userInput: IUserInput = this.qubits.map((q) => q.value);

        let customGates: INumericCustomGateInfo[] = [];
        if (this.markup.customGates) {
            customGates = this.serializerService.serializeCustomGates(
                this.markup.customGates,
                this.gateService
            );
        }

        const params = {
            input: {
                userCircuit: userCircuit,
                userInput: userInput,
                customGates: customGates,
                measurements: this.measurements.length,
            },
        };
        this.isResultCheckingRunning = true;
        const r = await this.postAnswer<{
            web: {result?: string; error?: string};
        }>(params);

        this.isResultCheckingRunning = false;
        if (r.ok) {
            const resText = r.result.web.result ?? "";
            const customCorrectMessage = this.markup.feedbackText?.correct;
            if (
                (resText === "saved" || resText === "correct") &&
                customCorrectMessage
            ) {
                this.result = customCorrectMessage;
            } else if (resText === "saved") {
                this.result = $localize`saved`;
            } else if (resText === "correct") {
                this.result = $localize`correct`;
            } else {
                this.result = resText;
            }
            const e = r.result.web.error;
            if (e && e !== "null") {
                try {
                    this.error = this.parseError(e);
                    const customMessage = this.markup.feedbackText?.wrong;
                    const customConditionIncorrectMessage =
                        this.markup.feedbackText?.conditionWrong;
                    // show user defined error message as feedback if defined
                    if (
                        customMessage &&
                        (this.error.errorType === "answer-incorrect" ||
                            this.error.errorType === "answer-incorrect-exact")
                    ) {
                        this.error = undefined;
                        this.showErrorMessage(customMessage);
                    } else if (
                        customConditionIncorrectMessage &&
                        this.error.errorType === "condition-not-satisfied"
                    ) {
                        this.error = undefined;
                        this.showErrorMessage(customConditionIncorrectMessage);
                    }
                } catch (err) {
                    // We got an error, but it either was not valid JSON or it was not a valid ServerError instance
                    // This could happen e.g. if user sets a custom error via `postprogram` attribute.
                    this.showErrorMessage((err as Error).message);
                }
            } else {
                this.error = undefined;
                this.errorString = "";
            }
            return {saved: true, message: e};
        } else {
            this.result = "";
            this.errorString = r.result.error.error;
            return {saved: false, message: r.result.error.error};
        }
    }

    /**
     * Reset circuit to its initial state.
     */
    reset() {
        this.initializeBoard(true);
        if (this.board) {
            this.simulator.setBoard(this.board);
        }
        void this.runSimulation();
        this.setSizes();
    }

    /**
     * Shows message on screen and hides it after three seconds.
     * @param message text to show
     */
    showNotification(message: string) {
        this.notification = message;
        setTimeout(() => {
            this.notification = "";
        }, 3000);
    }

    /**
     * Copy current circuit to clipboard
     */
    async copyCircuit() {
        if (!this.board) {
            return;
        }
        const circuit = this.serializerService.serializeUserCircuit(this.board);
        if (!circuit) {
            return;
        }
        const url = "/quantumCircuit/circuitToYaml";
        const r = await this.httpPost<{web: string}>(url, circuit);
        if (r.ok) {
            const yaml = r.result.web;
            copyToClipboard(yaml);
            this.showNotification($localize`copied`);
        } else {
            this.result = "";
            this.errorString = r.result.error.error;
        }
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
     */
    async runSimulation() {
        const startTime = new Date();
        console.log(`started simulating at: ${startTime.toLocaleTimeString()}`);
        this.isSimulatorRunning = true;
        const res = await this.simulator.run(this.qubits);
        if (!res.ok) {
            const error = res.result;
            if (typeof error === "string") {
                this.showErrorMessage(error);
            } else {
                this.error = error;
            }
            this.isSimulatorRunning = false;
            const endTime = new Date();

            const timeDiff = endTime.getTime() - startTime.getTime();
            console.error(`simulation ended in error at: ${timeDiff}ms`);

            return;
        }

        const endTime = new Date();

        const timeDiff = endTime.getTime() - startTime.getTime();
        console.log(`simulation ended in: ${timeDiff}ms`);

        this.collectStats();
        this.isSimulatorRunning = false;
    }

    /**
     * Toggle state of qubit between basis states.
     * @param qubitId qubit to toggle
     */
    handleQubitChange(qubitId: number) {
        if (qubitId < 0 || qubitId >= this.qubits.length) {
            console.error("non-existing qubitId", qubitId);
            return;
        }
        this.qubits[qubitId] = this.qubits[qubitId].toggled();

        // don't update anything if nothing changed
        if (this.qubits[qubitId].editable) {
            void this.runSimulation();
            this.setSizes();
        }
    }

    /**
     * Override reference to board for change detection to work.
     */
    updateBoard() {
        if (this.board) {
            this.board = this.board.clone();
            this.simulator.setBoard(this.board);
        }
    }

    /**
     * Replaces cell of the board with gate.
     * @param gate gate to put in cell
     */
    handleGateDrop(gate: GateDrop) {
        if (!this.gateService.isMenuGate(gate.name)) {
            return;
        }
        if (!this.board) {
            return;
        }

        if (gate.name === "control") {
            this.board.addControl(gate, this.selectedGate, false);
        } else if (gate.name === "antiControl") {
            this.board.addControl(gate, this.selectedGate, true);
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

        void this.runSimulation();
        this.setSizes();
    }

    /**
     * Move gate or control from one cell to another.
     * @param gateMove object describing gate movement
     */
    handleGateMove(gateMove: GateMove) {
        if (!this.board) {
            return;
        }
        this.board.moveCell(gateMove.from, gateMove.to, this.selectedGate);
        this.selectedGate = null;
        this.updateBoard();
        this.handleActiveGateHide();
        void this.runSimulation();
        this.setSizes();
    }

    /**
     * Removes gate from board and controls or swap pair associated with it.
     * @param gate gate to remove
     */
    handleGateRemove(gate: GatePos) {
        if (!this.board) {
            return;
        }
        if (this.board.get(gate.target, gate.time)?.editable !== false) {
            this.board.remove(gate.target, gate.time);
        }
        this.selectedGate = null;
        this.updateBoard();
        this.handleActiveGateHide();
        void this.runSimulation();
        this.setSizes();
    }

    /**
     * Gets the name of the gate resolving controls and multi-qubit gate cells
     * to their target gate.
     * @param pos position of gate to get name for
     */
    getGateName(pos: GatePos) {
        if (!this.board) {
            return;
        }
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
     * Determine whether gate info should be hidden from user.
     * @param name name of the gate
     */
    isHiddenGateInfo(name: string) {
        if (!this.markup.hideGateInfo) {
            return false;
        }
        for (const namePattern of this.markup.hideGateInfo) {
            // has to match whole string
            const fullNamePattern = "^" + namePattern + "$";
            try {
                if (name.match(fullNamePattern)) {
                    return true;
                }
            } catch (error) {
                this.showErrorMessage(
                    $localize`hideGateInfo contains invalid regex: ${name}`
                );
            }
        }
        return false;
    }

    /**
     * Sets active gate based on chosen cell.
     * @param gate position of gate
     */
    updateActiveGate(gate: GatePos) {
        if (!this.board) {
            return;
        }
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
        const controls = this.board.getControls(gate, false);
        const antiControls = this.board.getControls(gate, true);

        const editable =
            this.board.get(gate.target, gate.time)?.editable === true;

        const hide = this.isHiddenGateInfo(name);

        this.activeGateInfo = new CircuitActiveGateInfo(
            gate.target,
            gate.time,
            name,
            gateInfo.matrix,
            controls,
            antiControls,
            this.qubits,
            gateInfo.info,
            editable,
            hide,
            swapInfo
        );
    }

    handleActiveGateHide() {
        this.activeGateInfo = undefined;
    }

    handleMenuGateSelect(gateName: string) {
        const gateInfo = this.gateService.getGate(gateName);
        if (
            gateInfo &&
            (gateInfo.name === "control" || gateInfo.name === "antiControl")
        ) {
            this.activeGateInfo = undefined;
        } else if (gateInfo) {
            const hide = this.isHiddenGateInfo(gateInfo.name);
            this.activeGateInfo = new ActiveGateInfo(
                gateInfo.name,
                gateInfo.info,
                gateInfo.matrix,
                hide
            );
        }
    }

    /**
     * Marks gate as selected.
     * @param gate position of gate on board
     */
    handleGateSelect(gate: GatePos) {
        if (!this.board) {
            return;
        }
        const cell = this.board.get(gate.target, gate.time);

        this.updateActiveGate(gate);

        if (
            cell instanceof Gate ||
            cell instanceof MultiQubitGate ||
            cell instanceof MultiQubitGateCell ||
            cell instanceof Swap
        ) {
            // same was selected so unselect it
            if (
                this.selectedGate &&
                this.selectedGate.time === gate.time &&
                this.selectedGate.target === gate.target
            ) {
                this.selectedGate = null;
            } else if (
                this.selectedGate &&
                cell instanceof MultiQubitGateCell &&
                cell.target === this.selectedGate.target &&
                this.selectedGate.time === gate.time
            ) {
                // multi-qubit gate was selected, and it's subcomponent was clicked, so unselect
                this.selectedGate = null;
            } else if (
                this.selectedGate &&
                cell instanceof Swap &&
                cell.target === this.selectedGate.target &&
                this.selectedGate.time === gate.time
            ) {
                this.selectedGate = null;
            } else {
                if (cell instanceof Swap && cell.target < gate.target) {
                    this.selectedGate = {
                        target: cell.target,
                        time: gate.time,
                    };
                } else if (cell instanceof MultiQubitGateCell) {
                    this.selectedGate = {
                        target: cell.target,
                        time: gate.time,
                    };
                } else {
                    this.selectedGate = gate;
                }
            }
        }
        this.updateBoard();
    }

    /**
     * Sample and add a new measurement to beginning of measurements.
     */
    handleMeasure() {
        const measurement = this.simulator.sample(this.qubits);
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

    handleExport(type: string) {
        if (type === "probabilities") {
            const res = this.simulator.result;
            if (res) {
                copyToClipboard(JSON.stringify(res));
                this.showNotification($localize`copied`);
            }
        } else if (type === "stateVector") {
            const state = this.simulator.stateVector;
            if (state) {
                copyToClipboard(JSON.stringify(state));
                this.showNotification($localize`copied`);
            }
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

    /**
     * Add gates to board.
     * @param circuit gates to add to board
     */
    addInitialGates(circuit: ICircuit) {
        if (!circuit) {
            return;
        }
        if (!this.board) {
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
                    gateData.editable,
                    gateData.controls,
                    gateData.antiControls
                );
            } else {
                if (!this.gateService.getGate(gateData.name)) {
                    this.showErrorMessage(
                        $localize`Tried to add gate to circuit that is not part of known gates: ${gateData.name}`
                    );
                    continue;
                }

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
                    this.board.set(gateData.target, gateData.time, gate);
                }

                for (const controlTarget of gateData.controls) {
                    if (controlTarget === gateData.target) {
                        continue;
                    }
                    // would go in cell that is occupied by multi-qubit gate
                    if (
                        size > 1 &&
                        gateData.target <= controlTarget &&
                        controlTarget < gateData.target + size
                    ) {
                        continue;
                    }

                    const control = new Control(
                        gateData.target,
                        gateData.editable,
                        false
                    );
                    this.board.set(controlTarget, gateData.time, control);
                }

                for (const controlTarget of gateData.antiControls) {
                    if (controlTarget === gateData.target) {
                        continue;
                    }
                    // would go in cell that is occupied by multi-qubit gate
                    if (
                        size > 1 &&
                        gateData.target <= controlTarget &&
                        controlTarget < gateData.target + size
                    ) {
                        continue;
                    }

                    const control = new Control(
                        gateData.target,
                        gateData.editable,
                        true
                    );
                    this.board.set(controlTarget, gateData.time, control);
                }
            }
        }
    }

    showErrorMessage(message: string) {
        this.errorString = message;
    }

    initializeQubits(reset: boolean) {
        if (this.markup.nQubits < 1) {
            this.showErrorMessage(
                $localize`invalid nQubits value ${this.markup.nQubits}`
            );
            return false;
        }

        if (
            this.markup.qubits &&
            this.markup.qubits.length !== this.markup.nQubits
        ) {
            this.showErrorMessage(
                $localize`Got incorrect amount of qubits ${this.markup.qubits.length}. There needs to be ${this.markup.nQubits} qubits.`
            );
            return false;
        }

        this.qubits = [];
        const userInput = this.attrsall.state?.userInput;

        for (let i = 0; i < this.markup.nQubits; i++) {
            let value = 0;
            let name = `q[${i}]`;
            let editable = true;

            if (this.markup.qubits) {
                const q = this.markup.qubits[i];
                if (q.name !== undefined && q.name !== null) {
                    name = q.name;
                }
                if (q.value !== undefined && q.value !== null) {
                    if (q.value === 1 || q.value === 0) {
                        value = q.value;
                    } else {
                        this.showErrorMessage(
                            $localize`Got incorrect value for qubit: ${q.value}. Value needs to be either 0 or 1.`
                        );
                        return false;
                    }
                }
                if (q.editable === false) {
                    editable = false;
                }
            }

            // when resetting, use original value.
            // if userInput isn't the same length as nQubits then yaml was edited and
            // existing answers with different nQubits value wouldn't work
            if (
                userInput &&
                userInput.length === this.markup.nQubits &&
                !reset
            ) {
                value = userInput[i];
            }

            this.qubits.push(
                new Qubit(value, name, editable, this.circuitOptions)
            );
        }
        return true;
    }

    /**
     * Initializes board, qubits and outputs.
     * @param reset whether this call is to reset board to initial state
     */
    initializeBoard(reset: boolean) {
        const qubitInitSuccess = this.initializeQubits(reset);
        if (!qubitInitSuccess) {
            return;
        }

        this.board = new QuantumBoard(
            this.markup.nQubits,
            this.markup.nMoments,
            this.markup
        );

        try {
            this.gateService.registerUserDefinedGates(
                this.markup.gates,
                this.markup.customGates
            );
        } catch (error) {
            this.showErrorMessage((error as Error).message);
            return;
        }

        const userCircuit = this.attrsall.state?.userCircuit;

        if (userCircuit && !reset) {
            this.addInitialGates(userCircuit);
        } else if (this.markup.initialCircuit) {
            this.addInitialGates(this.markup.initialCircuit);
        }

        this.qubitOutputs = [];

        if (
            this.markup.outputNames &&
            this.markup.outputNames.length !== this.markup.nQubits
        ) {
            this.showErrorMessage(
                $localize`Got incorrect amount of outputNames: ${this.markup.outputNames.length}. There needs to be ${this.markup.nQubits} names.`
            );
        }
        for (let i = 0; i < this.markup.nQubits; i++) {
            if (
                this.markup.outputNames &&
                this.markup.outputNames.length === this.markup.nQubits
            ) {
                this.qubitOutputs.push({
                    value: "0",
                    probability: 0,
                    probabilityText: "0",
                    name: this.markup.outputNames[i],
                });
            } else {
                this.qubitOutputs.push({
                    value: "0",
                    probability: 0,
                    probabilityText: "0",
                });
            }
        }
    }

    /**
     * Set qubit output value based on random sample from output's distribution.
     */
    setQubitOutputs() {
        this.qubitOutputs = [];
        const vals = this.simulator.sample(this.qubits);
        if (!vals) {
            return;
        }

        this.qubitOutputs = vals.output.split("").map((bit) => ({
            value: bit,
            probability: 0,
            probabilityText: "0",
        }));

        if (this.showOutputBits) {
            const outputProbabilities = this.simulator.getOutputProbabilities();
            if (outputProbabilities) {
                for (let i = 0; i < outputProbabilities.length; i++) {
                    this.qubitOutputs[i].probability = outputProbabilities[i];
                    this.qubitOutputs[i].probabilityText =
                        outputProbabilities[i].toFixed(2) + "%";
                }
            }
        }

        // set names but only if there is right amount of them
        if (
            this.markup.outputNames &&
            this.markup.outputNames.length === this.markup.nQubits
        ) {
            for (let i = 0; i < this.qubitOutputs.length; i++) {
                this.qubitOutputs[i].name = this.markup.outputNames[i];
            }
        }
    }

    initializeSimulator() {
        if (!this.board) {
            return;
        }
        if (this.markup.simulate === "browser") {
            this.simulator = new BrowserQuantumCircuitSimulator(
                this.gateService,
                this.board
            );
        } else if (this.markup.simulate === "server" && !Users.isLoggedIn()) {
            this.simulator = new BrowserQuantumCircuitSimulator(
                this.gateService,
                this.board
            );
            this.showErrorMessage(
                $localize`Using browser simulator because user isn't logged in: ${
                    Users.getCurrent().name
                }`
            );
        } else {
            this.simulator = new ServerQuantumCircuitSimulator(
                this.http,
                this.gateService,
                this.board,
                this.serializerService,
                this.markup.customGates
            );
        }

        void this.runSimulation();
    }

    /**
     * Compute sizes for board cells based on available space and number of cells.
     */
    setSizes() {
        if (!this.board) {
            return;
        }
        // qubit name | qubit bit value | nMoments * space for gate | measure-logo | output bit | maybe output name
        const extraSpaces = this.markup.outputNames ? 5 : 4;
        // try to fit everything on screen
        let baseSize =
            this.qcContainer.nativeElement.clientWidth /
            (this.markup.nMoments + extraSpaces);
        // don't make gates excessively large
        baseSize = Math.min(50, baseSize);
        // don't make gates too small
        baseSize = Math.max(baseSize, 30);

        const useBraket = this.markup.qubitNotation === "braket";

        const colWidths = [];
        for (let momentI = 0; momentI < this.markup.nMoments; momentI++) {
            let maxColWidth = baseSize;
            for (let qubitI = 0; qubitI < this.markup.nQubits; qubitI++) {
                const cell = this.board.board[qubitI][momentI];
                if (cell instanceof Gate || cell instanceof MultiQubitGate) {
                    const w = this.gateService.getTextWidth(cell.name);
                    maxColWidth = Math.max(w * 1.25, maxColWidth);
                }
            }
            colWidths.push(maxColWidth);
        }
        const colWidthSums: number[] = [];
        if (colWidths.length > 0) {
            colWidthSums.push(colWidths[0]);
            for (let i = 1; i < colWidths.length; i++) {
                colWidthSums.push(colWidthSums[i - 1] + colWidths[i]);
            }
        }
        const colWidthsSum = colWidths.reduce((a, b) => a + b, 0);

        const maxGateNameWidth = this.gateService
            .getGateNames()
            .map((name) => this.gateService.getTextWidth(name))
            .reduce((p, c) => Math.max(p, c), baseSize);

        this.circuitOptions = {
            baseSize: baseSize,
            baseWidth: maxGateNameWidth * 1.25,
            gateSize: 0.8 * baseSize,
            gateWidth: maxGateNameWidth,
            colors: {
                dark: "black",
                medium: "grey",
                light: "white",
            },
            gateColors: this.circuitOptions.gateColors,
            useBraket: useBraket,
            timeAxisHeight: this.circuitOptions.timeAxisHeight,
            gateBorderRadius: this.circuitOptions.gateBorderRadius,
            columnWidths: colWidths,
            columnWidthsSum: colWidthsSum,
            columnWidthSums: colWidthSums,
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
            qubit.updateNotation(this.circuitOptions);
        }
    }

    /**
     * Check if there's any gate which info can be viewed.
     */
    checkViewableGateInfo() {
        for (const gate of this.gateService.getGates()) {
            if (!this.isHiddenGateInfo(gate.name)) {
                return true;
            }
        }
        return false;
    }

    ngOnInit(): void {
        super.ngOnInit();

        this.gateService.setMarkup(this.markup);

        this.initializeBoard(false);

        this.initializeSimulator();

        const item = genericglobals().curr_item;

        this.hasEditRights = item?.rights.editable ?? false;

        this.applyPolyfill();

        // don't render unnecessary height for gate info if there isn't any viewable gate info
        this.isToolboxStaticHeight = this.checkViewableGateInfo();

        this.vctrl.addTimComponent(this);
    }

    private applyPolyfill() {
        // TODO: This should be a TIM global polyfill instead of per-component
        if (QuantumCircuitComponent.dndPolyfillApplied) {
            return;
        }
        QuantumCircuitComponent.dndPolyfillApplied = true;
        // apply polyfills to make drag-and-drop work for touch screen devices
        polyfill({
            // Use this to make use of the scroll behaviour.
            dragImageTranslateOverride:
                scrollBehaviourDragImageTranslateOverride,
            forceApply: isIOS(),
        });

        // iOS>=10 supports passive event listeners
        // but make sure to catch or check passive event listener support
        // regarding this code running on other platforms.
        window.addEventListener("touchmove", () => {}, {passive: false});
        this.vctrl.addTimComponent(this);
    }

    getAttributeType() {
        return QuantumCircuitFields;
    }

    getDefaultMarkup() {
        return {};
    }

    getContent() {
        return ""; // TODO
    }

    isUnSaved() {
        return false; // TODO
    }
}

@NgModule({
    declarations: [
        QuantumCircuitComponent,
        QuantumGateMenuComponent,
        QuantumToolboxComponent,
        QuantumCircuitBoardComponent,
        QuantumCellComponent,
        QuantumStatsComponent,
        QuantumErrorComponent,
    ],
    exports: [QuantumCircuitComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        NgChartsModule,
        TimUtilityModule,
        FormsModule,
        PurifyModule,
        InstanceofModule,
    ],
})
export class QuantumCircuitModule implements DoBootstrap {
    ngDoBootstrap() {}
}

registerPlugin(
    "tim-quantum-circuit",
    QuantumCircuitModule,
    QuantumCircuitComponent
);
