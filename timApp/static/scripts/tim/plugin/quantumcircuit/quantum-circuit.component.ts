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
import {
    MeasurementsPipe,
    QuantumStatsComponent,
} from "tim/plugin/quantumcircuit/quantum-stats.component";
import {NgChartsModule} from "ng2-charts";
import {QuantumCircuitSimulator} from "tim/plugin/quantumcircuit/quantum-simulation";

export interface Gate {
    name: string;
}

export type Board = (Gate | undefined)[][];

export interface Qubit {
    value: number;
    name: string;
}

export interface QubitOutput {
    text: string;
    value: number;
}

/**
 * One input output pair measured from circuit
 */
export interface Measurement {
    input: string;
    output: string;
}

// All settings that are defined in the plugin markup YAML
const QuantumCircuitMarkup = t.intersection([
    t.partial({}),
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
                        [board]="board" [qubits]="qubits"
                        [qubitOutputs]="qubitOutputs"
                        [circuitStyleOptions]="circuitStyleOptions"
                        (qubitChange)="handleQubitChange($event)"
                        (gateDrop)="handleGateDrop($event)"
                        (gateMove)="handleGateMove($event)"
                        (gateRemove)="handleGateRemove($event)"
                ></tim-quantum-circuit-board>
            </div>

            <div class="stats">
                <tim-quantum-stats [measurements]="measurements"
                                   [quantumChartData]="quantumChartData"
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

    board: Board = [];

    measurements: Measurement[] = [];

    get nQubits() {
        return this.markup.nQubits;
    }

    get nMoments() {
        return this.markup.nMoments;
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

        this.simulator.run();
        this.quantumChartData = this.simulator.getProbabilities();
    }

    /**
     * Replaces cell of the board with gate
     * @param gate gate to put in cell
     */
    handleGateDrop(gate: GateDrop) {
        const {time, target} = gate;
        this.board[target][time] = {name: gate.name};

        this.simulator.run();
        this.quantumChartData = this.simulator.getProbabilities();
    }

    /**
     * Move gate from one cell to another.
     * @param gateMove object describing gate movement
     */
    handleGateMove(gateMove: GateMove) {
        const {
            from: {target: target1, time: time1},
            to: {target: target2, time: time2},
        } = gateMove;
        const name = this.board[target1][time1]?.name;
        if (name !== undefined) {
            this.board[target2][time2] = {
                name: name,
            };

            this.board[target1][time1] = undefined;

            this.simulator.run();
            this.quantumChartData = this.simulator.getProbabilities();
        }
    }

    /**
     * Removes gate from board.
     * @param gate gate to remove
     */
    handleGateRemove(gate: GatePos) {
        this.board[gate.target][gate.time] = undefined;

        this.simulator.run();
        this.quantumChartData = this.simulator.getProbabilities();
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
     * Initializes board, qubits and outputs.
     */
    initializeBoard() {
        this.board = [];

        this.qubits = [];
        for (let i = 0; i < this.nQubits; i++) {
            this.qubits.push({
                name: `q[${i}]`,
                value: 0,
            });
        }

        for (let i = 0; i < this.nQubits; i++) {
            const row: (Gate | undefined)[] = [];
            for (let j = 0; j < this.nMoments; j++) {
                row.push(undefined);
            }
            this.board.push(row);
        }

        // mock data
        this.board[0][0] = {name: "H"};
        this.board[2][3] = {name: "X"};

        this.qubitOutputs = [];
        for (let i = 0; i < this.nQubits; i++) {
            this.qubitOutputs.push({
                text: `out ${i}`,
                value: 0,
            });
        }
    }

    initializeSimulator() {
        this.simulator = new QuantumCircuitSimulator(this.board, this.qubits);

        this.simulator.run();

        this.quantumChartData = this.simulator.getProbabilities();
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
    ],
    exports: [QuantumCircuitComponent],
    imports: [CommonModule, HttpClientModule, NgChartsModule],
})
export class QuantumCircuitModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin(
    "tim-quantum-circuit",
    QuantumCircuitModule,
    QuantumCircuitComponent
);
