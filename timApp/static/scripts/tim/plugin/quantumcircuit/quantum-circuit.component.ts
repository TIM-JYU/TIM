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
import {
    FlatteningPipe,
    QuantumCircuitBoardComponent,
} from "tim/plugin/quantumcircuit/quantum-circuit-board.component";
import {QuantumStatsComponent} from "tim/plugin/quantumcircuit/quantum-stats.component";
import {NgChartsModule} from "ng2-charts";

export interface Gate {
    time: number;
    target: number;
    name?: string;
    x: number;
    y: number;
    w: number;
    h: number;
    textX: number;
    textY: number;
}

export interface Qubit {
    value: number;
    name: string;
    w: number;
    h: number;
    id: number;
}

export interface QubitOutput {
    text: string;
    value: number;
    w: number;
    h: number;
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
    // whether to use bra-ket notation for input qubits or just (0,1)
    useBraket: boolean;
    // timeAxisHeight: height of first row showing time steps 0,1,2...
    timeAxisHeight: number;
    // border radius value for gates (rounded corners)
    gateBorderRadius: number;
}

@Component({
    selector: "tim-quantum-circuit",
    template: `
        <div #qcContainer>
            <div class="top-menu">
                <tim-quantum-gate-menu [circuitStyleOptions]="circuitStyleOptions"></tim-quantum-gate-menu>
                <tim-quantum-toolbox></tim-quantum-toolbox>
            </div>

            <div class="circuit">
                <tim-quantum-circuit-board
                        [board]="board" [qubits]="qubits"
                        [qubitOutputs]="qubitOutputs"
                        (qubitChange)="handleQubitChange($event)"
                        [nMoments]="nMoments"
                        (gateDrop)="handleGateDrop($event)"
                        [circuitStyleOptions]="circuitStyleOptions"
                ></tim-quantum-circuit-board>
            </div>

            <div class="stats">
                <tim-quantum-stats [measurements]="measurements"
                                   (clear)="handleClearMeasurements()"></tim-quantum-stats>
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

    circuitStyleOptions!: CircuitStyleOptions;

    get nQubits() {
        return this.markup.nQubits;
    }

    get nMoments() {
        return this.markup.nMoments;
    }

    board: Gate[][] = [];

    measurements: Measurement[] = [];

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
    }

    handleGateDrop(gate: Gate) {
        const {time, target} = gate;
        this.board[target][time] = gate;
    }

    /**
     * Deletes done measurements.
     */
    handleClearMeasurements() {
        this.measurements = [];
    }

    initializeBoard() {
        this.board = [];

        const size = this.circuitStyleOptions.baseSize;
        this.qubits = [];
        for (let i = 0; i < this.nQubits; i++) {
            this.qubits.push({
                name: `q[${i}]`,
                value: 0,
                w: size,
                h: size,
                id: i,
            });
        }

        for (let i = 0; i < this.nQubits; i++) {
            const row: Gate[] = [];
            for (let j = 0; j < this.nMoments; j++) {
                row.push({
                    x: j * size,
                    y: i * size,
                    time: j,
                    target: i,
                    name: undefined,
                    w: size,
                    h: size,
                    textX: j * size + size / 2,
                    textY: i * size + size / 2,
                });
            }
            this.board.push(row);
        }

        this.board[0][0].name = "H";

        this.qubitOutputs = [];
        for (let i = 0; i < this.nQubits; i++) {
            this.qubitOutputs.push({
                text: `out ${i}`,
                value: 0,
                w: size,
                h: size,
            });
        }
    }

    ngAfterViewInit() {
        this.initializeBoard();
        this.measurements = [
            {
                input: "000",
                output: "000",
            },
            {
                input: "000",
                output: "011",
            },
        ];
    }

    ngOnInit(): void {
        super.ngOnInit();
        this.circuitStyleOptions = {
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
        FlatteningPipe,
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
