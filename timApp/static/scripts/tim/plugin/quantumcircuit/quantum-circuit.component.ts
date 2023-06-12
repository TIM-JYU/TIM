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
    text: string;
    value: number;
    x: number;
    y: number;
    w: number;
    h: number;
    textX: number;
    textY: number;
}

export interface QubitOutput {
    text: string;
    value: number;
    x: number;
    y: number;
    w: number;
    h: number;
    textX: number;
    textY: number;
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

@Component({
    selector: "tim-quantum-circuit",
    template: `
        <div #qcContainer>
            <div class="top-menu">
                <tim-quantum-gate-menu></tim-quantum-gate-menu>
                <tim-quantum-toolbox></tim-quantum-toolbox>
            </div>
            
            <tim-quantum-circuit-board [board]="board" [svgW]="qcContainer.offsetWidth" [qubits]="qubits" [svgH]="qcContainer.offsetWidth" [qubitOutputs]="qubitOutputs"></tim-quantum-circuit-board>
            
            <tim-quantum-stats></tim-quantum-stats>
            
            {{qcContainer.offsetHeight}} {{qcContainer.offsetWidth}}
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

    get nQubits() {
        return this.markup.nQubits;
    }

    get nMoments() {
        return this.markup.nMoments;
    }

    board: Gate[][] = [];

    ngAfterViewInit() {
        this.board = [];

        // input    moments       | output
        // size | nMoments * size | size
        const size =
            this.qcContainer.nativeElement.offsetWidth / (this.nMoments + 2);

        this.qubits = [];
        for (let i = 0; i < this.nQubits; i++) {
            this.qubits.push({
                text: `q[${i}]`,
                value: 0,
                w: size,
                h: size,
                x: 0,
                y: i * size,
                textX: size / 2,
                textY: i * size + size / 2,
            });
        }

        for (let i = 0; i < this.nQubits; i++) {
            const row: Gate[] = [];
            for (let j = 0; j < this.nMoments; j++) {
                row.push({
                    x: j * size + size,
                    y: i * size,
                    time: j,
                    target: i,
                    name: undefined,
                    w: size,
                    h: size,
                    textX: j * size + size / 2 + size,
                    textY: i * size + size / 2,
                });
            }
            this.board.push(row);
        }

        this.qubitOutputs = [];
        for (let i = 0; i < this.nQubits; i++) {
            this.qubitOutputs.push({
                text: `out ${i}`,
                value: 0,
                w: size,
                h: size,
                x: (this.nMoments + 1) * size,
                y: i * size,
                textX: (this.nMoments + 1) * size + size / 2,
                textY: i * size + size / 2,
            });
        }
    }

    ngOnInit(): void {
        super.ngOnInit();
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
    imports: [CommonModule, HttpClientModule],
})
export class QuantumCircuitModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin(
    "tim-quantum-circuit",
    QuantumCircuitModule,
    QuantumCircuitComponent
);
