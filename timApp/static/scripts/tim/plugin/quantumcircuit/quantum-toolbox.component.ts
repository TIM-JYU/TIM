import type {OnChanges, OnInit, SimpleChanges} from "@angular/core";
import {Component, EventEmitter, Input, Output} from "@angular/core";
import type {TableCellData} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import type {FormatOptions, Matrix} from "mathjs";
import {format} from "mathjs";
import type {Qubit} from "tim/plugin/quantumcircuit/qubit";

export class ActiveGateInfo {
    matrix: Matrix;
    name: string;
    target: number;
    time: number;
    controls: number[];
    qubits: Qubit[];
    description: string;
    editable: boolean;
    swap?: [number, number];

    /**
     * @param target the index of qubit related to this gate
     * @param time the time moment related to this gate
     * @param name the name of gate
     * @param mat actual matrix presentation of the gate
     * @param controls indices of qubits that control this gate if any or just an empty array
     * @param qubits qubit objects used to get names of qubits
     * @param description more detailed info about gate than its name
     * @param swap pair of qubit indices for swap gate
     * @param editable whether this gate can be edited or not
     */
    constructor(
        target: number,
        time: number,
        name: string,
        mat: Matrix,
        controls: number[],
        qubits: Qubit[],
        description: string,
        editable: boolean,
        swap?: [number, number]
    ) {
        this.target = target;
        this.time = time;
        this.name = name;
        this.matrix = mat;
        this.qubits = qubits;
        this.controls = controls;
        this.description = description;
        this.editable = editable;
        this.swap = swap;
    }

    /**
     * Formats the matrix into an array with elements as strings
     * to be used in html table.
     */
    formatMatrixAsTable() {
        const arr = this.matrix.toArray();
        const formatOptions: FormatOptions = {
            precision: 2,
        };
        const res: TableCellData[][] = [];
        arr.forEach((row) => {
            if (Array.isArray(row)) {
                const rowValues = row.map((v) => ({
                    rounded: format(v, formatOptions).replace(/\s/g, ""),
                    long: format(v).replace(/\s/g, ""),
                }));
                res.push(rowValues);
            }
        });
        return res;
    }

    formatControlsAsString() {
        return this.controls.map((ci) => this.qubits[ci].name).join(", ");
    }

    formatSwapAsString() {
        if (!this.swap) {
            return "";
        }
        return (
            this.qubits[this.swap[0]].name +
            ", " +
            this.qubits[this.swap[1]].name
        );
    }

    formatTimeAsString() {
        return this.time.toString();
    }

    formatQubitAsString() {
        return this.qubits[this.target].name;
    }
}

@Component({
    selector: "tim-quantum-toolbox",
    template: `
        <div class="toolbox">
            <div class="gate-info" *ngIf="activeGateInfo">
                <div class="gate-info-header">
                    <div>{{activeGateInfo.description}}</div>
                    <div *ngIf="!activeGateInfo.editable" class="text-info gate-editable-message">
                        <span class="glyphicon glyphicon-info-sign" aria-hidden="true"></span>
                        Tätä porttia ei pysty muokkaamaan
                    </div>
                    <button title="piilota" type="button" class="btn btn-default btn-sm" (click)="handleClose()">
                        <span class="glyphicon glyphicon-remove"></span>
                    </button>
                </div>
                <hr>
                Matriisi:
                <table>
                    <tbody>
                    <tr *ngFor="let row of matrixTable">
                        <td [attr.title]="value.long" *ngFor="let value of row">{{value.rounded}}</td>
                    </tr>
                    </tbody>
                </table>
                <p>Qubitti: {{qubitString}}</p>
                <p>Aika: {{timeString}}</p>
                <p *ngIf="controlsString" class="controls">
                    Kontrollit:
                    {{controlsString}}
                </p>

                <p *ngIf="activeGateInfo.swap">
                    Swap:
                    {{swapString}}
                </p>
            </div>

            <a href="https://tim.jyu.fi/view/tim/TIMin-kehitys/pluginien-suunnittelu/kvantti"
               target="_blank">
                <span class="glyphicon glyphicon-question-sign help-icon" title="Instructions"></span>
            </a>

        </div>
    `,
    styleUrls: ["./quantum-toolbox.component.scss"],
})
export class QuantumToolboxComponent implements OnInit, OnChanges {
    @Input()
    activeGateInfo?: ActiveGateInfo;

    matrixTable: TableCellData[][] = [];
    controlsString: string = "";
    swapString: string = "";

    qubitString: string = "";
    timeString: string = "";

    @Output()
    close = new EventEmitter<void>();

    constructor() {}

    ngOnInit(): void {}

    ngOnChanges(changes: SimpleChanges): void {
        if (!this.activeGateInfo) {
            return;
        }
        this.matrixTable = this.activeGateInfo.formatMatrixAsTable();
        this.controlsString = this.activeGateInfo.formatControlsAsString();
        this.swapString = this.activeGateInfo.formatSwapAsString();

        this.qubitString = this.activeGateInfo.formatQubitAsString();
        this.timeString = this.activeGateInfo.formatTimeAsString();
    }

    handleClose() {
        this.close.emit();
    }
}
