import type {OnChanges, OnInit, SimpleChanges} from "@angular/core";
import {Component, EventEmitter, Input, Output} from "@angular/core";
import type {TableCellData} from "tim/plugin/quantumcircuit/active-gate";
import {
    CircuitActiveGateInfo,
    ActiveGateInfo,
} from "tim/plugin/quantumcircuit/active-gate";

@Component({
    selector: "tim-quantum-toolbox",
    template: `
        <div class="toolbox">
            <div class="gate-info" *ngIf="activeGateInfo">
                <div class="gate-info-header">
                    <div>{{activeGateInfo.description}}</div>
                    <div *ngIf="showEditableInfo" class="text-info gate-editable-message">
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
                <p *ngIf="qubitString">Qubitti: {{qubitString}}</p>
                <p *ngIf="timeString">Aika: {{timeString}}</p>
                <p *ngIf="controlsString" class="controls">
                    Kontrollit:
                    {{controlsString}}
                </p>

                <p *ngIf="swapString">
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

    showEditableInfo: boolean = false;

    @Output()
    close = new EventEmitter<void>();

    constructor() {}

    ngOnInit(): void {}

    ngOnChanges(changes: SimpleChanges): void {
        if (!this.activeGateInfo) {
            return;
        }
        this.matrixTable = this.activeGateInfo.formatMatrixAsTable();
        if (this.activeGateInfo instanceof CircuitActiveGateInfo) {
            this.controlsString = this.activeGateInfo.formatControlsAsString();
            this.swapString = this.activeGateInfo.formatSwapAsString();

            this.qubitString = this.activeGateInfo.formatQubitAsString();
            this.timeString = this.activeGateInfo.formatTimeAsString();
            this.showEditableInfo = !this.activeGateInfo.editable;
        } else {
            this.controlsString = "";
            this.swapString = "";
            this.qubitString = "";
            this.timeString = "";
        }
    }

    handleClose() {
        this.close.emit();
    }
}
