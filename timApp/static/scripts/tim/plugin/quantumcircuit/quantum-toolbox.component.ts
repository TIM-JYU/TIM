import type {OnChanges, OnInit, SimpleChanges} from "@angular/core";
import {Component, EventEmitter, Input, Output} from "@angular/core";
import {ActiveGateInfo} from "tim/plugin/quantumcircuit/quantum-circuit.component";

@Component({
    selector: "tim-quantum-toolbox",
    template: `
        <div class="toolbox">
            <div class="toolbox-item" *ngIf="activeGateInfo">
                <div class="gate-info-header">
                    <div>{{activeGateInfo.description}}</div>
                    <button title="piilota" type="button" class="btn btn-default btn-sm" (click)="handleClose()">
                        <span class="glyphicon glyphicon-remove"></span>
                    </button>
                </div>
                matriisi:
                <pre>
                    <code>{{matrixString}}</code>
                </pre>
                <p *ngIf="controlsString" class="controls">
                    kontrollit:
                    {{controlsString}}
                </p>

                <p *ngIf="activeGateInfo.swap">
                    swap: 
                    {{swapString}}
                </p>
            </div>

            <a href="https://tim.jyu.fi/view/tim/TIMin-kehitys/pluginien-suunnittelu/kvantti"
               class="toolbox-item"
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

    matrixString: string = "";
    controlsString: string = "";
    swapString: string = "";

    @Output()
    close = new EventEmitter<void>();

    constructor() {}

    ngOnInit(): void {}

    ngOnChanges(changes: SimpleChanges): void {
        if (!this.activeGateInfo) {
            return;
        }
        this.matrixString = this.activeGateInfo.formatMatrixAsString();
        this.controlsString = this.activeGateInfo.formatControlsAsString();
        this.swapString = this.activeGateInfo.formatSwapAsString();
    }

    handleClose() {
        this.close.emit();
    }
}
