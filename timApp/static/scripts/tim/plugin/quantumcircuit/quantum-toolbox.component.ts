import type {OnChanges, OnInit} from "@angular/core";
import {
    Component,
    EventEmitter,
    Input,
    Output,
    ChangeDetectionStrategy,
} from "@angular/core";
import type {TableCellData} from "tim/plugin/quantumcircuit/active-gate";
import {
    CircuitActiveGateInfo,
    ActiveGateInfo,
} from "tim/plugin/quantumcircuit/active-gate";

@Component({
    selector: "tim-quantum-toolbox",
    template: `
        <div class="toolbox" [class.toolbox-static-height]="isToolboxStaticHeight">
          @if (activeGateInfo && !activeGateInfo.hide) {
            <div class="gate-info">
              <div class="gate-info-header">
                <div>{{activeGateInfo.description}}</div>
                @if (showEditableInfo) {
                  <div class="text-info gate-editable-message">
                    <span class="glyphicon glyphicon-info-sign" aria-hidden="true"></span>
                    <ng-container i18n>This gate can't be edited</ng-container>
                  </div>
                }
                <button i18n-title title="hide" type="button" class="btn btn-default btn-sm" (click)="handleClose()">
                  <span class="glyphicon glyphicon-remove"></span>
                </button>
              </div>
              <hr>
                <ng-container i18n>Matrix</ng-container>:
                <table>
                  <tbody>
                    @for (row of matrixTable; track row) {
                      <tr>
                        @for (value of row; track value) {
                          <td [attr.title]="value.long">{{value.rounded}}</td>
                        }
                      </tr>
                    }
                  </tbody>
                </table>
                @if (qubitString) {
                  <p><ng-container i18n>Qubit</ng-container>: {{qubitString}}</p>
                }
                @if (timeString) {
                  <p><ng-container i18n>Time</ng-container>: {{timeString}}</p>
                }
                @if (controlsString) {
                  <p class="controls">
                    <ng-container i18n>Controls</ng-container>:
                    {{controlsString}}
                  </p>
                }
                @if (antiControlsString) {
                  <p class="anti-controls">
                    <ng-container i18n>Anti-controls</ng-container>:
                    {{antiControlsString}}
                  </p>
                }
                @if (swapString) {
                  <p>
                    Swap:
                    {{swapString}}
                  </p>
                }
              </div>
            }
        
            <a href="/view/tim/ohjeita/kvanttisimulaattori"
              target="_blank">
              <span class="glyphicon glyphicon-question-sign help-icon" i18n-title title="Instructions"></span>
            </a>
        
          </div>
        `,
    styleUrls: ["./quantum-toolbox.component.scss"],
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class QuantumToolboxComponent implements OnInit, OnChanges {
    @Input()
    activeGateInfo?: ActiveGateInfo;

    @Input()
    isToolboxStaticHeight: boolean = true;

    matrixTable: TableCellData[][] = [];
    controlsString: string = "";
    antiControlsString: string = "";
    swapString: string = "";

    qubitString: string = "";
    timeString: string = "";

    showEditableInfo: boolean = false;

    @Output()
    close = new EventEmitter<void>();

    constructor() {}

    ngOnInit(): void {}

    ngOnChanges(): void {
        if (!this.activeGateInfo) {
            return;
        }
        this.matrixTable = this.activeGateInfo.formatMatrixAsTable();
        if (this.activeGateInfo instanceof CircuitActiveGateInfo) {
            this.controlsString = this.activeGateInfo.formatControlsAsString();
            this.antiControlsString =
                this.activeGateInfo.formatAntiControlsAsString();
            this.swapString = this.activeGateInfo.formatSwapAsString();

            this.qubitString = this.activeGateInfo.formatQubitAsString();
            this.timeString = this.activeGateInfo.formatTimeAsString();
            this.showEditableInfo = !this.activeGateInfo.editable;
        } else {
            this.controlsString = "";
            this.antiControlsString = "";
            this.swapString = "";
            this.qubitString = "";
            this.timeString = "";
        }
    }

    handleClose() {
        this.close.emit();
    }
}
