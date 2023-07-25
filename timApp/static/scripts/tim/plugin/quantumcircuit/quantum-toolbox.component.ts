import type {OnChanges, OnInit, SimpleChanges} from "@angular/core";
import {Component, Input} from "@angular/core";
import type {FormatOptions, Matrix} from "mathjs";
import {format} from "mathjs";
import {ActiveGateInfo} from "tim/plugin/quantumcircuit/quantum-circuit.component";

@Component({
    selector: "tim-quantum-toolbox",
    template: `
        <div class="toolbox">
            <div class="toolbox-item">
                <pre>
                    <code>{{matrixString}}</code>
                </pre>
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

    constructor() {}

    ngOnInit(): void {}

    formatMatrixAsString(m: Matrix) {
        const arr = m.toArray();
        const formatOptions: FormatOptions = {
            precision: 2,
        };
        let res = "";
        arr.forEach((row) => {
            if (Array.isArray(row)) {
                res += row
                    .map((value) => format(value, formatOptions))
                    .join(" ");
                res += "\n";
            }
        });
        return res;
    }

    ngOnChanges(changes: SimpleChanges): void {
        if (!this.activeGateInfo) {
            return;
        }
        this.matrixString = "";
        this.matrixString = this.formatMatrixAsString(
            this.activeGateInfo.matrix
        );
    }
}
