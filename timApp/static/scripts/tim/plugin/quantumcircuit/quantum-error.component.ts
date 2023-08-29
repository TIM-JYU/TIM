import type {OnChanges, SimpleChanges} from "@angular/core";
import {Component, Input} from "@angular/core";
import {equal} from "mathjs";
import {IServerError} from "tim/plugin/quantumcircuit/quantum-circuit.component";

@Component({
    selector: "tim-quantum-error",
    template: `
        <div class="error-container">
            <div *ngIf="error.errorType === 'condition-not-satisfied'">
                <p i18n>Answer doesn't satisfy condition:</p>
                <p>{{error.condition}}</p>
                <p i18n>With values:</p>
                <p>{{error.values}}</p>
            </div>

            <div *ngIf="error.errorType === 'condition-not-interpretable'">
                <p i18n>Unable to interpret condition:</p>
                <p>{{error.condition}}</p>
            </div>

            <div *ngIf="error.errorType === 'condition-invalid'">
                <p i18n>Condition has invalid characters:</p>
                <p>{{error.condition}}</p>
            </div>

            <div *ngIf="error.errorType === 'matrix-incorrect'">
                <p i18n>Failed to parse matrix from input:</p>
                <p>{{error.matrix}}</p>
            </div>


            <div *ngIf="error.errorType === 'answer-incorrect'">
                <p i18n>Circuit gives wrong probabilities with input:</p>
                <div>{{error.bitstring}}</div>
                <p i18n>Output probabilities were:</p>
                <div class="table-container">
                    <table class="answer-table">
                        <thead>
                        <tr>
                            <th i18n>Output</th>
                            <th i18n>Expected</th>
                            <th i18n>Actual</th>
                        </tr>
                        </thead>

                        <tbody>
                        <tr *ngFor="let output of outputs; let i = index;">
                            <td class="answer-table-td">{{output}}</td>
                            <td class="answer-table-td"
                                [title]="error.expected[i]">{{error.expected[i] | number: '1.1-3'}}</td>
                            <td class="answer-table-td" [title]="error.actual[i]"
                                [class.incorrect]="!answersCorrect[i]">{{error.actual[i] | number: '1.1-3'}}</td>
                        </tr>
                        </tbody>

                    </table>
                </div>

            </div>

            <div *ngIf="error.errorType === 'too-many-qubits'">
                <p i18n>Couldn't simulate circuit because there were too many qubits</p>
                <p i18n>Circuit contains {{error.qubits}} qubits</p>
                <p i18n>But max is {{error.maxQubits}}</p>
            </div>

            <div *ngIf="error.errorType === 'regex-invalid'">
                <p i18n>modelInput contains invalid regex pattern</p>
                <p>{{error.regex}}</p>
            </div>
        </div>
    `,
    styleUrls: ["quantum-error.component.scss"],
})
export class QuantumErrorComponent implements OnChanges {
    @Input()
    error!: IServerError;

    answersCorrect!: boolean[];
    outputs!: string[];

    indexToBitstring(i: number, nQubits: number) {
        return i.toString(2).padStart(nQubits, "0");
    }

    /**
     * Create fields necessary to display answer error
     */
    createAnswerDisplayData() {
        if (this.error.errorType === "answer-incorrect") {
            const nQubits = Math.floor(Math.log2(this.error.actual.length));

            this.answersCorrect = [];
            this.outputs = [];
            for (let i = 0; i < this.error.actual.length; i++) {
                this.outputs.push(this.indexToBitstring(i, nQubits));

                const actual = this.error.actual[i];
                const expected = this.error.expected[i];
                const res = equal(actual, expected);
                if (typeof res === "boolean") {
                    this.answersCorrect.push(res);
                } else {
                    this.answersCorrect.push(true);
                }
            }
        }
    }

    ngOnChanges(changes: SimpleChanges): void {
        this.createAnswerDisplayData();
    }
}
