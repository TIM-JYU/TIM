import type {OnChanges, SimpleChanges} from "@angular/core";
import {Component, Input} from "@angular/core";
import {equal} from "mathjs";
import {IServerError} from "tim/plugin/quantumcircuit/quantum-circuit.component";

interface AnswerIncorrectRow {
    output: string;
    expected: number;
    actual: number;
    correct: boolean;
}

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
                <label>
                    <ng-container i18n>Show all rows</ng-container>
                    <input type="checkbox" [(ngModel)]="answerIncorrectShowAll" (ngModelChange)="createAnswerDisplayData()">
                </label>
                
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
                        <tr *ngFor="let row of answerIncorrectRows">
                            <td class="answer-table-td">{{row.output}}</td>
                            <td class="answer-table-td"
                                [title]="row.expected">{{row.expected | number: '1.1-3'}}</td>
                            <td class="answer-table-td" [title]="row.actual"
                                [class.incorrect]="!row.correct">{{row.actual | number: '1.1-3'}}</td>
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

            <div *ngIf="error.errorType === 'too-many-moments'">
                <p i18n>Couldn't simulate circuit because there were too many moments</p>
                <p i18n>Circuit contains {{error.moments}} moments</p>
                <p i18n>But max is {{error.maxMoments}}</p>
            </div>

            <div *ngIf="error.errorType === 'regex-invalid'">
                <p i18n>modelInput contains invalid regex pattern</p>
                <p>{{error.regex}}</p>
            </div>

            <div *ngIf="error.errorType === 'simulation-timed-out'">
                <p i18n>Simulator timed out</p>
                <p i18n>Simulation time can be extended up to 25 seconds using maxRunTimeout attribute.</p>
            </div>
            
            <div *ngIf="error.errorType === 'too-long-timeout'">
                <p i18n>The value of maxRunTimeout is too large</p>
                <p i18n>maxRunTimeout has value: {{error.timeout}}</p>
                <p i18n>But maximum is: {{error.maxTimeout}}</p>
            </div>

            <div *ngIf="error.errorType === 'circuit-uninterpretable'">
                <p i18n>Couldn't interpret circuits</p>
                <p>{{error.message}}</p>
            </div>
        </div>
    `,
    styleUrls: ["quantum-error.component.scss"],
})
export class QuantumErrorComponent implements OnChanges {
    @Input()
    error!: IServerError;

    answerIncorrectShowAll: boolean = false;
    answerIncorrectRows!: AnswerIncorrectRow[];

    indexToBitstring(i: number, nQubits: number) {
        return i.toString(2).padStart(nQubits, "0");
    }

    /**
     * Bits in result should be in reverse order in terms of qubits.
     * probability(001) -> probability(100)
     * @param result result with qubit order changed
     */
    protected reverseResultQubitOrder(result: number[]) {
        const rev = [];
        const nQubits = Math.floor(Math.log2(result.length));
        for (let i = 0; i < 2 ** nQubits; i++) {
            const bitString = this.indexToBitstring(i, nQubits);
            const bitStringReversed = bitString.split("").reverse().join("");
            const j = parseInt(bitStringReversed, 2);
            rev.push(result[j]);
        }
        return rev;
    }

    /**
     * Create fields necessary to display answer error
     */
    createAnswerDisplayData() {
        if (this.error.errorType === "answer-incorrect") {
            const nQubits = Math.floor(Math.log2(this.error.actual.length));

            this.answerIncorrectRows = [];
            const actual = this.reverseResultQubitOrder(this.error.actual);
            const expected = this.reverseResultQubitOrder(this.error.expected);
            for (let i = 0; i < actual.length; i++) {
                const actualI = actual[i];
                const expectedI = expected[i];
                let correct = true;
                const res = equal(actualI, expectedI);
                if (typeof res === "boolean") {
                    correct = res;
                }

                if (this.answerIncorrectShowAll || !correct) {
                    this.answerIncorrectRows.push({
                        output: this.indexToBitstring(i, nQubits)
                            .split("")
                            .reverse()
                            .join(""),
                        expected: expectedI,
                        actual: actualI,
                        correct: correct,
                    });
                }
            }
        }
    }

    ngOnChanges(changes: SimpleChanges): void {
        this.createAnswerDisplayData();
    }
}
