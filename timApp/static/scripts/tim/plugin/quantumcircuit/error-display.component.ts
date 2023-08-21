import {Component, Input} from "@angular/core";
import {IServerError} from "tim/plugin/quantumcircuit/quantum-circuit.component";

@Component({
    selector: "tim-error-display",
    template: `
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
            <p>{{error.bitstring}}</p>
            <p i18n>Probabilities should've been:</p>
            <p>{{error.expected}}</p>
            <p i18n>But were:</p>
            <p>{{error.actual}}</p>

        </div>
    `,
})
export class ErrorDisplayComponent {
    @Input()
    error!: IServerError;
}
