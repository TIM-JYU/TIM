import moment from "moment";
import {Component, EventEmitter, Input, Output} from "@angular/core";

@Component({
    selector: "tim-duration-picker",
    template: `
        <div class="form-group" timErrorState>
            <div class="col-md-6">
                <input [(ngModel)]="amount"
                       (ngModelChange)="amountChange.emit($event)"
                       class="form-control"
                       name="durAmount"
                       placeholder="{{ required ? '' : 'Optional' }}"
                       type="number"
                       min="0"
                       step="0.1"
                       [required]="!disabled && required"
                       [disabled]="disabled"/>
            </div>
            <div class="col-md-6">
                <select class="form-control"
                        [(ngModel)]="type"
                        (ngModelChange)="typeChange.emit($event)"
                        [disabled]="disabled">
                    <option *ngFor="let d of durationTypes" [value]="d">{{d}}</option>
                </select>
            </div>
            <tim-error-message></tim-error-message>
        </div>
    `,
})
export class DurationPickerComponent {
    durationTypes: moment.unitOfTime.Base[];
    @Input() amount?: number;
    @Output() amountChange = new EventEmitter<number>();
    @Input() disabled!: boolean;
    @Input() required!: boolean;
    @Input() type!: DurationChoice;
    @Output() typeChange = new EventEmitter<DurationChoice>();

    constructor() {
        this.durationTypes = durationTypes;
    }
}

export type DurationChoice =
    | "seconds"
    | "minutes"
    | "hours"
    | "days"
    | "weeks"
    | "months"
    | "years";

export const durationTypes: DurationChoice[] = [
    "seconds",
    "minutes",
    "hours",
    "days",
    "weeks",
    "months",
    "years",
];
