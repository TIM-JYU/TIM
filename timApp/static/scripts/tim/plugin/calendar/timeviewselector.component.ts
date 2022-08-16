/**
 * Component that controls the time period and the length
 * of the time slots that are shown to the user
 *
 * @author Miika Immonen
 * @author Terhi Kamula
 * @author Anssi Lepikko
 * @author Touko Miettinen
 * @author Joose Tikkanen
 * @license MIT
 * @date 24.5.2022
 */
import {Component, EventEmitter, Input, Output} from "@angular/core";

export const TIME_VIEW_SLOT_SIZES: number[] = [15, 20, 30, 60, 120];
export const TIME_VIEW_MORNING_HOURS: number[] = new Array(12)
    .fill(0)
    .map((_, i) => i);
export const TIME_VIEW_EVENING_HOURS: number[] = TIME_VIEW_MORNING_HOURS.map(
    (i) => i + 12
);

@Component({
    selector: "tim-time-view-selector",
    template: `
        <ng-container>
            <span i18n>Set view with timeslots of </span>
            <select [(ngModel)]="segmentDuration" (ngModelChange)="segmentDurationChange.emit($event)">
                <option *ngFor="let item of accuracies" [value]="item">{{ item | number:'2.0' }}</option>
            </select>
            <span i18n> minutes from </span>
            <select [(ngModel)]="startHour" (ngModelChange)="startHourChange.emit($event)">
                <option *ngFor="let hour of morningHours" [value]="hour">{{ hour | number:'2.0' }}</option>
            </select>
            <span i18n> to </span>
            <select [(ngModel)]="endHour" (ngModelChange)="endHourChange.emit($event)">
                <option *ngFor="let hour of eveningHours" [value]="hour">{{ hour | number:'2.0' }}</option>
            </select>
        </ng-container>`,
    styleUrls: ["calendar.component.scss"],
})
export class TimeViewSelectorComponent {
    @Input() segmentDuration: number = 20;
    @Input() startHour: number = 8;
    @Input() endHour: number = 20;
    @Output() segmentDurationChange = new EventEmitter<number>();
    @Output() startHourChange = new EventEmitter<number>();
    @Output() endHourChange = new EventEmitter<number>();

    accuracies = TIME_VIEW_SLOT_SIZES;
    eveningHours = TIME_VIEW_EVENING_HOURS;
    morningHours = TIME_VIEW_MORNING_HOURS;
}
