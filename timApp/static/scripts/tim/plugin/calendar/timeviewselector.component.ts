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
import {Component, EventEmitter, OnInit, Output} from "@angular/core";

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
        <span i18n >Set view with timeslots of </span>
        <select [(ngModel)]="selectedAccuracy" (change)="submit(selectedAccuracy, selectedStart, selectedEnd)">
            <option *ngFor="let item of accuracies" [value]="item">{{ item | number:'2.0' }}</option>
        </select>
        <span i18n > minutes from </span>
        <select [(ngModel)]="selectedStart" (change)="submit(selectedAccuracy, selectedStart, selectedEnd)">
            <option *ngFor="let hour of morningHours" [value]="hour">{{ hour | number:'2.0' }}</option>
        </select>
        <span i18n > to </span>
        <select [(ngModel)]="selectedEnd" (change)="submit(selectedAccuracy, selectedStart, selectedEnd)">
            <option *ngFor="let hour of eveningHours" [value]="hour">{{ hour | number:'2.0' }}</option>
        </select>
    </ng-container>`,
    styleUrls: ["calendar.component.scss"],
})
export class TimeViewSelectorComponent implements OnInit {
    selectedAccuracy: number = 20;
    selectedStart: number = 8;
    selectedEnd: number = 20;

    accuracies = accuracies;
    eveningHours = eveningHours;
    morningHours = morningHours;

    @Output() accuracy = new EventEmitter<number>();

    @Output() evening = new EventEmitter<number>();

    @Output() morning = new EventEmitter<number>();

    accuracies = TIME_VIEW_SLOT_SIZES;
    eveningHours = TIME_VIEW_EVENING_HOURS;
    morningHours = TIME_VIEW_MORNING_HOURS;
}
