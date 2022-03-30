import {Component, EventEmitter, OnInit, Output} from "@angular/core";

const accuracies: number[] = [15, 20, 30, 60];
const morningHours: number[] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];
const eveningHours: number[] = [13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24];

@Component({
    selector: "app-timeview-selectors",
    template: `
    <ng-container>
        <span>Näytä tarkkuudella </span>
        <select [(ngModel)]="selectedAccuracy" >
            <option *ngFor="let item of accuracies" [value]="item">{{ item | number:'2.0' }}</option>
        </select>
        <span> Väliltä </span>
        <select [(ngModel)]="selectedStart" >
            <option *ngFor="let hour of morningHours" [value]="hour">{{ hour | number:'2.0' }}</option>
        </select>
        <span> - </span>
        <select [(ngModel)]="selectedEnd">
            <option *ngFor="let hour of eveningHours" [value]="hour">{{ hour | number:'2.0' }}</option>
        </select>
        <div><button id="selectorsBtn" (click)="submit(selectedAccuracy, selectedStart, selectedEnd)" class="btn btn-primary">Käytä</button></div>
    </ng-container>`,
    styleUrls: ["calendar.component.scss"],
})
export class TimeViewSelectorComponent implements OnInit {
    selectedAccuracy: number = 20;
    selectedStart: number = 8;
    selectedEnd: number = 16;

    accuracies = accuracies;
    eveningHours = eveningHours;
    morningHours = morningHours;

    @Output() accuracy = new EventEmitter<number>();

    @Output() evening = new EventEmitter<number>();

    @Output() morning = new EventEmitter<number>();

    constructor() {}

    ngOnInit() {}

    submit(
        selectedAccuracy: number,
        selectedStart: number,
        selectedEnd: number
    ) {
        this.accuracy.emit(selectedAccuracy);
        this.evening.emit(selectedEnd);
        this.morning.emit(selectedStart);
    }
}
