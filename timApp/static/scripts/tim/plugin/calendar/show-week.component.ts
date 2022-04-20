import {Component, EventEmitter, Input, Output} from "@angular/core";
import {CalendarView} from "angular-calendar";
import {getISOWeek} from "date-fns";

@Component({
    selector: "app-show-week",
    template: `
        <ng-container>
        <p> Week: {{getWeekNumberFromDate(viewDate)}} </p>
        </ng-container>`,
    styleUrls: ["calendar.component.scss"],
})
export class ShowWeekComponent {
    @Input() view: CalendarView = CalendarView.Week;

    @Input() viewDate: Date = new Date();

    // @Input() locale: string = "fi-FI";

    @Output() viewChange = new EventEmitter<CalendarView>();

    @Output() viewDateChange = new EventEmitter<Date>();

    CalendarView = CalendarView;

    /**
     * Calculates the current week number according to given Date-object.
     *
     * @param viewDate Current Date's object
     */
    getWeekNumberFromDate(viewDate: Date) {
        return getISOWeek(viewDate);
    }
}
