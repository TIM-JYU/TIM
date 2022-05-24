/**
 * A component that provides week number for the calendar plugin
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
import {CalendarView} from "angular-calendar";
import {getISOWeek} from "date-fns";

@Component({
    selector: "tim-show-week",
    template: `
        <ng-container>
        <span> Week: {{getWeekNumberFromDate(viewDate)}} </span>
        </ng-container>`,
    styleUrls: ["calendar.component.scss"],
})
export class ShowWeekComponent {
    @Input() view: CalendarView = CalendarView.Week;

    @Input() viewDate: Date = new Date();

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
