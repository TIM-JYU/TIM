/**
 * Component that provides navigation buttons and date header
 * of the current view for the calendar plugin
 *
 * @author vesal
 * @license MIT
 * @date 25.8.2022
 */

import {Component, EventEmitter, Input, Output} from "@angular/core";
import {CalendarView} from "angular-calendar";
import {Users} from "tim/user/userService";

@Component({
    selector: "tim-calendar-minimal-header",
    template: `
     <div class="tim-calendar-minimal-header">
          <button class="btn btn-default btn-sm"
            mwlCalendarPreviousView
            [view]="view"
            [(viewDate)]="viewDate"
            (viewDateChange)="viewDateChange.next(viewDate)">
              <i class="glyphicon glyphicon-arrow-left"></i>
          </button>
          <button class="btn btn-default btn-sm"
            mwlCalendarNextView
            [view]="view"
            [(viewDate)]="viewDate"
            (viewDateChange)="viewDateChange.next(viewDate)">
              <i class="glyphicon glyphicon-arrow-right"></i>
          </button>
          <span class="tim-calendar-minimal-display"
            mwlCalendarToday
            [(viewDate)]="viewDate"
            (viewDateChange)="viewDateChange.next(viewDate)">
              <span [hidden]="(view!== CalendarView.Day)" >{{viewDate | calendarDate:'viewDay':locale}}</span>
              <tim-show-week [hidden]="(view !== CalendarView.Week)" [(view)]="view" [(viewDate)]="viewDate"></tim-show-week>
              <span [hidden]="(view !== CalendarView.Week)"> / {{viewDate | calendarDate :'viewYear':locale}}</span>
              <span  [hidden]="(view !== CalendarView.Month)">{{viewDate | calendarDate :'viewMonth':locale}}</span>
              <span [hidden]="(view !== CalendarView.Month)"> / {{viewDate | calendarDate :'viewYear':locale}}</span>
          </span>
          <a class="tim-calendar-minimal-display-week"
            (click)="viewChange.emit(CalendarView.Week)"
            [hidden]="view === CalendarView.Week">
            <tim-show-week  [(view)]="view" [(viewDate)]="viewDate"></tim-show-week>
          </a>
        </div>
  `,
    styleUrls: ["calendar.component.scss"],
})
export class CalendarHeaderMinimalComponent {
    @Input() view: CalendarView = CalendarView.Week;

    @Input() viewDate: Date = new Date();

    @Input() locale: string = Users.getCurrentLocale();

    @Input() weekStartsOn: number = 1;

    @Output() viewChange = new EventEmitter<CalendarView>();

    @Output() viewDateChange = new EventEmitter<Date>();

    CalendarView = CalendarView;
}
