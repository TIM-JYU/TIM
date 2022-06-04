/**
 * Component that provides navigation buttons and date header
 * of the current view for the calendar plugin
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
import {Users} from "../../user/userService";

@Component({
    selector: "tim-calendar-header",
    template: `
     <div class="row text-center">
      <div class="col-md-4">
          <div class="row">
              <div class="row">
                  <div class="btn-group time-period-btn">
                      <button
                        class="btn btn-primary"
                        mwlCalendarPreviousView
                        [view]="view"
                        [(viewDate)]="viewDate"
                        (viewDateChange)="viewDateChange.next(viewDate)">
                          <span class="glyphicon glyphicon-arrow-left"></span>
                      </button>
                      <button
                        class="btn btn-outline-secondary"
                        mwlCalendarToday
                        [(viewDate)]="viewDate"
                        (viewDateChange)="viewDateChange.next(viewDate)">
                          <ng-container >  <span [hidden]="!(view== CalendarView.Day)" >{{viewDate | calendarDate:'weekViewColumnSubHeader':locale}}</span> </ng-container>
                        <tim-show-week [hidden]="!(view == CalendarView.Week)" [(view)]="view" [(viewDate)]="viewDate"></tim-show-week>
                          <ng-container >  <span  [hidden]="!(view== CalendarView.Month)">{{viewDate | calendarDate :'viewMonth':locale}}</span> </ng-container>
                      </button>
                      <button
                        class="btn btn-primary"
                        mwlCalendarNextView
                        [view]="view"
                        [(viewDate)]="viewDate"
                        (viewDateChange)="viewDateChange.next(viewDate)">
                          <span class="glyphicon glyphicon-arrow-right"></span>
                      </button>
                  </div>
              </div>
              <div class="row">
                  <button i18n class="btn timButton currentDay"
                        mwlCalendarToday
                        [(viewDate)]="viewDate"
                        (viewDateChange)="viewDateChange.next(viewDate)"
                        >Back to current date
                  </button>
              </div>
      </div>
      </div>
      <div class="col-md-4">
          <h2 [hidden]="view != 'day'">{{ viewDate | calendarDate:('viewDay'):locale}}</h2>
          <h2 [hidden]="view == 'day'">{{ viewDate | calendarDate:(view + 'ViewTitle'):locale:weekStartsOn }}</h2>
      </div>
      <div class="col-md-4">
        <div class="btn-group">
          <button i18n
            class="btn btn-primary"
            (click)="viewChange.emit(CalendarView.Month)"
            [class.active]="view === CalendarView.Month">
            Month
          </button>
          <button i18n
            class="btn btn-primary"
            (click)="viewChange.emit(CalendarView.Week)"
            [class.active]="view === CalendarView.Week">
            Week
          </button>
          <button i18n
            class="btn btn-primary"
            (click)="viewChange.emit(CalendarView.Day)"
            [class.active]="view === CalendarView.Day">
            Day
          </button>
        </div>
      </div>
    </div>
    <br />
  `,
    styleUrls: ["calendar.component.scss"],
})
export class CalendarHeaderComponent {
    @Input() view: CalendarView = CalendarView.Week;

    @Input() viewDate: Date = new Date();

    @Input() locale: string = Users.getCurrentLocale();

    @Input() weekStartsOn: number = 1;

    @Output() viewChange = new EventEmitter<CalendarView>();

    @Output() viewDateChange = new EventEmitter<Date>();

    CalendarView = CalendarView;

    ngOnInit() {
        console.log(this.locale);
    }
}
