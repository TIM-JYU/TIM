import {Component, EventEmitter, Input, NgModule, Output} from "@angular/core";
import {
    CalendarDateFormatter,
    CalendarModule,
    CalendarView,
} from "angular-calendar";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {ShowWeekComponent} from "./show-week.component";
import {CustomDateFormatter} from "./custom-date-formatter.service";

@Component({
    selector: "mwl-utils-calendar-header",
    template: `
    <div class="row text-center">
        <button class="btn timButton currentDay"
                mwlCalendarToday
                [(viewDate)]="viewDate"
                (viewDateChange)="viewDateChange.next(viewDate)"
                >Back to current date
        </button>
        </div>
     <div class="row text-center">   
      <div class="col-md-4">
        <div class="btn-group time-period-btn">
          <button
            class="btn btn-primary"
            mwlCalendarPreviousView
            [view]="view"
            [(viewDate)]="viewDate"
            (viewDateChange)="viewDateChange.next(viewDate)"
          >
              <span class="glyphicon glyphicon-arrow-left"></span>
          </button>
          <button
            class="btn btn-outline-secondary"
            mwlCalendarToday
            [(viewDate)]="viewDate"
            (viewDateChange)="viewDateChange.next(viewDate)"
          >
              <ng-container >  <span [hidden]="!(view== CalendarView.Day)" >{{viewDate | calendarDate:'weekViewColumnSubHeader':locale}}</span> </ng-container>
            <app-show-week [hidden]="!(view == CalendarView.Week)" [(view)]="view" [(viewDate)]="viewDate"></app-show-week>
              <ng-container >  <span  [hidden]="!(view== CalendarView.Month)">{{viewDate | calendarDate :'viewMonth':locale}}</span> </ng-container>
          </button>
          <button
            class="btn btn-primary"
            mwlCalendarNextView
            [view]="view"
            [(viewDate)]="viewDate"
            (viewDateChange)="viewDateChange.next(viewDate)"
          >
              <span class="glyphicon glyphicon-arrow-right"></span>
          </button>
        </div>
      </div>
      <div class="col-md-4">
          <h3>{{ viewDate | calendarDate:(view + 'ViewTitle'):locale:weekStartsOn }}</h3>
      </div>
      <div class="col-md-4">
        <div class="btn-group">
          <button
            class="btn btn-primary"
            (click)="viewChange.emit(CalendarView.Month)"
            [class.active]="view === CalendarView.Month"
          >
            Month
          </button>
          <button
            class="btn btn-primary"
            (click)="viewChange.emit(CalendarView.Week)"
            [class.active]="view === CalendarView.Week"
          >
            Week
          </button>
          <button
            class="btn btn-primary"
            (click)="viewChange.emit(CalendarView.Day)"
            [class.active]="view === CalendarView.Day"
          >
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

    @Input() locale: string = "fi-FI";

    @Input() weekStartsOn: number = 1;

    @Output() viewChange = new EventEmitter<CalendarView>();

    @Output() viewDateChange = new EventEmitter<Date>();

    CalendarView = CalendarView;
}

@NgModule({
    imports: [CommonModule, FormsModule, CalendarModule],
    declarations: [CalendarHeaderComponent, ShowWeekComponent],
    exports: [CalendarHeaderComponent],
    providers: [
        {
            provide: CalendarDateFormatter,
            useClass: CustomDateFormatter,
        },
    ],
})
export class CalendarHeaderModule {}
