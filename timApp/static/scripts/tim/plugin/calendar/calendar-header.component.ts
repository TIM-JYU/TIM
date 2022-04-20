import {Component, EventEmitter, Input, NgModule, Output} from "@angular/core";
import {CalendarModule, CalendarView} from "angular-calendar";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {ShowWeekComponent} from "./show-week.component";

@Component({
    selector: "mwl-utils-calendar-header",
    template: `
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
            Tänään
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
              <app-show-week [hidden] = "view == CalendarView.Month" [(view)]="view" [(viewDate)]="viewDate"></app-show-week>
      </div>
      <div class="col-md-4">
        <div class="btn-group">
          <button
            class="btn btn-primary"
            (click)="viewChange.emit(CalendarView.Month)"
            [class.active]="view === CalendarView.Month"
          >
            Kuukausi
          </button>
          <button
            class="btn btn-primary"
            (click)="viewChange.emit(CalendarView.Week)"
            [class.active]="view === CalendarView.Week"
          >
            Viikko
          </button>
          <button
            class="btn btn-primary"
            (click)="viewChange.emit(CalendarView.Day)"
            [class.active]="view === CalendarView.Day"
          >
            Päivä
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
})
export class CalendarHeaderModule {}
