import {Component, EventEmitter, Input, NgModule, Output} from "@angular/core";
import {CalendarModule, CalendarView} from "angular-calendar";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";

@Component({
    selector: "mwl-utils-calendar-header",
    template: `
    <div class="row text-center">
      <div class="col-md-4">
        <div class="btn-group">
          <div
            class="btn btn-primary"
            mwlCalendarPreviousView
            [view]="view"
            [(viewDate)]="viewDate"
            (viewDateChange)="viewDateChange.next(viewDate)"
          >
            Edellinen
          </div>
          <div
            class="btn btn-outline-secondary"
            mwlCalendarToday
            [(viewDate)]="viewDate"
            (viewDateChange)="viewDateChange.next(viewDate)"
          >
            Tänään
          </div>
          <div
            class="btn btn-primary"
            mwlCalendarNextView
            [view]="view"
            [(viewDate)]="viewDate"
            (viewDateChange)="viewDateChange.next(viewDate)"
          >
            Seuraava
          </div>
        </div>
      </div>
      <div class="col-md-4">
        <h3>{{ viewDate | calendarDate: view + 'ViewTitle':locale }}</h3>
      </div>
      <div class="col-md-4">
        <div class="btn-group">
          <div
            class="btn btn-primary"
            (click)="viewChange.emit(CalendarView.Month)"
            [class.active]="view === CalendarView.Month"
          >
            Kuukausi
          </div>
          <div
            class="btn btn-primary"
            (click)="viewChange.emit(CalendarView.Week)"
            [class.active]="view === CalendarView.Week"
          >
            Viikko
          </div>
          <div
            class="btn btn-primary"
            (click)="viewChange.emit(CalendarView.Day)"
            [class.active]="view === CalendarView.Day"
          >
            Päivä
          </div>
        </div>
      </div>
    </div>
    <br />
  `,
})
export class CalendarHeaderComponent {
    @Input() view: CalendarView = CalendarView.Week;

    @Input() viewDate: Date = new Date();

    @Input() locale: string = "fi-FI";

    @Output() viewChange = new EventEmitter<CalendarView>();

    @Output() viewDateChange = new EventEmitter<Date>();

    CalendarView = CalendarView;
}

@NgModule({
    imports: [CommonModule, FormsModule, CalendarModule],
    declarations: [CalendarHeaderComponent],
    exports: [CalendarHeaderComponent],
})
export class CalendarHeaderModule {}
