import {
    ApplicationRef,
    ChangeDetectionStrategy,
    Component,
    DoBootstrap,
    NgModule,
    OnInit,
} from "@angular/core";
import {BrowserAnimationsModule} from "@angular/platform-browser/animations";
import * as t from "io-ts";
import {
    CalendarDateFormatter,
    CalendarEvent,
    CalendarModule,
    CalendarView,
    DateAdapter,
} from "angular-calendar";
import {adapterFactory} from "angular-calendar/date-adapters/date-fns";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {CommonModule, registerLocaleData} from "@angular/common";
import localeFr from "@angular/common/locales/fi";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {BrowserModule} from "@angular/platform-browser";
import {createDowngradedModule, doDowngrade} from "../../downgrade";
import {AngularPluginBase} from "../angular-plugin-base.directive";
import {GenericPluginMarkup, getTopLevelFields, nullable} from "../attributes";
import {CalendarHeaderModule} from "./calendar-header.component";
import {CustomDateFormatter} from "./custom-date-formatter.service";

const CalendarItem = t.type({
    done: t.boolean,
    text: t.string,
});

const CalendarMarkup = t.intersection([
    t.partial({
        todos: nullable(t.array(CalendarItem)),
    }),
    GenericPluginMarkup,
]);

const CalendarFields = t.intersection([
    getTopLevelFields(CalendarMarkup),
    t.type({}),
]);
registerLocaleData(localeFr);

@Component({
    selector: "mwl-calendar-component",
    changeDetection: ChangeDetectionStrategy.OnPush,
    providers: [
        {
            provide: CalendarDateFormatter,
            useClass: CustomDateFormatter,
        },
    ],
    template: `
        <mwl-utils-calendar-header [(view)]="view" [(viewDate)]="viewDate">
        </mwl-utils-calendar-header>
        
        <div class="alert alert-info">
          Click on a day or time slot on the view.
          <strong *ngIf="clickedDate"
            >You clicked on this time: {{ clickedDate | date:'medium' }}</strong
          >
          <strong *ngIf="clickedColumn !== undefined"
            >You clicked on this column: {{ clickedColumn }}</strong
          >
        </div>
        
        <div [ngSwitch]="view">
          <mwl-calendar-month-view
            *ngSwitchCase="'month'"
            [viewDate]="viewDate"
            [events]="events"
            [locale]="'fi-FI'"
            [weekStartsOn]= "1"
            (columnHeaderClicked)="clickedColumn = $event.isoDayNumber"
            (dayClicked)="clickedDate = $event.day.date"
          >
          </mwl-calendar-month-view>
          <mwl-calendar-week-view
            *ngSwitchCase="'week'"
            [viewDate]="viewDate"
            [events]="events"
            [locale]="'fi-FI'"
            [weekStartsOn]= "1"
            (dayHeaderClicked)="clickedDate = $event.day.date"
            (hourSegmentClicked)="clickedDate = $event.date"
          >
          </mwl-calendar-week-view>
          <mwl-calendar-day-view
            *ngSwitchCase="'day'"
            [viewDate]="viewDate"
            [events]="events"
            [locale]="'fi-FI'"
            (hourSegmentClicked)="clickedDate = $event.date"
          >
          </mwl-calendar-day-view>
        </div>
    `,
    // styleUrls: ["calendar.component.scss"],
    // templateUrl: "template.html",
})
export class CalendarComponent
    extends AngularPluginBase<
        t.TypeOf<typeof CalendarMarkup>,
        t.TypeOf<typeof CalendarFields>,
        typeof CalendarFields
    >
    implements OnInit
{
    view: CalendarView = CalendarView.Week;

    viewDate: Date = new Date();

    events: CalendarEvent[] = [];

    clickedDate?: Date;

    clickedColumn?: number;

    getAttributeType() {
        return CalendarFields;
    }

    getDefaultMarkup() {
        return {};
    }

    ngOnInit() {
        super.ngOnInit();
    }
}

@NgModule({
    imports: [
        BrowserAnimationsModule,
        CommonModule,
        BrowserModule,
        HttpClientModule,
        FormsModule,
        CalendarModule.forRoot({
            provide: DateAdapter,
            useFactory: adapterFactory,
        }),
        CalendarHeaderModule,
    ],
    declarations: [CalendarComponent],
    exports: [CalendarComponent],
})
export class KATTIModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

const angularJsModule = createDowngradedModule((extraProviders) =>
    platformBrowserDynamic(extraProviders).bootstrapModule(KATTIModule)
);

doDowngrade(angularJsModule, "timCalendar", CalendarComponent);

export const moduleDefs = [angularJsModule];
