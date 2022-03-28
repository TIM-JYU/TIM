import {
    ApplicationRef,
    ChangeDetectionStrategy,
    ChangeDetectorRef,
    // ChangeDetectorRef,
    Component,
    DoBootstrap,
    ElementRef,
    // Injectable,
    NgModule,
    OnInit,
    ViewEncapsulation,
} from "@angular/core";
import {BrowserAnimationsModule} from "@angular/platform-browser/animations";
import * as t from "io-ts";
import {
    CalendarDateFormatter,
    CalendarEvent,
    // CalendarEventTitleFormatter,
    CalendarModule,
    CalendarView,
    DateAdapter,
} from "angular-calendar";
import {WeekViewHourSegment} from "calendar-utils";
import {adapterFactory} from "angular-calendar/date-adapters/date-fns";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {CommonModule, registerLocaleData} from "@angular/common";
import localeFr from "@angular/common/locales/fi";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {BrowserModule, DomSanitizer} from "@angular/platform-browser";
import {finalize, fromEvent, takeUntil} from "rxjs";
import {addDays, addMinutes, endOfWeek} from "date-fns";
import {createDowngradedModule, doDowngrade} from "../../downgrade";
import {AngularPluginBase} from "../angular-plugin-base.directive";
import {GenericPluginMarkup, getTopLevelFields, nullable} from "../attributes";
import {CalendarHeaderModule} from "./calendar-header.component";
import {CustomDateFormatter} from "./custom-date-formatter.service";

function floorToNearest(amount: number, precision: number) {
    return Math.floor(amount / precision) * precision;
}

function ceilToNearest(amount: number, precision: number) {
    return Math.ceil(amount / precision) * precision;
}

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

/**
 * For customizing the event tooltip
 */
/* @Injectable()
export class CustomEventTitleFormatter extends CalendarEventTitleFormatter {
    weekTooltip(event: CalendarEvent<{tmpEvent?: boolean}>, title: string) {
        if (!event.meta?.tmpEvent) {
            return super.weekTooltip(event, title);
        }
        return "";
    }

    dayTooltip(event: CalendarEvent<{tmpEvent?: boolean}>, title: string) {
        if (!event.meta?.tmpEvent) {
            return super.dayTooltip(event, title);
        }
        return "";
    }
}*/

@Component({
    selector: "mwl-calendar-component",
    changeDetection: ChangeDetectionStrategy.OnPush,
    providers: [
        {
            provide: CalendarDateFormatter,
            useClass: CustomDateFormatter,
        },
        // {
        //     provide: CalendarEventTitleFormatter,
        //     useClass: CustomEventTitleFormatter,
        // },
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
        
        <ng-template
          #weekViewHourSegmentTemplate
          let-segment="segment"
          let-locale="locale"
          let-segmentHeight="segmentHeight"
          let-isTimeLabel="isTimeLabel"
        >
          <div
            #segmentElement
            class="cal-hour-segment"
            [style.height.px]="segmentHeight"
            [class.cal-hour-start]="segment.isStart"
            [class.cal-after-hour-start]="!segment.isStart"
            [ngClass]="segment.cssClass"
            (mousedown)="startDragToCreate(segment, $event, segmentElement)"
          >
            <div class="cal-time" *ngIf="isTimeLabel">
              {{ segment.date | calendarDate:'weekViewHour':locale }}
            </div>
          </div>
        </ng-template>
        
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
            [hourDuration]="60"
            [hourSegments]="3"
            [dayStartHour]="8"
            [dayEndHour]="19"
            [locale]="'fi-FI'"
            [weekStartsOn]= "1"
            (dayHeaderClicked)="clickedDate = $event.day.date"
            (hourSegmentClicked)="clickedDate = $event.date"
            [hourSegmentTemplate]="weekViewHourSegmentTemplate"
          >
          </mwl-calendar-week-view>
          <mwl-calendar-day-view
            *ngSwitchCase="'day'"
            [viewDate]="viewDate"
            [events]="events"
            [hourDuration]="60"
            [hourSegments]="3"
            [dayStartHour]="8" 
            [dayEndHour]="19"
            [locale]="'fi-FI'"
            (hourSegmentClicked)="clickedDate = $event.date"
          >
          </mwl-calendar-day-view>
        </div>
    `,
    encapsulation: ViewEncapsulation.None,
    styleUrls: ["calendar.component.scss"],
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

    dragToCreateActive = false;

    weekStartsOn: 1 = 1;

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        private cdr: ChangeDetectorRef
    ) {
        super(el, http, domSanitizer);
    }

    startDragToCreate(
        segment: WeekViewHourSegment,
        mouseDownEvent: MouseEvent,
        segmentElement: HTMLElement
    ) {
        const dragToSelectEvent: CalendarEvent<{tmpEvent?: boolean}> = {
            id: this.events.length,
            title: `${segment.date.toTimeString().substr(0, 5)}–${addMinutes(
                segment.date,
                20
            )
                .toTimeString()
                .substr(0, 5)} Varattava aika`,
            start: segment.date,
            meta: {
                tmpEvent: true,
            },
        };
        this.events = [...this.events, dragToSelectEvent];
        this.dragToCreateActive = true;
        const segmentPosition = segmentElement.getBoundingClientRect();
        const endOfView = endOfWeek(this.viewDate, {
            weekStartsOn: this.weekStartsOn,
        });

        fromEvent<MouseEvent>(document, "mousemove")
            .pipe(
                finalize(() => {
                    if (dragToSelectEvent.meta) {
                        delete dragToSelectEvent.meta.tmpEvent;
                    }
                    this.dragToCreateActive = false;
                    this.refresh();
                }),
                takeUntil(fromEvent(document, "mouseup"))
            )
            .subscribe((mouseMoveEvent: MouseEvent) => {
                const minutesDiff = ceilToNearest(
                    mouseMoveEvent.clientY - segmentPosition.top,
                    20
                );

                const daysDiff =
                    floorToNearest(
                        mouseMoveEvent.clientX - segmentPosition.left,
                        segmentPosition.width
                    ) / segmentPosition.width;

                const newEnd = addDays(
                    addMinutes(segment.date, minutesDiff),
                    daysDiff
                );
                if (newEnd > segment.date && newEnd < endOfView) {
                    dragToSelectEvent.end = newEnd;
                    dragToSelectEvent.title = `${segment.date
                        .toTimeString()
                        .substr(0, 5)}–${newEnd
                        .toTimeString()
                        .substr(0, 5)} Varattava aika`;
                }
                this.refresh();
            });
    }

    private refresh() {
        this.events = [...this.events];
        this.cdr.detectChanges();
    }

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
