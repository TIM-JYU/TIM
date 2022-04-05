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
import moment from "moment";
import {createDowngradedModule, doDowngrade} from "../../downgrade";
import {AngularPluginBase} from "../angular-plugin-base.directive";
import {GenericPluginMarkup, getTopLevelFields, nullable} from "../attributes";
import {toPromise} from "../../util/utils";
import {Users} from "../../user/userService";
import {CalendarHeaderModule} from "./calendar-header.component";
import {CustomDateFormatter} from "./custom-date-formatter.service";
import {TimeViewSelectorComponent} from "./timeviewselector.component";

/**
 * Helps calculate the size of a horizontally dragged event on the calendar view.
 *
 * @param amount movement of mouse in pixels
 * @param segmentWidth the width of a single day in week view
 */
function floorToNearest(amount: number, segmentWidth: number) {
    return Math.floor(amount / segmentWidth) * segmentWidth;
}

/**
 * Helps calculate the size of a vertically dragged event on the calendar view.
 *
 * @param amount movement of mouse in pixels
 * @param minutesInSegment the length of a single segment in calendar in minutes. An hour is divided into slots in view.
 * @param segmentHeight the height of a single slot in calendar in pixels
 */
function ceilToNearest(
    amount: number,
    minutesInSegment: number,
    segmentHeight: number
) {
    return Math.ceil(amount / segmentHeight) * minutesInSegment;
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

const segmentHeight = 30;
// const minutesInSegment = 20;

registerLocaleData(localeFr);

Date.prototype.toJSON = function () {
    return moment(this).format();
};

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
        <div class="row text-center">
        <div class="btn-group edit-btn col-md-4">
            <button (click)="enableEditing()" [class.active]="editEnabled" class="btn timButton">Edit</button>
            <button (click)="disableEditing()" [class.active]="!editEnabled" class="btn timButton">View</button>
        </div>
            <div class="col-md-4"> Show events:
                <div *ngFor="let box of checkboxEvents">
                    <input (change)="getEventsToView()" type="checkbox" name="checkboxEvents" value="box.value" [(ngModel)]="box.checked" [checked]="" >{{box.name}}
                </div>
        </div>
        <div [style.visibility] = "editEnabled ? 'visible' : 'hidden'" class="btn-group event-btn col-md-4">
        <button (click)="setEventType($event)" *ngFor="let button of eventTypes" [class.active]="selectedEvent == (button.valueOf() +eventTypes.indexOf((button)))" class="btn timButton" id="{{button.valueOf() + eventTypes.indexOf(button) }}">{{button.valueOf()}}</button>
        </div>
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
            (mousedown)="editEnabled && startDragToCreate(segment, $event, segmentElement)"
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
            [hourSegmentHeight]="30"
            [hourDuration]="60"
            [hourSegments]="segmentsInHour"
            [dayStartHour]="dayStartHour"
            [dayEndHour]="dayEndHour"
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
            [hourSegments]="segmentsInHour"
            [dayStartHour]="dayStartHour" 
            [dayEndHour]="dayEndHour"
            [locale]="'fi-FI'"
            (hourSegmentClicked)="clickedDate = $event.date"
          >
          </mwl-calendar-day-view>
        </div>
        <app-timeview-selectors (accuracy)="setAccuracy($event)" (morning)="setMorning($event)" (evening)="setEvening($event)"></app-timeview-selectors>
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

    editEnabled: boolean = false;

    eventTypes: string[] = ["Ohjaus", "Luento", "Opetusryhmä"];
    eventType: string = this.eventTypes[0];
    selectedEvent: string = this.eventType + 0;

    checkboxEvents = [
        {name: "Ohjaus", value: "1", checked: true},
        {name: "Luento", value: "2", checked: true},
        {name: "Opetusryhmä", value: "3", checked: true},
    ];

    weekStartsOn: 1 = 1;

    /* The default values of calendar view that can be adjusted with the time view selector -component. */
    dayStartHour: number = 8;
    dayEndHour: number = 19;
    segmentMinutes: number = 20;
    segmentsInHour: number = 3;

    lastEvent: number = 0;

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        private cdr: ChangeDetectorRef
    ) {
        super(el, http, domSanitizer);
    }

    /**
     * Set type of event user wants to add while in edit-mode
     * @param event
     */
    setEventType(event: Event) {
        const elementId: string = (event.target as Element).id;
        this.selectedEvent = elementId;
    }

    setAccuracy(accuracy: number) {
        this.segmentMinutes = accuracy;
        this.segmentsInHour = 60 / this.segmentMinutes;
    }

    setMorning(morning: number) {
        this.dayStartHour = morning;
    }

    setEvening(evening: number) {
        this.dayEndHour = evening - 1;
    }

    enableEditing() {
        this.editEnabled = true;
    }

    disableEditing() {
        this.editEnabled = false;
    }

    /**
     * Get what type of events user wants to view in view-mode
     */
    getEventsToView() {
        const viewEvents = this.checkboxEvents
            .filter((box) => box.checked)
            .map((box) => box.name);
        console.log(viewEvents);
        return viewEvents;
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
                this.segmentMinutes
            )
                .toTimeString()
                .substr(0, 5)} Varattava aika`,
            start: segment.date,
            end: addMinutes(segment.date, this.segmentMinutes),
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
                    // The promise is resolved inside saveChanges -function
                    this.saveChanges();
                }),
                takeUntil(fromEvent(document, "mouseup"))
            )
            .subscribe((mouseMoveEvent: MouseEvent) => {
                const minutesDiff = ceilToNearest(
                    mouseMoveEvent.clientY - segmentPosition.top,
                    this.segmentMinutes,
                    segmentHeight
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
        if (Users.isLoggedIn()) {
            void this.loadEvents();
        }
    }

    private async loadEvents() {
        const result = await toPromise(
            this.http.get<CalendarEvent[]>("/calendar/events")
        );
        if (result.ok) {
            result.result.forEach((event) => {
                event.start = new Date(event.start);
                if (event.end) {
                    event.end = new Date(event.end);
                }
            });
            this.events = result.result;
            this.lastEvent = result.result.length;
            this.refresh();
        } else {
            // TODO: Handle error responses properly
            console.error(result.result.error.error);
        }
    }

    async saveChanges() {
        const eventsToAdd = this.events.slice(this.lastEvent);
        if (eventsToAdd.length > 0) {
            const result = await toPromise(
                this.http.post<CalendarEvent[]>("/calendar/events", {
                    events: eventsToAdd,
                })
            );
            // TODO: handle server responses properly
            if (result.ok) {
                console.log("events sent");
                console.log(result.result);
                this.lastEvent = this.events.length;
                this.refresh();
            } else {
                console.error(result.result.error.error);
            }
        }
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
    declarations: [CalendarComponent, TimeViewSelectorComponent],
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
