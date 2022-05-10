import {
    ApplicationRef,
    ChangeDetectionStrategy,
    ChangeDetectorRef,
    Component,
    DoBootstrap,
    ElementRef,
    NgModule,
    OnInit,
    TemplateRef,
    ViewChild,
    ViewEncapsulation,
} from "@angular/core";
import {BrowserAnimationsModule} from "@angular/platform-browser/animations";
import * as t from "io-ts";
import {
    CalendarDateFormatter,
    CalendarEvent,
    CalendarEventTimesChangedEvent,
    CalendarModule,
    CalendarView,
    DateAdapter,
} from "angular-calendar";
import {WeekViewHourSegment} from "calendar-utils";
import {adapterFactory} from "angular-calendar/date-adapters/date-fns";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {CommonModule, registerLocaleData} from "@angular/common";
import localeFr from "@angular/common/locales/fi";
import localeFi from "@angular/common/locales/fi";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {BrowserModule, DomSanitizer} from "@angular/platform-browser";
import {finalize, fromEvent, takeUntil} from "rxjs";
import {addDays, addMinutes, endOfWeek} from "date-fns";
import {NgbModalModule} from "@ng-bootstrap/ng-bootstrap";
import {createDowngradedModule, doDowngrade} from "../../downgrade";
import {AngularPluginBase} from "../angular-plugin-base.directive";
import {GenericPluginMarkup, getTopLevelFields, nullable} from "../attributes";
import {to2, toPromise} from "../../util/utils";
import {Users} from "../../user/userService";
import {itemglobals} from "../../util/globals";
import {CalendarHeaderModule} from "./calendar-header.component";
import {CustomDateFormatter} from "./custom-date-formatter.service";
import {TimeViewSelectorComponent} from "./timeviewselector.component";
import {showCalendarEventDialog} from "./showCalendarEventDialog";
import {DateTimeValidatorDirective} from "./datetimevalidator.directive";

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

// const CalendarItem = t.type({
//     opiskelijat: t.string,
//     ohjaajat: t.string,
// });

const EventTemplate = t.type({
    title: nullable(t.string),
    bookers: t.array(t.string),
    setters: t.array(t.string),
    tags: t.array(t.string),
    capacity: t.number,
});

const FilterOptions = t.type({
    groups: nullable(t.array(t.string)),
    tags: nullable(t.array(t.string)),
    fromDate: nullable(t.string), // TODO: figure out correct type for dates
    toDate: nullable(t.string),
});

const CalendarMarkup = t.intersection([
    t.partial({
        // ryhmat: nullable(t.array(CalendarItem)),
        filter: FilterOptions,
        eventTemplates: t.record(t.string, EventTemplate),
    }),
    GenericPluginMarkup,
]);

const CalendarFields = t.intersection([
    getTopLevelFields(CalendarMarkup),
    t.type({}),
]);

const colors = {
    red: {
        primary: "#ad2121",
        secondary: "#FAE3E3",
    },
    blue: {
        primary: "#1e90ff",
        secondary: "#D1E8FF",
    },
    yellow: {
        primary: "#e3bc08",
        secondary: "#FDF1BA",
    },
    gray: {
        primary: "#d5d5d5",
        secondary: "#d5d5d5",
    },
    green: {
        primary: "#4ce629",
        secondary: "#d9ffc2",
    },
};

const segmentHeight = 30;
// const minutesInSegment = 20;

registerLocaleData(localeFr);
registerLocaleData(localeFi);

/**
 * For customizing the event tooltip
 */
// @Injectable()
// export class CustomEventTitleFormatter extends CalendarEventTitleFormatter {
//     weekTooltip(event: TIMCalendarEvent, title: string) {
//         if (event.end) {
//             return `${event.start.toTimeString().substr(0, 5)}-${event.end
//                 .toTimeString()
//                 .substr(0, 5)}`;
//         }
//         return "";
//     }
//
//     dayTooltip(event: CalendarEvent<{tmpEvent?: boolean}>, title: string) {
//         if (!event.meta?.tmpEvent) {
//             return super.dayTooltip(event, title);
//         }
//         return "";
//     }
// }

export type TIMCalendarEvent = CalendarEvent<{
    tmpEvent: boolean;
    deleted?: boolean;
    editEnabled?: boolean;
    enrollments: number;
    maxSize: number;
    booker_groups: {
        name: string;
        users: {id: number; name: string; email: string | null}[];
    }[];
}>;

@Component({
    selector: "tim-calendar",
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
        <tim-calendar-header [locale]="locale" [(view)]="view" [(viewDate)]="viewDate">
        </tim-calendar-header>
        <div class="row text-center">
            <div class="col-md-4">
                <div class="btn-group edit-btn" [hidden]="!userIsManager()">
                    <button (click)="enableEditing(false)" [class.active]="!editEnabled" class="btn timButton">View</button>
                    <button (click)="enableEditing(true)" [class.active]="editEnabled" class="btn timButton">Edit</button>
                </div>
            </div>
            <div class="col-md-4">
                <div [style.visibility]="editEnabled ? 'visible' : 'hidden'" class="btn-group event-btn">
                    <button (click)="setEventType($event, button)" *ngFor="let button of eventTypes"
                            [class.active]="selectedEvent === button"
                            class="btn timButton"
                            id="{{button.valueOf() + eventTypes.indexOf(button) }}">{{button}}</button>
                </div>
            </div>
            <div class="col-md-4">
                <div class="checkbox-group"> Show:
                    <div *ngFor="let box of checkboxEvents"> 
                        <input (change)="getEventsToView()" type="checkbox" name="checkboxEvents" value="box.value"
                               [(ngModel)]="box.checked" [checked]="">{{box.name}}
                    </div>
                </div>
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
                    [locale]="locale"
                    [weekStartsOn]="1"
                    (columnHeaderClicked)="clickedColumn = $event.isoDayNumber"
                    (dayClicked)="changeToDay($event.day.date)"
                    (eventClicked)="handleEventClick($event.event)"
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
                    [locale]="locale"
                    [minimumEventHeight]="minimumEventHeight"
                    [weekStartsOn]="1"
                    (dayHeaderClicked)="viewDay($event.day.date)"
                    (hourSegmentClicked)="clickedDate = $event.date"
                    [hourSegmentTemplate]="weekViewHourSegmentTemplate"
                    (eventClicked)="handleEventClick($event.event)"
                    (eventTimesChanged)="eventTimesChanged($event)"
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
                    [locale]="locale"
                    (hourSegmentClicked)="clickedDate = $event.date"
                    [hourSegmentTemplate]="weekViewHourSegmentTemplate"
                    (eventClicked)="handleEventClick($event.event)"
                    (eventTimesChanged)="eventTimesChanged($event)"

            >
            </mwl-calendar-day-view>
        </div>
        <tim-time-view-selector [style.visibility]="view == 'month' ? 'hidden' : 'visible'"
                (accuracy)="setAccuracy($event)" (morning)="setMorning($event)"
                                (evening)="setEvening($event)"></tim-time-view-selector>
        <div>
            <button class="btn timButton" (click)="export()">Vie kalenterin tiedot</button>
            <input type="text" [(ngModel)]="icsURL" name="icsURL" class="icsURL">
        </div>
        <ng-template #modalContent let-close="close">
            <div class="modal-header">
                <h5 class="modal-title">Event action occurred</h5>
                <button type="button" class="close" (click)="close()">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <div>
                    Action:
                    <pre>{{ modalData?.action }}</pre>
                </div>
                <div>
                    Event:
                    <pre>{{ modalData?.event | json }}</pre>
                </div>
            </div>
            <div class="modal-footer">
                <!-- <button [style.visibility] = "editEnabled ? 'visible' : 'hidden'" type="button" class="btn btn-out
                line-secondary timButton"
                        (click)=" close(); deleteEvent(modalData?.event)">
                    Delete
                </button> -->
                <button type="button" class="btn btn-outline-secondary timButton" (click)="close()">
                    OK
                </button>
            </div>
        </ng-template>
        
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
    @ViewChild("modalContent", {static: true})
    modalContent?: TemplateRef<never>;
    icsURL: string = "";
    view: CalendarView = CalendarView.Week;

    viewDate: Date = new Date();

    events: TIMCalendarEvent[] = [];

    clickedDate?: Date;

    clickedColumn?: number;

    dragToCreateActive = false;

    editEnabled: boolean = false;
    dialogOpen: boolean = false;

    // eventTypes: string[] = ["Ohjaus", "Luento", "Opetusryhmä"];
    eventTypes: string[] = [];
    // eventType: string = this.eventTypes[0];
    selectedEvent: string = "";

    checkboxEvents = [
        {name: "Ohjaus", value: "1", checked: true},
        {name: "Luento", value: "2", checked: true},
        {name: "Opetusryhmä", value: "3", checked: true},
    ];

    locale: string = "Fi-fi";

    weekStartsOn: 1 = 1;

    /* The default values of calendar view that can be adjusted with the time view selector -component. */
    dayStartHour: number = 8;
    dayEndHour: number = 19;
    segmentMinutes: number = 20;
    segmentsInHour: number = 3;
    minimumEventHeight: number = this.segmentMinutes;

    modalData?: {
        action: string;
        event?: TIMCalendarEvent;
    };

    // actions: CalendarEventAction[] = [
    //     {
    //         label: '<i class="fas fa-fw fa-pencil-alt"></i>',
    //         a11yLabel: "Edit",
    //         onClick: ({event}: {event: CalendarEvent}): void => {
    //             this.handleEventClick(event);
    //         },
    //     },
    //     {
    //         label: '<i class="fas fa-fw fa-trash-alt"></i>',
    //         a11yLabel: "Delete",
    //         onClick: ({event}: {event: CalendarEvent}): void => {
    //             this.events = this.events.filter((iEvent) => iEvent !== event);
    //             this.handleEventClick(event);
    //         },
    //     },
    // ];

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
     * @param button
     */
    setEventType(event: Event, button: string) {
        this.selectedEvent = button;
    }
    isTempEvent(event: TIMCalendarEvent) {
        if (event.meta) {
            return event.meta.tmpEvent;
        }
    }

    /**
     * Sets the desired size of timeslot in the day&week -views
     * @param accuracy Size of time slot in minutes 15, 20, 30 or 60
     */
    setAccuracy(accuracy: number) {
        this.minimumEventHeight = accuracy;
        this.segmentMinutes = accuracy;
        if (accuracy == 60) {
            this.minimumEventHeight = 30;
        }
        this.segmentsInHour = 60 / this.segmentMinutes;
    }

    setLanguage() {
        const language = navigator.language;
        switch (language.toLowerCase()) {
            case "fi-fi":
                this.locale = "fi-fi";
                break;
            case "fi":
                this.locale = "fi";
                break;
            case "en-us":
                this.locale = "en-us";
                break;
            default:
                this.locale = "en-us";
                break;
        }
    }

    /**
     * Set the starting hours of day&week -views
     * @param morning hour to start the day
     */
    setMorning(morning: number) {
        this.dayStartHour = morning;
    }

    /**
     * Set the ending hour of day&week -views
     * @param evening hour to end the day
     */
    setEvening(evening: number) {
        this.dayEndHour = evening - 1;
    }

    /**
     * Enables the user to click on a date in the month view to see the week view of that day
     * @param date the date of clicked day
     */
    changeToDay(date: Date) {
        this.clickedDate = date;
        this.viewDate = date;
        this.view = CalendarView.Week;
    }

    /**
     * Enables the user to click on a date in the week view to see the day view of that day
     * @param date the date of clicked day
     */
    viewDay(date: Date) {
        this.clickedDate = date;
        this.viewDate = date;
        this.view = CalendarView.Day;
    }

    /**
     * Sets the view/edit mode in the UI
     * @param enabled whether the edit mode is enabled
     */
    enableEditing(enabled: boolean) {
        this.editEnabled = enabled;
        this.events.forEach((event) => {
            event.resizable = {
                beforeStart: enabled,
                afterEnd: enabled,
            };
            if (event.meta) {
                event.meta.editEnabled = enabled;
            }
        });
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

    /**
     * Called when the user starts creating a new event by clicking and dragging
     *
     * @param segment
     * @param mouseDownEvent
     * @param segmentElement
     */
    startDragToCreate(
        segment: WeekViewHourSegment,
        mouseDownEvent: MouseEvent,
        segmentElement: HTMLElement
    ) {
        let fullName = Users.getCurrent().real_name;
        if (!fullName) {
            fullName = Users.getCurrent().name;
        }
        const dragToSelectEvent: TIMCalendarEvent = {
            id: this.events.length,
            // title: `${segment.date.toTimeString().substr(0, 5)}–${addMinutes(
            //     segment.date,
            //     this.segmentMinutes
            // )
            //     .toTimeString()
            //     .substr(0, 5)} Varattava aika`,
            title: fullName,
            start: segment.date,
            end: addMinutes(segment.date, this.segmentMinutes),
            meta: {
                tmpEvent: true,
                enrollments: 0,
                maxSize: 1, // TODO: temporary solution
                booker_groups: [],
            },
            // actions: this.actions,
        };
        if (Date.now() > dragToSelectEvent.start.getTime()) {
            dragToSelectEvent.color = colors.gray;
        }
        this.events = [...this.events, dragToSelectEvent];
        this.dragToCreateActive = true;
        const segmentPosition = segmentElement.getBoundingClientRect();
        const endOfView = endOfWeek(this.viewDate, {
            weekStartsOn: this.weekStartsOn,
        });

        fromEvent<MouseEvent>(document, "mousemove")
            .pipe(
                finalize(async () => {
                    this.dragToCreateActive = false;
                    await this.saveChanges();
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
                    // dragToSelectEvent.title = `${segment.date
                    //     .toTimeString()
                    //     .substr(0, 5)}–${newEnd
                    //     .toTimeString()
                    //     .substr(0, 5)} Varattava aika`;
                    if (dragToSelectEvent.end) {
                        dragToSelectEvent.end = new Date(dragToSelectEvent.end);
                    }
                }
                this.refresh();
            });
    }

    /**
     * Called when the event is resized by dragging
     *
     * @param event Resized event
     * @param newStart New starting datetime
     * @param newEnd New ending datetime
     */
    async eventTimesChanged({
        event,
        newStart,
        newEnd,
    }: CalendarEventTimesChangedEvent) {
        if (newEnd) {
            event.start = newStart;
            event.end = newEnd;
            // this.updateEventTitle(event);
            this.refresh();
            await this.editEvent(event);
        }
    }

    /**
     * @deprecated
     * DEPRECATED: Not used anymore, time expressions removed from event titles
     *
     * Updates the event's title if the begin matches the regular expression, e.g. "10:00-11.00"
     *
     * TODO: handle localized time expressions (e.g. AM and PM)
     *
     * @param event Event to be updated
     * @private
     */
    private updateEventTitle(event: TIMCalendarEvent) {
        const rExp: RegExp = /[0-9]{2}:[0-9]{2}–[0-9]{2}:[0-9]{2}/;
        if (rExp.test(event.title)) {
            if (event.end) {
                event.title = `${event.start
                    .toTimeString()
                    .substr(0, 5)}–${event.end
                    .toTimeString()
                    .substr(0, 5)} ${event.title.substr(12)}`;
            }
        }
        this.refresh();
    }

    /**
     * Refreshes the view
     * @private
     */
    private refresh() {
        this.events = [...this.events];
        this.events.forEach((event) => {
            if (event.meta!.enrollments >= event.meta!.maxSize) {
                event.color = colors.red;
            } else {
                event.color = colors.blue;
            }
            if (event.meta!.booker_groups) {
                event.meta!.booker_groups.forEach((group) => {
                    group.users.forEach((user) => {
                        if (user.id === Users.getCurrent().id) {
                            event.color = colors.green;
                        }
                    });
                });
            }
            if (Date.now() > event.start.getTime()) {
                event.color = colors.gray;
            }
        });
        this.cdr.detectChanges();
    }

    getAttributeType() {
        return CalendarFields;
    }

    getDefaultMarkup() {
        return {};
    }

    /**
     * Called when the plugin is loaded. Loads the user's events
     */
    ngOnInit() {
        this.icsURL = "";
        super.ngOnInit();
        this.initEventTypes();
        this.setLanguage();
        if (Users.isLoggedIn()) {
            void this.loadEvents();
        }
    }

    /**
     * Loads the user's events from the TIM server
     * @private
     */
    private async loadEvents() {
        const result = await toPromise(
            this.http.get<TIMCalendarEvent[]>("/calendar/events")
        );
        if (result.ok) {
            result.result.forEach((event) => {
                event.start = new Date(event.start);
                if (event.end) {
                    event.end = new Date(event.end);
                }
                // event.actions = this.actions;
                event.meta = {
                    tmpEvent: false,
                    enrollments: event.meta!.enrollments,
                    maxSize: event.meta!.maxSize,
                    booker_groups: event.meta!.booker_groups,
                };
                event.resizable = {
                    beforeStart: this.editEnabled,
                    afterEnd: this.editEnabled,
                };
            });
            this.events = result.result;
            this.refresh();
        } else {
            // TODO: Handle error responses properly
            console.error(result.result.error.error);
        }
    }

    /**
     * Sends the newly added event to the TIM server to be persisted.
     * Handles sending multiple events at the same time.
     */
    async saveChanges() {
        let eventsToAdd = this.events.filter((event: TIMCalendarEvent) =>
            this.isTempEvent(event)
        );

        // TODO: korjaa
        // console.log(this.markup.ryhmat);
        // const eventGroups: string[] = [];
        // if (this.markup.ryhmat) {
        //     console.log(this.markup.ryhmat[0].opiskelijat);
        //     eventGroups.push(this.markup.ryhmat[0].opiskelijat);
        //     eventGroups.push(this.markup.ryhmat[0].ohjaajat);
        // }
        console.log(this.markup.eventTemplates);
        console.log(this.markup.filter);
        console.log(this.selectedEvent);

        let eventTitle: string | null = "";
        const eventGroups: string[] = [];
        let capacity: number = 0;
        if (this.markup.eventTemplates) {
            eventTitle = this.markup.eventTemplates[this.selectedEvent].title;

            this.markup.eventTemplates[this.selectedEvent].bookers.forEach(
                (group) => {
                    eventGroups.push(group);
                }
            );
            this.markup.eventTemplates[this.selectedEvent].setters.forEach(
                (group) => {
                    eventGroups.push(group);
                }
            );
            capacity = this.markup.eventTemplates[this.selectedEvent].capacity;
        }

        let titleToAdd = "";
        if (!eventTitle) {
            titleToAdd = this.selectedEvent;
        } else {
            titleToAdd = eventTitle;
        }

        // for (const type of this.eventTypes) {
        //     if (this.selectedEvent === type) {
        //         console.log(this.markup.eventTemplates![type]);
        //     }
        // }

        if (eventsToAdd.length > 0) {
            eventsToAdd = eventsToAdd.map<TIMCalendarEvent>((event) => {
                return {
                    title: titleToAdd,
                    start: event.start,
                    end: event.end,
                    event_groups: eventGroups,
                    max_size: capacity,
                };
            });
            console.log(eventsToAdd);
            const result = await toPromise(
                this.http.post<TIMCalendarEvent[]>("/calendar/events", {
                    events: eventsToAdd,
                })
            );
            if (result.ok) {
                // Remove added events with wrong id from the event list
                eventsToAdd.forEach((event) => {
                    this.events.splice(this.events.indexOf(event), 1);
                });
                // Push new events with updated id to the event list
                result.result.forEach((event) => {
                    if (event.end) {
                        this.events.push({
                            id: event.id,
                            title: event.title,
                            start: new Date(event.start),
                            end: new Date(event.end),
                            meta: {
                                tmpEvent: false,
                                editEnabled: this.editEnabled,
                                enrollments: event.meta!.enrollments,
                                maxSize: event.meta!.maxSize,
                                booker_groups: [],
                            },
                            // actions: this.actions,
                            resizable: {
                                beforeStart: true,
                                afterEnd: true,
                            },
                        });
                    }
                });
                console.log("events sent");
                console.log(result.result);
                this.refresh();
            } else {
                // TODO: Handle error responses properly
                console.error(result.result.error.error);
            }
        }
    }

    /**
     * Sends the updated event to the TIM server after resizing by dragging
     * @param event Resized event
     */
    async editEvent(event: CalendarEvent) {
        if (!event.id) {
            return;
        }
        const id = event.id;
        const eventToEdit = {
            title: event.title,
            start: event.start,
            end: event.end,
        };
        const result = await toPromise(
            this.http.put(`/calendar/events/${id}`, {
                event: eventToEdit,
            })
        );
        if (result.ok) {
            console.log(result.result);
        } else {
            // TODO: Handle error responses properly
            console.error(result.result.error.error);
        }
    }

    async export() {
        const result = await toPromise(
            this.http.get("/calendar/export", {
                responseType: "text",
            })
        );
        if (result.ok) {
            this.icsURL = result.result;
            this.refresh();
        } else {
            // TODO: Handle error responses properly
            console.error(result.result.error.error);
        }
    }

    /**
     * Opens the event dialog when event is clicked
     *
     * @param event Clicked event
     */
    async handleEventClick(event: TIMCalendarEvent): Promise<void> {
        if (this.dialogOpen) {
            return;
        }
        this.dialogOpen = true;
        const result = await to2(showCalendarEventDialog(event));
        this.dialogOpen = false;
        if (result.ok) {
            const modifiedEvent = result.result;
            if (modifiedEvent.meta) {
                if (modifiedEvent.meta.deleted) {
                    console.log("deleted");
                    this.events.splice(this.events.indexOf(modifiedEvent), 1);
                }
            }
            this.refresh();
        }
    }

    userIsManager(): boolean {
        return (
            itemglobals().curr_item.rights.manage ||
            itemglobals().curr_item.rights.owner
        );
    }

    private initEventTypes(): void {
        for (const eventTemplate in this.markup.eventTemplates) {
            if (eventTemplate !== "") {
                this.eventTypes.push(eventTemplate);
            }
        }
        if (this.eventTypes.length === 0) {
            // TODO: handle when no templates given in markup
        } else {
            this.selectedEvent = this.eventTypes[0];
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
        NgbModalModule,
    ],
    declarations: [
        CalendarComponent,
        TimeViewSelectorComponent,
        DateTimeValidatorDirective,
    ],
    exports: [CalendarComponent, DateTimeValidatorDirective],
})
export class KATTIModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

const angularJsModule = createDowngradedModule((extraProviders) =>
    platformBrowserDynamic(extraProviders).bootstrapModule(KATTIModule)
);

doDowngrade(angularJsModule, "timCalendar", CalendarComponent);

export const moduleDefs = [angularJsModule];
