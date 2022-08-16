/**
 * Main module the TIM-calendar plugin
 *
 * @author Miika Immonen
 * @author Terhi Kamula
 * @author Anssi Lepikko
 * @author Touko Miettinen
 * @author Joose Tikkanen
 * @license MIT
 * @date 24.5.2022
 */
import {
    ApplicationRef,
    ChangeDetectionStrategy,
    ChangeDetectorRef,
    Component,
    DoBootstrap,
    ElementRef,
    NgModule,
    OnInit,
    ViewEncapsulation,
} from "@angular/core";
import {BrowserAnimationsModule} from "@angular/platform-browser/animations";
import * as t from "io-ts";
import {
    CalendarDateFormatter,
    CalendarEvent,
    CalendarEventTimesChangedEvent,
    CalendarEventTitleFormatter,
    CalendarModule,
    CalendarView,
    DateAdapter,
} from "angular-calendar";
import {WeekViewHourSegment} from "calendar-utils";
import {adapterFactory} from "angular-calendar/date-adapters/date-fns";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {CommonModule, registerLocaleData} from "@angular/common";
import localeFi from "@angular/common/locales/fi";
import localeSv from "@angular/common/locales/sv";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {BrowserModule, DomSanitizer} from "@angular/platform-browser";
import {finalize, fromEvent, takeUntil} from "rxjs";
import {addDays, addMinutes, endOfWeek} from "date-fns";
import {createDowngradedModule, doDowngrade} from "../../downgrade";
import {AngularPluginBase} from "../angular-plugin-base.directive";
import {GenericPluginMarkup, getTopLevelFields, nullable} from "../attributes";
import {closest, to2, toPromise} from "../../util/utils";
import {Users} from "../../user/userService";
import {itemglobals} from "../../util/globals";
import {showConfirm} from "../../ui/showConfirmDialog";
import {showMessageDialog} from "../../ui/showMessageDialog";
import {CustomDateFormatter} from "./custom-date-formatter.service";
import {CustomEventTitleFormatter} from "./custom-event-title-formatter.service";
import {
    TIME_VIEW_EVENING_HOURS,
    TIME_VIEW_MORNING_HOURS,
    TIME_VIEW_SLOT_SIZES,
    TimeViewSelectorComponent,
} from "./timeviewselector.component";
import {showCalendarEventDialog} from "./showCalendarEventDialog";
import {DateTimeValidatorDirective} from "./datetimevalidator.directive";
import {CalendarHeaderComponent} from "./calendar-header.component";
import {ShowWeekComponent} from "./show-week.component";

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

const ViewOptions = t.type({
    dayStartHour: t.number,
    dayEndHour: t.number,
    segmentDuration: t.number,
});

const CalendarMarkup = t.intersection([
    t.partial({
        filter: FilterOptions,
        eventTemplates: t.record(t.string, EventTemplate),
    }),
    GenericPluginMarkup,
    t.type({
        viewOptions: ViewOptions,
    }),
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
        primary: "#aaaaaa",
        secondary: "#d5d5d5",
    },
    green: {
        primary: "#4ce629",
        secondary: "#d9ffc2",
    },
};

const TIME_SLOT_HEIGHT = 30;

registerLocaleData(localeFi);
registerLocaleData(localeSv);

export type TIMEventMeta = {
    tmpEvent: boolean;
    deleted?: boolean;
    editEnabled: boolean;
    signup_before: Date;
    location: string;
    description: string;
    enrollments: number;
    maxSize: number;
    booker_groups: {
        name: string;
        message: string;
        users: {id: number; name: string; email: string | null}[];
    }[];
};

export type TIMCalendarEvent = CalendarEvent<TIMEventMeta>;

@Component({
    selector: "tim-calendar",
    changeDetection: ChangeDetectionStrategy.OnPush,
    providers: [
        {
            provide: CalendarDateFormatter,
            useClass: CustomDateFormatter,
        },
        {
            provide: CalendarEventTitleFormatter,
            useClass: CustomEventTitleFormatter,
        },
    ],
    template: `
        <tim-calendar-header [locale]="locale" [(view)]="view" [(viewDate)]="viewDate">
        </tim-calendar-header>
        <div class="row text-center">
            <div class="col-md-4">
                <div class="btn-group edit-btn" [hidden]="!userIsManager() || this.eventTypes.length == 0">
                    <button i18n (click)="enableEditing(false)" [class.active]="!editEnabled" class="btn timButton">View</button>
                    <button i18n (click)="enableEditing(true)" [class.active]="editEnabled" class="btn timButton">Edit</button>
                </div>
            </div>
            <div class="col-md-4">
                <div [style.visibility]="editEnabled ? 'visible' : 'hidden'" class="btn-group event-btn">
                    <button (click)="setEventType(button)" *ngFor="let button of eventTypes"
                            [class.active]="selectedEvent === button"
                            class="btn timButton"
                            id="{{button.valueOf() + eventTypes.indexOf(button) }}">{{button}}</button>
                </div>
            </div>
            <div class="col-md-4">
                <!-- TODO: Checkboxes for filtering purposes
                <div class="checkbox-group"> Show:
                    <div *ngFor="let box of checkboxEvents"> 
                        <input i18n (change)="getEventsToView()" type="checkbox" name="checkboxEvents" value="box.value"
                               [(ngModel)]="box.checked" [checked]="">{{box.name}}
                    </div>
                </div>
                -->
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
                    [weekStartsOn]="weekStartsOn"
                    (columnHeaderClicked)="clickedColumn = $event.isoDayNumber"
                    (dayClicked)="changeToDay($event.day.date)"
                    (eventClicked)="handleEventClick($event.event)"
            >
            </mwl-calendar-month-view>
            <mwl-calendar-week-view
                    *ngSwitchCase="'week'"
                    [viewDate]="viewDate"
                    [events]="events"
                    [hourSegmentHeight]="segmentHeight"
                    [hourDuration]="hourDuration"
                    [hourSegments]="segmentsInHour"
                    [dayStartHour]="dayStartHour"
                    [dayEndHour]="dayEndHour"
                    [locale]="locale"
                    [minimumEventHeight]="minimumEventHeight"
                    [weekStartsOn]="weekStartsOn"
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
                    [hourDuration]="hourDuration"
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
                                [(segmentDuration)]="segmentDuration"
                                [(startHour)]="dayStartHour"
                                [(endHour)]="dayEndHour">
        </tim-time-view-selector>
        <div>
            <button i18n class="btn timButton" (click)="export()">Export calendar</button>
            <span class="exportDone"><b>{{exportDone}}</b></span>
        </div>
    `,
    encapsulation: ViewEncapsulation.None,
    styleUrls: [
        "calendar.component.scss",
        "../../../../../node_modules/angular-calendar/css/angular-calendar.css",
    ],
})
export class CalendarComponent
    extends AngularPluginBase<
        t.TypeOf<typeof CalendarMarkup>,
        t.TypeOf<typeof CalendarFields>,
        typeof CalendarFields
    >
    implements OnInit
{
    exportDone: string = "";
    icsURL: string = "";
    view: CalendarView = CalendarView.Week;

    viewDate: Date = new Date();

    events: TIMCalendarEvent[] = [];

    clickedDate?: Date;

    clickedColumn?: number;

    dragToCreateActive = false;

    editEnabled: boolean = false;
    dialogOpen: boolean = false;

    eventTypes: string[] = [];
    selectedEvent: string = "";

    /*
    TODO: Checkbox values
    checkboxEvents = [
        {name: "Ohjaus", value: "1", checked: true},
        {name: "Luento", value: "2", checked: true},
        {name: "Opetusryhmä", value: "3", checked: true},
    ];
    */

    locale: string = Users.getCurrentLocale();

    weekStartsOn: 1 = 1;

    /* The default values of calendar view that can be adjusted with the time view selector -component. */
    dayStartHour!: number;
    dayEndHour!: number;
    segmentsInHour!: number;
    segmentHeight = TIME_SLOT_HEIGHT;

    private segmentMinutes!: number;
    minimumEventHeight!: number;

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
     * @param button
     */
    setEventType(button: string) {
        this.selectedEvent = button;
    }

    /**
     * True if event is set to be temporary (events that are yet to be sent to the TIM server)
     * @param event
     */
    isTempEvent(event: TIMCalendarEvent) {
        if (event.meta) {
            return event.meta.tmpEvent;
        }
    }

    /**
     * Sets the segment duration in minutes.
     *
     * A segment is a single slot in an "hour" (see `hourDuration` for more info).
     * A segment will be visible as a single slot in the calendar view.
     *
     * @param accuracy Duration of a single segment in minutes.
     *                 Durations over 60 minutes will only get one segment per hour.
     */
    set segmentDuration(accuracy: number) {
        this.minimumEventHeight = accuracy;
        this.segmentMinutes = accuracy;
        this.minimumEventHeight = accuracy;
        if (accuracy >= 60) {
            this.minimumEventHeight = 30;
        }
        this.segmentsInHour = this.hourDuration / this.segmentMinutes;
    }

    get segmentDuration(): number {
        return this.segmentMinutes;
    }

    /**
     * Duration of a single "hour" in minutes.
     *
     * Note: An "hour" in MWL Calendar is defined as a group of consecutive time slots within a time frame.
     * For example
     *   * a 60-minute "hour" means that the calendar will display groups 8:00, 9:00, 10:00 etc
     *   * a 30-minute "hour" means that the calendar will display groups 8:00, 9:30, 10:00 etc
     *   * a 120-minute "hour" means that the calendar will display groups 8:00, 10:00, 11:00
     */
    get hourDuration() {
        return this.segmentMinutes > 60 ? this.segmentMinutes : 60;
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
     * TODO: To be used with the checkbox filtering
    getEventsToView() {
        const viewEvents = this.checkboxEvents
            .filter((box) => box.checked)
            .map((box) => box.name);
        console.log(viewEvents);
        return viewEvents;
    }
    */

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
        let title: string | null = "";
        if (this.markup.eventTemplates) {
            title = this.markup.eventTemplates[this.selectedEvent].title;
        }
        if (!title) {
            title = this.selectedEvent;
        }

        const dragToSelectEvent: TIMCalendarEvent = {
            id: this.events.length,
            title: title,
            start: segment.date,
            end: addMinutes(segment.date, this.segmentMinutes),
            meta: {
                tmpEvent: true,
                signup_before: new Date(segment.date),
                description: "",
                enrollments: 0,
                location: "",
                maxSize: 1, // TODO: temporary solution
                booker_groups: [],
                editEnabled: this.editEnabled,
            },
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
                    this.segmentHeight
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
    }: CalendarEventTimesChangedEvent<TIMEventMeta>) {
        if (newEnd && event.meta) {
            const oldStart = event.start;
            const oldEnd = event.end;
            event.meta.signup_before = newStart;
            event.start = newStart;
            event.end = newEnd;
            this.refresh();
            await this.editEvent(event, oldStart, oldEnd);
        } else {
            // TODO: handle undefined event.meta. Shouldn't be possible for the event.meta to be undefined.
            this.refresh();
            return;
        }
    }

    /**
     * @deprecated
     * DEPRECATED: Not used anymore, time expressions removed from event titles
     *
     * Updates the event's title if the begin matches the regular expression, e.g. "10:00-11.00"
     *
     * TODO: Can be used to add the time to event title.
     * TODO: Handle localized time expressions (e.g. AM and PM)
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
        this.events = [...this.events]; // TODO: Find out what is the purpose of this line
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

    /**
     * Returns calendar fields
     */
    getAttributeType() {
        return CalendarFields;
    }

    /**
     * Returns empty markup
     */
    getDefaultMarkup() {
        return {
            viewOptions: {
                dayStartHour: 8,
                dayEndHour: 20,
                segmentDuration: 60,
            },
        };
    }

    /**
     * Called when the plugin is loaded. Loads the user's events
     */
    ngOnInit() {
        this.icsURL = "";
        super.ngOnInit();

        this.segmentDuration = closest(
            TIME_VIEW_SLOT_SIZES,
            this.markup.viewOptions.segmentDuration
        );
        this.dayStartHour = closest(
            TIME_VIEW_MORNING_HOURS,
            this.markup.viewOptions.dayStartHour
        );
        this.dayEndHour = closest(
            TIME_VIEW_EVENING_HOURS,
            this.markup.viewOptions.dayEndHour
        );

        this.initEventTypes();
        void this.loadEvents();
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
                event.meta = {
                    description: event.meta!.description,
                    tmpEvent: false,
                    enrollments: event.meta!.enrollments,
                    maxSize: event.meta!.maxSize,
                    location: event.meta!.location,
                    booker_groups: event.meta!.booker_groups,
                    editEnabled: this.editEnabled,
                    signup_before: new Date(event.meta!.signup_before),
                };
                event.resizable = {
                    beforeStart: this.editEnabled,
                    afterEnd: this.editEnabled,
                };
            });
            this.events = result.result;
            this.refresh();
        } else {
            if (result.result.error.error) {
                await showMessageDialog(result.result.error.error);
            } else {
                await showMessageDialog(
                    `Something went wrong. TIM admins have been notified about the issue.`
                );
            }
        }
    }

    /**
     * Sends the newly added event to the TIM server to be persisted.
     * Handles sending multiple events at the same time.
     */
    async saveChanges() {
        let eventsToAdd = this.events.filter((e) => this.isTempEvent(e));
        if (!eventsToAdd) {
            return;
        }

        let bookerGroups: string[] = [];
        let setterGroups: string[] = [];
        let capacity: number = 0;
        let tags: string[] = [];
        if (this.markup.eventTemplates) {
            const template = this.markup.eventTemplates[this.selectedEvent];
            bookerGroups = template.bookers;
            setterGroups = template.setters;
            capacity = template.capacity;
            tags = template.tags;
        }

        const result = await toPromise(
            this.http.post<TIMCalendarEvent[]>("/calendar/events", {
                events: eventsToAdd.map((event) => ({
                    id: event.id,
                    title: event.title,
                    location: event.meta!.location,
                    description: event.meta!.description,
                    start: event.start,
                    end: event.end,
                    signup_before: new Date(event.meta!.signup_before),
                    booker_groups: bookerGroups,
                    setter_groups: setterGroups,
                    max_size: capacity,
                    tags: tags,
                })),
            })
        );
        if (result.ok) {
            // Remove temporary events as they have been added
            this.events = this.events.filter((e) => !this.isTempEvent(e));
            const addedEvents = result.result;
            for (const event of addedEvents) {
                if (!event.end) {
                    continue;
                }
                this.events.push({
                    id: event.id,
                    title: event.title,
                    start: new Date(event.start),
                    end: new Date(event.end),
                    meta: {
                        tmpEvent: false,
                        editEnabled: this.editEnabled,
                        enrollments: event.meta!.enrollments,
                        description: event.meta!.description,
                        maxSize: event.meta!.maxSize,
                        location: event.meta!.location,
                        booker_groups: [],
                        signup_before: new Date(event.meta!.signup_before),
                    },
                    resizable: {
                        beforeStart: true,
                        afterEnd: true,
                    },
                });
            }
            this.refresh();
        } else {
            if (result.result.error.error) {
                await showMessageDialog(
                    $localize`Sorry, you do not have a permission to add events for given group(s): ${result.result.error.error}`
                );
            } else {
                await showMessageDialog(
                    $localize`Something went wrong. TIM admins have been notified about the issue.`
                );
            }
            this.events.forEach((event) => {
                if (this.isTempEvent(event)) {
                    this.events.splice(this.events.indexOf(event));
                }
            });
            this.refresh();
        }
    }

    /**
     * Sends the updated event to the TIM server after resizing by dragging
     * @param event Resized event
     * @param oldStart event start time before resizing
     * @param oldEnd event end time before resizing
     */
    async editEvent(event: CalendarEvent, oldStart: Date, oldEnd?: Date) {
        if (!event.id) {
            return;
        }
        const values: {
            location: string;
            signup_before: Date;
            description: string;
            maxSize: number;
        } = event.meta;

        const id = event.id;
        const eventToEdit = {
            title: event.title,
            start: event.start,
            description: values.description,
            location: values.location,
            end: event.end,
            signup_before: values.signup_before,
            max_size: values.maxSize,
        };

        const result = await toPromise(
            this.http.put(`/calendar/events/${id}`, {
                event: eventToEdit,
            })
        );
        if (result.ok) {
        } else {
            if (result.result.error.error) {
                await showMessageDialog(result.result.error.error);
            } else {
                await showMessageDialog(
                    $localize`Something went wrong. TIM admins have been notified about the issue.`
                );
            }
            event.start = oldStart;
            event.end = oldEnd;
            this.refresh();
        }
    }

    /**
     * Collects event information in ICS-format and copies a link
     * to the ICS-file to the users clipboard
     */
    async export() {
        if (
            !(await showConfirm(
                "ICS",
                $localize`Export calendar information in ics-format?`
            ))
        ) {
            return;
        }
        const result = await toPromise(
            this.http.get("/calendar/export", {
                responseType: "text",
            })
        );
        if (result.ok) {
            this.icsURL = result.result;
            const copyResult = await to2(
                navigator.clipboard.writeText(this.icsURL)
            );
            if (copyResult.ok) {
                this.exportDone = $localize`ICS-url copied to clipboard.`;
            } else {
                this.exportDone = $localize`Error occurred when creating ICS-url.`;
            }
            this.refresh();
        } else {
            if (result.result.error.error) {
                await showMessageDialog(result.result.error.error);
            } else {
                await showMessageDialog(
                    $localize`Something went wrong. TIM admins have been notified about the issue.`
                );
            }
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
                    this.events.splice(this.events.indexOf(modifiedEvent), 1);
                }
            }
            this.refresh();
        }
    }

    /**
     * Returns true if current user is manager or owner, otherwise false
     */
    userIsManager(): boolean {
        return (
            itemglobals().curr_item.rights.manage ||
            itemglobals().curr_item.rights.owner
        );
    }

    /**
     * Initializes which different types of events the user can add to the calendar based on the markup
     * @private
     */
    private initEventTypes(): void {
        for (const eventTemplate in this.markup.eventTemplates) {
            if (this.markup.eventTemplates.hasOwnProperty(eventTemplate)) {
                // If current user is owner, add option to add all types of events
                if (itemglobals().curr_item.rights.owner) {
                    this.eventTypes.push(eventTemplate);
                }
                // Otherwise, only add options for types that has a setter group in which the current user belongs to
                else {
                    Users.getCurrent().groups.forEach((group) => {
                        if (!this.markup.eventTemplates) {
                            return;
                        }

                        if (
                            this.markup.eventTemplates[
                                eventTemplate
                            ].setters.includes(group.name)
                        ) {
                            if (!this.eventTypes.includes(eventTemplate)) {
                                this.eventTypes.push(eventTemplate);
                            }
                        }
                    });
                }
            }
        }
        if (this.eventTypes.length > 0) {
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
    ],
    declarations: [
        CalendarComponent,
        TimeViewSelectorComponent,
        DateTimeValidatorDirective,
        CalendarHeaderComponent,
        ShowWeekComponent,
    ],
    exports: [CalendarComponent, DateTimeValidatorDirective],
    providers: [
        {
            provide: CalendarDateFormatter,
            useClass: CustomDateFormatter,
        },
    ],
})
export class TimCalendarModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

const angularJsModule = createDowngradedModule((extraProviders) =>
    platformBrowserDynamic(extraProviders).bootstrapModule(TimCalendarModule)
);

doDowngrade(angularJsModule, "timCalendar", CalendarComponent);

export const moduleDefs = [angularJsModule];
