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
import type {ApplicationRef, DoBootstrap, OnInit} from "@angular/core";
import {
    ChangeDetectionStrategy,
    ChangeDetectorRef,
    Component,
    ElementRef,
    NgModule,
    ViewEncapsulation,
} from "@angular/core";
import * as t from "io-ts";
import type {
    CalendarEvent,
    CalendarEventTimesChangedEvent,
} from "angular-calendar";
import {
    CalendarDateFormatter,
    CalendarEventTitleFormatter,
    CalendarModule,
    CalendarView,
    DateAdapter,
} from "angular-calendar";
import type {WeekViewHourSegment} from "calendar-utils";
import {adapterFactory} from "angular-calendar/date-adapters/date-fns";
import {CommonModule, registerLocaleData} from "@angular/common";
import localeFi from "@angular/common/locales/fi";
import localeSv from "@angular/common/locales/sv";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {DomSanitizer} from "@angular/platform-browser";
import {finalize, fromEvent, takeUntil} from "rxjs";
import {addDays, addMinutes, endOfWeek, getISOWeek, setISOWeek} from "date-fns";
import moment from "moment";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {
    capitalizeFirstLetter,
    closest,
    DateFromString,
    defaultWuffMessage,
    MomentDurationFromString,
    MomentFromString,
    to2,
    toPromise,
} from "tim/util/utils";
import {Users} from "tim/user/userService";
import {itemglobals} from "tim/util/globals";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {CustomDateFormatter} from "tim/plugin/calendar/custom-date-formatter.service";
import {CustomEventTitleFormatter} from "tim/plugin/calendar/custom-event-title-formatter.service";
import {
    TIME_VIEW_EVENING_HOURS,
    TIME_VIEW_MORNING_HOURS,
    TIME_VIEW_SLOT_SIZES,
    TimeViewSelectorComponent,
} from "tim/plugin/calendar/timeviewselector.component";
import {showCalendarEventDialog} from "tim/plugin/calendar/showCalendarEventDialog";
import {DateTimeValidatorDirective} from "tim/plugin/calendar/datetimevalidator.directive";
import {CalendarHeaderComponent} from "tim/plugin/calendar/calendar-header.component";
import {CalendarHeaderMinimalComponent} from "tim/plugin/calendar/calendar-header-minimal.component";
import {ShowWeekComponent} from "tim/plugin/calendar/show-week.component";
import {showEditJsonEventDialog} from "tim/plugin/calendar/showEditJsonEventDialog";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {showExportICal} from "tim/plugin/calendar/showExportICal";

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
    location: nullable(t.string),
    description: nullable(t.string),
    bookers: t.array(t.string),
    setters: t.array(t.string),
    extraBookers: t.array(t.string),
    tags: t.array(t.string),
    signupBefore: t.union([MomentFromString, MomentDurationFromString, t.null]),
    capacity: t.number,
    sendNotifications: t.boolean,
    important: t.boolean,
});

const FilterOptions = t.type({
    groups: nullable(t.array(t.string)),
    tags: nullable(t.array(t.string)),
    fromDate: nullable(DateFromString),
    toDate: nullable(DateFromString),
    showBooked: withDefault(t.boolean, true),
    showImportant: withDefault(t.boolean, false),
    includeOwned: withDefault(t.boolean, false),
    includeDocumentEvents: withDefault(t.boolean, true),
});

const CalendarViewMode = t.union([
    t.literal("month"),
    t.literal("week"),
    t.literal("day"),
]);

const ViewOptions = t.type({
    dayStartHour: t.number,
    dayEndHour: t.number,
    segmentDuration: t.number,
    date: nullable(DateFromString),
    week: nullable(t.number),
    minWeek: nullable(t.number),
    mode: CalendarViewMode,
    advancedUI: nullable(t.boolean),
});

const CalendarMarkup = t.intersection([
    t.partial({
        filter: FilterOptions,
        eventTemplates: t.record(t.string, EventTemplate),
        viewOptions: ViewOptions,
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
    description_html?: string;
    enrollments: number;
    extraEnrollments?: number | null;
    maxSize: number;
    isExtra: boolean;
    send_notifications: boolean;
    important: boolean;
    owner?: {
        name: string;
        email: string;
    };
    rights?: {
        can_enroll: boolean;
        manager: boolean;
        creator: boolean;
    };
    booker_groups: {
        name: string;
        message: string;
        users: {id: number; name: string; email: string | null}[];
    }[];
};

export type TIMCalendarEvent = CalendarEvent<TIMEventMeta>;

export function postProcessCalendarEvent(
    event: TIMCalendarEvent,
    editable: boolean
) {
    event.start = new Date(event.start);
    if (event.end) {
        event.end = new Date(event.end);
    }
    event.meta = {
        description: event.meta!.description,
        description_html: event.meta!.description_html,
        tmpEvent: false,
        enrollments: event.meta!.enrollments,
        extraEnrollments: event.meta!.extraEnrollments,
        maxSize: event.meta!.maxSize,
        location: event.meta!.location,
        booker_groups: event.meta!.booker_groups,
        isExtra: event.meta!.isExtra,
        editEnabled: editable,
        send_notifications: event.meta!.send_notifications,
        important: event.meta!.important,
        owner: event.meta!.owner,
        rights: event.meta!.rights,
        signup_before: new Date(event.meta!.signup_before),
    };
    event.resizable = {
        beforeStart: editable,
        afterEnd: editable,
    };
    return event;
}

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
        <tim-calendar-header 
                [locale]="locale" 
                [(view)]="viewMode" 
                [(viewDate)]="viewDate" 
                *ngIf="advancedUI">
        </tim-calendar-header>
        <div class="row text-center" *ngIf="advancedUI">
            <div class="col-md-4">
                <div class="btn-group edit-btn" [hidden]="!userIsManager() || this.eventTypes.length == 0">
                    <button i18n (click)="enableEditing(false)" [class.active]="!editEnabled" class="btn timButton">View</button>
                    <button i18n (click)="enableEditing(true)" [class.active]="editEnabled" class="btn timButton">Edit</button>
                </div>
            </div>
            <div class="col-md-4">
                <div [style.visibility]="editEnabled ? 'visible' : 'hidden'" class="event-btn">
                    <select [(ngModel)]="selectedEvent" class="form-control">
                        <option *ngFor="let button of eventTypes" [value]="button">{{button}}</option>
                    </select>
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
        <div class="cal-ui-switch">
            <label><input type="checkbox" [(ngModel)]="advancedUI" /><ng-container i18n>Advanced</ng-container></label>
            <tim-calendar-minimal-header 
                    [locale]="locale" 
                    [(view)]="viewMode" 
                    [(viewDate)]="viewDate" 
                    *ngIf="!advancedUI">
            </tim-calendar-minimal-header>
        </div>
        <div class="progress" *ngIf="loadingEvents">
            <div class="progress-bar progress-bar-striped active" role="progressbar" [style.width.%]="100"></div>    
        </div>
        <div [ngSwitch]="viewMode">
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
        <tim-time-view-selector [style.visibility]="viewMode == 'month' ? 'hidden' : 'visible'"
                                [(segmentDuration)]="segmentDuration"
                                [(startHour)]="dayStartHour"
                                [(endHour)]="dayEndHour"
                                *ngIf="advancedUI">
        </tim-time-view-selector>
        <div class="button-panel" *ngIf="advancedUI">
            <button i18n class="btn timButton" (click)="export()">Export calendar (ICS)</button>
            <button i18n class="btn timButton" *ngIf="editEnabled" (click)="openEventEditor()">Mass edit (JSON)</button>
            <span class="exportDone">{{exportDone}}</span>
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
    viewMode!: CalendarView;
    viewDate!: Date;

    events: TIMCalendarEvent[] = [];

    clickedDate?: Date;

    clickedColumn?: number;

    dragToCreateActive = false;

    editEnabled: boolean = false;
    dialogOpen: boolean = false;
    advancedUI: boolean = true;

    eventTypes: string[] = [];
    selectedEvent: string = "";
    loadingEvents = false;

    /*
    TODO: Checkbox values
    checkboxEvents = [
        {name: "Ohjaus", value: "1", checked: true},
        {name: "Luento", value: "2", checked: true},
        {name: "Opetusryhmä", value: "3", checked: true},
    ];
    */

    locale: string = Users.getCurrentLocale();

    weekStartsOn = 1 as const;

    /* The default values of calendar view that can be adjusted with the time view selector -component. */
    dayStartHour!: number;
    dayEndHour!: number;
    segmentsInHour!: number;
    segmentHeight = TIME_SLOT_HEIGHT;

    private segmentMinutes!: number;
    minimumEventHeight!: number;
    requiresTaskId = false;

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
        this.viewMode = CalendarView.Week;
    }

    /**
     * Enables the user to click on a date in the week view to see the day view of that day
     * @param date the date of clicked day
     */
    viewDay(date: Date) {
        this.clickedDate = date;
        this.viewDate = date;
        this.viewMode = CalendarView.Day;
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
        let location: string | null = "";
        let description: string = "";
        const signupBefore = new Date(segment.date);
        if (this.markup.eventTemplates) {
            const template = this.markup.eventTemplates[this.selectedEvent];
            title = template.title;
            location = template.location ?? "";
            description = template.description ?? "";
            if (template.signupBefore) {
                if (moment.isMoment(template.signupBefore)) {
                    signupBefore.setTime(template.signupBefore.valueOf());
                } else if (moment.isDuration(template.signupBefore)) {
                    signupBefore.setTime(
                        moment(signupBefore)
                            .subtract(template.signupBefore)
                            .valueOf()
                    );
                }
            }
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
                signup_before: signupBefore,
                description: description,
                enrollments: 0,
                location: location,
                isExtra: false,
                maxSize: 1, // TODO: temporary solution
                booker_groups: [],
                owner: {
                    name: `${Users.getCurrent().real_name ?? ""} (${
                        Users.getCurrent().last_name ?? ""
                    })`,
                    email: Users.getCurrent().email ?? "",
                },
                rights: {
                    can_enroll: true,
                    creator: true,
                    manager: true,
                },
                editEnabled: this.editEnabled,
                send_notifications: true,
                important: false,
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
        this.loadingEvents = true;
        this.events = [...this.events]; // TODO: Find out what is the purpose of this line
        const now = Date.now();
        this.events.forEach((event) => {
            if (now > event.start.getTime()) {
                event.color = colors.gray;
                return;
            }
            // Event without max capacity are always in blue (since they are not bookable)
            if (event.meta!.maxSize === 0) {
                event.color = colors.blue;
                return;
            }
            if (event.meta!.booker_groups) {
                const isCurrentUserBooker = event.meta!.booker_groups.some(
                    (group) =>
                        group.users.some(
                            (user) => user.id === Users.getCurrent().id
                        )
                );
                if (isCurrentUserBooker) {
                    event.color = colors.green;
                    return;
                }
            }
            if (event.meta!.enrollments >= event.meta!.maxSize) {
                event.color = colors.red;
                return;
            }
            if (
                event.meta?.signup_before &&
                now > event.meta.signup_before.getTime()
            ) {
                event.color = colors.gray;
                return;
            }
            event.color = colors.blue;
        });
        this.loadingEvents = false;
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
        return {};
    }

    private get viewOptions() {
        return this.markup.viewOptions!;
    }

    /**
     * Called when the plugin is loaded. Loads the user's events
     */
    ngOnInit() {
        this.icsURL = "";
        super.ngOnInit();
        this.viewMode =
            CalendarView[capitalizeFirstLetter(this.viewOptions.mode)];
        if (this.viewOptions.date) {
            this.viewDate = this.viewOptions.date;
        } else if (this.viewOptions.week) {
            this.viewDate = setISOWeek(new Date(), this.viewOptions.week);
        } else if (this.viewOptions.minWeek) {
            const now = new Date();
            this.viewDate = setISOWeek(
                now,
                Math.max(this.viewOptions.minWeek, getISOWeek(now))
            );
        } else {
            this.viewDate = new Date();
        }

        this.segmentDuration = closest(
            TIME_VIEW_SLOT_SIZES,
            this.viewOptions.segmentDuration
        );
        this.dayStartHour = closest(
            TIME_VIEW_MORNING_HOURS,
            this.viewOptions.dayStartHour
        );
        this.dayEndHour = closest(
            TIME_VIEW_EVENING_HOURS,
            this.viewOptions.dayEndHour
        );

        this.initEventTypes();
        if (!this.isPreview()) {
            void this.loadEvents();
        }
        this.advancedUI = this.viewOptions.advancedUI ?? true;
    }

    private get filterParams() {
        const res: Record<string, string> = {};
        if (!this.markup.filter) {
            return res;
        }
        if (
            this.markup.filter.groups !== null &&
            this.markup.filter.groups !== undefined
        ) {
            res.groups = this.markup.filter.groups.join(",");
        }
        if (
            this.markup.filter.tags !== null &&
            this.markup.filter.tags !== undefined
        ) {
            res.tags = this.markup.filter.tags.join(",");
        }
        if (this.markup.filter.fromDate) {
            res.fromDate = this.markup.filter.fromDate.toISOString();
        }
        if (this.markup.filter.toDate) {
            res.toDate = this.markup.filter.toDate.toISOString();
        }
        res.showBooked = this.markup.filter.showBooked.toString();
        res.includeOwned = this.markup.filter.includeOwned.toString();
        res.showImportant = this.markup.filter.showImportant.toString();
        res.docId = itemglobals().curr_item.id.toString();
        res.includeDocumentEvents =
            this.markup.filter.includeDocumentEvents.toString();
        return res;
    }

    /**
     * Loads the user's events from the TIM server
     * @private
     */
    private async loadEvents() {
        this.loadingEvents = true;
        const result = await toPromise(
            this.http.get<TIMCalendarEvent[]>("/calendar/events", {
                params: this.filterParams,
            })
        );
        if (result.ok) {
            result.result.forEach((event) =>
                postProcessCalendarEvent(event, this.editEnabled)
            );
            this.events = result.result;
            this.refresh();
        } else {
            if (result.result.error.error) {
                await showMessageDialog(result.result.error.error);
            } else {
                await showMessageDialog(defaultWuffMessage);
            }
        }
    }

    /**
     * Sends the newly added event to the TIM server to be persisted.
     * Handles sending multiple events at the same time.
     */
    async saveChanges() {
        const eventsToAdd = this.events.filter((e) => this.isTempEvent(e));
        if (!eventsToAdd) {
            return;
        }

        let bookerGroups: string[] = [];
        let setterGroups: string[] = [];
        let extraBookersGroups: string[] = [];
        let sendNotifications = true;
        let isImportant = false;
        let capacity: number = 0;
        let tags: string[] = [];
        if (this.markup.eventTemplates) {
            const template = this.markup.eventTemplates[this.selectedEvent];
            bookerGroups = template.bookers;
            setterGroups = template.setters;
            extraBookersGroups = template.extraBookers;
            capacity = template.capacity;
            tags = template.tags;
            sendNotifications = template.sendNotifications;
            isImportant = template.important;
        }

        const result = await toPromise(
            this.http.post<TIMCalendarEvent[]>("/calendar/events", {
                origin_doc_id: itemglobals().curr_item.id,
                events: eventsToAdd.map((event) => ({
                    title: event.title,
                    location: event.meta!.location,
                    description: event.meta!.description,
                    start: event.start,
                    end: event.end,
                    signup_before: new Date(event.meta!.signup_before),
                    booker_groups: bookerGroups,
                    setter_groups: setterGroups,
                    extra_booker_groups: extraBookersGroups,
                    send_notifications: sendNotifications,
                    important: isImportant,
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
                        extraEnrollments: event.meta!.extraEnrollments,
                        description: event.meta!.description,
                        maxSize: event.meta!.maxSize,
                        location: event.meta!.location,
                        isExtra: event.meta!.isExtra,
                        send_notifications: event.meta!.send_notifications,
                        important: event.meta!.important,
                        booker_groups: [],
                        owner: event.meta!.owner,
                        rights: event.meta!.rights,
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
                await showMessageDialog(defaultWuffMessage);
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
                await showMessageDialog(defaultWuffMessage);
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
    export() {
        void showExportICal();
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
            const originalEvent = this.events.find(
                (e) => e.id === modifiedEvent.id
            );
            if (modifiedEvent.meta && originalEvent) {
                if (modifiedEvent.meta.deleted) {
                    this.events.splice(this.events.indexOf(originalEvent), 1);
                } else {
                    Object.assign(originalEvent, modifiedEvent);
                    originalEvent.meta = modifiedEvent.meta;
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

    async openEventEditor() {
        const res = await showEditJsonEventDialog({
            filterParams: this.filterParams,
        });
        if (res.ok && res.result) {
            await this.loadEvents();
        }
    }
}

@NgModule({
    imports: [
        CommonModule,
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
        CalendarHeaderMinimalComponent,
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

registerPlugin("tim-calendar", TimCalendarModule, CalendarComponent);
