/**
 * Component for the dialog that is used for modifying, deleting and booking events
 *
 * @author Miika Immonen
 * @author Terhi Kamula
 * @author Anssi Lepikko
 * @author Touko Miettinen
 * @author Joose Tikkanen
 * @license MIT
 * @date 24.5.2022
 */
import {Component, NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {defaultWuffMessage, toPromise} from "tim/util/utils";
import {Users} from "tim/user/userService";
import {itemglobals} from "tim/util/globals";
import {showConfirm} from "tim/ui/showConfirmDialog";
import type {TIMCalendarEvent} from "tim/plugin/calendar/calendar.component";
import {
    postProcessCalendarEvent,
    TimCalendarModule,
} from "tim/plugin/calendar/calendar.component";
import {CommonModule} from "@angular/common";
import {PurifyModule} from "tim/util/purify.module";

@Component({
    selector: "tim-calendar-event-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container i18n header>
                Edit event
            </ng-container>
            <ng-container body>
                <form #form="ngForm" class="form-horizontal" timCalDateTimeValidator>
                    <fieldset [disabled]="loadingEvent">
                        <div class="form-group"
                             [ngClass]="{'has-error': ngModelTitle.invalid && ngModelTitle.dirty}">

                            <label i18n for="title" class="col-sm-3 control-label">Title</label>
                            <div class="col-sm-8">
                                <input i18n type="text" required
                                       maxlength="280"
                                       [(ngModel)]="title" #ngModelTitle="ngModel"
                                       (ngModelChange)="setMessage()"
                                       pattern="[^/]*"
                                       id="title" name="title"
                                       class="form-control"
                                       placeholder="Set title"
                                       [disabled]="!isEditEnabled()"/>
                            </div>
                            <div class="col-sm-3 control-label">
                                <label i18n for="owner">Owner</label>
                                <a class="mail-icon" href="mailto:{{ownerEmail}}"><i
                                        class="glyphicon glyphicon-envelope"></i></a>
                            </div>
                            <div class="col-sm-8">
                                <input i18n type="text"
                                       [(ngModel)]="ownerName"
                                       id="owner" name="owner"
                                       class="form-control"
                                       [disabled]="true"/>
                            </div>
                            <label i18n for="location" class="col-sm-3 control-label">Location</label>
                            <div class="col-sm-8">
                                <input type="text"
                                       maxlength="120"
                                       [(ngModel)]="location" #ngModelLocation="ngModel"
                                       (ngModelChange)="setMessage()"
                                       id="location"
                                       placeholder="Set location" i18n-placeholder
                                       name="location"
                                       class="form-control"
                                       [disabled]="!isEditEnabled()"/>
                            </div>
                            <label i18n for="maxSize" class="col-sm-3 control-label">Capacity</label>
                            <div class="col-sm-8">
                                <input type="number"
                                       min="0" max="2000" [(ngModel)]="maxSize"
                                       #ngModelMaxSize="ngModel"
                                       (ngModelChange)="setMessage()"
                                       id="maxSize" name="maxSize" class="form-control"
                                       [disabled]="!isEditEnabled()">
                            </div>
                            <label i18n for="sendNotifications" class="col-sm-3 control-label">Notifications</label>
                            <div class="col-sm-8">
                                <input type="checkbox"
                                       [(ngModel)]="sendNotifications"
                                       (ngModelChange)="setMessage()"
                                       id="sendNotifications" name="sendNotifications" class="form-control checkbox"
                                       [disabled]="!isEditEnabled()">
                            </div>
                            <label i18n for="important" class="col-sm-3 control-label">Important</label>
                            <div class="col-sm-8">
                                <input type="checkbox"
                                       [(ngModel)]="important"
                                       (ngModelChange)="setMessage()"
                                       id="important" name="important" class="form-control checkbox"
                                       [disabled]="!isEditEnabled()">
                            </div>
                        </div>
                        <div class="form-group" [hidden]="isPersonalEvent()">
                            <label i18n for="capacity" class="col-sm-3 control-label">Capacity</label>
                            <div class="col-sm-8">
                                <b><abbr title="Amount of enrolled users">{{getEventEnrollments()}}</abbr> / <abbr
                                        title="Maximum capacity of the event">{{getEventCapacity()}}</abbr></b>
                            </div>
                        </div>
                        <div [hidden]="multipleBookers()">
                            <div class="form-group" [hidden]="!userIsManager() || !eventHasBookings()">
                                <label i18n for="booker" class="col-sm-3 control-label">Booker</label>
                                <div class="col-sm-8">
                                    <input type="text"
                                           [(ngModel)]="booker"
                                           (ngModelChange)="setMessage()"
                                           id="booker" name="booker"
                                           class="form-control"
                                           placeholder=""
                                           disabled="true"/>
                                </div>
                            </div>
                            <div class="form-group" [hidden]="!userIsManager() || !eventHasBookings()">
                                <label i18n for="bookerEmail" class="col-sm-3 control-label">Booker email</label>
                                <div class="col-sm-8">
                                    <input type="text"
                                           [(ngModel)]="bookerEmail"
                                           (ngModelChange)="setMessage()"
                                           id="bookerEmail" name="bookerEmail"
                                           class="form-control"
                                           placeholder=""
                                           disabled="true"/>
                                </div>
                            </div>
                        </div>
                        <div [hidden]="hideBookerListLink()" class="form-group">
                            <label i18n for="bookers" class="col-sm-3 control-label">Bookers</label>
                            <a i18n href="/calendar/events/{{this.data.id}}/bookers" target="_blank" class="col-sm-9">Show
                                list
                                of all bookers</a>
                        </div>

                        <div class="form-group">
                            <div class="col-sm-4">
                                <label i18n for="startDate" class="dialog-label col-sm-8 control-label">From</label>

                                <div class="input-group">
                                    <input type="date"
                                           required
                                           [(ngModel)]="startDate"
                                           (ngModelChange)="setMessage()"
                                           id="startDate" name="startDate"
                                           class="form-control"
                                           [disabled]="!isEditEnabled()"
                                    >

                                    <input type="time"
                                           required
                                           [(ngModel)]="startTime"
                                           (ngModelChange)="setMessage()"
                                           id="startTime" name="startTime"
                                           class="form-control"
                                           [disabled]="!isEditEnabled()"
                                    >
                                </div>
                            </div>
                            <div class="col-sm-4">
                                <label i18n for="endDate" class="dialog-label col-sm-6 control-label">To</label>
                                <div class="input-group">
                                    <input type="date"
                                           required
                                           [(ngModel)]="endDate"
                                           (ngModelChange)="setMessage()"
                                           id="endDate" name="endDate"
                                           class="form-control"
                                           [disabled]="!isEditEnabled()"
                                    >
                                    <input type="time"
                                           required
                                           [(ngModel)]="endTime"
                                           (ngModelChange)="setMessage()"
                                           id="endTime" name="endTime"
                                           class="form-control"
                                           [disabled]="!isEditEnabled()">
                                </div>
                            </div>
                            <div class="col-sm-4" [hidden]="!isEditEnabled() || isPersonalEvent()">
                                <label i18n for="bookingStopDate" class="dialog-label col-sm-12 control-label">Book
                                    before</label>
                                <div class="input-group">

                                    <input type="date"
                                           required
                                           [(ngModel)]="bookingStopDate"
                                           (ngModelChange)="setMessage()"
                                           id="bookingStopDate" name="bookingStopDate"
                                           class="form-control"
                                           [disabled]="!isEditEnabled()"
                                    >

                                    <input type="time"
                                           required
                                           [(ngModel)]="bookingStopTime"
                                           (ngModelChange)="setMessage()"
                                           id="bookingStopTime" name="bookingStopTime"
                                           class="form-control"
                                           [disabled]="!isEditEnabled()"
                                    >
                                </div>
                            </div>
                        </div>
                        <div class="form-group">
                            <div class="input-group">
                                <div class="col-sm-12" *ngIf="description.length > 0 || description_html.length > 0 || isEditEnabled()">
                                    <label i18n class="col-sm-12 control-label" for="description">Event
                                        description</label>
                                    <ng-container *ngIf="isEditEnabled(); else viewMode">
                                        <textarea  maxlength="1020" id="description"
                                              [(ngModel)]="description" #ngModelDescription="ngModel"
                                              (ngModelChange)="setMessage()"
                                              name="description"
                                              class="form-control"
                                              [disabled]="!isEditEnabled()"></textarea>
                                    </ng-container>
                                    <ng-template #viewMode>
                                        <div [innerHTML]="description_html | purify"></div>
                                    </ng-template>
                                </div>
                                <div class="col-sm-12" [hidden]="hideBookerMessage()">
                                    <label i18n for="bookerMessage" class="col-sm-12 control-label">Message
                                        (optional)</label>
                                    <input type="text" [disabled]="hideBookerMessage()"
                                           [(ngModel)]="messageText"
                                           (ngModelChange)="setMessage()"
                                           name="messageText"
                                           class="form-control">

                                    <button i18n class="btn timButton message-btn" type="button"
                                            [disabled]="messageText.length<1"

                                            (click)="updateBookMessage()">Send message
                                    </button>
                                </div>
                            </div>
                            <div [(ngModel)]="bookerMessage" [hidden]="hideBookerMessage()" name="bookerMessage"
                                 ngDefaultControl class="col-sm-12" style="white-space: pre-line">{{bookerMessage}}
                            </div>
                        </div>
                    </fieldset>
                </form>
                <tim-alert *ngIf="loadingEventError">
                    <ng-container i18n>Could not load latest event data, is there internet connection? Please close the dialog and try again.</ng-container>
                </tim-alert>
                <tim-alert *ngIf="(maxSize>2000 || maxSize<0) && ngModelMaxSize.dirty">
                    <ng-container i18n>Event capacity must be 0-2000</ng-container>
                </tim-alert>
                <tim-alert *ngIf="form.invalid" severity="danger" [hidden]="!form.errors?.['bookingEndInvalid']">
                    <ng-container i18n *ngIf="form.errors?.['bookingEndInvalid']">Booking must be done before the
                        event
                    </ng-container>
                </tim-alert>
                <tim-alert *ngIf="form.invalid" severity="danger" [hidden]="!form.errors?.['dateInvalid']">
                    <ng-container i18n *ngIf="form.errors?.['dateInvalid']">Start of the event must be before end.
                    </ng-container>
                </tim-alert>
                <tim-alert *ngIf="ngModelTitle.invalid && ngModelTitle.dirty" severity="danger">
                    <ng-container i18n *ngIf="ngModelTitle.errors?.['required']">
                        Title is required.
                    </ng-container>

                    <ng-container i18n *ngIf="ngModelTitle.errors?.['pattern']">
                        Title should not contain the slash character.
                    </ng-container>
                </tim-alert>
                <tim-alert *ngIf="message" severity="danger">
                    <ng-container *ngIf="message">
                        {{message}}
                    </ng-container>
                </tim-alert>
            </ng-container>
            <ng-container class="col-sm-12" footer>
                <div class="col-sm-12 row">
                <span [hidden]="hideEventFulLSpan()" style="float: left; margin-left: 10px">
                    <span i18n>The event is full</span>
                    <span *ngIf="data.meta?.isExtra" i18n> (you can still book as extra)</span>
                </span>
                    <span [hidden]="!userHasBooked()" style="float: left">
                    <b i18n>You have booked this event.</b>
                </span>
                </div>
                <div class="col-sm-12 row">
                    <button i18n class="btn timButton btn-danger col-sm-4" type="button"
                            [hidden]="!userHasBooked() || isEditEnabled()" (click)="cancelBooking()"
                            [disabled]="loadingEvent"
                            style="float: left">
                        Cancel Booking
                    </button>
                    <button i18n class="btn timButton btn-danger col-sm-2" type="button" style="float: left"
                            (click)="deleteEvent()" [disabled]="form.invalid || loadingEvent" [hidden]="!isEditEnabled()">
                        Delete
                    </button>
                    <span *ngIf="showBookingButton()" [tooltip]="eventBookState.reason" class="booking-button"
                          container="body">
                        <button i18n
                                class="btn timButton" type="button"
                                (click)="bookEvent()"
                                [disabled]="!eventBookState.canBook || loadingEvent">
                            Book event
                        </button>    
                    </span>
                    <button i18n class="btn btn-default col-sm-2" type="button" style="float:right" (click)="dismiss()">
                        Cancel
                    </button>
                    <button i18n class="btn timButton col-sm-2" type="submit" style="float:right"
                            (click)="saveChanges()" [disabled]="form.invalid || loadingEvent"
                            [hidden]="!isEditEnabled()">
                        Save
                    </button>
                </div>
            </ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["./calendar-event-dialog.component.scss"],
})
export class CalendarEventDialogComponent extends AngularDialogComponent<
    TIMCalendarEvent,
    TIMCalendarEvent
> {
    protected dialogName = "CalendarEventEdit";

    loadingEvent = true;
    loadingEventError = false;
    title = "";
    ownerName = "";
    ownerEmail = "";
    location = "";
    maxSize = 0;
    sendNotifications = true;
    important = false;
    message?: string;
    bookingStopTime = "";
    bookingStopDate = "";
    messageText = "";
    startDate = "";
    startTime = "";
    endDate = "";
    endTime = "";
    booker = "";
    bookerEmail: string | null = "";
    bookerMessage = "";
    description = "";
    description_html = "";
    userBooked = false;
    eventBookState: {canBook: boolean; reason?: string} = {
        canBook: true,
    };

    constructor(private http: HttpClient) {
        super();
    }

    /**
     * Sends the event information to the TIM server to be persisted
     */
    async saveChanges(): Promise<void> {
        const id = this.data.id;
        if (!id) {
            return;
        }
        if (!this.description) {
            this.description = "";
        }
        if (!this.location) {
            this.location = "";
        }
        if (this.eventHasBookings() && this.maxSize < this.getEventCapacity()) {
            this.maxSize = this.getEventCapacity();
        }

        const eventToEdit = {
            max_size: this.maxSize,
            description: this.description,
            title: this.title,
            location: this.location,
            start: new Date(`${this.startDate}T${this.startTime}`),
            end: new Date(`${this.endDate}T${this.endTime}`),
            signup_before: new Date(
                `${this.bookingStopDate}T${this.bookingStopTime}`
            ),
            send_notifications: this.sendNotifications,
            important: this.important,
        };

        const result = await toPromise(
            this.http.put(`/calendar/events/${id}`, {
                event: eventToEdit,
            })
        );
        if (result.ok) {
            this.data.meta!.maxSize = eventToEdit.max_size;
            this.data.title = eventToEdit.title;
            this.data.meta!.description = eventToEdit.description;
            this.data.meta!.location = eventToEdit.location;
            this.data.start = eventToEdit.start;
            this.data.end = eventToEdit.end;
            this.data.meta!.signup_before = eventToEdit.signup_before;
            this.data.meta!.send_notifications = eventToEdit.send_notifications;
            this.data.meta!.important = eventToEdit.important;
            this.close(this.data);
        } else {
            if (result.result.error.error) {
                this.setMessage(result.result.error.error);
            } else {
                this.setMessage(defaultWuffMessage);
            }
        }
    }

    /**
     * Sends the delete request for the event to the TIM server
     */
    async deleteEvent() {
        const eventToDelete = this.data;

        if (!eventToDelete.id || !eventToDelete.meta) {
            return;
        }
        const enrollmentMessage =
            eventToDelete.meta.enrollments > 0
                ? " " +
                  $localize`This will also delete ${eventToDelete.meta.enrollments} enrollments!`
                : "";
        if (
            !eventToDelete.meta.tmpEvent &&
            (await showConfirm(
                $localize`Delete an Event`,
                $localize`Are you sure you want to delete the event "${this.data.title}"?${enrollmentMessage}`
            ))
        ) {
            const result = await toPromise(
                this.http.delete(`/calendar/events/${eventToDelete.id}`)
            );
            if (result.ok) {
                eventToDelete.meta.deleted = true;
                this.close(eventToDelete);
            } else {
                if (result.result.error.error) {
                    this.setMessage(result.result.error.error);
                } else {
                    this.setMessage(defaultWuffMessage);
                }
            }
        } else {
            // Do nothing
        }
    }

    /**
     * Sets the error message to the dialog
     * @param message Error message
     */
    setMessage(message?: string): void {
        this.message = message;
    }

    /**
     * Sets the dialog data with event information when loaded
     */
    async ngOnInit() {
        this.initForm();

        const res = await toPromise(
            this.http.get<TIMCalendarEvent>(`/calendar/events/${this.data.id!}`)
        );
        if (res.ok) {
            this.data = postProcessCalendarEvent(
                res.result,
                this.data.meta!.editEnabled
            );
            this.initForm();
            this.loadingEvent = false;
        } else {
            this.loadingEventError = true;
        }
    }

    private initForm() {
        this.maxSize = this.getEventCapacity();
        this.title = this.data.title;
        this.location = this.data.meta!.location;
        this.description = this.data.meta!.description;
        this.description_html = this.data.meta!.description_html ?? "";
        this.sendNotifications = this.data.meta!.send_notifications;
        this.important = this.data.meta!.important;
        this.ownerName = this.data.meta!.owner?.name ?? "";
        this.ownerEmail = this.data.meta!.owner?.email ?? "";
        const startOffset = this.data.start.getTimezoneOffset();
        const startDate = new Date(
            this.data.start.getTime() - startOffset * 60 * 1000
        );
        const bookerGroups = this.data.meta!.booker_groups;
        if (bookerGroups) {
            bookerGroups.forEach((group) => {
                // TODO: These attributes are only used with events with maximum capacity of 1
                if (group.message) {
                    this.bookerMessage = group.message;
                }
                group.users.forEach((user) => {
                    this.bookerEmail = user.email;
                    this.booker = user.name;
                });
            });
        }

        const startDateTime = startDate.toISOString().split("T");

        this.startDate = startDateTime[0];

        this.startTime = startDateTime[1].split(".")[0];

        const finalBookDate: Date = this.data.meta!.signup_before;
        const bookOffset = finalBookDate.getTimezoneOffset();
        const bookStopDate = new Date(
            finalBookDate.getTime() - bookOffset * 60 * 1000
        );

        const bookDateTime = bookStopDate.toISOString().split("T");

        this.bookingStopDate = bookDateTime[0];
        this.bookingStopTime = bookDateTime[1].split(".")[0];

        if (this.data.end) {
            const endOffset = this.data.start.getTimezoneOffset();
            const endDate = new Date(
                this.data.end.getTime() - endOffset * 60 * 1000
            );
            const endDateTime = endDate.toISOString().split("T");

            this.endDate = endDateTime[0];
            this.endTime = endDateTime[1].split(".")[0];
        }
        this.initEventBookState();
    }

    /**
     * True if editing is enabled for the event, otherwise false
     */
    isEditEnabled() {
        if (this.data.meta) {
            return this.data.meta.editEnabled;
        }
        return false; // Events should always have their meta field
    }

    /**
     *  Used when a booker of an event sends message to the enrollment of an event.
     *  Updates the message linked to the enrollment with a chat-like timestamp + identifier of the booker.
     *  TODO: Currently works only with events that has maximum capacity of 1
     */
    async updateBookMessage() {
        this.setMessage();
        if (!this.messageText) {
            this.messageText = "";
        }
        if (!this.bookerMessage) {
            this.bookerMessage = "";
        }
        const eventToBook = this.data;
        let bookerGroup = "";
        if (this.data.meta!.booker_groups.length > 0) {
            bookerGroup = this.data.meta!.booker_groups[0].name;
        }

        if (
            !(await showConfirm(
                $localize`Post message`,
                $localize`Post message to booking?`
            ))
        ) {
            return;
        }
        const result = await toPromise(
            this.http.put<{bookMessage: string}>("/calendar/bookings", {
                event_id: eventToBook.id,
                booker_msg: this.messageText,
                booker_group: bookerGroup,
            })
        );
        if (result.ok) {
            this.bookerMessage = result.result.bookMessage;
            this.messageText = "";
            if (this.data.meta) {
                if (this.data.meta.booker_groups.length > 0) {
                    this.data.meta.booker_groups[0].message =
                        result.result.bookMessage;
                }
            }
        } else {
            this.setMessage(
                $localize`Could not post message: ${result.result.error.error}`
            );
        }
    }

    /**
     * Sends the booking request for the event to the TIM-server
     */
    async bookEvent() {
        const eventToBook = this.data;
        if (
            !(await showConfirm(
                $localize`Book an Event`,
                $localize`Book the event "${this.data.title}"?`
            ))
        ) {
            return;
        }
        if (!this.eventBookTimeOpen()) {
            return;
        }

        const result = await toPromise(
            this.http.post("/calendar/bookings", {
                event_id: eventToBook.id,
                booker_msg: this.messageText,
            })
        );
        if (result.ok) {
            this.data.meta!.enrollments++;

            const booker = Users.getCurrent().groups.find((group) => {
                return group.name === Users.getCurrent().name;
            });
            if (booker) {
                let fullName = Users.getCurrent().real_name;
                if (!fullName) {
                    fullName = Users.getCurrent().name;
                }
                this.data.meta!.booker_groups.push({
                    name: booker.name,
                    message: this.bookerMessage,
                    users: [
                        {
                            id: Users.getCurrent().id,
                            name: `${fullName}`,
                            email: Users.getCurrent().email,
                        },
                    ],
                });
            }

            this.close(eventToBook);
        } else {
            if (result.result.error.error) {
                this.setMessage(result.result.error.error);
            } else {
                this.setMessage(defaultWuffMessage);
            }
        }
    }

    /**
     * Cancel booking of a selected event from the current user. Sends the id of the event to the API, which handles
     * recognition of the current user group.
     *
     */
    async cancelBooking() {
        const openEvent = this.data;
        const eventId = this.data.id;
        if (
            !eventId ||
            !(await showConfirm(
                $localize`Cancel booking`,
                $localize`Are you sure you want to cancel booking "${this.data.title}"?`
            ))
        ) {
            return;
        } //
        const result = await toPromise(
            this.http.delete(`/calendar/bookings/${eventId}`)
        );
        if (result.ok) {
            this.data.meta!.enrollments--;

            const currentUserName = Users.getCurrent().name;
            this.data.meta!.booker_groups =
                this.data.meta!.booker_groups.filter(
                    (g) => g.name != currentUserName
                );
            this.close(openEvent);
        } else {
            if (result.result.error.error) {
                this.setMessage(result.result.error.error);
            } else {
                this.setMessage(defaultWuffMessage);
            }
        }
    }

    /**
     * Returns true if the event is full otherwise false. Events with the capacity of 0 are considered not full.
     */
    eventIsFull() {
        if (this.data.meta) {
            if (this.data.meta.maxSize == 0) {
                return false;
            }
            return this.data.meta.enrollments >= this.data.meta.maxSize;
        }
        return false; // Events should always have their meta field
    }

    /**
     * Returns true if user is manager or owner, otherwise false
     */
    userIsManager() {
        return (
            itemglobals().curr_item.rights.manage ||
            itemglobals().curr_item.rights.owner
        );
    }

    /**
     * Returns true if there are any bookings on the handled event otherwise false
     */
    eventHasBookings() {
        return this.data.meta!.enrollments > 0;
    }

    /**
     *
     * Compares the current time to the time set by the maker of an event as the end time/date for booking.
     * Returns true if the current time allows the booking of the event.
     */
    eventBookTimeOpen() {
        const nowDate = new Date();
        const bookBefore = new Date(this.data.meta!.signup_before);
        return bookBefore.getTime() > nowDate.getTime();
    }

    initEventBookState() {
        if (this.data.meta?.isExtra) {
            this.eventBookState = {canBook: true, reason: undefined};
            return;
        }
        if (!this.data.meta?.rights?.can_enroll) {
            this.eventBookState = {
                canBook: false,
                reason: $localize`You do not have the right to book this event`,
            };
        } else if (this.eventIsFull()) {
            this.eventBookState = {
                canBook: false,
                reason: $localize`The event is full`,
            };
        } else if (!this.eventBookTimeOpen()) {
            this.eventBookState = {
                canBook: false,
                reason: $localize`The booking time is over or the event has already passed`,
            };
        } else {
            this.eventBookState = {canBook: true, reason: undefined};
        }
    }

    /**
     * Returns true if the booker message -field should not be shown to the user
     */
    hideBookerMessage() {
        if (this.data.meta!.maxSize != 1) {
            return true;
            // TODO: Only supports use of booker message in events for one attendee
        }
        if (
            this.data.meta?.extraEnrollments !== undefined &&
            this.data.meta?.extraEnrollments !== null
        ) {
            // TODO: Extras should not affect booker messages either
            return true;
        }
        if (this.isEditEnabled() || !this.eventHasBookings()) {
            return true;
        } else if (this.userIsManager() || this.userHasBooked()) {
            return false;
        } else if (this.eventIsFull()) {
            return true;
        }
        return true;
    }

    /**
     * Returns true if a user has booked the handled event otherwise false
     */
    userHasBooked() {
        this.userBooked = false;
        const bookers = this.data.meta!.booker_groups;
        for (const booker1 of bookers) {
            for (const userGroup of Users.getCurrent().groups) {
                if (booker1.name == userGroup.name) {
                    this.userBooked = true;
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * True if the event can have multiple bookers, false if not
     */
    multipleBookers() {
        if (this.data.meta) {
            return (
                this.data.meta.maxSize > 1 ||
                this.data.meta.enrollments +
                    (this.data.meta.extraEnrollments ?? 0) >
                    1
            );
        }
        return false; // Events should always have their meta field
    }

    /**
     * True if event has no bookings.
     * If event has bookings: true if user is not manager or the event can only have one booking. Otherwise, false.
     */
    hideBookerListLink() {
        if (!this.eventHasBookings()) {
            return true;
        }
        return !this.userIsManager() || !this.multipleBookers();
    }

    /**
     * Amount of enrollments for the event
     */
    getEventEnrollments() {
        if (this.data.meta) {
            return this.data.meta.enrollments;
        }
        return -1; // Events should always have their meta field
    }

    /**
     * The maximum capacity for the event
     */
    getEventCapacity() {
        if (this.data.meta) {
            return this.data.meta.maxSize;
        }
        return -1; // Events should always have their meta field
    }

    /**
     * True if max size of the event is 0.
     * If not: true if editing is enabled or user has booked the event. Otherwise, false.
     */
    showBookingButton() {
        return (
            (!this.data.meta || this.data.meta.maxSize != 0) &&
            !this.isEditEnabled() &&
            !this.userHasBooked()
        );
    }

    /**
     * True if editing is enabled. If not: true if event is not full or user has booked the event. Otherwise, false.
     */
    hideEventFulLSpan() {
        if (this.isEditEnabled()) {
            return true;
        }
        return !this.eventIsFull() || this.userHasBooked();
    }

    /**
     * True if max size of the event is 0, otherwise false
     */
    isPersonalEvent() {
        if (this.data.meta) {
            return this.data.meta.maxSize === 0;
        }
        return false; // Events should always have their meta field
    }
}

@NgModule({
    declarations: [CalendarEventDialogComponent],
    imports: [
        DialogModule,
        FormsModule,
        TimUtilityModule,
        CommonModule,
        HttpClientModule,
        TimCalendarModule,
        TooltipModule.forRoot(),
        PurifyModule,
    ],
    exports: [CalendarEventDialogComponent],
})
export class CalendarEventDialogModule {}
