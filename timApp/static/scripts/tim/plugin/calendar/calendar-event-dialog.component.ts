import {Component, NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import {DialogModule} from "../../ui/angulardialog/dialog.module";
import {TimUtilityModule} from "../../ui/tim-utility.module";
import {AngularDialogComponent} from "../../ui/angulardialog/angular-dialog-component.directive";
import {toPromise} from "../../util/utils";
import {Users} from "../../user/userService";
import {itemglobals} from "../../util/globals";
import {showConfirm} from "../../ui/showConfirmDialog";
import {KATTIModule, TIMCalendarEvent} from "./calendar.component";

@Component({
    selector: "tim-calendar-event-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                Edit event
            </ng-container>
            <ng-container body>
                <form #form="ngForm" class="form-horizontal" timCalDateTimeValidator>
                    <div class="form-group"
                         [ngClass]="{'has-error': ngModelTitle.invalid && ngModelTitle.dirty}">

                        <label for="title" class="col-sm-2 control-label">Title</label>
                        <div class="col-sm-9">
                            <input type="text" required
                                   maxlength="280"
                                   [(ngModel)]="title" #ngModelTitle="ngModel"
                                   (ngModelChange)="setMessage()"
                                   pattern="[^/]*"
                                   id="title" name="title"
                                   class="form-control"
                                   placeholder="Set title"
                                   [disabled]="!isEditEnabled()"/>
                        </div>
                        <label for="location" class="col-sm-2 control-label">Location</label>
                        <div class="col-sm-9">
                            <input type="text"
                            maxlength="120"
                                   [(ngModel)]="location" #ngModelLocation="ngModel"
                                   (ngModelChange)="setMessage()"
                                   id="location"
                                   placeholder="Set location"
                                   name="location"
                                   class="form-control"
                                   [disabled]="!isEditEnabled()"/>
                        </div>
                    </div>
                    <div class="form-group" [hidden]="!userIsManager() || !eventHasBookings()">
                        <label for="booker" class="col-sm-2 control-label">Booker</label>
                        <div class="col-sm-9">
                            <input type="text"
                                   [(ngModel)]="booker"
                                   (ngModelChange)="setMessage()"
                                   id="booker" name="booker"
                                   class="form-control"
                                   placeholder="Not booked"
                                   disabled="true"/>
                        </div>
                        <label for="bookerEmail" class="col-sm-2 control-label">Booker email</label>
                        <div class="col-sm-9">
                            <input type="text"
                                   [(ngModel)]="bookerEmail"
                                   (ngModelChange)="setMessage()"
                                   id="bookerEmail" name="bookerEmail"
                                   class="form-control"
                                   placeholder=""
                                   disabled="true"/>
                        </div>
                    </div>
                    <div class="form-group ">
                        <div class="col-sm-4">
                            <label for="startDate" class="col-sm-8 control-label">From</label>
                            <div class="input-group">
                                <input i18n-placeholder type="date"
                                       required
                                       [(ngModel)]="startDate"
                                       (ngModelChange)="setMessage()"
                                       id="startDate" name="startDate"
                                       class="form-control"
                                       [disabled]="!isEditEnabled()"
                                       >

                                <input i18n-placeholder type="time"
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
                        <label for="endDate" class="col-sm-6 control-label">To</label>
                            <div class="input-group">
                                <input i18n-placeholder type="date"
                                       required
                                       [(ngModel)]="endDate"
                                       (ngModelChange)="setMessage()"
                                       id="endDate" name="endDate"
                                       class="form-control"
                                       [disabled]="!isEditEnabled()"
                                        >
                                <input i18n-placeholder type="time"
                                       required
                                       [(ngModel)]="endTime"
                                       (ngModelChange)="setMessage()"
                                       id="endTime" name="endTime"
                                       class="form-control"
                                       [disabled]="!isEditEnabled()">
                            </div>
                        </div>
                        <div class="col-sm-4" [hidden]="!isEditEnabled()">
                        <label for="bookingStopDate" class="col-sm-12 control-label">Book before</label>
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
                            <div class="col-sm-12">
                            <label class="col-sm-12 control-label" for="description">Event description</label>
                            <textarea maxlength="1020"
                             [(ngModel)]="description"
                             (ngModelChange)="setMessage()"
                             name="description"
                             class="form-control"
                             [disabled]="!isEditEnabled()">
                            </textarea>
                        </div>
                    <div class="col-sm-12">
                        <label for="bookerMessage" class="col-sm-12 control-label">Message (optional)</label>
                            <textarea [disabled] = "eventIsFull()"
                                    [(ngModel)]="bookerMessage"
                            (ngModelChange)="setMessage()"
                            name="bookerMessage"
                            class="form-control">
                            </textarea>
                            </div>
                        </div>
                    </div>
                </form>
                <tim-alert *ngIf="form.invalid" severity="danger" [hidden] ="!form.errors?.['bookingEndInvalid']">
                <ng-container *ngIf="form.errors?.['bookingEndInvalid']">Booking must be done before the event</ng-container>
                </tim-alert>
                
                <tim-alert *ngIf="form.invalid" severity="danger" [hidden] ="!form.errors?.['dateInvalid']">
                <ng-container *ngIf="form.errors?.['dateInvalid']">Start of the event must be before end.</ng-container>
                </tim-alert>
                <tim-alert *ngIf="ngModelTitle.invalid && ngModelTitle.dirty" severity="danger">
                    <ng-container *ngIf="ngModelTitle.errors?.['required']">
                        Title is required.
                    </ng-container>
                    
                    <ng-container *ngIf="ngModelTitle.errors?.['pattern']">
                        Title should not contain the slash character. <!--TODO: Think about the pattern-->
                    </ng-container>
                    <!-- <ng-container i18n *ngIf="ngModelTitle.errors?.['pattern']">
                        End time has to be after the start. TODO: Figure aut custom form validation 
                    </ng-container> -->
                </tim-alert>
                <tim-alert *ngIf="message" severity="danger">
                    <ng-container *ngIf="message">
                        {{message}}
                    </ng-container>
                </tim-alert>

            </ng-container>
            <ng-container footer>
                <button class="timButton" type="button" style="background-color: red; float: left"
                        (click)="deleteEvent()" [disabled]="form.invalid" [hidden]="!isEditEnabled()">
                    Delete
                </button>
                <button class="timButton" type="button" style="float: left"
                        (click)="bookEvent()" [disabled]="eventIsFull() || !eventCanBeBooked()" [hidden]="isEditEnabled()">
                    Book event
                </button>
                <button class="btn timButton" type="button" [hidden]="!userHasBooked()" (click)="cancelBooking()" style="background-color: red;">
                    Cancel Booking
                </button>
                <button class="timButton" type="submit" (click)="saveChanges()" [disabled]="form.invalid"
                        [hidden]="!isEditEnabled()">
                    Save
                </button>
                <button i18n class="btn btn-default" type="button" (click)="dismiss()">
                    Cancel
                </button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class CalendarEventDialogComponent extends AngularDialogComponent<
    TIMCalendarEvent,
    TIMCalendarEvent
> {
    protected dialogName = "CalendarEventEdit";

    title = "";
    location = "";
    message?: string;
    bookingStopTime = "";
    bookingStopDate = "";
    startDate = "";
    startTime = "";
    endDate = "";
    endTime = "";
    booker = "";
    bookerEmail: string | null = "";
    bookerMessage = "";
    description = "";
    userBooked = false;

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
        console.log(this.description);
        console.log(this.location);
        console.log(this.title);
        console.log(this.bookingStopDate);
        console.log(this.bookingStopTime);

        const eventToEdit = {
            description: this.description,
            title: this.title,
            location: this.location,
            start: new Date(`${this.startDate}T${this.startTime}`),
            end: new Date(`${this.endDate}T${this.endTime}`),
            signup_before: new Date(
                `${this.bookingStopDate}T${this.bookingStopTime}`
            ),
        };

        const result = await toPromise(
            this.http.put(`/calendar/events/${id}`, {
                event: eventToEdit,
            })
        );
        if (result.ok) {
            console.log(result.result);
            this.data.title = eventToEdit.title;
            this.data.meta!.description = eventToEdit.description;
            this.data.meta!.location = eventToEdit.location;
            this.data.start = eventToEdit.start;
            this.data.end = eventToEdit.end;
            this.data.meta!.signup_before = eventToEdit.signup_before;
            this.close(this.data);
        } else {
            // TODO: Handle error responses properly
            console.error(result.result.error.error);
            this.setMessage(result.result.error.error);
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
        if (
            !eventToDelete.meta.tmpEvent &&
            (await showConfirm(
                "Delete an Event",
                `Are you sure you want to delete the event "${this.data.title}"?`
            ))
        ) {
            const result = await toPromise(
                this.http.delete(`/calendar/events/${eventToDelete.id}`)
            );
            if (result.ok) {
                console.log(result.result);
                eventToDelete.meta.deleted = true;
                this.close(eventToDelete);
            } else {
                console.error(result.result.error.error);
                this.setMessage(result.result.error.error);
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
    ngOnInit() {
        this.title = this.data.title;
        this.location = this.data.meta!.location;
        this.description = this.data.meta!.description;
        const startOffset = this.data.start.getTimezoneOffset();
        const startDate = new Date(
            this.data.start.getTime() - startOffset * 60 * 1000
        );
        const bookerGroups = this.data.meta!.booker_groups;
        if (bookerGroups) {
            bookerGroups.forEach((group) => {
                // TODO: Make a list of all bookers when the event capacity is higher than 1
                this.bookerMessage = group.message;
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
    }

    /**
     * True if editing is enabled for the event, otherwise false
     */
    isEditEnabled() {
        if (this.data.meta) {
            return this.data.meta.editEnabled;
        }
    }

    /**
     * Sends the booking request for the event to the TIM-server
     */
    async bookEvent() {
        const eventToBook = this.data;
        console.log(eventToBook);
        if (
            !(await showConfirm(
                "Book an Event",
                `Book the event "${this.data.title}"?`
            ))
        ) {
            return;
        }
        if (!this.eventCanBeBooked()) {
            return;
        }

        const result = await toPromise(
            this.http.post("/calendar/bookings", {
                event_id: eventToBook.id,
                booker_msg: this.bookerMessage,
            })
        );
        console.log(result);
        if (result.ok) {
            console.log(result.result);
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
            console.error(result.result.error.error);
            this.setMessage(result.result.error.error);
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
                "Cancel booking",
                `Are you sure you want to cancel booking "${this.data.title}"?`
            ))
        ) {
            return;
        } //
        const result = await toPromise(
            this.http.delete(`/calendar/bookings/${eventId}`)
        );
        console.log(result);
        if (result.ok) {
            console.log(result.result);
            this.data.meta!.enrollments--;

            this.data.meta!.booker_groups.forEach((group) => {
                if (group.name == Users.getCurrent().name) {
                    group.name = "";
                    group.message = "";
                    group.users.forEach((user) => {
                        user.id = -1;
                        user.email = "";
                        user.name = "";
                    });
                }
            });
            this.close(openEvent);
        } else {
            console.error(result.result.error.error);
            this.setMessage(result.result.error.error);
        }
    }

    /**
     * Returns true if the event is full otherwise false
     */
    eventIsFull() {
        return this.data.meta!.enrollments >= this.data.meta!.maxSize;
    }

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

    eventCanBeBooked() {
        const nowDate = new Date();
        const bookBefore = new Date(this.data.meta!.signup_before);
        return bookBefore.getTime() > nowDate.getTime();
    }

    /**
     * Returns true if a user has booked the handled event otherwise false
     */
    userHasBooked() {
        this.userBooked = false;
        const bookers = this.data.meta!.booker_groups;
        bookers.forEach((booker) => {
            Users.getCurrent().groups.forEach((userGroup) => {
                if (booker.name == userGroup.name) {
                    this.userBooked = true;
                }
            });
        });
        return this.userBooked;
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
        KATTIModule,
    ],
    exports: [CalendarEventDialogComponent],
})
export class CalendarEventDialogModule {}
