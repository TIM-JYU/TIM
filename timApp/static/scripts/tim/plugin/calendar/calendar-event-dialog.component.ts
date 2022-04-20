import {Component, NgModule} from "@angular/core";
import {FormsModule, NgForm} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import {DialogModule} from "../../ui/angulardialog/dialog.module";
import {TimUtilityModule} from "../../ui/tim-utility.module";
import {AngularDialogComponent} from "../../ui/angulardialog/angular-dialog-component.directive";
import {toPromise} from "../../util/utils";
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
                        <div class="col-sm-10">
                            <input type="text" required
                                   [(ngModel)]="title" #ngModelTitle="ngModel"
                                   (ngModelChange)="setMessage()"
                                   pattern="[^/]*"
                                   id="title" name="title"
                                   class="form-control"
                                   placeholder="Set title"
                                   [disabled]="!isEditEnabled()"/>
                        </div>
                    </div>

                    <div class="form-group">
                        <label for="from" class="col-sm-2 control-label">From</label>
                        <div class="col-sm-10">
                            <div class="input-group">

                                <input i18n-placeholder type="date"
                                       [(ngModel)]="startDate"
                                       (ngModelChange)="setMessage()"
                                       id="startDate" name="startDate"
                                       class="form-control"
                                       [disabled]="!isEditEnabled()"
                                       >

                                <input i18n-placeholder type="time"
                                       [(ngModel)]="startTime"
                                       (ngModelChange)="setMessage()"
                                       id="startTime" name="startTime"
                                       class="form-control"
                                       [disabled]="!isEditEnabled()"
                                        >
                            </div>
                        </div>
                    </div>

                    <div class="form-group">
                        <label for="to" class="col-sm-2 control-label">To</label>
                        <div class="col-sm-10">
                            <div class="input-group">
                                <input i18n-placeholder type="date"
                                       [(ngModel)]="endDate"
                                       (ngModelChange)="setMessage()"
                                       id="endDate" name="endDate"
                                       class="form-control"
                                       [disabled]="!isEditEnabled()"
                                        >
                                <input i18n-placeholder type="time"
                                       [(ngModel)]="endTime"
                                       (ngModelChange)="setMessage()"
                                       id="endTime" name="endTime"
                                       class="form-control"
                                       [disabled]="!isEditEnabled()">
                            </div>
                        </div>
                    </div>
                </form>
                

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
                        (click)="bookTime()" [disabled]="form.invalid" [hidden]="isEditEnabled()">
                    Book time
                </button>
                <button class="timButton" type="submit" (click)="onSubmit(form)" [disabled]="form.invalid"
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
    message?: string;
    startDate = "";
    startTime = "";
    endDate = "";
    endTime = "";

    constructor(private http: HttpClient) {
        super();
    }

    async onSubmit(form: NgForm) {
        console.log(form.errors);
        await this.saveChanges();
    }

    /**
     * Sends the event information to the TIM server to be persisted
     */
    async saveChanges(): Promise<void> {
        const id = this.data.id;

        if (!id) {
            return;
        }

        const eventToEdit = {
            title: this.title,
            start: new Date(`${this.startDate}T${this.startTime}`),
            end: new Date(`${this.endDate}T${this.endTime}`),
        };

        const result = await toPromise(
            this.http.put(`/calendar/events/${id}`, {
                event: eventToEdit,
            })
        );
        if (result.ok) {
            console.log(result.result);
            this.data.title = eventToEdit.title;
            this.data.start = eventToEdit.start;
            this.data.end = eventToEdit.end;
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
            confirm("Are you sure you want to delete the event?") // TODO: make more sophisticated confirmation dialog
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
        const startOffset = this.data.start.getTimezoneOffset();
        const startDate = new Date(
            this.data.start.getTime() - startOffset * 60 * 1000
        );
        const startDateTime = startDate.toISOString().split("T");

        this.startDate = startDateTime[0];

        this.startTime = startDateTime[1].split(".")[0];

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
    async bookTime() {
        const eventToBook = this.data;

        const result = await toPromise(
            this.http.post("/calendar/bookings", {
                event_id: eventToBook.id,
            })
        );
        if (result.ok) {
            console.log(result.result);
            this.data.meta!.enrollments++;
            this.close(eventToBook);
        } else {
            console.error(result.result.error.error);
            this.setMessage(result.result.error.error);
        }
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
