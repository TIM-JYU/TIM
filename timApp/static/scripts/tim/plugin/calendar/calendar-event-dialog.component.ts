import {Component, NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import {DialogModule} from "../../ui/angulardialog/dialog.module";
import {TimUtilityModule} from "../../ui/tim-utility.module";
import {AngularDialogComponent} from "../../ui/angulardialog/angular-dialog-component.directive";
import {toPromise} from "../../util/utils";
import {TIMCalendarEvent} from "./calendar.component";

@Component({
    selector: "tim-calendar-event-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container i18n header>
                Edit event
            </ng-container>
            <ng-container body>
                <form #form="ngForm" class="form-horizontal">
                    <div class="form-group"
                         [ngClass]="{'has-error': ngModelTitle.invalid && ngModelTitle.dirty}">

                        <label i18n for="title" class="col-sm-2 control-label">Title</label>
                        <div class="col-sm-10">
                            <input i18n-placeholder type="text" required
                                   [(ngModel)]="title" #ngModelTitle="ngModel"
                                   (ngModelChange)="setMessage()"
                                   pattern="[^/]*"
                                   id="title" name="title"
                                   class="form-control"
                                   placeholder="Set title"
                                   [disabled]="!editEnabled()"/>
                            </div>
                    </div>

                    <!--
                    Do not mix form groups or grid column classes directly with input groups.
                    Instead, nest the input group inside of the form group or grid-related element.
                    Refer to documentation: https://getbootstrap.com/docs/3.3/components/#input-groups
                    -->

                    <div class="form-group">
                        <label i18n for="from" class="col-sm-2 control-label">From</label>
                        <div class="col-sm-10">
                            <div class="input-group">
                                
                                <input i18n-placeholder type="date"
                                       [(ngModel)]="startDate"
                                       (ngModelChange)="setMessage()"
                                       id="startDate" name="startDate"
                                       class="form-control"
                                       [disabled]="!editEnabled()">
                                
                                <input i18n-placeholder type="time"
                                       [(ngModel)]="startTime"
                                       (ngModelChange)="setMessage()"
                                       id="startTime" name="startTime"
                                       class="form-control"
                                       [disabled]="!editEnabled()">
                            </div>
                        </div>
                    </div>
                    
                    <div class="form-group">
                        <label i18n for="to" class="col-sm-2 control-label">To</label>
                        <div class="col-sm-10">
                            <div class="input-group">
                                
                                <input i18n-placeholder type="date"
                                       [(ngModel)]="endDate"
                                       (ngModelChange)="setMessage()"
                                       id="endDate" name="endDate"
                                       class="form-control"
                                       [disabled]="!editEnabled()">
                                <input i18n-placeholder type="time"
                                       [(ngModel)]="endTime"
                                       (ngModelChange)="setMessage()"
                                       id="endTime" name="endTime"
                                       class="form-control"
                                       [disabled]="!editEnabled()">
                            </div>
                        </div>
                    </div>
                </form>

                <!--
                User group validation has two (2) stages.
                1) Browser validates that at least a name is required and does not contain a character slash
                before sending a request to the server.
                2) Server responses with an error message if a given folder or name of the user group failed.
                Refer to documentation: https://angular.io/guide/form-validation#validating-form-input
                -->

                <tim-alert *ngIf="ngModelTitle.invalid && ngModelTitle.dirty" severity="danger">
                    <ng-container i18n *ngIf="ngModelTitle.errors?.['required']">
                        Title is required.
                    </ng-container>
                    <ng-container i18n *ngIf="ngModelTitle.errors?.['pattern']">
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
                <button i18n class="timButton" type="button" style="background-color: red; float: left" (click)="deleteEvent()" [disabled]="form.invalid" [hidden]="!editEnabled()">
                    Delete
                </button>
                <button i18n class="timButton" type="button" (click)="saveChanges()" [disabled]="form.invalid" [hidden]="!editEnabled()">
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

    // name = "";
    title = "";
    // folder = "";
    message?: string;
    startDate = "";
    startTime = "";
    endDate = "";
    endTime = "";

    constructor(private http: HttpClient) {
        super();
    }

    async saveChanges(): Promise<void> {
        console.log(this.title);

        const id = this.data.id;
        console.log(this.startDate);
        console.log(this.startTime);
        if (!id) {
            return;
        }

        const eventToEdit = {
            title: this.title,
            start: new Date(`${this.startDate}T${this.startTime}`),
            end: new Date(`${this.endDate}T${this.endTime}`),
        };
        console.log(eventToEdit);
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
            // event.resizable = {
            //     beforeStart: true,
            //     afterEnd: true,
            // };
            // event.id = id;
            // event.meta = {
            //     tmpEvent: false,
            // };
            // if (Date.now() > event.start.getTime()) {
            //     event.color = colors.gray;
            // }
        } else {
            // TODO: Handle error responses properly
            console.error(result.result.error.error);
            this.setMessage(result.result.error.error);
        }
    }

    async deleteEvent() {
        const eventToDelete = this.data;

        if (!eventToDelete) {
            return;
        }

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

    setMessage(message?: string): void {
        this.message = message;
    }

    ngOnInit() {
        this.title = this.data.title;
        const startOffset = this.data.start.getTimezoneOffset();
        const startDate = new Date(
            this.data.start.getTime() - startOffset * 60 * 1000
        );
        const startDateTime = startDate.toISOString().split("T");
        console.log(startDateTime[1]);
        this.startDate = startDateTime[0];
        console.log(startDateTime[1].split(".")[0]);
        this.startTime = startDateTime[1].split(".")[0];

        if (this.data.end) {
            console.log(this.data.end);
            const endOffset = this.data.start.getTimezoneOffset();
            const endDate = new Date(
                this.data.end.getTime() - endOffset * 60 * 1000
            );
            const endDateTime = endDate.toISOString().split("T");
            console.log(endDateTime);
            this.endDate = endDateTime[0];
            this.endTime = endDateTime[1].split(".")[0];
        }
    }

    // validateAfter() {
    //     if (new Date(`${this.startDate}T${this.startTime}`).getTime() > new Date(`${this.endDate}T${this.endTime}`).getTime()){
    //
    //     }
    // }
    editEnabled() {
        if (this.data.meta) {
            return this.data.meta.editEnabled;
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
    ],
    exports: [CalendarEventDialogComponent],
})
export class CalendarEventDialogModule {}
