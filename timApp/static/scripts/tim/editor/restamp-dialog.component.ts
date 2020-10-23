import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {to2} from "../util/utils";
import {IStampingData} from "./pareditor";

// The close states dialog can return.
export enum RestampDialogClose {
    RestampedReturnToEditor,
    NoRestampingReturnToEditor,
    RestampingFailedReturnToEditor,
    SaveAndExit,
}

/*
 * Restamping dialog.
 */
@Component({
    selector: "tim-restamp-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <tim-alert *ngIf="!successMessage && !errorMessage" severity="info">
                    There are changes in the contents of the macro that may not have been updated to the stamps yet!
                </tim-alert>
                <tim-alert severity="success" *ngIf="successMessage">{{successMessage}}</tim-alert>
                <tim-alert severity="warning" *ngIf="errorMessage">{{errorMessage}}</tim-alert>
                <div>
                    <p>
                        You can update the stamps by clicking the restamp button below. You can also return to the
                        editor to manually check and reupload the attachments.
                    </p>
                    <div>
                        <button class="timButton" (click)="restamp()">
                            Restamp <ng-container *ngIf="data.attachments.length > 1">all</ng-container>
                        </button>
                        <tim-loading *ngIf="stamping"></tim-loading>
                    </div>
                </div>
            </ng-container>
            <ng-container footer>
                <button class="timButton" (click)="returnToEditor()">Return to editor</button>
                <button class="timButton" (click)="saveAndExit()">Save and exit</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class RestampDialogComponent extends AngularDialogComponent<
    IStampingData,
    RestampDialogClose
> {
    protected dialogName = "restamp";
    stamping = false;
    stampingDone = false;
    errorMessage?: string;
    successMessage?: string;

    constructor(private http: HttpClient) {
        super();
    }

    dismiss() {
        this.returnToEditor();
    }

    /**
     * Restamps all attachments in the open editor.
     */
    async restamp() {
        if (this.stamping) {
            return;
        }
        this.stamping = true;
        if (this.data.attachments.length > 0) {
            const stampingParams = {
                attachments: this.data.attachments,
                customStampModel: this.data.customStampModel,
                meetingDate: this.data.meetingDate,
                stampFormat: this.data.stampFormat,
            };
            const r = await to2(
                this.http.post(`/upload/restamp`, stampingParams).toPromise()
            );

            if (!r.ok) {
                this.errorMessage = r.result.error.error;
                this.successMessage = undefined;
                if (!this.errorMessage) {
                    this.errorMessage = "Unknown error while updating stamps";
                }
            } else {
                this.errorMessage = undefined;
                this.successMessage = `Stamps successfully updated.`;
            }
        } else {
            // This should never show up.
            this.errorMessage = "Unable to find attachments!";
        }
        this.stampingDone = true;
        this.stamping = false;
    }

    public getTitle() {
        return "Update stamps";
    }

    /**
     * Returns an appropriate close state when returning to editor
     * based on whether restamping was done and whether it was successful.
     */
    returnToEditor() {
        if (this.stampingDone && this.errorMessage) {
            this.close(RestampDialogClose.RestampingFailedReturnToEditor);
            return;
        }
        if (this.stampingDone) {
            this.close(RestampDialogClose.RestampedReturnToEditor);
            return;
        }
        this.close(RestampDialogClose.NoRestampingReturnToEditor);
    }

    /**
     * When continuing exiting from editor.
     * Whether restamping was done or not doesn't matter here.
     */
    saveAndExit() {
        this.close(RestampDialogClose.SaveAndExit);
    }
}

@NgModule({
    declarations: [RestampDialogComponent],
    imports: [BrowserModule, DialogModule, HttpClientModule, TimUtilityModule],
})
export class RestampDialogModule {}
