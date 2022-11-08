import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {FormsModule} from "@angular/forms";
import {to} from "tim/util/utils";
import {$http} from "tim/util/ngimport";
import {CommonModule} from "@angular/common";

export interface IRestTaskLockDialogParams {
    currentUser: string;
    taskId: string;
}

@Component({
    selector: "tim-reset-task-lock-dialog",
    template: `
        <tim-dialog-frame [dialogOptions]="dialogOptions" [dialogName]="dialogName" [align]="'center'">
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <form class="form-horizontal">
                    <div class="form-group">
                        <label for="targetUser" class="col-sm-3 control-label">Username:</label>
                        <div class="col-sm-9">
                            <input id="targetUser"
                                   class="form-control"
                                   type="text"
                                   [(ngModel)]="targetUser"
                                   name="targetUser"
                                   focusMe/>
                            <button class="timButton" type="button" (click)="doClearRequest()">Clear</button>
                        </div>
                    </div>
                </form>
                <div *ngIf="responseMessage">{{responseMessage}}</div>
            </ng-container>
            <ng-container footer>
                <button class="btn btn-default" type="button" (click)="done()">Close</button>
            </ng-container>
        </tim-dialog-frame>`,
})
export class ResetTaskLockDialogComponent extends AngularDialogComponent<
    {currentUser: string; taskId: string},
    void
> {
    protected dialogName = "ResetTaskLock";

    targetUser = "";
    responseMessage = "";

    getTitle() {
        return `Clear task locking for task ${this.data.taskId}`;
    }

    ngOnInit() {
        this.targetUser = this.data.currentUser;
    }

    async doClearRequest() {
        // TODO: Clear for everyone
        this.responseMessage = "";
        const r = await to(
            $http.post<{cleared: boolean; error?: string}>(`/clearTaskBlock`, {
                user: this.targetUser,
                task_id: this.data.taskId,
            })
        );
        if (r.ok) {
            if (r.result.data.cleared) {
                this.responseMessage = `Cleared lock for user ${this.targetUser}`;
            } else {
                this.responseMessage =
                    r.result.data.error ??
                    `No lock found for user ${this.targetUser}`;
            }
        } else {
            this.responseMessage = r.result.data.error;
        }
    }

    done() {
        this.dismiss();
    }
}

@NgModule({
    declarations: [ResetTaskLockDialogComponent],
    imports: [CommonModule, DialogModule, TimUtilityModule, FormsModule],
})
export class ResetTaskLockDialogModule {}
