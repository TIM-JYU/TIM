import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {FormsModule} from "@angular/forms";
import {defaultWuffMessage, to} from "tim/util/utils";
import {$http} from "tim/util/ngimport";
import {CommonModule} from "@angular/common";
import type {IUser} from "tim/user/IUser";
import {sortByEmail, sortByRealName, sortLang} from "tim/user/IUser";
import {showConfirm} from "tim/ui/showConfirmDialog";

export interface IRestTaskLockDialogParams {
    currentUser: string;
    taskId: string;
}

interface IUserData {
    user: IUser;
    time: moment.Moment;
    selected: boolean;
    visible: boolean;
}

type sortOption = "name" | "username" | "email" | "time";

@Component({
    selector: "tim-reset-task-lock-dialog",
    template: `
        <tim-dialog-frame [autoHeight]="false">
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <div class="mainContainer">
                    <div class="controlContainer">
                        <div i18n>Users who have an active lock on task {{data.taskId}}</div>
                        <div class="userFilter">
                            <label i18n class="col-sm-2 control-label">Find user</label>
                            <div class="col-sm-10">
                                <input id="searchInput"
                                       class="form-control"
                                       type="text"
                                       [(ngModel)]="searchInput"
                                       (ngModelChange)="updateSearch()"
                                       name="searchInput"
                                       focusMe/>
                            </div>
                        </div>
                        <div class="controls">
                            <button i18n class="btn btn-default" type="button" (click)="refresh()">Refresh</button>
                            <button i18n class="timButton" type="button" (click)="doClearRequest()">Clear locks for selected
                                users
                            </button>
                            <div class="response" *ngIf="responseMessage">{{responseMessage}}</div>
                        </div>
                    </div>
                    <div class="tableContainer">
                        <table>
                            <thead>
                            <tr class="row">
                                <th i18n (click)="selectAll()">Select</th>
                                <th i18n (click)="sort('name')">Name</th>
                                <th i18n (click)="sort('username')">Username</th>
                                <th i18n (click)="sort('email')">Email</th>
                                <th i18n (click)="sort('time')">Task locked on</th>
                            </tr>
                            </thead>
                            <tbody *ngIf="userData">
                            <ng-container *ngFor="let user of userData">
                                <tr class="row" *ngIf="user.visible">
                                    <td>
                                        <input type="checkbox" [(ngModel)]="user.selected">
                                    </td>
                                    <td>{{user.user.real_name}}</td>
                                    <td>{{user.user.name}}</td>
                                    <td>{{user.user.email}}</td>
                                    <td>{{user.time | timdate}}</td>
                                </tr>
                            </ng-container>
                            </tbody>
                            <tbody *ngIf="fetchError">
                            <td>{{fetchError}}</td>
                            </tbody>
                        </table>
                    </div>
                </div>
            </ng-container>
            <ng-container footer>
                <button i18n class="btn btn-default" type="button" (click)="done()">Close</button>
            </ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["reset-task-lock-dialog.component.scss"],
})
export class ResetTaskLockDialogComponent extends AngularDialogComponent<
    {currentUser: string; taskId: string},
    void
> {
    protected dialogName = "ResetTaskLock";

    searchInput = "";
    responseMessage = "";
    fetchError = "";
    userData: IUserData[] = [];
    sortOption: sortOption = "time";
    reverseSort = true;

    getTitle() {
        return $localize`Clear task locking for task ${this.data.taskId}`;
    }

    ngOnInit() {
        this.fetchData();
    }

    selectAll() {
        const visible = this.userData.filter((u) => u.visible);
        const anyUnSelected = visible.some((u) => !u.selected);
        visible.map((u) => (u.selected = anyUnSelected));
        return;
    }

    sort(s: sortOption) {
        if (s == this.sortOption) {
            this.reverseSort = !this.reverseSort;
        } else {
            this.reverseSort = false;
        }
        this.sortOption = s;
        this.doSort();
    }

    doSort() {
        let sortFunction: (a: IUserData, b: IUserData) => number;
        switch (this.sortOption) {
            case "name":
                sortFunction = (a, b) => sortByRealName(a.user, b.user);
                break;
            case "email":
                sortFunction = (a, b) => sortByEmail(a.user, b.user);
                break;
            case "username":
                sortFunction = (a, b) =>
                    (a.user.name ?? "").localeCompare(
                        b.user.name ?? "",
                        sortLang
                    );
                break;
            case "time":
                sortFunction = (a, b) => a.time.diff(b.time);
        }
        if (this.reverseSort) {
            this.userData.sort((a, b) => sortFunction(b, a));
        } else {
            this.userData.sort(sortFunction);
        }
    }

    updateSearch() {
        if (!this.searchInput) {
            this.setAllDataVisible();
            return;
        }
        const l = this.searchInput.toLowerCase();
        this.userData.forEach((u) => {
            u.visible =
                u.user.name.toLowerCase().includes(l) ||
                !!u.user.real_name?.toLowerCase().includes(l) ||
                !!u.user.email?.toLowerCase().includes(l);
        });
    }

    setAllDataVisible() {
        this.userData.map((u) => (u.visible = true));
    }

    refresh() {
        this.searchInput = "";
        this.fetchData();
    }

    async fetchData() {
        const r = await to(
            $http.get<{user: IUser; time: moment.Moment}[]>(
                `/getTaskBlocks/${this.data.taskId}`
            )
        );
        if (!r.ok) {
            this.userData = [];
            this.fetchError = $localize`Failed to fetch data: \n${r.result.data.error}`;
            return;
        }
        const uData = r.result.data;
        if (uData.length == 0) {
            this.fetchError = $localize`No locks found`;
            return;
        }
        this.fetchError = "";
        this.userData = uData.map((u) => {
            return {...u, visible: true, selected: false};
        });
        this.doSort();
    }

    async doClearRequest() {
        this.responseMessage = "";
        const userids = this.userData
            .filter((v) => v.selected)
            .map((v) => v.user.id);
        if (userids.length == 0) {
            this.responseMessage = $localize`No targets selected`;
            return;
        }
        const names = this.userData
            .filter((v) => v.selected)
            .map((v) => v.user.name)
            .join("\n");
        if (
            !(await showConfirm(
                $localize`Confirm`,
                $localize`Clear locks for following users? (${userids.length} selected)\n\n${names}`
            ))
        ) {
            return;
        }

        const r = await to(
            $http.post<{cleared: {name: string; id: number}[]; error?: string}>(
                `/clearTaskBlock`,
                {
                    userids: userids,
                    task_id: this.data.taskId,
                }
            )
        );
        if (r.ok) {
            const cl = r.result.data.cleared;
            if (cl.length > 1) {
                this.responseMessage = $localize`Cleared locks for ${cl.length} users`;
            } else if (r.result.data.cleared.length == 1) {
                this.responseMessage = $localize`Cleared lock for ${cl[0].name}`;
            } else {
                this.responseMessage =
                    r.result.data.error ??
                    $localize`No locks found for selected targets`;
            }
            this.userData = this.userData.filter(
                (ld) => !cl.map((cleared) => cleared.id).includes(ld.user.id)
            );
        } else {
            if (r.result.status == 500) {
                this.responseMessage = defaultWuffMessage;
            } else {
                this.responseMessage = r.result.data.error;
            }
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
