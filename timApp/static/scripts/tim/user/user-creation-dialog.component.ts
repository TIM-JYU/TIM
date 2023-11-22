import {Component, NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import type {GroupMember} from "tim/ui/group-management.component";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {toPromise} from "tim/util/utils";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";

/**
 * Defines additional parameters for creating new users via the UserCreationDialog.
 */
export interface UserCreationDialogParams {
    group: number;
    // type of association for users created within the current manage document,
    // used only to represent the type of association in the UI
    extraInfoTitle?: string;
    // options for hiding/displaying certain input fields for user creation
}

@Component({
    selector: "tim-user-creation-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container i18n header>
                Create user
            </ng-container>
            <ng-container body>
                <form #form="ngForm" class="form-horizontal">
                    <div class="form-group"
                         [ngClass]="{'has-error': ngModelName.invalid && ngModelName.dirty}">

                        <label i18n for="username" class="col-sm-2 control-label">Username</label>
                        <div class="col-sm-10">
                            <input i18n-placeholder type="text" required
                                   [(ngModel)]="username" #ngModelName="ngModel"
                                   (ngModelChange)="setMessage()"
                                   pattern="[^/]*"
                                   id="username" name="username"
                                   class="form-control"
                                   placeholder="Enter a username for the user"/>
                        </div>
                    </div>

                    <div class="form-group">
                        <label i18n for="given_name" class="col-sm-2 control-label">First name(s)</label>
                        <div class="col-sm-10">
                            <input i18n-placeholder type="text"
                                   [(ngModel)]="given_name"
                                   (ngModelChange)="setMessage()"
                                   id="given_name" name="given_name"
                                   class="form-control"
                                   placeholder="User's given name, list first and middle names in order">
                        </div>
                    </div>

                    <div class="form-group">
                        <label i18n for="surname" class="col-sm-2 control-label">Surname</label>
                        <div class="col-sm-10">
                            <input i18n-placeholder type="text"
                                   [(ngModel)]="surname"
                                   (ngModelChange)="setMessage()"
                                   id="surname" name="surname"
                                   class="form-control"
                                   placeholder="User's surname">
                        </div>
                    </div>

                    <div class="form-group">
                        <label i18n for="extra_info" class="col-sm-2 control-label">{{getExtraInfoTitle()}}</label>
                        <div class="col-sm-10">
                            <input i18n-placeholder type="text"
                                   [(ngModel)]="extra_info"
                                   (ngModelChange)="setMessage()"
                                   id="extra_info" name="extra_info"
                                   class="form-control"
                                   placeholder="Additional information related to the user, eg. class or group name">
                        </div>
                    </div>

                    <div class="form-group">
                        <label i18n for="email" class="col-sm-2 control-label">Email</label>
                        <div class="col-sm-10">
                            <input i18n-placeholder type="text"
                                   [(ngModel)]="email"
                                   (ngModelChange)="setMessage()"
                                   id="email" name="email"
                                   class="form-control"
                                   placeholder="User's email">
                        </div>
                    </div>

                </form>

                <tim-alert *ngIf="ngModelName.invalid && ngModelName.dirty" severity="danger">
                    <ng-container i18n *ngIf="ngModelName.errors?.['required']">
                        Name is required.
                    </ng-container>
                    <ng-container i18n *ngIf="ngModelName.errors?.['pattern']">
                        Name should not contain the slash character.
                    </ng-container>
                </tim-alert>
                <tim-alert *ngIf="message" severity="danger">
                    <ng-container *ngIf="message">
                        {{message}}
                    </ng-container>
                </tim-alert>

            </ng-container>
            <ng-container footer>
                <button i18n class="timButton" type="button" (click)="saveUser()" [disabled]="form.invalid">
                    Create
                </button>
                <button i18n class="btn btn-default" type="button" (click)="dismiss()">
                    Cancel
                </button>
            </ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["user-creation-dialog.component.scss"],
})
export class UserCreationDialogComponent extends AngularDialogComponent<
    UserCreationDialogParams,
    GroupMember
> {
    protected dialogName = "UserCreate";
    username: string = "";
    given_name: string = "";
    surname: string = "";
    extra_info?: string = "";
    email: string = "";

    message?: string;

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit() {}

    async saveUser(): Promise<void> {
        const url = `/loginCode/addMembers/${this.getGroup()}`;
        const response = await toPromise(
            this.http.post<GroupMember>(url, {
                username: this.username,
                given_name: this.given_name,
                surname: this.surname,
                email: this.email,
                extra_info: this.extra_info ?? "",
            })
        );

        if (response.ok) {
            this.close(response.result);
        } else {
            this.setMessage(response.result.error.error);
        }
    }

    private getGroup(): number {
        return this.data.group;
    }

    getExtraInfoTitle(): string {
        return this.data.extraInfoTitle ?? "Extra info";
    }

    setMessage(message?: string): void {
        this.message = message;
    }
}

@NgModule({
    declarations: [UserCreationDialogComponent],
    imports: [
        DialogModule,
        FormsModule,
        TimUtilityModule,
        CommonModule,
        HttpClientModule,
    ],
    exports: [UserCreationDialogComponent],
})
export class UserCreationDialogModule {}
