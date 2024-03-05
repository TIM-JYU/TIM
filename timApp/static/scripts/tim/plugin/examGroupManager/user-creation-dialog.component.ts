import {Component} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {toPromise} from "tim/util/utils";
import type {GroupMember} from "tim/plugin/examGroupManager/exam-group-manager.component";

/**
 * Defines additional parameters for creating new users via the UserCreationDialog.
 *
 *  - group (number): id of the group that the new user will be added to
 *  - extraInfoTitle (string): optional alternate title for the extra information field in the UI
 *  - hiddenFields (string[]): options for hiding certain input fields
 *
 *  Valid field names for *hiddenFields*:
 *    - 'username' - username for the user
 *    - 'email' - user's email
 *    - 'extra_info' - extra information regarding the user
 *
 *      For fields which the system requires some input to create the new user, dummy data will be generated
 *      when those fields are hidden.
 */
export interface UserCreationDialogParams {
    group: number;
    extraInfoTitle?: string;
    hiddenFields?: string[];
}

// TODO input validation for conditionally hidden fields
@Component({
    selector: "tim-user-creation-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container i18n header>
                Create user
            </ng-container>
            <ng-container body>
                <form #form="ngForm" class="form-horizontal">
                    <ng-container *ngIf="!isHidden('username')">
                        <div class="form-group">
                            <label i18n for="username" class="col-sm-2 control-label">Username</label>
                            <div class="col-sm-10">
                                <input i18n-placeholder type="text" 
                                       [(ngModel)]="username"
                                       id="username" name="username"
                                       class="form-control"
                                       placeholder="Enter a username for the user"/>
                            </div>
                        </div>
                    </ng-container>

                    <div class="form-group">
                        <label i18n for="given_name" class="col-sm-2 control-label">First name(s)</label>
                        <div class="col-sm-10">
                            <input i18n-placeholder type="text" required
                                   [(ngModel)]="given_name" #firstName="ngModel"
                                   pattern="[^/]*"
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
                                   [(ngModel)]="surname" #surName="ngModel"
                                   required
                                   pattern="[^/]*"
                                   (ngModelChange)="setMessage()"
                                   id="surname" name="surname"
                                   class="form-control"
                                   placeholder="User's surname">
                        </div>
                    </div>

                    <div class="form-group" *ngIf="!isHidden('extra_info')">
                        <label for="extra_info" class="col-sm-2 control-label">{{getExtraInfoTitle()}}</label>
                        <div class="col-sm-10">
                            <input i18n-placeholder type="text"
                                   [(ngModel)]="extra_info"
                                   id="extra_info" name="extra_info"
                                   class="form-control"
                                   placeholder="Additional information related to the user, eg. class or group name">
                        </div>
                    </div>

                    <div class="form-group" *ngIf="!isHidden('email')">
                        <label i18n for="email" class="col-sm-2 control-label">Email</label>
                        <div class="col-sm-10">
                            <input i18n-placeholder type="text"
                                   [(ngModel)]="email"
                                   id="email" name="email"
                                   class="form-control"
                                   placeholder="User's email">
                        </div>
                    </div>

                </form>

                <tim-alert *ngIf="firstName.invalid && firstName.dirty" severity="danger">
                    <ng-container i18n *ngIf="firstName.errors?.['required']">
                        First name is required.
                    </ng-container>
                    <ng-container i18n *ngIf="firstName.errors?.['pattern']">
                        First name should not contain the slash character.
                    </ng-container>
                </tim-alert>
                
                <tim-alert *ngIf="surName.invalid && surName.dirty" severity="danger">
                    <ng-container i18n *ngIf="surName.errors?.['required']">
                        Surname is required.
                    </ng-container>
                    <ng-container i18n *ngIf="surName.errors?.['pattern']">
                        Surname should not contain the slash character.
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
                    Add
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
    hiddenFields?: string[];
    message?: string;

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit() {
        this.hiddenFields = this.data.hiddenFields;
    }

    async saveUser(): Promise<void> {
        const url = `/examGroupManager/addMember/${this.data.group}`;
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

    getExtraInfoTitle(): string {
        return this.data.extraInfoTitle ?? "Extra info";
    }

    isHidden(fieldName: string): boolean {
        return this.hiddenFields === undefined
            ? false
            : this.hiddenFields.includes(fieldName);
    }

    setMessage(message?: string): void {
        this.message = message;
    }
}
