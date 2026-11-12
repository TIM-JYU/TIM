import {Component, ChangeDetectionStrategy} from "@angular/core";
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
        <tim-dialog-frame [size]="'lg'">
          <ng-container i18n header>
                Create user
            </ng-container>
          <ng-container body>
            <form #form="ngForm" class="form-horizontal">
              @if (!isHidden('username')) {
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
              }
        
              <div class="form-group">
                <label for="given_name" class="col-sm-2 control-label"><ng-container i18n>First name(s)</ng-container> <span class="mandatory-asterisk">(<i class="glyphicon glyphicon-asterisk mandatory"></i>)</span></label>
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
                <label for="surname" class="col-sm-2 control-label"><ng-container i18n>Surname</ng-container> <span class="mandatory-asterisk">(<i class="glyphicon glyphicon-asterisk mandatory"></i>)</span></label>
                <div class="col-sm-10">
                  <input i18n-placeholder type="text"
                    [(ngModel)]="surname" #surnameInput="ngModel"
                    required
                    pattern="[^/]*"
                    (ngModelChange)="setMessage()"
                    id="surname" name="surname"
                    class="form-control"
                    placeholder="User's surname">
                </div>
              </div>
        
              @if (!isHidden('extra_info')) {
                <div class="form-group">
                  <label for="extra_info" class="col-sm-2 control-label">{{getExtraInfoTitle()}}</label>
                  <div class="col-sm-10">
                    <input i18n-placeholder type="text"
                      [(ngModel)]="extra_info"
                      id="extra_info" name="extra_info"
                      class="form-control"
                      placeholder="Additional information related to the user, eg. class or group name">
                  </div>
                </div>
              }
        
              @if (!isHidden('email')) {
                <div class="form-group">
                  <label i18n for="email" class="col-sm-2 control-label">Email</label>
                  <div class="col-sm-10">
                    <input i18n-placeholder type="text"
                      [(ngModel)]="email"
                      id="email" name="email"
                      class="form-control"
                      placeholder="User's email">
                  </div>
                </div>
              }
        
              <p class="mandatory-expl">
                (<i class="glyphicon glyphicon-asterisk mandatory"></i>) = <ng-container i18n>required field</ng-container>
              </p>
            </form>
        
            @if (firstName.invalid && firstName.dirty) {
              <tim-alert severity="danger">
                @if (firstName.errors?.['required']) {
                  <ng-container i18n>
                        First name is required.
                    </ng-container>
                }
                @if (firstName.errors?.['pattern']) {
                  <ng-container i18n>
                        First name should not contain the slash character.
                    </ng-container>
                }
              </tim-alert>
            }
        
            @if (surnameInput.invalid && surnameInput.dirty) {
              <tim-alert severity="danger">
                @if (surnameInput.errors?.['required']) {
                  <ng-container i18n>
                        Surname is required.
                    </ng-container>
                }
                @if (surnameInput.errors?.['pattern']) {
                  <ng-container i18n>
                        Surname should not contain the slash character.
                    </ng-container>
                }
              </tim-alert>
            }
        
            @if (message) {
              <tim-alert severity="danger">
                @if (message) {
                  {{message}}
                }
              </tim-alert>
            }
        
          </ng-container>
          <ng-container footer>
            @if (loading) {
              <tim-loading></tim-loading>
            }
            <button i18n class="timButton" type="button" (click)="saveUser()" [disabled]="form.invalid || loading">
                    Add
                </button>
            <button i18n class="btn btn-default" type="button" (click)="dismiss()" [disabled]="loading">
                    Cancel
                </button>
          </ng-container>
        </tim-dialog-frame>
        `,
    styleUrls: ["user-creation-dialog.component.scss"],
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
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
    loading = false;

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit() {
        this.hiddenFields = this.data.hiddenFields;
    }

    async saveUser(): Promise<void> {
        this.loading = true;
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
        this.loading = false;

        if (response.ok) {
            this.close(response.result);
        } else {
            this.setMessage(response.result.error.error);
        }
    }

    getExtraInfoTitle(): string {
        return this.data.extraInfoTitle ?? $localize`Extra`;
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
