/*
 * Dialog for log in and sign up.
 */

import * as focusMe from "tim/ui/focusMe";
import * as onEnter from "tim/ui/onEnter";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component} from "@angular/core";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {saveCurrentScreenPar} from "../document/parhelpers";
import {genericglobals} from "../util/globals";
import {$http} from "../util/ngimport";
import {capitalizeFirstLetter, IOkResponse, markAsUsed, to, ToReturn} from "../util/utils";
import * as hakaLogin from "./haka-login.component";
import {IDiscoveryFeedEntry, loadIdPs} from "./haka-login.component";
import {Users} from "./userService";

interface INameResponse {
    status: "name";
    name: string;
    can_change_name: boolean;
}

interface ILoginParams {
    showSignup: boolean;
    addingToSession: boolean;
}

markAsUsed(focusMe, onEnter, hakaLogin);

let instance: AngularDialogComponent<ILoginParams, void> | undefined;

const orgNames: Record<string, string | undefined> = {
    "aalto.fi": "Aalto",
    "jyu.fi": "JYU",
    "tuni.fi": "TUNI",
};

function getHomeOrgDisplay(s: string): string {
    return orgNames[s] ?? s;
}


@Component({
    selector: "tim-login-dialog",
    template: `
<tim-dialog-frame>
    <ng-container header>
        {{ getTitle() }}
    </ng-container>
    <ng-container body>
<div class="row">
    <div class="col-sm-12">
    <ng-container *ngIf="!showSignup">
        <p class="text-center" i18n>
            Members from universities and other Haka organizations, please use Haka to log in.
        </p>
        <tim-haka-login [idps]="idps"
                    [homeOrg]="homeOrg"
                    [addingUser]="addingToSession"></tim-haka-login>
        <hr>
        <p class="text-center" i18n>
            Others, please log in with your TIM account.
        </p>

        <div class="form-group">
            <label for="email" class="control-label" i18n>Email or username</label>
            <input class="form-control"
                   id="email"
                   [(ngModel)]="loginForm.email"
                   (keydown.enter)="pw.focus()"
                   type="text">
        </div>

        <div class="form-group">
            <label for="password" class="control-label" i18n>Password</label>
            <input class="form-control"
                   id="password"
                   #pw
                   [(ngModel)]="loginForm.password"
                   (keydown.enter)="loginWithEmail()"
                   type="password">
            <p class="text-smaller"><a href="#" (click)="forgotPassword()" i18n>I forgot my password</a></p>
        </div>

        <button class="center-block timButton" (click)="loginWithEmail()" i18n>Log in</button>
        <tim-alert severity="danger" *ngIf="loginError">
            <tim-error-description [error]="loginError"></tim-error-description>
        </tim-alert>
        <hr>
        <p class="text-center" *ngIf="!showSignup" i18n>
            Not a {{getHomeOrgDisplayName()}} student or staff member and don't have a TIM account?
        </p>
        <button *ngIf="!showSignup" class="center-block timButton" type="button"
                (click)="beginSignup()" i18n="@@signUp">
            Sign up
        </button>
    </ng-container>
    <div class="form" *ngIf="showSignup">
        <div class="text-center" *ngIf="!resetPassword">
            <p i18n>If you don't have an existing TIM or {{getHomeOrgDisplayName()}} account, you can create a TIM account here.</p>
            <p i18n>Please input your email address to receive a temporary password.</p>
        </div>
        <p class="text-center" *ngIf="resetPassword && !emailSent" i18n>
            To reset password, enter your email or username first.
        </p>
        <div class="form-group">
            <label for="email-signup" class="control-label" i18n>Email or username</label>
            <input class="form-control"
                   id="email-signup"
                   focusMe
                   [(ngModel)]="email"
                   [disabled]="emailSent"
                   (keydown.enter)="provideEmail()"
                   name="email"
                   required
                   i18n-placeholder
                   placeholder="Email or username"
                   type="text"/>
            <label [ngStyle]="urlStyle" for="url" class="control-label" i18n>Do not type anything here</label>
            <input class="form-control"
                   [ngStyle]="urlStyle"
                   tabindex="-1"
                   id="url"
                   [(ngModel)]="url"
                   [disabled]="emailSent"
                   name="url"
                   i18n-placeholder
                   placeholder="Do not type anything here"
                   type="text"/>
        </div>
        <button (click)="provideEmail()"
                [disabled]="!email"
                *ngIf="!emailSent"
                class="timButton" i18n>
            Continue
        </button>
        <button (click)="cancelSignup()"
                *ngIf="!emailSent"
                class="btn btn-default" i18n>
            Cancel
        </button>
        <div *ngIf="emailSent">
            <div class="form-group" *ngIf="!tempPasswordProvided">
                <label for="password-signup" class="control-label" i18n>
                    TIM sent you a temporary password. Please check your email and type the password below to continue.
                </label>
                <input class="form-control"
                       id="password-signup"
                       focusMe
                       [(ngModel)]="tempPassword"
                       (keydown.enter)="provideTempPassword()"
                       name="tempPassword"
                       required
                       i18n-placeholder
                       placeholder="Password you received"
                       type="password"/>
            </div>
            <button (click)="provideTempPassword()"
                    [disabled]="!tempPassword"
                    *ngIf="!tempPasswordProvided"
                    class="center-block timButton" i18n>Continue</button>
            <div *ngIf="tempPasswordProvided">
                <div class="form-group">
                    <label for="name-signup" class="control-label" i18n>
                        Enter your name (Lastname Firstname)
                    </label>
                    <input class="form-control"
                           id="name-signup"
                           focusMe
                           [(ngModel)]="name"
                           [disabled]="nameProvided || !canChangeName"
                           (keydown.enter)="newpw.focus()"
                           name="name"
                           required
                           i18n-placeholder
                           placeholder="Your name"
                           type="text"/>
                </div>
                <div class="form-group">
                    <label for="newpassword-signup" class="control-label" i18n>
                        Create a new password
                    </label>
                    <input class="form-control"
                           id="newpassword-signup"
                           #newpw
                           [(ngModel)]="newPassword"
                           [disabled]="nameProvided"
                           (keydown.enter)="repw.focus()"
                           name="newPassword"
                           required
                           i18n-placeholder
                           placeholder="Password"
                           type="password"/>
                </div>
                <div class="form-group">
                    <label for="repassword-signup" class="control-label" i18n>
                        Retype the above password
                    </label>
                    <input class="form-control"
                           id="repassword-signup"
                           #repw
                           [(ngModel)]="rePassword"
                           [disabled]="nameProvided"
                           (keydown.enter)="provideName()"
                           name="rePassword"
                           required
                           i18n-placeholder
                           placeholder="Retype password"
                           type="password"/>
                </div>
                <button (click)="provideName()"
                        [disabled]="!name"
                        *ngIf="!nameProvided"
                        class="center-block timButton" i18n>Finish
                </button>
            </div>
            <span *ngIf="finishStatus === 'registered'" i18n>
                Thank you!
            </span>
            <span *ngIf="finishStatus === 'updated'" i18n>
                Your information was updated successfully.
            </span>
            <span *ngIf="finishStatus" i18n>
                Now you can
                <a href="." focusMe>refresh</a>
                the page to log in.
            </span>
        </div>
        <tim-loading *ngIf="signUpRequestInProgress"></tim-loading>
        <tim-alert severity="danger" *ngIf="signUpError">
            <tim-error-description [error]="signUpError"></tim-error-description>
        </tim-alert>
    </div>
    </div>
</div>
    </ng-container>
    <ng-container footer>
        <button class="timButton" (click)="dismiss()" i18n>Close</button>
    </ng-container>
</tim-dialog-frame>
    `,
})
export class LoginDialogComponent extends AngularDialogComponent<ILoginParams, void> {
    showSignup?: boolean; // Show sign up form instead of log in.
    private loggingout = false;
    loginForm = {email: "", password: ""};
    addingToSession = false;

    // Fields related to signup.
    canChangeName = true;
    email: string | undefined;
    emailSent = false;
    finishStatus: undefined | "registered" | "updated";
    loginError: string | undefined;
    name: string | undefined;
    nameProvided = false;
    newPassword: string | undefined;
    rePassword: string | undefined;
    resetPassword = false;
    signUpError: string | undefined;
    signUpRequestInProgress = false;
    tempPassword: string | undefined;
    tempPasswordProvided = false;
    url: string | undefined;
    urlStyle = {position: "absolute", top: "-10em", width: "50%"}; // hide the fake URL field
    homeOrg = genericglobals().homeOrganization;
    idps: IDiscoveryFeedEntry[] = [];

    ngOnInit() {
        const params = this.data;
        if (params) {
            if (params.addingToSession !== undefined) {
                this.addingToSession = params.addingToSession;
            }
            // By default show log in instead of sign up.
            if (params.showSignup !== undefined) {
                this.showSignup = params.showSignup;
            } else {
                this.showSignup = false;
            }
        }
        (async () => {
            this.idps = await loadIdPs();
        })();
    }

    getHomeOrgDisplayName() {
        return getHomeOrgDisplay(this.homeOrg);
    }

    public getTitle() {
        if (this.addingToSession) {
            return $localize `:@@addToSession:Add a user to this session`;
        }
        if (this.showSignup) {
            return $localize `:@@signUp:Sign up`;
        } else {
            return $localize `:@@logIn:Log in`;
        }
    }

    public async loginWithEmail() {
        const result = await Users.loginWithEmail(
            this.loginForm.email,
            this.loginForm.password,
            this.addingToSession);
        if (!result.ok) {
            this.loginError = result.result.data.error;
        } else {
            this.loginError = undefined;
            if (!this.addingToSession) {
                saveCurrentScreenPar();
                window.location.reload();
            }
            this.close();
        }
    }

    public async provideEmail() {
        if (!this.email || this.signUpRequestInProgress) {
            return;
        }
        const r = await this.sendRequest("/altsignup", {
            email: this.email,
            url: this.url,
        });
        if (!r.ok) {
            this.signUpError = r.result.data.error;
        } else {
            this.signUpError = undefined;
            this.emailSent = true;
        }
    }

    public async provideTempPassword() {
        if (!this.email || !this.tempPassword || this.signUpRequestInProgress) {
            return;
        }
        const r = await this.sendRequest<IOkResponse | INameResponse>("/checkTempPass", {
            email: this.email,
            token: this.tempPassword,
        });
        if (!r.ok) {
            this.signUpError = r.result.data.error;
        } else {
            this.signUpError = undefined;
            this.tempPasswordProvided = true;
            if (r.result.data.status === "name") {
                this.name = r.result.data.name;
                this.canChangeName = r.result.data.can_change_name;
                if (!this.canChangeName) {
                }
            } else {
                const nameParts = this.email.split("@")[0].split(".");
                for (const n of nameParts) {
                    // don't try to form name if there are any special characters
                    if (n.match(/[^a-zöäåé]/i)) {
                        return;
                    }
                }
                const lastName = capitalizeFirstLetter(nameParts[nameParts.length - 1]);
                let firstName = "";
                if (nameParts.length > 1) {
                    firstName = capitalizeFirstLetter(nameParts[0]);
                }
                this.name = `${lastName} ${firstName}`.trim();
            }
        }
    }

    public async provideName() {
        if (!this.name || this.signUpRequestInProgress) {
            return;
        }
        const r = await this.sendRequest<{status: "registered" | "updated"}>("/altsignup2", {
            email: this.email,
            passconfirm: this.rePassword,
            password: this.newPassword,
            realname: this.name,
            token: this.tempPassword,
        });
        if (!r.ok) {
            this.signUpError = r.result.data.error;
        } else {
            this.finishStatus = r.result.data.status;
            this.signUpError = undefined;
            this.nameProvided = true;
        }
    }

    public forgotPassword() {
        this.resetPassword = true;
        this.beginSignup();
    }

    /**
     * Return to login from sign up.
     */
    public cancelSignup() {
        this.showSignup = false;
        this.resetPassword = false;
    }

    /**
     * Move on to sign up from login.
     */
    public beginSignup() {
        this.showSignup = true;
        this.url = "";
        if (this.loginForm.email) {
            this.email = this.loginForm.email;
        }
    }

    private async sendRequest<T>(url: string, data: unknown): ToReturn<T> {
        this.signUpRequestInProgress = true;
        const r = await to($http.post<T>(url, data));
        this.signUpRequestInProgress = false;
        return r;
    }
}

/**
 * Open login dialog if no other instances are opened.
 */
export async function showLoginDialog(params: ILoginParams) {
    if (instance) {
        return;
    }
    const dialog = angularDialog.open(LoginDialogComponent, params);
    instance = await dialog;
    await to(instance.result);
    instance = undefined;
    return;
}
