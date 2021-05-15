/*
 * Dialog for log in and sign up.
 */

import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, ElementRef, NgModule, ViewChild} from "@angular/core";
import {getVisibilityVars, IVisibilityVars} from "tim/timRoot";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {FormsModule} from "@angular/forms";
import {BrowserModule} from "@angular/platform-browser";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {JsonValue} from "tim/util/jsonvalue";
import {saveCurrentScreenPar} from "../document/parhelpers";
import {genericglobals} from "../util/globals";
import {$http} from "../util/ngimport";
import {
    capitalizeFirstLetter,
    IOkResponse,
    mapSuccess,
    Result,
    to,
    ToReturn,
} from "../util/utils";
import {
    HakaLoginComponent,
    IDiscoveryFeedEntry,
    loadIdPs,
} from "./haka-login.component";
import {ILoginResponse, Users} from "./userService";

interface INameResponse {
    status: "name";
    name: string;
    can_change_name: boolean;
}

export interface ILoginParams {
    showSignup: boolean;
    addingToSession: boolean;
}

const orgNames: Record<string, string | undefined> = {
    "aalto.fi": "Aalto",
    "jyu.fi": "JYU",
    "tuni.fi": "TUNI",
};

function getHomeOrgDisplay(s: string): string {
    return orgNames[s] ?? s;
}

export interface ISimpleLoginResponse {
    type: "login";
    data: ILoginResponse;
}

interface ISimpleRegistrationResponse {
    type: "registration";
    data: {can_change_name: boolean; name: string | null};
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
        <ng-container *ngIf="!hideVars.hakaLogin">
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
        </ng-container>

        <ng-container *ngIf="!hideVars.emailLogin">
        <div class="form-group">
            <label for="email" class="control-label" i18n>Email or username</label>
            <input class="form-control"
                   id="email"
                   [disabled]="simpleLoginEmailGiven"
                   #loginEmail
                   [(ngModel)]="loginForm.email"
                   (keydown.enter)="handleEmailGiven()"
                   type="text">
        </div>

            <ng-container *ngIf="simpleLoginEmailGiven">
                <p>
                    <ng-container *ngIf="useStudyInfoMessage; else normalHelpMsg" i18n>
                        If you have not logged in before,
                        and your email corresponds to the one in Studyinfo.fi,
                        TIM sent a password to your email now.
                        Please read the email in a separate browser tab,
                        and please wait for the email at least 5 minutes
                        if you don't get the email right away.
                    </ng-container>
                    <ng-template #normalHelpMsg i18n>
                        If you have not logged in to TIM before, TIM sent a password to your email now.
                    </ng-template>
                </p>
                <p i18n>If you have logged in before, use your current password.</p>
            </ng-container>

        <div class="form-group" [hidden]="simpleEmailLogin && !simpleLoginEmailGiven">
            <label for="password" class="control-label" i18n>Password</label>
            <input class="form-control"
                   id="password"
                   #pw
                   [(ngModel)]="loginForm.password"
                   (keydown.enter)="loginWithEmail()"
                   focusMe
                   [enable]="focusPassword"
                   type="password">
            <p *ngIf="!hideVars.passwordRecovery" class="text-smaller"><a href="#" (click)="forgotPassword()" i18n>I forgot my password</a></p>
        </div>

            <div class="flex cl align-center" *ngIf="simpleEmailLogin && !simpleLoginEmailGiven">
                <button [disabled]="!loginForm.email || signUpRequestInProgress"
                        class="timButton"
                        (click)="handleEmailGiven()" i18n>Continue</button>
                <tim-loading *ngIf="signUpRequestInProgress"></tim-loading>
            </div>

        <div class="flex cl align-center" *ngIf="simpleLoginEmailGiven || !simpleEmailLogin">
            <button [disabled]="loggingIn" class="timButton" (click)="loginWithEmail()" i18n>Log in</button>
            <tim-loading *ngIf="loggingIn"></tim-loading>
        </div>
        <tim-alert severity="danger" *ngIf="loginError">
            <tim-error-description [error]="loginError"></tim-error-description>
        </tim-alert>
        </ng-container>

        <ng-container *ngIf="!hideVars.signup">
            <hr>
            <p class="text-center" *ngIf="!showSignup" i18n>
                Not a {{getHomeOrgDisplayName()}} student or staff member and don't have a TIM account?
            </p>
            <button *ngIf="!showSignup" class="center-block timButton" type="button"
                (click)="beginSignup()" i18n="@@signUp">
                Sign up
            </button>
        </ng-container>
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
                        Create a new password (at least {{ minPasswordLength }} characters)
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
                <a href="" focusMe>refresh</a>
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
export class LoginDialogComponent extends AngularDialogComponent<
    ILoginParams,
    void
> {
    hideVars: IVisibilityVars = getVisibilityVars();
    showSignup?: boolean; // Show sign up form instead of log in.
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
    newPassword = "";
    rePassword = "";
    resetPassword = false;
    signUpError: string | undefined;
    signUpRequestInProgress = false;
    tempPassword: string | undefined;
    tempPasswordProvided = false;
    url: string | undefined;
    urlStyle = {position: "absolute", top: "-10em", width: "50%"}; // hide the fake URL field
    homeOrg = genericglobals().homeOrganization;
    config = genericglobals().config;
    idps: IDiscoveryFeedEntry[] = [];
    protected dialogName = "login";

    // Reserve space for possible login error so that it will be directly visible and not behind a scrollbar.
    // TODO: This is probably no longer needed because the login dialog will resize itself whenever the dialog DOM
    //  is changed. That's why this is set to 0 now.
    protected extraVerticalSize = 0;
    loggingIn = false;

    @ViewChild("loginEmail") private loginField!: ElementRef<HTMLInputElement>;
    @ViewChild("pw") private passwordField!: ElementRef<HTMLInputElement>;
    simpleEmailLogin = this.config.simpleEmailLogin;
    simpleLoginEmailGiven = false;
    focusPassword = false;
    useStudyInfoMessage = this.config.simpleLoginUseStudyInfoMessage;
    minPasswordLength = this.config.minPasswordLength;

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

    ngAfterViewInit() {
        super.ngAfterViewInit();
        if (this.hideVars.hakaLogin) {
            this.loginField.nativeElement.focus();
        }
    }

    getHomeOrgDisplayName() {
        return getHomeOrgDisplay(this.homeOrg);
    }

    public getTitle() {
        if (this.addingToSession) {
            return $localize`:@@addToSession:Add a user to this session`;
        }
        if (this.showSignup) {
            return $localize`:@@signUp:Sign up`;
        } else {
            return $localize`:@@logIn:Log in`;
        }
    }

    public async loginWithEmail() {
        if (this.loggingIn) {
            return;
        }
        this.loginError = undefined;
        this.loggingIn = true;
        let result: Result<
            ISimpleLoginResponse | ISimpleRegistrationResponse,
            {data: {error: string}}
        >;
        if (this.simpleEmailLogin) {
            result = mapSuccess(
                await this.sendRequest<
                    ISimpleLoginResponse | ISimpleRegistrationResponse
                >("/simpleLogin/password", {
                    email: this.loginForm.email,
                    password: this.loginForm.password,
                }),
                (r) =>
                    r.data.type === "login"
                        ? {type: r.data.type, data: r.data.data}
                        : {type: r.data.type, data: r.data.data}
            );
        } else {
            result = mapSuccess(
                await Users.loginWithEmail(
                    this.loginForm.email,
                    this.loginForm.password,
                    this.addingToSession
                ),
                (r) => ({type: "login", data: r.data})
            );
        }

        this.loggingIn = false;
        if (!result.ok) {
            this.loginError = result.result.data.error;
        } else {
            if (result.result.type === "login") {
                if (!this.addingToSession) {
                    saveCurrentScreenPar();
                    window.location.reload();
                }
                this.close();
            } else {
                this.tempPasswordProvided = true;
                this.tempPassword = this.loginForm.password;
                this.email = this.loginForm.email;
                this.canChangeName = result.result.data.can_change_name;
                this.name = result.result.data.name ?? "";
                this.showSignup = true;
                this.emailSent = true;
                this.resetPassword = true;
            }
        }
    }

    public async provideEmail() {
        if (!this.email || this.signUpRequestInProgress) {
            return;
        }
        const r = await this.sendRequest("/emailSignup", {
            email: this.email,
            url: this.url,
            reset_password: this.resetPassword,
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
        const r = await this.sendRequest<IOkResponse | INameResponse>(
            "/checkTempPass",
            {
                email: this.email,
                token: this.tempPassword,
            }
        );
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
                const lastName = capitalizeFirstLetter(
                    nameParts[nameParts.length - 1]
                );
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
        const r = await this.sendRequest<{status: "registered" | "updated"}>(
            "/emailSignupFinish",
            {
                email: this.email,
                passconfirm: this.rePassword,
                password: this.newPassword,
                realname: this.name,
                token: this.tempPassword,
            }
        );
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

    private async sendRequest<T>(url: string, data: JsonValue): ToReturn<T> {
        this.signUpRequestInProgress = true;
        const r = await to($http.post<T>(url, data));
        this.signUpRequestInProgress = false;
        return r;
    }

    async continueSimpleLogin() {
        this.loginError = undefined;
        const r = await this.sendRequest(`/simpleLogin/email`, {
            email: this.loginForm.email,
        });
        if (!r.ok) {
            this.loginError = r.result.data.error;
            return;
        }
        this.simpleLoginEmailGiven = true;
    }

    async handleEmailGiven() {
        if (this.simpleEmailLogin) {
            await this.continueSimpleLogin();
        }
        this.focusPassword = true;
    }
}

@NgModule({
    declarations: [LoginDialogComponent, HakaLoginComponent],
    imports: [BrowserModule, DialogModule, FormsModule, TimUtilityModule],
})
export class LoginDialogModule {}
