/*
 * Dialog for log in and sign up.
 */

import {IRootElementService, IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import * as onEnter from "tim/ui/onEnter";
import {saveCurrentScreenPar} from "../document/parhelpers";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {tr} from "../ui/language";
import {$http} from "../util/ngimport";
import {capitalizeFirstLetter, getURLParameter, IOkResponse, markAsUsed, to, ToReturn} from "../util/utils";
import * as hakaLogin from "./hakaLogin";
import {IUser} from "./IUser";
import {Users} from "./userService";

markAsUsed(hakaLogin);

interface INameResponse {
    status: "name";
    name: string;
    can_change_name: boolean;
}

interface ILoginParams {
    showSignup: boolean;
    addingToSession: boolean;
}

markAsUsed(focusMe, onEnter);

let instance: LoginDialogController | undefined;

export class LoginDialogController extends DialogController<{params: ILoginParams}, {}> {
    static component = "loginDialog";
    static $inject = ["$element", "$scope"] as const;
    private showSignup?: boolean; // Show sign up form instead of log in.
    private loggingout: boolean;
    private loginForm: {email: string, password: string};
    private addingToSession: boolean; // Adding another user.

    // Fields related to signup.
    private canChangeName = true;
    private email: string | undefined;
    private emailSent = false;
    private finishStatus: undefined | "registered" | "updated";
    private focusEmail = false;
    private focusLink = false;
    private focusName = false;
    private focusNewPassword = false;
    private focusPassword = false;
    private loginError: string | undefined;
    private name: string | undefined;
    private nameProvided = false;
    private newPassword: string | undefined;
    private rePassword: string | undefined;
    private resetPassword = false;
    private signUpError: string | undefined;
    private signUpRequestInProgress = false;
    private tempPassword: string | undefined;
    private tempPasswordProvided = false;
    private url: string | undefined;
    private urlStyle = {position: "absolute", top: "-10em", width: "50%"}; // hide the fake URL field

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
        this.loginForm = {email: "", password: ""};
        this.loggingout = false;
        this.addingToSession = false;
    }

    async $onInit() {
        const params = this.resolve.params;
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
        // Parameters related to the dialog title need to be decided before this, because getTitle() is called here.
        super.$onInit();
    }

    public getTitle() {
        if (this.addingToSession) {
            return tr("Add a user to this session");
        }
        if (this.showSignup) {
            return tr("Sign up");
        } else {
            return tr("Log in");
        }
    }

    logout = (user: IUser, logoutFromKorppi = false) => Users.logout(user, logoutFromKorppi);

    korppiLogin(addingToSession: boolean) {
        Users.korppiLogin(addingToSession);
    }

    // noinspection JSMethodCanBeStatic
    public stopClick($event: Event) {
        $event.stopPropagation();
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
            this.close({});
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
            this.focusPassword = true;
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
            this.focusName = true;
            if (r.result.data.status === "name") {
                this.name = r.result.data.name;
                this.canChangeName = r.result.data.can_change_name;
                if (!this.canChangeName) {
                    this.focusName = false;
                    this.focusNewPassword = true;
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
            this.focusLink = true;
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
        this.updateTitle();
    }

    /**
     * Move on to sign up from login.
     */
    public beginSignup() {
        this.showSignup = true;
        this.focusEmail = true;
        if (this.loginForm.email) {
            this.email = this.loginForm.email;
        }
        this.updateTitle();
    }

    /**
     * Updates dialog title.
     */
    private updateTitle() {
        this.draggable.setCaption(this.getTitle());
    }

    private async sendRequest<T>(url: string, data: unknown): ToReturn<T> {
        this.signUpRequestInProgress = true;
        const r = await to($http.post<T>(url, data));
        this.signUpRequestInProgress = false;
        return r;
    }

    showHaka() {
        return getURLParameter("haka") != null;
    }
}

registerDialogComponent(LoginDialogController,
    {
        template:
            `<tim-dialog>
    <dialog-header>
    </dialog-header>
    <dialog-body>
<div class="row" ng-click="$ctrl.stopClick($event)">
        <div class="col-sm-12">
        <form ng-submit="$ctrl.loginWithEmail()" ng-show="!$ctrl.showSignup">
            <p class="text-center">
                {{ 'JYU students and staff, please log in with JYU account:' | tr }}
            </p>

            <button class="timButton center-block" type="button"
                    ng-click="$ctrl.korppiLogin($ctrl.addingToSession)">{{ 'Log in with JYU account' | tr }}
            </button>
            <p class="text-center text-smaller"><a href="/view/tim/ongelmia-kirjautumisessa">{{ 'Problems logging in?' | tr }}</a></p>
            <div ng-if="$ctrl.showHaka()">
                <hr>
                <haka-login></haka-login>
            </div>
            <hr>
            <p class="text-center">
                {{ 'Others, please log in with your TIM account:' | tr }}
            </p>

            <div class="form-group">
                <label for="email" class="control-label">{{ 'Email or username' | tr }}</label>
                <input class="form-control"
                       id="email"
                       ng-model="$ctrl.loginForm.email"
                       on-enter="$ctrl.focusLoginPassword = true"
                       type="text">
            </div>

            <div class="form-group">
                <label for="password" class="control-label">{{ 'Password' | tr }}</label>
                <input class="form-control"
                       id="password"
                       focus-me="$ctrl.focusLoginPassword"
                       ng-model="$ctrl.loginForm.password"
                       type="password">
                <p class="text-smaller"><a href="#" ng-click="$ctrl.forgotPassword()">{{ 'I forgot my password' | tr }}</a></p>
            </div>

            <button class="center-block timButton" type="submit">{{ 'Log in' | tr }}</button>
            <tim-alert severity="danger" ng-show="$ctrl.loginError">
                {{ $ctrl.loginError }}
            </tim-alert>
            <hr>
            <p class="text-center" ng-show="!$ctrl.showSignup">
                {{ "Not a JYU student or staff member and don't have a TIM account?" | tr }}
            </p>
            <button ng-show="!$ctrl.showSignup" class="center-block timButton" type="button"
                    ng-click="$ctrl.beginSignup()">
                {{ 'Sign up' | tr }}
            </button>
        </form>
        <div class="form" ng-show="$ctrl.showSignup">
            <div class="text-center" ng-if="!$ctrl.resetPassword">
                <p>{{ "If you don't have an existing TIM or JYU account, you can create a TIM account here." | tr }}</p>
                <p>{{ 'Please input your email address to receive a temporary password.' | tr }}</p>
            </div>
            <p class="text-center" ng-if="$ctrl.resetPassword && !$ctrl.emailSent">
                {{ 'To reset password, enter your email or username first.' | tr }}
            </p>
            <div class="form-group">
                <label for="email-signup" class="control-label">{{ 'Email or username' | tr }}</label>
                <input class="form-control"
                       id="email-signup"
                       focus-me="$ctrl.focusEmail"
                       ng-model="$ctrl.email"
                       ng-disabled="$ctrl.emailSent"
                       on-enter="$ctrl.provideEmail()"
                       name="email"
                       required
                       placeholder="{{ 'Email or username' | tr }}"
                       type="text"/>
                <label ng-style="$ctrl.urlStyle" for="url" class="control-label">{{ 'Do not type anything here' | tr }}</label>
                <input class="form-control"
                       ng-style="$ctrl.urlStyle"
                       tabindex="-1"
                       id="url"
                       ng-model="$ctrl.url"
                       ng-disabled="$ctrl.emailSent"
                       name="url"
                       placeholder="{{ 'Do not type anything here' | tr }}"
                       type="text"/>
            </div>
            <button ng-click="$ctrl.provideEmail()"
                    ng-disabled="!$ctrl.email"
                    ng-show="!$ctrl.emailSent"
                    class="timButton">
                {{ 'Continue' | tr }}
            </button>
            <button ng-click="$ctrl.cancelSignup()"
                    ng-show="!$ctrl.emailSent"
                    class="btn btn-default">
                {{ 'Cancel' | tr }}
            </button>
            <div ng-show="$ctrl.emailSent">
                <div class="form-group" ng-show="!$ctrl.tempPasswordProvided">
                    <label for="password-signup" class="control-label">
                        {{ 'TIM sent you a temporary password. Please check your email and type the password below to continue.' | tr }}
                    </label>
                    <input class="form-control"
                           id="password-signup"
                           focus-me="$ctrl.focusPassword"
                           ng-model="$ctrl.tempPassword"
                           on-enter="$ctrl.provideTempPassword()"
                           name="tempPassword"
                           required
                           placeholder="{{ 'Password you received' | tr }}"
                           type="password"/>
                </div>
                <button ng-click="$ctrl.provideTempPassword()"
                        ng-disabled="!$ctrl.tempPassword"
                        ng-show="!$ctrl.tempPasswordProvided"
                        class="center-block timButton">{{ 'Continue' | tr }}
                </button>
                <div ng-show="$ctrl.tempPasswordProvided">
                    <div class="form-group">
                        <label for="name-signup" class="control-label">
                            {{ 'Enter your name (Lastname Firstname)' | tr }}
                        </label>
                        <input class="form-control"
                               id="name-signup"
                               focus-me="$ctrl.focusName"
                               ng-model="$ctrl.name"
                               ng-disabled="$ctrl.nameProvided || !$ctrl.canChangeName"
                               on-enter="$ctrl.focusNewPassword = true"
                               name="name"
                               required
                               placeholder="{{ 'Your name' | tr }}"
                               type="text"/>
                    </div>
                    <div class="form-group">
                        <label for="newpassword-signup" class="control-label">
                            {{ 'Create a new password' | tr }}
                        </label>
                        <input class="form-control"
                               id="newpassword-signup"
                               focus-me="$ctrl.focusNewPassword"
                               ng-model="$ctrl.newPassword"
                               ng-disabled="$ctrl.nameProvided"
                               on-enter="$ctrl.focusRePassword = true"
                               name="newPassword"
                               required
                               placeholder="{{ 'Password' | tr }}"
                               type="password"/>
                    </div>
                    <div class="form-group">
                        <label for="repassword-signup" class="control-label">
                            {{ 'Retype the above password' | tr }}
                        </label>
                        <input class="form-control"
                               id="repassword-signup"
                               focus-me="$ctrl.focusRePassword"
                               ng-model="$ctrl.rePassword"
                               ng-disabled="$ctrl.nameProvided"
                               on-enter="$ctrl.provideName()"
                               name="rePassword"
                               required
                               placeholder="{{ 'Retype password' | tr }}"
                               type="password"/>
                    </div>
                    <button ng-click="$ctrl.provideName()"
                            ng-disabled="!$ctrl.name"
                            ng-show="!$ctrl.nameProvided"
                            class="center-block timButton">{{ 'Finish' | tr }}
                    </button>
                </div>
                <span ng-if="$ctrl.finishStatus === 'registered'">
                    {{ 'Thank you!' | tr }}
                </span>
                <span ng-if="$ctrl.finishStatus === 'updated'">
                    {{ 'Your information was updated successfully.' | tr }}
                </span>
                <span ng-if="$ctrl.finishStatus">
                    {{ 'Now you can' | tr }}
                    <a href="." focus-me="$ctrl.focusLink">{{ 'refresh' | tr }}</a>
                    {{ 'the page to log in.' | tr }}
                </span>
            </div>
            <tim-loading ng-show="$ctrl.signUpRequestInProgress"></tim-loading>
            <tim-alert severity="danger" ng-show="$ctrl.signUpError">
                {{ $ctrl.signUpError }}
            </tim-alert>
        </div>
        </div>
</div>
    </dialog-body>
    <dialog-footer>
        <button class="timButton" ng-click="$ctrl.dismiss()">{{ 'Close' | tr }}</button>
    </dialog-footer>
</tim-dialog>
`,
    });

/**
 * Open login dialog if no other instances are opened.
 */
export async function showLoginDialog(params: ILoginParams) {
    if (instance) {
        return;
    }
    const dialog = showDialog(LoginDialogController, {params: () => params});
    instance = await dialog.dialogInstance.promise;
    await to(dialog.result);
    instance = undefined;
    return;
}
