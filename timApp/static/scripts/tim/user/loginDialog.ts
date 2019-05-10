/*
 * Dialog for log in and sign up.
 */

import {IRootElementService, IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {capitalizeFirstLetter, IOkResponse, markAsUsed, to, ToReturn} from "../util/utils";
import {$http} from "../util/ngimport";
import {IUser} from "./IUser";
import {Users} from "./userService";
import {saveCurrentScreenPar} from "../document/parhelpers";

interface INameResponse {
    status: "name";
    name: string;
    can_change_name: boolean;
}

interface ILoginParams {
    showSignup?: boolean;
    addingToSession?: boolean;
}

markAsUsed(focusMe);

let instance: LoginDialogController | undefined;

export class LoginDialogController extends DialogController<{params: ILoginParams}, {}> {
    static component = "loginDialog";
    static $inject = ["$element", "$scope"] as const;
    private showSignup?: boolean; // Show sign up form instead of log in.
    private loggingout: boolean;
    private loginForm: {email: string, password: string};
    private addingToSession: boolean; // Adding another user.
    private korppiLoading: boolean = false;

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

    /*
     * Dialog title.
     */
    public getTitle() {
        if (this.addingToSession) {
            return "Add a user to this session";
        }
        if (this.showSignup) {
            return "Sign up";
        } else {
            return "Log in";
        }
    }

    logout = (user: IUser, logoutFromKorppi = false) => Users.logout(user, logoutFromKorppi);

    korppiLogin(addingToSession: boolean) {
        this.korppiLoading = true;
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
        }
    }

    public async provideEmail() {
        if (!this.email || this.signUpRequestInProgress) {
            return;
        }
        const r = await this.sendRequest("/altsignup", {
            email: this.email,
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

    public cancelSignup() {
        this.showSignup = false;
        this.resetPassword = false;
    }

    public getEmailOrUserText(capitalize?: boolean) {
        let txt;
        if (this.resetPassword) {
            txt = "email or username";
        } else {
            txt = "email";
        }
        if (capitalize) {
            return capitalizeFirstLetter(txt);
        } else {
            return txt;
        }
    }

    public beginSignup() {
        this.showSignup = true;
        this.focusEmail = true;
        if (this.loginForm.email) {
            this.email = this.loginForm.email;
        }
    }

    private async sendRequest<T>(url: string, data: any): ToReturn<T> {
        this.signUpRequestInProgress = true;
        const r = await to($http.post<T>(url, data));
        this.signUpRequestInProgress = false;
        return r;
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
                JYU students and staff, please log in with Korppi:
            </p>

            <button class="timButton center-block" type="button"
                    ng-click="$ctrl.korppiLogin($ctrl.addingToSession)">Log in
                with Korppi
            </button>
            <p class="text-center text-smaller"><a href="/view/tim/ongelmia-kirjautumisessa">Problems logging
                in?</a></p>
            <img class="center-block" ng-show="$ctrl.korppiLoading" src="/static/images/loading.gif">
            <hr>
            <p class="text-center">
                Others, please log in with your TIM account.
            </p>

            <div class="form-group">
                <label for="email" class="control-label">Email or username</label>
                <input class="form-control"
                       id="email"
                       ng-model="$ctrl.loginForm.email"
                       on-enter="$ctrl.focusLoginPassword = true"
                       type="text">
            </div>

            <div class="form-group">
                <label for="password" class="control-label">Password</label>
                <input class="form-control"
                       id="password"
                       focus-me="$ctrl.focusLoginPassword"
                       ng-model="$ctrl.loginForm.password"
                       type="password">
                <p class="text-smaller"><a href="#" ng-click="$ctrl.forgotPassword()">I forgot my password</a></p>
            </div>

            <button class="center-block timButton" type="submit">Log in</button>
            <tim-alert severity="danger" ng-show="$ctrl.loginError">
                {{ $ctrl.loginError }}
            </tim-alert>
            <hr>
            <p class="text-center" ng-show="!$ctrl.showSignup">
                Not a JYU student or staff member and don't have a TIM account?
            </p>
            <button ng-show="!$ctrl.showSignup" class="center-block timButton" type="button"
                    ng-click="$ctrl.beginSignup()">
                Sign up
            </button>
        </form>
        <div class="form" ng-show="$ctrl.showSignup">
            <div class="text-center" ng-if="!$ctrl.resetPassword"></div>
                <p>If you don't have an existing TIM or Korppi account, you can create a new TIM account here.</p>
                <p>Please input your email address to receive a temporary password.</p>
            </div>
            <p class="text-center" ng-if="$ctrl.resetPassword && !$ctrl.emailSent">
                To reset password, enter your email or username first.
            </p>
            <div class="form-group">
                <label for="email-signup" class="control-label">{{ $ctrl.getEmailOrUserText(true) }}</label>
                <input class="form-control"
                       id="email-signup"
                       focus-me="$ctrl.focusEmail"
                       ng-model="$ctrl.email"
                       ng-disabled="$ctrl.emailSent"
                       on-enter="$ctrl.provideEmail()"
                       name="email"
                       required
                       placeholder="Enter your {{ $ctrl.getEmailOrUserText() }}"
                       type="text"/>
            </div>
            <button ng-click="$ctrl.provideEmail()"
                    ng-disabled="!$ctrl.email"
                    ng-show="!$ctrl.emailSent"
                    class="timButton">
                Continue
            </button>
            <button ng-click="$ctrl.cancelSignup()"
                    ng-show="!$ctrl.emailSent"
                    class="btn btn-default">
                Cancel
            </button>
            <div ng-show="$ctrl.emailSent">
                <div class="form-group" ng-show="!$ctrl.tempPasswordProvided">
                    <label for="password-signup" class="control-label">
                        TIM sent you a temporary password. Please check your email and type the password below to
                        continue creating your account.
                    </label>
                    <input class="form-control"
                           id="password-signup"
                           focus-me="$ctrl.focusPassword"
                           ng-model="$ctrl.tempPassword"
                           on-enter="$ctrl.provideTempPassword()"
                           name="tempPassword"
                           required
                           placeholder="Password you received"
                           type="password"/>
                </div>
                <button ng-click="$ctrl.provideTempPassword()"
                        ng-disabled="!$ctrl.tempPassword"
                        ng-show="!$ctrl.tempPasswordProvided"
                        class="center-block timButton">Continue
                </button>
                <div ng-show="$ctrl.tempPasswordProvided">
                    <div class="form-group">
                        <label for="name-signup" class="control-label">
                            Enter your name (Lastname Firstname)
                        </label>
                        <input class="form-control"
                               id="name-signup"
                               focus-me="$ctrl.focusName"
                               ng-model="$ctrl.name"
                               ng-disabled="$ctrl.nameProvided || !$ctrl.canChangeName"
                               on-enter="$ctrl.focusNewPassword = true"
                               name="name"
                               required
                               placeholder="Your name"
                               type="text"/>
                    </div>
                    <div class="form-group">
                        <label for="newpassword-signup" class="control-label">
                            Create a new password
                        </label>
                        <input class="form-control"
                               id="newpassword-signup"
                               focus-me="$ctrl.focusNewPassword"
                               ng-model="$ctrl.newPassword"
                               ng-disabled="$ctrl.nameProvided"
                               on-enter="$ctrl.focusRePassword = true"
                               name="newPassword"
                               required
                               placeholder="Password"
                               type="password"/>
                    </div>
                    <div class="form-group">
                        <label for="repassword-signup" class="control-label">
                            Retype the above password
                        </label>
                        <input class="form-control"
                               id="repassword-signup"
                               focus-me="$ctrl.focusRePassword"
                               ng-model="$ctrl.rePassword"
                               ng-disabled="$ctrl.nameProvided"
                               on-enter="$ctrl.provideName()"
                               name="rePassword"
                               required
                               placeholder="Retype password"
                               type="password"/>
                    </div>
                    <button ng-click="$ctrl.provideName()"
                            ng-disabled="!$ctrl.name"
                            ng-show="!$ctrl.nameProvided"
                            class="center-block timButton">Finish
                    </button>
                </div>
                <span ng-if="$ctrl.finishStatus === 'registered'">
                    Thank you!
                </span>
                <span ng-if="$ctrl.finishStatus === 'updated'">
                    Your information was updated successfully.
                </span>
                <span ng-if="$ctrl.finishStatus">
                    Now you can
                    <a href="." focus-me="$ctrl.focusLink">refresh</a>
                    the page to log in.
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
        <button class="timButton" ng-click="$ctrl.dismiss()">Close</button>
    </dialog-footer>
</tim-dialog>
`,
    });

/**
 * Open login dialog if no other instances are opened.
 * @param params Contains showSignup and addingToSession.
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
