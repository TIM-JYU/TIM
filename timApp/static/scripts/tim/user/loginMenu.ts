import {IController} from "angular";
import {timApp} from "tim/app";
import * as loading from "tim/ui/loadingIndicator";
import * as onEnter from "tim/ui/onEnter";
import {$http} from "../util/ngimport";
import {capitalizeFirstLetter, IOkResponse, markAsUsed, to, ToReturn} from "../util/utils";
import {IUser} from "./IUser";
import {Users} from "./userService";
import {saveCurrentScreenPar} from "../document/parhelpers";

markAsUsed(onEnter, loading);

interface INameResponse {
    status: "name";
    name: string;
    can_change_name: boolean;
}

class LoginMenuController implements IController {
    private loggingout: boolean;
    private loginForm: {email: string, password: string};
    private addingToSession: boolean;
    private korppiLoading: boolean = false;

    // fields related to signup
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
    private showSignup = false;
    private signUpError: string | undefined;
    private signUpRequestInProgress = false;
    private tempPassword: string | undefined;
    private tempPasswordProvided = false;

    constructor() {
        this.loginForm = {email: "", password: ""};
        this.loggingout = false;
        this.addingToSession = false;
    }

    getCurrentUser = () => Users.getCurrent();
    getSessionUsers = () => Users.getSessionUsers();

    addUser($event: Event) {
        $event.stopPropagation();
        this.addingToSession = !this.addingToSession;
    }

    logout = (user: IUser, logoutFromKorppi = false) => Users.logout(user, logoutFromKorppi);
    isLoggedIn = () => Users.isLoggedIn();

    korppiLogin(addingToSession: boolean) {
        this.korppiLoading = true;
        Users.korppiLogin(addingToSession);
    }

    isKorppi = () => Users.isKorppi();

    // noinspection JSMethodCanBeStatic
    public stopClick($event: Event) {
        $event.stopPropagation();
    }

    public toggled(open: boolean) {
        if (!open) {
            this.addingToSession = false;
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
        }
    }

    beginLogout($event: Event) {
        if (Users.isKorppi()) {
            this.loggingout = true;
            $event.stopPropagation();
        } else {
            this.logout(this.getCurrentUser());
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

    public getTitle() {
        if (!this.showSignup) {
            return "Log in";
        } else if (this.resetPassword) {
            return "Reset password";
        } else {
            return "Sign up";
        }
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

timApp.component("loginMenu", {
    bindings: {},
    controller: LoginMenuController,
    templateUrl: "/static/templates/loginMenu.html",
});
