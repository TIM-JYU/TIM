import {IController, IHttpResponse, IPromise} from "angular";
import {timApp} from "tim/app";
import * as loading from "tim/ui/loadingIndicator";
import * as onEnter from "tim/ui/onEnter";
import {$http} from "../util/ngimport";
import {markAsUsed, to} from "../util/utils";
import {IUser} from "./IUser";
import {Users} from "./userService";

markAsUsed(onEnter, loading);

interface IOkResponse {
    status: "ok";
}

type ToReturn<T, U = {data: {error: string}}> = IPromise<[U, undefined] | [null, IHttpResponse<T>]>;
const ToReturn = Promise;

class LoginMenuController implements IController {
    private loggingout: boolean;
    private form: {email: string, password: string};
    private addingToSession: boolean;
    private korppiLoading: boolean = false;
    private emailSent = false;
    private showSignup = false;
    private focusEmail = false;
    private focusPassword = false;
    private tempPasswordProvided = false;
    private nameProvided = false;
    private focusName = false;
    private focusLink = false;
    private wrongPassword = false;
    private email: string | undefined;
    private tempPassword: string | undefined;
    private name: string | undefined;
    private signUpError: string | undefined;
    private signUpRequestInProgress = false;
    private newPassword: string | undefined;
    private rePassword: string | undefined;
    private finishStatus: undefined | "registered" | "updated";
    private resetPassword = false;

    constructor() {
        this.form = {email: "", password: ""};
        this.loggingout = false;
        this.addingToSession = false;
    }

    $onInit() {

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

    stopClick($event: Event) {
        $event.stopPropagation();
    }

    toggled(open: boolean) {
        if (!open) {
            this.addingToSession = false;
        }
    }

    loginWithEmail() {
        Users.loginWithEmail(this.form.email, this.form.password, this.addingToSession,
            () => {
                this.addingToSession = false;
            });
    }

    beginLogout($event: Event) {
        if (Users.isKorppi()) {
            this.loggingout = true;
            $event.stopPropagation();
        } else {
            this.logout(this.getCurrentUser());
        }
    }

    private async provideEmail() {
        if (!this.email || this.signUpRequestInProgress) {
            return;
        }
        const [err, resp] = await this.sendRequest("/altsignup", {
            email: this.email,
        });
        if (err) {
            this.signUpError = err.data.error;
        } else if (resp) {
            this.signUpError = undefined;
            this.emailSent = true;
            this.focusPassword = true;
        }
    }

    private async sendRequest<T>(url: string, data: any): ToReturn<T> {
        this.signUpRequestInProgress = true;
        const [err, resp] = await to($http.post<T>(url, data));
        this.signUpRequestInProgress = false;
        if (err && !resp) {
            return [err, resp];
        } else if (!err && resp) {
            return [err, resp];
        } else {
            throw new Error("unreachable");
        }
    }

    private beginSignup() {
        this.showSignup = true;
        this.focusEmail = true;
    }

    private async provideTempPassword() {
        if (!this.tempPassword || this.signUpRequestInProgress) {
            return;
        }
        const [err, resp] = await this.sendRequest("/checkTempPass", {
            email: this.email,
            token: this.tempPassword,
        });
        if (err) {
            this.signUpError = err.data.error;
        } else if (resp) {
            this.signUpError = undefined;
            this.tempPasswordProvided = true;
            this.focusName = true;
        }
    }

    private async provideName() {
        if (!this.name || this.signUpRequestInProgress) {
            return;
        }
        const [err, resp] = await this.sendRequest<{status: "registered" | "updated"}>("/altsignup2", {
            email: this.email,
            passconfirm: this.rePassword,
            password: this.newPassword,
            realname: this.name,
            token: this.tempPassword,
        });
        if (err) {
            this.signUpError = err.data.error;
        } else if (resp) {
            this.finishStatus = resp.data.status;
            this.signUpError = undefined;
            this.nameProvided = true;
            this.focusLink = true;
        }
    }

    private forgotPassword() {
        this.resetPassword = true;
        this.beginSignup();
    }
}

timApp.component("loginMenu", {
    bindings: {},
    controller: LoginMenuController,
    templateUrl: "/static/templates/loginMenu.html",
});
