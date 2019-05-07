/*
 * Dialog for log in and sign up.
 */

import {IRootElementService, IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {markAsUsed, to, ToReturn} from "../util/utils";
import {$http} from "../util/ngimport";
import {IUser} from "./IUser";
import {Users} from "./userService";
import {showLoginDialog} from "./loginDialog";

interface INameResponse {
    status: "name";
    name: string;
    can_change_name: boolean;
}

markAsUsed(focusMe);

export class UserMenuDialogController extends DialogController<{}, {}> {
    static component = "userMenuDialog";
    static $inject = ["$element", "$scope"] as const;
    private showSignup?: boolean;
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
        super.$onInit();
    }

    getCurrentUser = () => Users.getCurrent();
    getSessionUsers = () => Users.getSessionUsers();

    addUser($event: Event) {
        $event.stopPropagation();
        this.addingToSession = !this.addingToSession;
        void showLoginDialog(false);
    }

    logout = (user: IUser, logoutFromKorppi = false) => Users.logout(user, logoutFromKorppi);

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

    beginLogout($event: Event) {
        if (Users.isKorppi()) {
            this.loggingout = true;
            $event.stopPropagation();
        } else {
            this.logout(this.getCurrentUser());
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

    /*
     * Dialog title.
     */
    public getTitle() {
        return "User menu";
    }

    private async sendRequest<T>(url: string, data: any): ToReturn<T> {
        this.signUpRequestInProgress = true;
        const r = await to($http.post<T>(url, data));
        this.signUpRequestInProgress = false;
        return r;
    }
}

registerDialogComponent(UserMenuDialogController,
    {
        template:
            `<tim-dialog>
    <dialog-header>
    </dialog-header>
    <dialog-body>
    <div class="btn-group margin-4">
    <ul>
        <li><a ng-href="/view/{{ $ctrl.getCurrentUser().folder.path }}">My documents</a></li>
        <li><a
                ng-click="$ctrl.addUser($event)"
                href="#">Add a user to this session...</a></li>
        <li class="divider"></li>
        <li ng-show="!$ctrl.loggingout">
            <a ng-click="$ctrl.beginLogout($event)" href="#">Log <span
                    ng-show="$ctrl.getSessionUsers().length > 0">everyone</span>
                out<span ng-show="$ctrl.isKorppi()">...</span></a>
        </li>
        <li ng-show="$ctrl.loggingout">
            <a ng-click="$ctrl.logout($ctrl.getCurrentUser(), true)" href="#">Log out (TIM + Korppi)</a>
        </li>
        <li ng-show="$ctrl.loggingout">
            <a ng-click="$ctrl.logout($ctrl.getCurrentUser(), false)" href="#">Log out (TIM only)</a>
        </li>
        <li ng-repeat="u in $ctrl.getSessionUsers()"><a ng-click="$ctrl.logout(u)"
                                                                        href="#">Log {{ u.real_name }} out</a></li>
    </ul>
</div>
    </dialog-body>
    <dialog-footer>
        <button class="timButton" ng-click="$ctrl.dismiss()">Close</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showUserMenuDialog() {
    return await showDialog(UserMenuDialogController, {}).result;
}
