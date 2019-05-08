/*
 * Dialog for user menu.
 */

import {IRootElementService, IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {markAsUsed} from "../util/utils";
import {IUser} from "./IUser";
import {Users} from "./userService";
import {showLoginDialog} from "./loginDialog";

markAsUsed(focusMe);

export class UserMenuDialogController extends DialogController<{}, {}> {
    static component = "userMenuDialog";
    static $inject = ["$element", "$scope"] as const;
    private loggingout: boolean;
    private addingToSession: boolean;

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
        this.loggingout = false;
        this.addingToSession = false;
    }

    async $onInit() {
        super.$onInit();
    }

    getCurrentUser = () => Users.getCurrent();
    getSessionUsers = () => Users.getSessionUsers(); // Used in html

    addUser($event: Event) {
        $event.stopPropagation();
        this.addingToSession = !this.addingToSession;
        void showLoginDialog(false);
    }

    logout = (user: IUser, logoutFromKorppi = false) => Users.logout(user, logoutFromKorppi);

    isKorppi = () => Users.isKorppi();

    beginLogout($event: Event) {
        if (Users.isKorppi()) {
            this.loggingout = true;
            $event.stopPropagation();
        } else {
            this.logout(this.getCurrentUser());
        }
    }

    /*
     * Dialog title.
     */
    public getTitle() {
        return "User menu";
    }

    // TODO: Change this into a component: a dropdown menu instead of dialog!
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
