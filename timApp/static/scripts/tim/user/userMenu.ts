import {IController} from "angular";
import {timApp} from "tim/app";
import {tr} from "../ui/language";
import {IUser} from "./IUser";
import {showLoginDialog} from "./loginDialog";
import {Users} from "./userService";

/**
 * User menu component with a button that displays current user name and the number of additional
 * users logged in the session and opens a dropdown menu with log out and other options.
 */
class UserMenuController implements IController {
    static component = "userMenu";
    static $inject = ["$element", "$scope"] as const;
    private loggingout: boolean;

    constructor() {
        this.loggingout = false;
    }

    isLoggedIn = () => Users.isLoggedIn();

    getCurrentUser = () => Users.getCurrent();
    getSessionUsers = () => Users.getSessionUsers(); // Used in HTML.

    /**
     * Add another user to the session using login dialog.
     */
    addUser() {
        void showLoginDialog({showSignup: false, addingToSession: true});
    }

    logout = (user: IUser, logoutFromKorppi = false) => Users.logout(user, logoutFromKorppi);

    isKorppi = () => Users.isKorppi(); // Used in html.

    beginLogout($event: Event) {
        if (Users.isKorppi()) {
            this.loggingout = true;
            $event.stopPropagation();
        } else {
            this.logout(this.getCurrentUser());
        }
    }

    tr(s: string) {
        return tr(s);
    }
}

timApp.component("userMenu", {
    controller: UserMenuController,
    template: `
<div class="btn-group margin-4" uib-dropdown on-toggle="$ctrl.toggled(open)">
    <button type="button" title="{{ 'You\\'re logged in' | tr }}" class="btn btn-primary" uib-dropdown-toggle>
    {{ $ctrl.getCurrentUser().real_name }} <span
        ng-if="$ctrl.getSessionUsers().length > 0">{{ 'and' | tr }} {{ $ctrl.getSessionUsers().length }} <ng-pluralize
        count="$ctrl.getSessionUsers().length"
        when="{'1': $ctrl.tr('other'),
                 'other': $ctrl.tr('others')}">
    </ng-pluralize></span> <span class="caret"></span>
    </button>
    <ul class="dropdown-menu"
    uib-dropdown-menu
    role="menu"
    aria-labelledby="single-button">
    <li role="menuitem"><a ng-href="/view/{{ $ctrl.getCurrentUser().folder.path }}">{{ 'My documents' | tr }}</a></li>
    <li role="menuitem"><a
            ng-click="$ctrl.addUser()"
            href="#">{{ 'Add a user to this session' | tr }}...</a></li>
    <li class="divider"></li>
    <li ng-if="!$ctrl.loggingout" role="menuitem">
        <a ng-click="$ctrl.beginLogout($event)" href="#">{{ 'Log' | tr:{self: $ctrl.getSessionUsers().length === 0} }} <span
                ng-if="$ctrl.getSessionUsers().length > 0">{{ 'everyone' | tr }}</span>
            {{ 'out' | tr }}<span ng-if="$ctrl.isKorppi()">...</span></a>
    </li>
    <li ng-if="$ctrl.loggingout" role="menuitem">
        <a ng-click="$ctrl.logout($ctrl.getCurrentUser(), true)" href="#">{{ 'Log out' | tr }} (TIM + Korppi)</a>
    </li>
    <li ng-if="$ctrl.loggingout" role="menuitem">
        <a ng-click="$ctrl.logout($ctrl.getCurrentUser(), false)" href="#">{{ 'Log out' | tr }} ({{ 'TIM only' | tr }})</a>
    </li>
    <li role="menuitem" ng-repeat="u in $ctrl.getSessionUsers()">
        <a ng-click="$ctrl.logout(u)" href="#">{{ 'Log' | tr:{self: false} }} {{ u.real_name }} {{ 'out' | tr }}</a>
    </li>
    </ul>
</div>
    `,
});
