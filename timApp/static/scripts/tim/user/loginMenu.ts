import {IController} from "angular";
import {timApp} from "tim/app";
import {showLoginDialog} from "./loginDialog";
import {Users} from "./userService";

/**
 * A component that displays either button for opening login dialog or the user menu component
 * depending on whether the user is logged in.
 */
class LoginMenuController implements IController {

    isLoggedIn() {
        return Users.isLoggedIn();
    }

    openLoginDialog() {
        void showLoginDialog({showSignup: false, addingToSession: false});
    }
}

timApp.component("loginMenu", {
    bindings: {},
    controller: LoginMenuController,
    template: `
<button ng-if="!$ctrl.isLoggedIn()"
        ng-click="$ctrl.openLoginDialog()"
        type="button"
        class="btn btn-default margin-4">
    {{ 'Log in' | tr }}
</button>
<user-menu ng-if="$ctrl.isLoggedIn()"></user-menu>
    `,
});
