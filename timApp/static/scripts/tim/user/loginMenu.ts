import {IController} from "angular";
import {timApp} from "tim/app";
import {showLoginDialog} from "./loginDialog";
import {Users} from "./userService";

class LoginMenuController implements IController {

    isLoggedIn = () => Users.isLoggedIn();

    openLoginDialog() {
        void showLoginDialog(false);
    }
}

timApp.component("loginMenu", {
    bindings: {},
    controller: LoginMenuController,
    template: `
     <button ng-cloak ng-if="!$ctrl.isLoggedIn()" ng-click="$ctrl.openLoginDialog()" type="button"
        class="btn btn-default margin-4">Log in</button>
     <user-menu ng-cloak ng-if="$ctrl.isLoggedIn()"></user-menu>
    `,
});
