import {IController} from "angular";
import {timApp} from "tim/app";
import {showLoginDialog} from "./loginDialog";
import {Users} from "./userService";
import {showUserMenuDialog} from "./userMenuDialog";

class LoginMenuController implements IController {
    private username: string | undefined;

    constructor() {
    }

    isLoggedIn = () => Users.isLoggedIn();

    $onInit(): void {
        this.username = Users.getCurrent().real_name;
    }

    openLoginDialog() {
        // TODO: Allow only one dialog open at a time.
        void showLoginDialog(false);
    }

    openUserMenuDialog() {
        void showUserMenuDialog();
    }
}

timApp.component("loginMenu", {
    bindings: {},
    controller: LoginMenuController,
    template: `
     <button ng-cloak ng-if="!$ctrl.isLoggedIn()" ng-click="$ctrl.openLoginDialog()" type="button"
        class="timButton margin-4">Log in</button>
     <button ng-cloak ng-if="$ctrl.isLoggedIn()" ng-click="$ctrl.openUserMenuDialog()" type="button"
        class="timButton margin-4">{{::$ctrl.username}}</button>
    `,
});
