import {IController} from "angular";
import {ngStorage} from "ngstorage";
import {timApp} from "tim/app";
import {Lang} from "../ui/language";
import {$localStorage} from "../util/ngimport";
import {showLoginDialog} from "./loginDialog";
import {Users} from "./userService";

/**
 * A component that displays either button for opening login dialog or the user menu component
 * depending on whether the user is logged in.
 */
class LoginMenuController implements IController {
    private storage: ngStorage.StorageService & {language: null | Lang};

    constructor() {
        this.storage = $localStorage.$default({language: null});
    }

    isLoggedIn = () => Users.isLoggedIn();

    openLoginDialog(lang: Lang) {
        void showLoginDialog({showSignup: false, addingToSession: false});
    }
}

timApp.component("loginMenu", {
    bindings: {},
    controller: LoginMenuController,
    template: `
     <div ng-switch="$ctrl.storage.language" ng-cloak>
        <div ng-switch-default>
            <button ng-cloak ng-if="!$ctrl.isLoggedIn()" ng-click="$ctrl.openLoginDialog('en')" type="button"
            class="btn btn-default margin-4">Log in</button>
            <user-menu language="en" ng-cloak ng-if="$ctrl.isLoggedIn()"></user-menu>
         </div>
         <div ng-switch-when="fi">
            <button ng-cloak ng-if="!$ctrl.isLoggedIn()" ng-click="$ctrl.openLoginDialog('fi')" type="button"
            class="btn btn-default margin-4">Kirjaudu</button>
            <user-menu language="fi" ng-cloak ng-if="$ctrl.isLoggedIn()"></user-menu>
         </div>
     </div>
    `,
});
