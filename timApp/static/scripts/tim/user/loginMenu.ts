import {IController} from "angular";
import {timApp} from "tim/app";
import {getURLParameter} from "tim/util/utils";
import {inIframe} from "tim/plugin/util";
import {$httpParamSerializer} from "tim/util/ngimport";
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
        const hakaLogin = getURLParameter("hakalogin");

        // Haka does not allow login inside iframe, so we have to open a top-level window for that and after login
        // send a message to the iframe that the login was successful.
        if (hakaLogin && inIframe()) {
            window.addEventListener("message", (e) => {
                if (e.data === "loginOk") {
                    location.reload();
                }
            });
            window.open("/saml/sso?" + $httpParamSerializer({
                addUser: false,
                entityID: hakaLogin === "aalto" ? "https://idp.aalto.fi/idp/shibboleth" : "https://login.jyu.fi/idp/shibboleth",
                return_to: document.location.toString() + "?" + $httpParamSerializer({
                    sendLoginSuccessMsg: true,
                }),
            }));
            return;
        }
        void showLoginDialog({showSignup: false, addingToSession: false});
    }

    $onInit() {
        const opener = window.opener as Window | null;
        if (getURLParameter("sendLoginSuccessMsg") && opener) {
            opener.postMessage("loginOk", "*");
            window.close();
        }
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
