import {getURLParameter} from "tim/util/utils";
import {inIframe} from "tim/plugin/util";
import {$httpParamSerializer} from "tim/util/ngimport";
import {Component, OnInit} from "@angular/core";
import {showLoginDialog} from "./login-dialog.component";
import {Users} from "./userService";

/**
 * Displays either a button for opening the login dialog or the user menu component
 * depending on whether the user is logged in.
 */
@Component({
    selector: "tim-login-menu",
    template: `
<button *ngIf="!isLoggedIn()"
        (click)="openLoginDialog()"
        type="button"
        class="btn btn-default" i18n="@@logIn">Log in</button>
<tim-user-menu *ngIf="isLoggedIn()"></tim-user-menu>
    `,
})
export class LoginMenuComponent implements OnInit {
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
            const entities: Record<string, string> = {
                test: "https://testidp.funet.fi/idp/shibboleth",
                aalto: "https://idp.aalto.fi/idp/shibboleth",
            };
            window.open(
                "/saml/sso?" +
                    $httpParamSerializer({
                        addUser: false,
                        entityID:
                            entities[hakaLogin] ??
                            "https://login.jyu.fi/idp/shibboleth",
                        return_to:
                            document.location.origin +
                            "?" +
                            $httpParamSerializer({
                                sendLoginSuccessMsg: true,
                            }),
                    })
            );
            return;
        }
        void showLoginDialog({showSignup: false, addingToSession: false});
    }

    ngOnInit() {
        const opener = window.opener as Window | null;
        if (getURLParameter("sendLoginSuccessMsg") && opener) {
            opener.postMessage("loginOk", "*");
            window.close();
        }
    }
}
