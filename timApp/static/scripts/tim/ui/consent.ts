/**
 * Controller and HTML template for consent dialog.
 */

import {IRootElementService, IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import {markAsUsed} from "../util/utils";
import * as ccheckbox from "./consentCheckbox";
import {DialogController, registerDialogComponent, showDialog} from "./dialog";

markAsUsed(focusMe, ccheckbox);

export enum ConsentType {
    CookieOnly = 1,
    CookieAndData = 2,
}

export class ConsentController extends DialogController<{}, ConsentType, "timConsent"> {
    private static $inject = ["$element", "$scope"];

    private consent: ConsentType = ConsentType.CookieAndData;

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    public getTitle() {
        return "Consent";
    }

    public accept() {
        this.close(this.consent);
    }
}

registerDialogComponent("timConsent",
    ConsentController,
    {
        template:
            `<tim-dialog>
    <dialog-header>
    </dialog-header>
    <dialog-body>
        This site uses cookies to personalize experience and analyze traffic. By using the site you accept the
        <a href="/view/tim/Rekisteriseloste">privacy policy</a>.

        <tim-consent-checkbox lang="'en'" consent="$ctrl.consent"></tim-consent-checkbox>
        <hr>
        Tämä sivusto käyttää evästeitä käyttäjäkokemuksen personointiin ja liikenteen analysointiin. Käyttämällä
        sivustoa hyväksyt <a href="/view/tim/Rekisteriseloste">tietosuojalausekkeen</a>.

        <tim-consent-checkbox lang="'fi'" consent="$ctrl.consent"></tim-consent-checkbox>
    </dialog-body>
    <dialog-footer>
        <button class="timButton" ng-click="$ctrl.accept()">Accept and close</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showConsentDialog() {
    return await showDialog<ConsentController>("timConsent", {}).result;
}
