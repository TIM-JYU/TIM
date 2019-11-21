/**
 * Controller and HTML template for consent dialog.
 */

import {IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import {markAsUsed} from "../util/utils";
import * as ccheckbox from "./consentChoice";
import {DialogController, registerDialogComponent, showDialog} from "./dialog";

markAsUsed(focusMe, ccheckbox);

export enum ConsentType {
    CookieOnly = 1,
    CookieAndData = 2,
}

export class ConsentController extends DialogController<{params: {showDataCollectionOptions: boolean}}, ConsentType | null> {
    static $inject = ["$element", "$scope"] as const;
    static component = "timConsent" as const;
    private consent: ConsentType | null = null;

    constructor(protected element: JQLite, protected scope: IScope) {
        super(element, scope);
    }

    public getTitle() {
        return "Consent / suostumus";
    }

    public accept() {
        this.close(this.consent);
    }

    showDataCollection() {
        return this.resolve.params.showDataCollectionOptions;
    }

    isIncomplete() {
        return this.showDataCollection() && this.consent == null;
    }
}

registerDialogComponent(ConsentController,
    {
        template:
            `<tim-dialog>
    <dialog-header>
    </dialog-header>
    <dialog-body>
        This site uses cookies to personalize experience and analyze traffic. By using the site you accept the
        <a href="/view/tim/Rekisteriseloste">privacy policy</a>.

        <hr>

        Tämä sivusto käyttää evästeitä käyttäjäkokemuksen personointiin ja liikenteen analysointiin. Käyttämällä
        sivustoa hyväksyt <a href="/view/tim/Rekisteriseloste">tietosuojalausekkeen</a>.
        <div ng-if="$ctrl.showDataCollection()">
            <hr>
            <tim-consent-choice consent="$ctrl.consent">

            </tim-consent-choice>
        </div>
    </dialog-body>
    <dialog-footer>
        <button ng-disabled="$ctrl.isIncomplete()" class="timButton" ng-click="$ctrl.accept()">Accept and close / Hyväksy ja sulje</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showConsentDialog(showDataCollectionOptions: boolean) {
    return await showDialog(
        ConsentController, {params: () => ({showDataCollectionOptions})}).result;
}
