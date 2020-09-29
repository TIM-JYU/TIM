import {timApp} from "../app";

class ConsentChoiceController {
    private txt?: string;

    getAllowanceText(lang: string, allow: boolean) {
        if (lang === "fi") {
            if (allow) {
                return "Sallin";
            } else {
                return "En salli";
            }
        } else {
            if (allow) {
                return "I allow";
            } else {
                return "I do not allow";
            }
        }
    }

    getText(lang: string, allow: boolean) {
        return lang === "fi"
            ? `${this.getAllowanceText(lang, allow)} käyttää <a
        href="/view/tim/Rekisteriseloste#rekisterin-tietosis%C3%A4lt%C3%B6">
    kerättyä tietoa</a> tieteelliseen tutkimustarkoitukseen anonymisoinnin jälkeen.`
            : `${this.getAllowanceText(lang, allow)} <a
        href="/view/tim/Rekisteriseloste#rekisterin-tietosis%C3%A4lt%C3%B6">
    the collected data</a> to be used for scientific research purposes after anonymization.`;
    }
}

timApp.component("timConsentChoice", {
    bindings: {
        consent: "=",
    },
    controller: ConsentChoiceController,
    template: `<div class="radio">
    <label>
        <input ng-model="$ctrl.consent"
               ng-value="2"
               type="radio"> <span ng-bind-html="$ctrl.getText('en', true)"></span><br>
        <span ng-bind-html="$ctrl.getText('fi', true)"></span>
    </label>
    <label style="font-size: small">
        <input ng-model="$ctrl.consent"
               ng-value="1"
               type="radio"> <span ng-bind-html="$ctrl.getText('en', false)"></span><br>
        <span ng-bind-html="$ctrl.getText('fi', false)"></span>
    </label>
</div>
    `,
});
