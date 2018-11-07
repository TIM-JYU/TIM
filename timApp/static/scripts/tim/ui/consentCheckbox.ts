import {timApp} from "../app";

timApp.component("timConsentCheckbox", {
    bindings: {
        consent: "=",
        lang: "<",
    },
    controller: class {
        private txt?: string;
        private lang = "en";

        $onInit() {
            this.txt = this.lang === "fi" ? `Sallin käyttää <a
        href="/view/tim/Rekisteriseloste#rekisterin-tietosis%C3%A4lt%C3%B6">
    kerättyä tietoa</a> tieteelliseen tutkimustarkoitukseen anonymisoinnin jälkeen.` : `I allow <a
        href="/view/tim/Rekisteriseloste#rekisterin-tietosis%C3%A4lt%C3%B6">
    the collected data</a> to be used for scientific research purposes after anonymization.`;
        }
    },
    template: `<div class="checkbox">
    <label>
        <input ng-model="$ctrl.consent"
               ng-true-value="2"
               ng-false-value="1"
               type="checkbox"> <span ng-bind-html="$ctrl.txt"></span>
    </label>
</div>
    `,
});
