import {timApp} from "../app";
import {language} from "../ui/language";
import {$http, $httpParamSerializer} from "../util/ngimport";
import {getStorage, setStorage, to} from "../util/utils";

function redirectTo(url: string) {
    window.location.href = url;
}

async function loadIdPs() {
    const result = await to($http.get<IDiscoveryFeedEntry[]>("/saml/feed"));
    if (result.ok) {
        return result.result.data;
    } else {
        return [];
    }
}

interface IDiscoveryFeedEntry {
    entityID: string;
    DisplayNames: Array<{value: string, lang: string}>;
    Keywords?: Array<{value: string}>;
    Logos?: Array<{value: string, height: number, width: number}>;
}

class HakaLoginController {
    selectedIdp?: IDiscoveryFeedEntry;
    idps: IDiscoveryFeedEntry[] = [];

    async $onInit() {
        this.idps = await loadIdPs();
        const last = getStorage("lastIdp");
        if (typeof last === "string") {
            const found = this.idps.find((idp) => idp.entityID === last);
            if (found) {
                this.selectedIdp = found;
            }
        }
    }

    getName(idp: IDiscoveryFeedEntry) {
        const found = idp.DisplayNames.find((dn) => dn.lang == language.lang);
        if (found) {
            return found.value;
        }
    }

    login() {
        if (!this.selectedIdp) {
            return;
        }
        setStorage("lastIdp", this.selectedIdp.entityID);
        redirectTo("/saml/sso?" + $httpParamSerializer({
            entityID: this.selectedIdp.entityID,
            return_to: document.location.toString(),
        }));
    }
}

timApp.component("hakaLogin", {
    controller: HakaLoginController,
    template: `
<div style="background:#F0F0F0;border-style: solid;border-color: #848484;border-width: 1px;padding: 10px;">
    <a href="https://wiki.eduuni.fi/display/CSCHAKA" target="_blank" class="pull-right">
        <img src="https://haka.funet.fi/shibboleth/images/Haka_nega_tiivis_pieni.svg" alt="Federation Logo">
    </a>
    <label>{{ 'Haka login' | tr }}
        <select class="form-control"
                style="margin-top: 6px;"
                ng-model="$ctrl.selectedIdp"
                ng-options="idp as $ctrl.getName(idp) for idp in $ctrl.idps | orderBy:$ctrl.getName">
            <option value="" disabled>{{ 'Select your home organisation...' | tr }}</option>
        </select></label>
    <button ng-disabled="!$ctrl.selectedIdp"
            type="button"
            ng-click="$ctrl.login()"
            class="timButton">
        {{ 'Log in' | tr }}
    </button>
</div>
    `,
});
