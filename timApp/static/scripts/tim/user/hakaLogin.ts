import {timApp} from "../app";
import {language} from "../ui/language";
import {$http, $httpParamSerializer} from "../util/ngimport";
import {getStorage, setStorage, to} from "../util/utils";
import {Users} from "./userService";

function redirectTo(url: string) {
    window.location.href = url;
}

export async function loadIdPs() {
    const result = await to($http.get<IDiscoveryFeedEntry[]>("/saml/feed"));
    if (result.ok) {
        return result.result.data;
    } else {
        return [];
    }
}

interface IDisplayName {
    value: string;
    lang: string;
}

export interface IDiscoveryFeedEntry {
    entityID: string;
    displayNames: IDisplayName[];
    scopes: string[];
}

export function ssoLogin(entityID: string, addUser?: boolean) {
    redirectTo("/saml/sso?" + $httpParamSerializer({
        entityID: entityID,
        return_to: document.location.toString(),
        addUser: addUser,
    }));
}

export function findIdPByScope(idps: IDiscoveryFeedEntry[], tmp: string) {
    return idps.find((idp) => idp.scopes.includes(tmp));
}

class HakaLoginController {
    selectedIdp?: IDiscoveryFeedEntry;
    idps: IDiscoveryFeedEntry[] = [];
    homeOrg?: string;
    addingUser?: boolean;
    alwaysKorppi?: boolean;

    async $onInit() {
        if (!this.idps) {
            this.idps = await loadIdPs();
        }
        const last = getStorage("lastIdp");
        if (typeof last === "string") {
            const found = this.idps.find((idp) => idp.entityID === last);
            if (found) {
                this.selectedIdp = found;
            }
        }
        if (!this.selectedIdp && this.homeOrg) {
            this.selectedIdp = findIdPByScope(this.idps, this.homeOrg);
        }
    }

    getName(idp: IDiscoveryFeedEntry) {
        const found = idp.displayNames.find((dn) => dn.lang == language.lang);
        if (found) {
            return found.value;
        }
    }

    login() {
        if (!this.selectedIdp) {
            return;
        }
        setStorage("lastIdp", this.selectedIdp.entityID);

        if (this.alwaysKorppi && this.selectedIdp.scopes.includes("jyu.fi")) {
            Users.korppiLogin(this.addingUser || false);
        } else {
            ssoLogin(this.selectedIdp.entityID, this.addingUser);
        }
    }
}

timApp.component("hakaLogin", {
    controller: HakaLoginController,
    bindings: {
        idps: "<?",
        homeOrg: "<?",
        addingUser: "<?",
        alwaysKorppi: "<?",
    },
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
            <option value="" disabled>{{ 'Select your home organization...' | tr }}</option>
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
