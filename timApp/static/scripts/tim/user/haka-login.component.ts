import {Component, Input} from "@angular/core";
import {Users} from "tim/user/userService";
import {$http, $httpParamSerializer} from "../util/ngimport";
import {getStorage, setStorage, to} from "../util/utils";

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

@Component({
    selector: "tim-haka-login",
    template: `
        <div style="background:#F0F0F0;border-style: solid;border-color: #848484;border-width: 1px;padding: 10px;">
            <a href="https://wiki.eduuni.fi/display/CSCHAKA" target="_blank" class="pull-right">
                <img src="https://haka.funet.fi/shibboleth/images/Haka_nega_tiivis_pieni.svg"
                     alt="Federation logo"
                     i18n-alt>
            </a>
            <label>
                <ng-container i18n>Haka login</ng-container>
                <select class="form-control"
                        style="margin-top: 6px;"
                        [(ngModel)]="selectedIdp">
                    <option [ngValue]="undefined" disabled i18n>Select your home organization...</option>
                    <option *ngFor="let idp of idps" [ngValue]="idp">{{getName(idp)}}</option>
                </select>
            </label>
            <button [disabled]="!selectedIdp"
                    type="button"
                    (click)="login()"
                    class="timButton" i18n>
                Log in
            </button>
        </div>
    `,
})
export class HakaLoginComponent {
    selectedIdp?: IDiscoveryFeedEntry;
    @Input() idps: IDiscoveryFeedEntry[] = [];
    @Input() homeOrg?: string;
    @Input() addingUser?: boolean;

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
        const found = idp.displayNames.find((dn) => dn.lang == Users.getCurrentLanguage());
        if (found) {
            return found.value;
        }
    }

    login() {
        if (!this.selectedIdp) {
            return;
        }
        setStorage("lastIdp", this.selectedIdp.entityID);

        ssoLogin(this.selectedIdp.entityID, this.addingUser);
    }
}
