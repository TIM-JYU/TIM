import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {Component, Input, NgModule} from "@angular/core";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import type {IOAuthGlobals} from "tim/util/globals";
import {oauthglobals} from "tim/util/globals";
import {getCookie} from "tim/util/utils";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {Users} from "tim/user/userService";
import {BrowserModule} from "@angular/platform-browser";

const OAUTH_SCOPES: Record<string, string> = {
    profile: $localize`Read basic profile information (username, full name, email address)`,
    user_tasks: $localize`Access your TIM-tasks`,
    submit_answers: $localize`Submit answers to TIM-tasks`,
    user_courses: $localize`Access your TIM-courses`,
};

@Component({
    selector: "tim-oauth-button",
    template: `
        <form action="" method="post">
            <input type="hidden" name="confirm" value="{{confirm}}">
            <input type="hidden" name="csrf_token" value="{{csrfToken}}">
            <input class="btn btn-{{type}}" type="submit" [value]="name">
        </form>
    `,
})
export class OAuthButtonComponent {
    @Input() confirm: boolean = false;
    @Input() type = "default";
    @Input() name = "";
    csrfToken: string = getCookie("XSRF-TOKEN") ?? "";
}

@Component({
    selector: "tim-oauth-authorize",
    host: {class: "flex justify-center"},
    template: `
        <div>
            <h1 i18n>Authenticate to {{data.oauthClientName}} with TIM</h1>
            <p i18n>You are attempting to authenticate to <strong>{{data.oauthClientName}}</strong> with TIM.</p>
            <p i18n><strong>{{data.oauthClientName}}</strong> will be able to</p>
            <ul>
                <li *ngFor="let scopeName of data.oauthScopes">{{scopeDescriptions[scopeName]}}</li>
            </ul>
            <ng-container *ngIf="loggedIn; else notLoggedIn">
                <p i18n>Do you want to proceed?</p>
                <div class="flex space-between pt-1">
                    <tim-oauth-button [confirm]="false" type="danger" name="Cancel" i18n-name></tim-oauth-button>
                    <tim-oauth-button [confirm]="true" type="default" name="Authenticate to {{data.oauthClientName}}"
                                      i18n-name></tim-oauth-button>
                </div>
            </ng-container>
            <ng-template #notLoggedIn>
                <strong i18n>Log in with the button above in order to authenticate</strong>
            </ng-template>
        </div>
    `,
})
export class OAuthAuthorizeComponent {
    data: IOAuthGlobals = oauthglobals();
    scopeDescriptions = OAUTH_SCOPES;
    loggedIn = Users.isRealUser();
}

@NgModule({
    declarations: [OAuthAuthorizeComponent, OAuthButtonComponent],
    exports: [OAuthAuthorizeComponent],
    imports: [BrowserModule, TimUtilityModule],
})
export class OAuthAuthorizeModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

export const angularJsModule = createDowngradedModule((extraProviders) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(OAuthAuthorizeModule);
});
doDowngrade(angularJsModule, "timOauthAuthorize", OAuthAuthorizeComponent);
export const moduleDefs = [angularJsModule];
