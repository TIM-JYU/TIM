import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {NgModule} from "@angular/core";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {HttpClientModule} from "@angular/common/http";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {TimArchiveHeaderComponent} from "tim/messaging/archive/tim-archive-header.component";
import {TimArchiveFooterComponent} from "tim/messaging/archive/tim-archive-footer.component";
import {ArchivedMessageStateService} from "tim/messaging/archive/archived-message-state.service";
import {BrowserModule} from "@angular/platform-browser";

@NgModule({
    declarations: [TimArchiveHeaderComponent, TimArchiveFooterComponent],
    imports: [
        BrowserModule,
        HttpClientModule,
        TooltipModule.forRoot(),
        TimUtilityModule,
    ],
    providers: [ArchivedMessageStateService],
})
export class TimArchiveModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

export const angularJsModule = createDowngradedModule((extraProviders) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(TimArchiveModule);
});
doDowngrade(angularJsModule, "timArchiveHeader", TimArchiveHeaderComponent);
doDowngrade(angularJsModule, "timArchiveFooter", TimArchiveFooterComponent);
export const moduleDefs = [angularJsModule];
