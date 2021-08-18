import {ApplicationRef, DoBootstrap, NgModule} from "@angular/core";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClientModule} from "@angular/common/http";
import {createDowngradedModule, doDowngrade} from "../../downgrade";
import {TimArchiveHeaderComponent} from "./tim-archive-header.component";
import {TimArchiveFooterComponent} from "./tim-archive-footer.component";
import {ArchivedMessageStateService} from "./archived-message-state.service";

@NgModule({
    declarations: [TimArchiveHeaderComponent, TimArchiveFooterComponent],
    imports: [BrowserModule, HttpClientModule],
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
