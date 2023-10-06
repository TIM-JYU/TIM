import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {TabsModule} from "ngx-bootstrap/tabs";
import {HttpClientModule} from "@angular/common/http";
import {BrowserModule} from "@angular/platform-browser";
import {GroupManagementComponent} from "tim/ui/group-management.component";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
// import {TimTableModule} from "tim/plugin/timTable/tim-table.component";

@NgModule({
    declarations: [GroupManagementComponent],
    exports: [GroupManagementComponent],
    imports: [
        HttpClientModule,
        BrowserModule,
        FormsModule,
        TimUtilityModule,
        // TimTableModule,
        TabsModule.forRoot(),
    ],
})
export class GroupManagementModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

export const angularJsModule = createDowngradedModule((extraProviders) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(GroupManagementModule);
});
doDowngrade(
    angularJsModule,
    "timGroupManagementConsole",
    GroupManagementComponent
);
export const moduleDefs = [angularJsModule];
