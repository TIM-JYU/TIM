import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {TabsModule} from "ngx-bootstrap/tabs";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {TimMessageSendModule} from "tim/messaging/tim-message-send.component";
import {MessageListAdminComponent} from "tim/messaging/manage/message-list-admin.component";
import {TimTableModule} from "tim/plugin/timTable/tim-table.component";
import {BrowserModule} from "@angular/platform-browser";
import {NoopAnimationsModule} from "@angular/platform-browser/animations";

@NgModule({
    declarations: [MessageListAdminComponent],
    exports: [MessageListAdminComponent],
    imports: [
        BrowserModule,
        FormsModule,
        NoopAnimationsModule,
        TimUtilityModule,
        TimMessageSendModule,
        TimTableModule,
        TabsModule.forRoot(),
    ],
})
export class MessageListManageModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

export const angularJsModule = createDowngradedModule((extraProviders) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(MessageListManageModule);
});
doDowngrade(angularJsModule, "timMessageListAdmin", MessageListAdminComponent);
export const moduleDefs = [angularJsModule];
