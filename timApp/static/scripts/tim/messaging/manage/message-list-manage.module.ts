import {ApplicationRef, DoBootstrap, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {TimUtilityModule} from "../../ui/tim-utility.module";
import {TableFormModule} from "../../plugin/tableForm";
import {createDowngradedModule, doDowngrade} from "../../downgrade";
import {MessageListAdminComponent} from "./message-list-admin.component";

@NgModule({
    declarations: [MessageListAdminComponent],
    exports: [MessageListAdminComponent],
    imports: [CommonModule, FormsModule, TimUtilityModule, TableFormModule],
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
