import {
        DoBootstrap,
        NgModule,
        StaticProvider,
        ApplicationRef,
    } from "@angular/core"
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {BrowserModule} from "@angular/platform-browser";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {EditorModule} from "./editor/module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {CsRunnerComponent, CsTextComponent, CsConsoleComponent} from "./csPlugin";

@NgModule({
    declarations: [
        CsRunnerComponent,
        CsTextComponent,
        CsConsoleComponent,
    ],
    exports: [
        CsRunnerComponent,
        CsTextComponent,
        CsConsoleComponent,
    ],
    imports: [
        EditorModule,
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
    ],
})
export class CsPluginModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {
    }
}

const bootstrapFn = (extraProviders: StaticProvider[]) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(CsPluginModule);
};

export const angularJsModule = createDowngradedModule(bootstrapFn);
doDowngrade(angularJsModule, "csRunner", CsRunnerComponent);
doDowngrade(angularJsModule, "csTextRunner", CsTextComponent);
doDowngrade(angularJsModule, "csConsole", CsConsoleComponent);
export const moduleDefs = [angularJsModule];
