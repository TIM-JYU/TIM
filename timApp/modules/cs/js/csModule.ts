import {
    ApplicationRef,
    DoBootstrap,
    NgModule,
    StaticProvider,
} from "@angular/core";
import {CommonModule} from "@angular/common";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {BrowserModule} from "@angular/platform-browser";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {PurifyModule} from "tim/util/purify.module";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {GraphVizComponent} from "tim/plugin/graph-viz/graph-viz.component";
import {VariablesComponent} from "tim/plugin/variables/variables.component";
import {CsRunnerComponent} from "./csPlugin";
import {CsTextComponent} from "./text";
import {CsConsoleComponent} from "./console";
import {
    CustomOutputDirective,
    ExtcheckComponent,
    OutputContainerComponent,
} from "./extcheck";
import {GitRegComponent} from "./gitreg";
import {CsErrorComponent} from "./language_error";
import {CsUtilityModule} from "./util/module";
import {EditorModule} from "./editor/module";

@NgModule({
    declarations: [
        CsRunnerComponent,
        CsTextComponent,
        CsConsoleComponent,
        ExtcheckComponent,
        OutputContainerComponent,
        CustomOutputDirective,
        GitRegComponent,
        CsErrorComponent,
        GraphVizComponent,
        VariablesComponent,
    ],
    exports: [
        CsRunnerComponent,
        CsTextComponent,
        CsConsoleComponent,
        ExtcheckComponent,
        GitRegComponent,
        CsErrorComponent,
    ],
    imports: [
        EditorModule,
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        CsUtilityModule,
        CommonModule,
        PurifyModule,
    ],
})
export class CsPluginModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

const bootstrapFn = (extraProviders: StaticProvider[]) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(CsPluginModule);
};

export const angularJsModule = createDowngradedModule(bootstrapFn);
doDowngrade(angularJsModule, "csRunner", CsRunnerComponent);
doDowngrade(angularJsModule, "csError", CsErrorComponent);
doDowngrade(angularJsModule, "csTextRunner", CsTextComponent);
doDowngrade(angularJsModule, "csConsole", CsConsoleComponent);
doDowngrade(angularJsModule, "csExtcheckRunner", ExtcheckComponent);
doDowngrade(angularJsModule, "csGitRegRunner", GitRegComponent);
export const moduleDefs = [angularJsModule];
