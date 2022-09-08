import {ApplicationRef, DoBootstrap, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {BrowserModule} from "@angular/platform-browser";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {PurifyModule} from "tim/util/purify.module";
import {GraphVizComponent} from "tim/plugin/graph-viz/graph-viz.component";
import {VariablesComponent} from "tim/plugin/variables/variables.component";
import {pluginMap} from "../../../static/scripts/tim/main";
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

pluginMap.set("cs-runner", CsRunnerComponent);
pluginMap.set("cs-error", CsErrorComponent);
pluginMap.set("cs-text-runner", CsTextComponent);
pluginMap.set("cs-console", CsConsoleComponent);
pluginMap.set("cs-extcheck-runner", ExtcheckComponent);
pluginMap.set("cs-git-reg-runner", GitRegComponent);
