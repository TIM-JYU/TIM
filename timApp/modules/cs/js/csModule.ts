import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {NgModule} from "@angular/core";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {PurifyModule} from "tim/util/purify.module";
import {GraphVizComponent} from "tim/plugin/graph-viz/graph-viz.component";
import {VariablesComponent} from "tim/plugin/variables/variables.component";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";
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
import {TooltipModule} from "ngx-bootstrap/tooltip";

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
        CommonModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        CsUtilityModule,
        PurifyModule,
        TooltipModule,
    ],
})
export class CsPluginModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("cs-runner", CsPluginModule, CsRunnerComponent);
registerPlugin("cs-error", CsPluginModule, CsErrorComponent);
registerPlugin("cs-text-runner", CsPluginModule, CsTextComponent);
registerPlugin("cs-console", CsPluginModule, CsConsoleComponent);
registerPlugin("cs-extcheck-runner", CsPluginModule, ExtcheckComponent);
registerPlugin("cs-git-reg-runner", CsPluginModule, GitRegComponent);
