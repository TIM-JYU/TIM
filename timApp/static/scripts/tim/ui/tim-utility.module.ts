import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {PluginHeaderComponent} from "tim/ui/plugin-header.component";
import {FocusMeDirective} from "tim/ui/focus-me.directive";
import {TrustHtmlPipe} from "tim/ui/trust-html.pipe";
import {SpellErrorComponent} from "tim/document/editing/spell-error.component";
import {DialogComponent} from "tim/ui/dialog.component";
import {PluginFrameComponent} from "./plugin-frame.component";
import {LoadingComponent} from "./loadingIndicator";
import {MarkupErrorComponent} from "./markup-error.component";

// noinspection AngularInvalidImportedOrDeclaredSymbol
@NgModule({
    declarations: [
        MarkupErrorComponent,
        LoadingComponent,
        PluginFrameComponent,
        PluginHeaderComponent,
        FocusMeDirective,
        TrustHtmlPipe,
        SpellErrorComponent,
        DialogComponent,
    ],
    exports: [
        MarkupErrorComponent,
        LoadingComponent,
        PluginFrameComponent,
        PluginHeaderComponent,
        FocusMeDirective,
        TrustHtmlPipe,
        SpellErrorComponent,
        DialogComponent,
    ],
    imports: [CommonModule],
})
export class TimUtilityModule {
}
