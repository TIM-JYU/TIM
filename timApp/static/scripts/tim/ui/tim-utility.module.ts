import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {PluginHeaderComponent} from "tim/ui/plugin-header.component";
import {FocusMeDirective} from "tim/ui/focus-me.directive";
import {TrustHtmlPipe} from "tim/ui/trust-html.pipe";
import {DialogComponent} from "tim/ui/dialog.component";
import {AddMemberComponent} from "tim/ui/add-member.component";
import {FormsModule} from "@angular/forms";
import {TimAlertComponent} from "tim/ui/tim-alert.component";
import {ErrorDescriptionComponent} from "tim/ui/error-description.component";
import {BootstrapPanelComponent} from "tim/ui/bootstrap-panel.component";
import {PluginFrameComponent} from "./plugin-frame.component";
import {LoadingComponent} from "./loadingIndicator";
import {MarkupErrorComponent} from "./markup-error.component";
import {CloseButtonComponent} from "./close-button.component";


// noinspection AngularInvalidImportedOrDeclaredSymbol
@NgModule({
    declarations: [
        MarkupErrorComponent,
        LoadingComponent,
        PluginFrameComponent,
        PluginHeaderComponent,
        FocusMeDirective,
        TrustHtmlPipe,
        DialogComponent,
        CloseButtonComponent,
        AddMemberComponent,
        TimAlertComponent,
        ErrorDescriptionComponent,
        BootstrapPanelComponent,
    ],
    exports: [
        MarkupErrorComponent,
        LoadingComponent,
        PluginFrameComponent,
        PluginHeaderComponent,
        FocusMeDirective,
        TrustHtmlPipe,
        DialogComponent,
        CloseButtonComponent,
        AddMemberComponent,
        TimAlertComponent,
        ErrorDescriptionComponent,
        BootstrapPanelComponent,
    ],
    imports: [
        CommonModule,
        FormsModule,
    ],
})
export class TimUtilityModule {
}
