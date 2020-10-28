import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {PluginHeaderComponent} from "tim/ui/plugin-header.component";
import {FocusMeDirective} from "tim/ui/focus-me.directive";
import {DialogComponent} from "tim/ui/dialog.component";
import {AddMemberComponent} from "tim/ui/add-member.component";
import {FormsModule} from "@angular/forms";
import {TimAlertComponent} from "tim/ui/tim-alert.component";
import {ErrorDescriptionComponent} from "tim/ui/error-description.component";
import {BootstrapPanelComponent} from "tim/ui/bootstrap-panel.component";
import {SaveButtonComponent} from "tim/user/settings.component";
import {TimeLeftComponent} from "tim/ui/time-left.component";
import {GotoLinkComponent} from "tim/ui/goto-link.component";
import {CountdownComponent} from "tim/ui/countdown.component";
import {DurationPickerComponent} from "tim/ui/duration-picker.component";
import {ErrorMessageComponent} from "tim/ui/error-message.component";
import {RelativeTimestampPipe} from "tim/ui/relative-timestamp.pipe";
import {DatePipe} from "tim/ui/date.pipe";
import {TimePipe} from "tim/ui/time.pipe";
import {CapitalizePipe} from "tim/ui/capitalize.pipe";
import {PluginFrameComponent} from "./plugin-frame.component";
import {LoadingComponent} from "./loading.component";
import {MarkupErrorComponent} from "./markup-error.component";
import {CloseButtonComponent} from "./close-button.component";

@NgModule({
    declarations: [
        MarkupErrorComponent,
        LoadingComponent,
        PluginFrameComponent,
        PluginHeaderComponent,
        FocusMeDirective,
        DialogComponent,
        CloseButtonComponent,
        AddMemberComponent,
        TimAlertComponent,
        ErrorDescriptionComponent,
        BootstrapPanelComponent,
        SaveButtonComponent,
        GotoLinkComponent,
        CountdownComponent,
        TimeLeftComponent,
        DurationPickerComponent,
        ErrorMessageComponent,
        DatePipe,
        RelativeTimestampPipe,
        TimePipe,
        CapitalizePipe,
    ],
    exports: [
        MarkupErrorComponent,
        LoadingComponent,
        PluginFrameComponent,
        PluginHeaderComponent,
        FocusMeDirective,
        DialogComponent,
        CloseButtonComponent,
        AddMemberComponent,
        TimAlertComponent,
        ErrorDescriptionComponent,
        BootstrapPanelComponent,
        SaveButtonComponent,
        GotoLinkComponent,
        CountdownComponent,
        TimeLeftComponent,
        DurationPickerComponent,
        ErrorMessageComponent,
        DatePipe,
        RelativeTimestampPipe,
        TimePipe,
        CapitalizePipe,
    ],
    imports: [CommonModule, FormsModule],
})
export class TimUtilityModule {}
