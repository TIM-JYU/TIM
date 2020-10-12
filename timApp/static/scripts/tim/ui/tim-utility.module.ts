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
import {
    RelativeTimestampPipe,
    TimDatePipe,
} from "tim/ui/relative-timestamp.pipe";
import {DatepickerModule} from "ngx-bootstrap/datepicker";
import {TimepickerModule} from "ngx-bootstrap/timepicker";
import {DatetimePopupModule} from "ngx-bootstrap-datetime-popup";
import {PluginFrameComponent} from "./plugin-frame.component";
import {LoadingComponent} from "./loadingIndicator";
import {MarkupErrorComponent} from "./markup-error.component";
import {CloseButtonComponent} from "./close-button.component";
import {DatetimePickerComponent} from "./datetime-picker/datetime-picker.component";

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
        TimDatePipe,
        RelativeTimestampPipe,
        DatetimePickerComponent,
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
        TimDatePipe,
        RelativeTimestampPipe,
        DatetimePickerComponent,
    ],
    imports: [
        CommonModule,
        FormsModule,
        DatepickerModule.forRoot(),
        TimepickerModule.forRoot(),
        DatetimePopupModule,
    ],
})
export class TimUtilityModule {}
