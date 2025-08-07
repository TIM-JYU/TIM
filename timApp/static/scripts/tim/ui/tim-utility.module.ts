import {NgModule} from "@angular/core";
import {PluginHeaderComponent} from "tim/ui/plugin-header.component";
import {FocusMeDirective} from "tim/ui/focus-me.directive";
import {DialogComponent} from "tim/ui/dialog.component";
import {AddMemberComponent} from "tim/ui/add-member.component";
import {FormsModule, ReactiveFormsModule} from "@angular/forms";
import {TimAlertComponent} from "tim/ui/tim-alert.component";
import {ErrorDescriptionComponent} from "tim/ui/error-description.component";
import {
    BootstrapFormPanelComponent,
    BootstrapPanelComponent,
} from "tim/ui/bootstrap-panel.component";
import {TimeLeftComponent} from "tim/ui/time-left.component";
import {GotoLinkComponent} from "tim/ui/goto-link.component";
import {CountdownComponent} from "tim/ui/countdown.component";
import {DurationPickerComponent} from "tim/ui/duration-picker.component";
import {ErrorMessageComponent} from "tim/ui/error-message.component";
import {RelativeTimestampPipe} from "tim/ui/relative-timestamp.pipe";
import {DatePipe} from "tim/ui/date.pipe";
import {TimePipe} from "tim/ui/time.pipe";
import {CapitalizePipe} from "tim/ui/capitalize.pipe";
import {LanguageSelectorComponent} from "tim/user/language-selector.component";
import {BookmarkNamePipe} from "tim/bookmark/bookmark-name.pipe";
import {MarkAllAsReadComponent} from "tim/ui/mark-all-as-read.component";
import {SwitchButtonComponent} from "tim/sidebarmenu/util/switch-button.component";
import {SessionVerify} from "tim/util/session-verify.interceptor";
import {PluginFrameComponent} from "tim/ui/plugin-frame.component";
import {LoadingComponent} from "tim/ui/loading.component";
import {MarkupErrorComponent} from "tim/ui/markup-error.component";
import {CloseButtonComponent} from "tim/ui/close-button.component";
import {CommonModule} from "@angular/common";
import {PrintButtonComponent} from "tim/ui/print-button.component";
import {CreateItemComponent} from "tim/item/create-item.component";
import {PurifyModule} from "tim/util/purify.module";
import {InputAutofocusDirective} from "tim/item/input-autofocus.directive";

@NgModule({
    providers: [SessionVerify],
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
        LanguageSelectorComponent,
        BootstrapPanelComponent,
        BootstrapFormPanelComponent,
        GotoLinkComponent,
        CountdownComponent,
        TimeLeftComponent,
        DurationPickerComponent,
        ErrorMessageComponent,
        DatePipe,
        RelativeTimestampPipe,
        TimePipe,
        CapitalizePipe,
        BookmarkNamePipe,
        MarkAllAsReadComponent,
        SwitchButtonComponent,
        PrintButtonComponent,
        CreateItemComponent,
        InputAutofocusDirective,
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
        BootstrapFormPanelComponent,
        GotoLinkComponent,
        LanguageSelectorComponent,
        CountdownComponent,
        TimeLeftComponent,
        DurationPickerComponent,
        ErrorMessageComponent,
        DatePipe,
        RelativeTimestampPipe,
        TimePipe,
        CapitalizePipe,
        BookmarkNamePipe,
        SwitchButtonComponent,
        CreateItemComponent,
        InputAutofocusDirective,
    ],
    imports: [CommonModule, FormsModule, PurifyModule, ReactiveFormsModule],
})
export class TimUtilityModule {}
