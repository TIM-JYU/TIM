import {NgModule} from "@angular/core";
import {provideHttpClient, withInterceptorsFromDi} from "@angular/common/http";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {FormsModule} from "@angular/forms";
import {NotificationOptionsComponent} from "tim/item/manage/notification-options.component";
import {CommonModule} from "@angular/common";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {DatetimePickerModule} from "tim/ui/datetime-picker/datetime-picker.component";
import {RightsEditorComponent} from "tim/item/rights-editor.component";

@NgModule({
    declarations: [NotificationOptionsComponent, RightsEditorComponent],
    exports: [NotificationOptionsComponent, RightsEditorComponent],
    imports: [
        CommonModule,
        TimUtilityModule,
        FormsModule,
        TooltipModule,
        DatetimePickerModule,
    ],
    providers: [provideHttpClient(withInterceptorsFromDi())],
})
export class TimManageModule {}
