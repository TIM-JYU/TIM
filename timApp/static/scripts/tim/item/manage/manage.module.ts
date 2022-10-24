import {NgModule} from "@angular/core";
import {HttpClientModule} from "@angular/common/http";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {FormsModule} from "@angular/forms";
import {NotificationOptionsComponent} from "tim/item/manage/notification-options.component";
import {CommonModule} from "@angular/common";

@NgModule({
    declarations: [NotificationOptionsComponent],
    imports: [CommonModule, HttpClientModule, TimUtilityModule, FormsModule],
})
export class TimManageModule {}
