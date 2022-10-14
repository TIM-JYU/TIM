import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {HttpClientModule} from "@angular/common/http";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {FormsModule} from "@angular/forms";
import {NotificationOptionsComponent} from "tim/item/manage/notification-options.component";

@NgModule({
    declarations: [NotificationOptionsComponent],
    imports: [CommonModule, HttpClientModule, TimUtilityModule, FormsModule],
})
export class TimManageModule {}
