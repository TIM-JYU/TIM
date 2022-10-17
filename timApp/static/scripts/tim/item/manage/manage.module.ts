import {NgModule} from "@angular/core";
import {HttpClientModule} from "@angular/common/http";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {FormsModule} from "@angular/forms";
import {NotificationOptionsComponent} from "tim/item/manage/notification-options.component";
import {BrowserModule} from "@angular/platform-browser";

@NgModule({
    declarations: [NotificationOptionsComponent],
    imports: [BrowserModule, HttpClientModule, TimUtilityModule, FormsModule],
})
export class TimManageModule {}
