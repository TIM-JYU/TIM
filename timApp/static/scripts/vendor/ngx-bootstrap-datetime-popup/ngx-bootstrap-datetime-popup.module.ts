import {NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {TimepickerModule} from "ngx-bootstrap/timepicker";
import {BsDropdownModule} from "ngx-bootstrap/dropdown";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BsDatepickerModule} from "ngx-bootstrap/datepicker";
import {BrowserModule} from "@angular/platform-browser";
import {DatetimePopupComponent} from "vendor/ngx-bootstrap-datetime-popup/ngx-bootstrap-datetime-popup.component";
import {OffClickDirective} from "vendor/ngx-bootstrap-datetime-popup/offclick.directive";

@NgModule({
    declarations: [DatetimePopupComponent, OffClickDirective],
    imports: [
        BrowserModule,
        FormsModule,
        BsDatepickerModule,
        TimepickerModule,
        BsDropdownModule,
        TimUtilityModule,
    ],
    exports: [DatetimePopupComponent],
})
export class DatetimePopupModule {}
