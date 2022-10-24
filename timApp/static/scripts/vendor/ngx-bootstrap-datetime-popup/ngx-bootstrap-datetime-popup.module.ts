import {NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {TimepickerModule} from "ngx-bootstrap/timepicker";
import {BsDropdownModule} from "ngx-bootstrap/dropdown";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BsDatepickerModule} from "ngx-bootstrap/datepicker";
import {DatetimePopupComponent} from "vendor/ngx-bootstrap-datetime-popup/ngx-bootstrap-datetime-popup.component";
import {OffClickDirective} from "vendor/ngx-bootstrap-datetime-popup/offclick.directive";
import {CommonModule} from "@angular/common";

@NgModule({
    declarations: [DatetimePopupComponent, OffClickDirective],
    imports: [
        CommonModule,
        FormsModule,
        BsDatepickerModule,
        TimepickerModule,
        BsDropdownModule,
        TimUtilityModule,
    ],
    exports: [DatetimePopupComponent],
})
export class DatetimePopupModule {}
