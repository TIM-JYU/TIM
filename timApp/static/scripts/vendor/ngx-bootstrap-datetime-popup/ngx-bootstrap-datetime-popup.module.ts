import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {TimepickerModule} from "ngx-bootstrap/timepicker";
import {BsDropdownModule} from "ngx-bootstrap/dropdown";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BsDatepickerModule} from "ngx-bootstrap/datepicker";
import {DatetimePopupComponent} from "./ngx-bootstrap-datetime-popup.component";
import {OffClickDirective} from "./offclick.directive";

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
