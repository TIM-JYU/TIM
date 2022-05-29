import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {DatepickerModule} from "ngx-bootstrap/datepicker";
import {TimepickerModule} from "ngx-bootstrap/timepicker";
import {BsDropdownModule} from "ngx-bootstrap/dropdown";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {DatetimePopupComponent} from "./ngx-bootstrap-datetime-popup.component";
import {OffClickDirective} from "./offclick.directive";

@NgModule({
    declarations: [DatetimePopupComponent, OffClickDirective],
    imports: [
        CommonModule,
        FormsModule,
        DatepickerModule,
        TimepickerModule,
        BsDropdownModule,
        TimUtilityModule,
    ],
    exports: [DatetimePopupComponent],
    entryComponents: [DatetimePopupComponent],
})
export class DatetimePopupModule {}
