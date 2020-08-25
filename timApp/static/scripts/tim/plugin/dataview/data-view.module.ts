import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {AlertModule} from "ngx-bootstrap/alert";
import {DataViewComponent} from "./data-view.component";

@NgModule({
    declarations: [
        DataViewComponent,
    ],
    imports: [
        CommonModule,
        FormsModule,
        TimUtilityModule,
        AlertModule.forRoot(),
    ],
    exports: [
        DataViewComponent,
    ],
})
export class DataViewModule {
}
