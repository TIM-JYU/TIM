import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {TabsModule} from "ngx-bootstrap/tabs";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {FormsModule} from "@angular/forms";
import {PopoverModule} from "ngx-bootstrap/popover";
import {DialogModule} from "../ui/angulardialog/dialog.module";
import {TimUtilityModule} from "../ui/tim-utility.module";
import {VelpSummaryComponent} from "./velp-summary.component";
import {
    AssertTypePipe,
    FilterByLabelsPipe,
    FilterByPipe,
    FilterByVelpGroupsPipe,
    OrderByWhenNotEditingPipe,
    VelpSelectionDialog,
} from "./velp-selection-dialog.component";
import {VelpWindowComponent} from "./velpWindow";

@NgModule({
    declarations: [
        VelpSummaryComponent,
        VelpSelectionDialog,
        FilterByLabelsPipe,
        FilterByVelpGroupsPipe,
        OrderByWhenNotEditingPipe,
        FilterByPipe,
        VelpWindowComponent,
        AssertTypePipe,
    ],
    imports: [
        CommonModule,
        TabsModule.forRoot(),
        DialogModule,
        TooltipModule.forRoot(),
        FormsModule,
        TimUtilityModule,
        PopoverModule,
    ],
    exports: [VelpSummaryComponent],
})
export class VelpsModule {}
