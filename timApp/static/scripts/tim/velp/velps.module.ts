import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {TabsModule} from "ngx-bootstrap/tabs";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {FormsModule} from "@angular/forms";
import {DialogModule} from "../ui/angulardialog/dialog.module";
import {VelpSummaryComponent} from "./velp-summary.component";
import {
    FilterByLabelsPipe,
    FilterByVelpGroupsPipe,
    FilterLabelsByContentPipe,
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
        FilterLabelsByContentPipe,
        VelpWindowComponent,
    ],
    imports: [
        CommonModule,
        TabsModule.forRoot(),
        DialogModule,
        TooltipModule.forRoot(),
        FormsModule,
    ],
    exports: [VelpSummaryComponent],
})
export class VelpsModule {}
