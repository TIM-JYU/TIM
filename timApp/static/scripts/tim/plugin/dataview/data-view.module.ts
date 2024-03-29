import {NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {DataViewComponent} from "tim/plugin/dataview/data-view.component";
import {CopyTableWidthDialogComponent} from "tim/plugin/dataview/copy-table-width-dialog.component";
import {CommonModule} from "@angular/common";

@NgModule({
    declarations: [DataViewComponent, CopyTableWidthDialogComponent],
    imports: [CommonModule, FormsModule, TimUtilityModule, DialogModule],
    exports: [DataViewComponent],
})
export class DataViewModule {}
