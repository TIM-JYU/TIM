import {NgModule} from "@angular/core";
import {DialogContainerComponent} from "tim/ui/angulardialog/dialog-container.component";
import {DialogHostDirective} from "tim/ui/angulardialog/dialog-host.directive";
import {DialogFrame} from "tim/ui/angulardialog/dialog-frame.component";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {AngularDraggableModule} from "angular2-draggable";
import {CommonModule} from "@angular/common";
import {
    TestDetachableDialogComponent,
    TestDetachableDialogDialog,
} from "./test-detachable-dialog.component";

@NgModule({
    imports: [TimUtilityModule, CommonModule, AngularDraggableModule],
    declarations: [
        DialogContainerComponent,
        DialogHostDirective,
        DialogFrame,
        TestDetachableDialogComponent,
        TestDetachableDialogDialog,
    ],
    exports: [DialogFrame, DialogContainerComponent],
})
export class DialogModule {}
