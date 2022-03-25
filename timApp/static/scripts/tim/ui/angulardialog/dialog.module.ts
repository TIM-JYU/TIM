import {NgModule} from "@angular/core";
import {DialogContainerComponent} from "tim/ui/angulardialog/dialog-container.component";
import {DialogHostDirective} from "tim/ui/angulardialog/dialog-host.directive";
import {DialogFrame} from "tim/ui/angulardialog/dialog-frame.component";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BrowserModule} from "@angular/platform-browser";
import {AngularDraggableModule} from "angular2-draggable";
import {DetachableWindowComponent} from "./draggable-window.component";

@NgModule({
    declarations: [
        DialogContainerComponent,
        DialogHostDirective,
        DialogFrame,
        DetachableWindowComponent,
    ],
    imports: [TimUtilityModule, BrowserModule, AngularDraggableModule],
    exports: [DialogFrame, DialogContainerComponent, DetachableWindowComponent],
})
export class DialogModule {}
