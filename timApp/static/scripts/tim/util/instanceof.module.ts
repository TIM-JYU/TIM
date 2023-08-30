import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {InstanceofPipe} from "tim/util/instanceof.pipe";

@NgModule({
    declarations: [InstanceofPipe],
    exports: [InstanceofPipe],
    imports: [CommonModule],
})
export class InstanceofModule {}
