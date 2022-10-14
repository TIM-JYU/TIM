import {NgModule} from "@angular/core";
import {PurifyPipe} from "tim/util/purify.pipe";

@NgModule({
    declarations: [PurifyPipe],
    exports: [PurifyPipe],
})
export class PurifyModule {}
