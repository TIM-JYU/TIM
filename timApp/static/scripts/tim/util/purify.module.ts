import {NgModule} from "@angular/core";
import {PurifyPipe} from "./purify.pipe";

@NgModule({
    declarations: [
        PurifyPipe,
    ],
    exports: [
        PurifyPipe,
    ],
})
export class PurifyModule {
}
