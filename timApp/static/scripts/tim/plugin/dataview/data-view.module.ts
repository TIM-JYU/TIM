import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {Class, DataViewComponent} from "./data-view.component";

@NgModule({
    declarations: [
        DataViewComponent,
        Class,
    ],
    imports: [
        CommonModule,
        FormsModule,
    ],
    exports: [
        DataViewComponent,
    ],
})
export class DataViewModule {
}
