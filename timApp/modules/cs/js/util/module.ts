import {NgModule} from "@angular/core";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {BrowserModule} from "@angular/platform-browser";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {FileSelectManagerComponent, FileSelectComponent} from "./file-select";
import {UploadResultComponent} from "./upload-result";
import {NotificationComponent} from "./notification";

@NgModule({
    declarations: [
        FileSelectManagerComponent,
        UploadResultComponent,
        FileSelectComponent,
        NotificationComponent,
    ],
    exports: [
        FileSelectManagerComponent,
        UploadResultComponent,
        FileSelectComponent,
        NotificationComponent,
    ],
    imports: [BrowserModule, HttpClientModule, FormsModule, TimUtilityModule],
})
export class CsUtilityModule {}
