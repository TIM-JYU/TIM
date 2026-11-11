import {NgModule} from "@angular/core";
import {provideHttpClient, withInterceptorsFromDi} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {CommonModule} from "@angular/common";
import {FileSelectComponent, FileSelectManagerComponent} from "./file-select";
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
    imports: [CommonModule, FormsModule, TimUtilityModule],
    providers: [provideHttpClient(withInterceptorsFromDi())],
})
export class CsUtilityModule {}
