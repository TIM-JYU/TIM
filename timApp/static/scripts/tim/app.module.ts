import {BrowserModule} from "@angular/platform-browser";
import {ApplicationRef, DoBootstrap, NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HeaderComponent} from "tim/header/header.component";
import {HTTP_INTERCEPTORS, HttpClientModule} from "@angular/common/http";
import {CreateItemComponent} from "tim/item/create-item.component";
import {ErrorStateDirective} from "tim/ui/error-state.directive";
import {ErrorMessageComponent} from "tim/ui/error-message.component";
import {ShortNameDirective} from "tim/ui/short-name.directive";
import {LocationDirective} from "tim/ui/location.directive";
import {TimAlertComponent} from "tim/ui/tim-alert.component";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {TimeStampToMomentConverter} from "tim/util/time-stamp-to-moment-converter.service";
import {AnnotationComponent} from "tim/velp/annotation.component";
import {RelativeTimestampPipe} from "tim/ui/relative-timestamp.pipe";
import {SignatureComponent} from "tim/ui/signature.component";
import {VelpSummaryComponent} from "tim/velp/velp-summary.component";
import {BookmarkDialogComponent} from "tim/bookmark/bookmark-dialog.component";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";

// noinspection AngularInvalidImportedOrDeclaredSymbol
@NgModule({
    declarations: [
        CreateItemComponent,
        ErrorMessageComponent,
        ErrorStateDirective,
        HeaderComponent,
        LocationDirective,
        ShortNameDirective,
        TimAlertComponent,
        AnnotationComponent,
        RelativeTimestampPipe,
        SignatureComponent,
        VelpSummaryComponent,
        BookmarkDialogComponent,
    ],
    imports: [
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        DialogModule,
    ],
    providers: [
        {provide: HTTP_INTERCEPTORS, useClass: TimeStampToMomentConverter, multi: true},
    ],
    bootstrap: [],
})
export class AppModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {
    }
}
