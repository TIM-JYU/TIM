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
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {TimeStampToMomentConverter} from "tim/util/time-stamp-to-moment-converter.service";
import {AnnotationComponent} from "tim/velp/annotation.component";
import {RelativeTimestampPipe} from "tim/ui/relative-timestamp.pipe";
import {SignatureComponent} from "tim/ui/signature.component";
import {VelpSummaryComponent} from "tim/velp/velp-summary.component";
import {BookmarkDialogComponent} from "tim/bookmark/bookmark-dialog.component";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimFooterComponent} from "tim/footer.component";
import {UserMenuComponent} from "tim/user/user-menu.component";
import {NoopAnimationsModule} from "@angular/platform-browser/animations";
import {LogoComponent} from "tim/ui/logo.component";
import {BsDropdownModule} from "ngx-bootstrap/dropdown";
import {setTheme} from "ngx-bootstrap/utils";
import {LoginMenuComponent} from "tim/user/login-menu.component";
import {HakaLoginComponent} from "tim/user/haka-login.component";
import {LoginDialogComponent} from "tim/user/login-dialog.component";
import {FrontPageComponent} from "tim/frontpage/front-page.component";
import {BookmarkFolderBoxComponent} from "tim/bookmark/bookmark-folder-box.component";
import {SiteHeaderComponent} from "tim/header/site-header.component";
import {SearchBoxComponent} from "tim/search/search-box.component";
import {TypeaheadModule} from "ngx-bootstrap/typeahead";
import {SettingsComponent} from "tim/user/settings.component";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {LanguageSelectorComponent} from "tim/user/language-selector.component";
import {AccessCountdownComponent} from "tim/item/access-countdown.component";

// noinspection AngularInvalidImportedOrDeclaredSymbol
@NgModule({
    declarations: [
        CreateItemComponent,
        ErrorMessageComponent,
        ErrorStateDirective,
        HeaderComponent,
        LocationDirective,
        ShortNameDirective,
        AnnotationComponent,
        RelativeTimestampPipe,
        SignatureComponent,
        VelpSummaryComponent,
        BookmarkDialogComponent,
        TimFooterComponent,
        UserMenuComponent,
        LogoComponent,
        LoginMenuComponent,
        HakaLoginComponent,
        LoginDialogComponent,
        FrontPageComponent,
        BookmarkFolderBoxComponent,
        SiteHeaderComponent,
        SearchBoxComponent,
        SettingsComponent,
        LanguageSelectorComponent,
        AccessCountdownComponent,
    ],
    imports: [
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        DialogModule,
        NoopAnimationsModule,
        BsDropdownModule.forRoot(),
        TypeaheadModule.forRoot(),
        TooltipModule.forRoot(),
    ],
    providers: [
        {provide: HTTP_INTERCEPTORS, useClass: TimeStampToMomentConverter, multi: true},
    ],
    bootstrap: [],
})
export class AppModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {
        setTheme("bs3");
    }
}
