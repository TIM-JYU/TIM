import {BrowserModule} from "@angular/platform-browser";
import {ApplicationRef, DoBootstrap, NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HeaderComponent} from "tim/header/header.component";
import {HTTP_INTERCEPTORS, HttpClientModule} from "@angular/common/http";
import {CreateItemComponent} from "tim/item/create-item.component";
import {ErrorStateDirective} from "tim/ui/error-state.directive";
import {ShortNameDirective} from "tim/ui/short-name.directive";
import {LocationDirective} from "tim/ui/location.directive";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {TimeStampToMomentConverter} from "tim/util/time-stamp-to-moment-converter.service";
import {AnnotationComponent} from "tim/velp/annotation.component";
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
import {SideBarMenuModule} from "tim/sidebarmenu/side-bar-menu.module";
import {DirectoryListComponent} from "tim/folder/directory-list.component";
import {TabsModule} from "ngx-bootstrap/tabs";
import {TemplateListComponent} from "tim/document/editing/template-list.component";
import {ConsentChoiceComponent} from "tim/ui/consent-choice.component";
import {ViewRangeNavigationComponent} from "tim/document/view-range-navigation.component";
import {ViewRangeEditDialogComponent} from "tim/document/view-range-edit-dialog.component";
import {HelpParContent} from "tim/document/editing/help-par-content.component";
import {RelevanceEditComponent} from "tim/item/relevance-edit.component";
import {RelevanceEditDialogComponent} from "tim/item/relevance-edit-dialog.component";
import {QuestionPreviewDialogComponent} from "tim/lecture/question-preview-dialog.component";
import {AnswerToQuestionDialogComponent} from "tim/lecture/answer-to-question-dialog.component";
import {ProgressbarModule} from "ngx-bootstrap/progressbar";
import {AnswerSheetModule} from "tim/document/question/answer-sheet.component";
import {QuestionEditDialogComponent} from "tim/document/question/question-edit-dialog.component";
import {QuestionMatrixComponent} from "tim/document/question/question-matrix.component";
import {LectureDialogComponent} from "tim/lecture/lecture-dialog.component";
import {AccordionModule} from "ngx-bootstrap/accordion";
import {DatepickerModule} from "ngx-bootstrap/datepicker";
import {TimepickerModule} from "ngx-bootstrap/timepicker";
import {DatetimePopupModule} from "ngx-bootstrap-datetime-popup";
import {LectureMenuComponent} from "tim/lecture/lectureMenu";

@NgModule({
    declarations: [
        CreateItemComponent,
        ErrorStateDirective,
        HeaderComponent,
        LocationDirective,
        ShortNameDirective,
        AnnotationComponent,
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
        DirectoryListComponent,
        TemplateListComponent,
        ConsentChoiceComponent,
        ViewRangeNavigationComponent,
        ViewRangeEditDialogComponent,
        HelpParContent,
        RelevanceEditComponent,
        RelevanceEditDialogComponent,
        QuestionPreviewDialogComponent,
        AnswerToQuestionDialogComponent,
        QuestionEditDialogComponent,
        QuestionMatrixComponent,
        LectureDialogComponent,
        LectureMenuComponent,
    ],
    imports: [
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        DialogModule,
        NoopAnimationsModule,
        SideBarMenuModule,
        AnswerSheetModule,
        BsDropdownModule.forRoot(),
        TypeaheadModule.forRoot(),
        TooltipModule.forRoot(),
        TabsModule.forRoot(),
        ProgressbarModule.forRoot(),
        AccordionModule.forRoot(),
        DatepickerModule.forRoot(),
        TimepickerModule.forRoot(),
        DatetimePopupModule,
    ],
    providers: [
        {
            provide: HTTP_INTERCEPTORS,
            useClass: TimeStampToMomentConverter,
            multi: true,
        },
    ],
    bootstrap: [],
})
export class AppModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {
        setTheme("bs3");
    }
}
