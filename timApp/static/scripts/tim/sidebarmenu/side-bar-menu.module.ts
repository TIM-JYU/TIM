import {NgModule, ModuleWithProviders} from "@angular/core";
import {CommonModule} from "@angular/common";
import {TabsModule} from "ngx-bootstrap/tabs";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {FormsModule} from "@angular/forms";
import {CollapseModule} from "ngx-bootstrap/collapse";
import {SidebarMenuComponent} from "tim/sidebarmenu/sidebar-menu.component";
import {BsDropdownModule} from "ngx-bootstrap/dropdown";
import {BookmarksTabComponent} from "./tabs/bookmarks-tab.component";
import {MenuTabDirective} from "./menu-tab.directive";
import {TabContainerComponent} from "./tab-container.component";
import {SettingsTabComponent} from "./tabs/settings-tab.component";
import {IndexTabComponent} from "./tabs/index-tab.component";
import {LectureInfoTabComponent} from "./tabs/lecture-info-tab.component";
import {LoadQuestionsTabComponent} from "./tabs/load-questions-tab.component";
import {LoggedUsersTabComponent} from "./tabs/logged-users-tab.component";
import {BookmarksComponent} from "./util/bookmarks.component";
import {TimeSincePipe} from "./util/time-since.pipe";
import {ScoreInfoTabComponent} from "./tabs/score-info-tab.component";

@NgModule({
    declarations: [
        SidebarMenuComponent,
        BookmarksTabComponent,
        MenuTabDirective,
        TabContainerComponent,
        SettingsTabComponent,
        IndexTabComponent,
        LectureInfoTabComponent,
        LoadQuestionsTabComponent,
        LoggedUsersTabComponent,
        BookmarksComponent,
        TimeSincePipe,
        ScoreInfoTabComponent,
    ],
    imports: [
        CommonModule,
        FormsModule,
        TabsModule.forRoot() as ModuleWithProviders<Record<string, unknown>>,
        TooltipModule.forRoot() as ModuleWithProviders<Record<string, unknown>>,
        BsDropdownModule.forRoot() as ModuleWithProviders<
            Record<string, unknown>
        >,
        CollapseModule.forRoot() as ModuleWithProviders<
            Record<string, unknown>
        >,
    ],
    entryComponents: [
        BookmarksTabComponent,
        SettingsTabComponent,
        IndexTabComponent,
        LectureInfoTabComponent,
        LoadQuestionsTabComponent,
        LoggedUsersTabComponent,
    ],
})
export class SideBarMenuModule {}
