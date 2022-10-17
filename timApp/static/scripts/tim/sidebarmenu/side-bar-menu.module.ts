import type {ModuleWithProviders} from "@angular/core";
import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {TabsModule} from "ngx-bootstrap/tabs";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {FormsModule} from "@angular/forms";
import {CollapseModule} from "ngx-bootstrap/collapse";
import {SidebarMenuComponent} from "tim/sidebarmenu/sidebar-menu.component";
import {BsDropdownModule} from "ngx-bootstrap/dropdown";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BookmarksTabComponent} from "tim/sidebarmenu/tabs/bookmarks-tab.component";
import {MenuTabDirective} from "tim/sidebarmenu/menu-tab.directive";
import {TabContainerComponent} from "tim/sidebarmenu/tab-container.component";
import {SettingsTabComponent} from "tim/sidebarmenu/tabs/settings-tab.component";
import {IndexTabComponent} from "tim/sidebarmenu/tabs/index-tab.component";
import {LectureInfoTabComponent} from "tim/sidebarmenu/tabs/lecture-info-tab.component";
import {LoadQuestionsTabComponent} from "tim/sidebarmenu/tabs/load-questions-tab.component";
import {LoggedUsersTabComponent} from "tim/sidebarmenu/tabs/logged-users-tab.component";
import {BookmarksComponent} from "tim/sidebarmenu/util/bookmarks.component";
import {TimeSincePipe} from "tim/sidebarmenu/util/time-since.pipe";
import {ScoreInfoTabComponent} from "tim/sidebarmenu/tabs/score-info-tab.component";

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
        TimUtilityModule,
        TabsModule,
    ],
})
export class SideBarMenuModule {}
