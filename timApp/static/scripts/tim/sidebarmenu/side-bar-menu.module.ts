import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {TabsModule} from "ngx-bootstrap/tabs";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {FormsModule} from "@angular/forms";
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
import { TimeSincePipe } from "./util/time-since.pipe";

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
    ],
    imports: [
        CommonModule,
        FormsModule,
        TabsModule.forRoot(),
        TooltipModule.forRoot(),
        BsDropdownModule.forRoot(),
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
export class SideBarMenuModule {
}
