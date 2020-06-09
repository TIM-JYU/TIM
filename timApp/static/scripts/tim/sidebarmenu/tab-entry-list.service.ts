import {Injectable} from "@angular/core";
import {TabEntry} from "tim/sidebarmenu/menu-tab.directive";
import {BookmarksTabComponent} from "tim/sidebarmenu/tabs/bookmarks-tab.component";
import {SettingsTabComponent} from "tim/sidebarmenu/tabs/settings-tab.component";
import {IndexTabComponent} from "tim/sidebarmenu/tabs/index-tab.component";
import {LectureInfoTabComponent} from "tim/sidebarmenu/tabs/lecture-info-tab.component";
import {LoadQuestionsTabComponent} from "tim/sidebarmenu/tabs/load-questions-tab.component";
import {LoggedUsersTabComponent} from "tim/sidebarmenu/tabs/logged-users-tab.component";

@Injectable({
    providedIn: "root",
})
export class TabEntryListService {

    constructor() {
    }

    getTabEntries(): TabEntry[] {
        return [
            {
                tabType: BookmarksTabComponent,
                icon: "bookmark",
                title: "Bookmarks",
                visible: true,
            },
            {
                tabType: SettingsTabComponent,
                icon: "cog",
                title: "Document settings",
                visible: true,
            },
            {
                tabType: IndexTabComponent,
                icon: "book",
                title: "Document index",
                visible: true,
            },
            {
                tabType: LectureInfoTabComponent,
                icon: "education",
                title: "Lecture",
                visible: true,
            },
            {
                tabType: LoadQuestionsTabComponent,
                icon: "question-sign",
                title: "Get question",
                visible: true,
            },
            {
                tabType: LoggedUsersTabComponent,
                icon: "user",
                title: "Lecture participants",
                visible: true,
            },
        ];
    }
}
