import {Injectable} from "@angular/core";
import {Users} from "tim/user/userService";
import {getVisibilityVars} from "tim/timRoot";
import {LectureController} from "tim/lecture/lectureController";
import {TabEntry} from "../menu-tab.directive";
import {BookmarksTabComponent} from "../tabs/bookmarks-tab.component";
import {SettingsTabComponent} from "../tabs/settings-tab.component";
import {IndexTabComponent} from "../tabs/index-tab.component";
import {LectureInfoTabComponent} from "../tabs/lecture-info-tab.component";
import {LoadQuestionsTabComponent} from "../tabs/load-questions-tab.component";
import {LoggedUsersTabComponent} from "../tabs/logged-users-tab.component";
import {HeaderIndexerService} from "./header-indexer.service";

@Injectable({
    providedIn: "root",
})
export class TabEntryListService {

    constructor(private headerIndexer: HeaderIndexerService) {
    }

    getTabEntries(): TabEntry[] {
        const lectureCtrl = LectureController.instance;
        const hide = getVisibilityVars();
        return [
            {
                tabType: BookmarksTabComponent,
                icon: "bookmark",
                title: "Bookmarks",
                visible: () => !hide.bookmarks && Users.isLoggedIn(),
            },
            {
                tabType: SettingsTabComponent,
                icon: "cog",
                title: "Document settings",
                visible: () => !hide.settings,
            },
            {
                tabType: IndexTabComponent,
                icon: "book",
                title: "Document index",
                visible: () => !hide.index && this.headerIndexer.headers.length > 0,
            },
            {
                tabType: LectureInfoTabComponent,
                icon: "education",
                title: "Lecture",
                visible: () => !hide.lecturetab && lectureCtrl.lectureSettings.lectureMode,
            },
            {
                tabType: LoadQuestionsTabComponent,
                icon: "question-sign",
                title: "Get question",
                visible: () => !hide.getquestion && lectureCtrl.lectureSettings.inLecture && !lectureCtrl.isLecturer,
            },
            {
                tabType: LoggedUsersTabComponent,
                icon: "user",
                title: "Lecture participants",
                visible: () => !hide.lecturer && lectureCtrl.lectureSettings.inLecture && lectureCtrl.isLecturer,
            },
        ];
    }
}
