import {Injectable} from "@angular/core";
import {Users} from "tim/user/userService";
import {getVisibilityVars} from "tim/timRoot";
import {LectureController} from "tim/lecture/lectureController";
import {TabEntry} from "../menu-tab.directive";
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
                id: "tab-bookmark",
                icon: "bookmark",
                title: "Bookmarks",
                visible: () => !hide.bookmarks && Users.isLoggedIn(),
                importComponent: async () =>
                    (await import("../tabs/bookmarks-tab.component")).BookmarksTabComponent,
            },
            {
                id: "tab-settings",
                icon: "cog",
                title: "Document settings",
                visible: () => !hide.settings,
                importComponent: async () =>
                    (await import("../tabs/settings-tab.component")).SettingsTabComponent,
            },
            {
                id: "tab-index",
                icon: "book",
                title: "Document index",
                visible: () => !hide.index && this.headerIndexer.headers.length > 0,
                importComponent: async () =>
                    (await import("../tabs/index-tab.component")).IndexTabComponent,
            },
            {
                id: "tab-lecture-info",
                icon: "education",
                title: "Lecture",
                visible: () => !hide.lecturetab && lectureCtrl.lectureSettings.lectureMode,
                importComponent: async () =>
                    (await import("../tabs/lecture-info-tab.component")).LectureInfoTabComponent,
            },
            {
                id: "tab-get-question",
                icon: "question-sign",
                title: "Get question",
                visible: () => !hide.getquestion && lectureCtrl.lectureSettings.inLecture && !lectureCtrl.isLecturer,
                importComponent: async () =>
                    (await import("../tabs/load-questions-tab.component")).LoadQuestionsTabComponent,
            },
            {
                id: "tab-logged-users",
                icon: "user",
                title: "Lecture participants",
                visible: () => !hide.lecturer && lectureCtrl.lectureSettings.inLecture && lectureCtrl.isLecturer,
                importComponent: async () =>
                    (await import("../tabs/logged-users-tab.component")).LoggedUsersTabComponent,
            },
        ];
    }
}
