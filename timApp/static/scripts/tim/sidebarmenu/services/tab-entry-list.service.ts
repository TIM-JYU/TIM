import {Injectable} from "@angular/core";
import {Users} from "tim/user/userService";
import {ScoreboardService} from "tim/sidebarmenu/services/scoreboard.service";
import {getVisibilityVars} from "tim/timRoot";
import {LectureController} from "tim/lecture/lectureController";
import {BookmarksTabComponent} from "tim/sidebarmenu/tabs/bookmarks-tab.component";
import {SettingsTabComponent} from "tim/sidebarmenu/tabs/settings-tab.component";
import {IndexTabComponent} from "tim/sidebarmenu/tabs/index-tab.component";
import {ScoreInfoTabComponent} from "tim/sidebarmenu/tabs/score-info-tab.component";
import {LectureInfoTabComponent} from "tim/sidebarmenu/tabs/lecture-info-tab.component";
import {LoadQuestionsTabComponent} from "tim/sidebarmenu/tabs/load-questions-tab.component";
import {LoggedUsersTabComponent} from "tim/sidebarmenu/tabs/logged-users-tab.component";
import type {TabEntry} from "tim/sidebarmenu/menu-tab.directive";
import {HeaderIndexerService} from "tim/sidebarmenu/services/header-indexer.service";

@Injectable({
    providedIn: "root",
})
export class TabEntryListService {
    constructor(
        private headerIndexer: HeaderIndexerService,
        private scoreboard: ScoreboardService
    ) {}

    get defaultTabOrder() {
        return ["tab-index", "tab-bookmark"];
    }

    getTabEntries(): TabEntry[] {
        // TODO: Convert to lazy imports when all sidebar menu dialogs are converted to Angular as well
        const lectureCtrl = LectureController.instance;
        const hide = getVisibilityVars();
        return [
            {
                id: "tab-bookmark",
                icon: "bookmark",
                title: $localize`Bookmarks`,
                visible: () => !hide.bookmarks && Users.isRealUser(),
                component: BookmarksTabComponent,
            },
            {
                id: "tab-settings",
                icon: "cog",
                title: $localize`Document settings`,
                visible: () => !hide.settings,
                component: SettingsTabComponent,
            },
            {
                id: "tab-index",
                icon: "book",
                title: $localize`Document index`,
                visible: () =>
                    !hide.index && this.headerIndexer.headers.length > 0,
                component: IndexTabComponent,
            },
            {
                id: "tab-score-info",
                icon: "stats",
                title: $localize`Scoreboard`,
                visible: () => !hide.scoreBoard && this.scoreboard.valid,
                component: ScoreInfoTabComponent,
            },
            {
                id: "tab-lecture-info",
                icon: "education",
                title: $localize`Lecture`,
                visible: () =>
                    !hide.lecturetab && lectureCtrl.lectureSettings.lectureMode,
                component: LectureInfoTabComponent,
            },
            {
                id: "tab-get-question",
                icon: "question-sign",
                title: $localize`Get question`,
                visible: () =>
                    !hide.getquestion &&
                    lectureCtrl.lectureSettings.inLecture &&
                    !lectureCtrl.isLecturer,
                component: LoadQuestionsTabComponent,
            },
            {
                id: "tab-logged-users",
                icon: "user",
                title: $localize`Lecture participants`,
                visible: () =>
                    !hide.lecturer &&
                    lectureCtrl.lectureSettings.inLecture &&
                    lectureCtrl.isLecturer,
                component: LoggedUsersTabComponent,
            },
        ];
    }
}
