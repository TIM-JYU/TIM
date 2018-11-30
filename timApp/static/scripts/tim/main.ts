import angular from "angular";
import bootstrap from "bootstrap";
import $ from "jquery";
import * as answerbrowser from "tim/answer/answerbrowser3";
import * as userlistController from "tim/answer/userlistController";
import * as bookmarkFolderBox from "tim/bookmark/bookmarkFolderBox";
import * as bookmarks from "tim/bookmark/bookmarks";
import * as templateList from "tim/document/editing/templateList";
import * as questionController from "tim/document/question/questionController";
import * as viewctrl from "tim/document/viewctrl";
import * as pareditor from "tim/editor/pareditor";
import * as indexCtrl from "tim/folder/indexCtrl";
import * as startController from "tim/frontpage/startController";
import * as loadMap from "tim/gamification/loadMap";
import * as breadcrumbs from "tim/item/breadcrumbs";
import * as createItem from "tim/item/createItem";
import * as manageCtrl from "tim/item/manageCtrl";
import * as rightsEditor from "tim/item/rightsEditor";
import * as taggedDocumentList from "tim/item/taggedDocumentList";
import * as answerToQuestionController from "tim/lecture/answerToQuestionController";
import * as createLectureCtrl from "tim/lecture/createLectureCtrl";
import * as lectureController from "tim/lecture/lectureController";
import * as lectureInfoController from "tim/lecture/lectureInfoController";
import * as lectureMenu from "tim/lecture/lectureMenu";
import * as questionAskController from "tim/lecture/questionAskController";
import * as showStatisticsToQuestionController from "tim/lecture/showStatisticsToQuestionController";
import * as qstController from "tim/plugin/qstController";
import * as tape from "tim/plugin/tape";
import * as timTable from "tim/plugin/timTable";
import * as pluginUtil from "tim/plugin/util";
import * as searchBox from "tim/search/searchBox";
import * as sidebarMenuCtrl from "tim/sidebar/sidebarMenuCtrl";
import * as bootstrapPanel from "tim/ui/bootstrapPanel";
import * as loginMenu from "tim/user/loginMenu";
import * as settingsCtrl from "tim/user/settingsCtrl";
import {markAsUsed} from "tim/util/utils";
import * as annotation from "tim/velp/annotation";
import * as reviewController from "tim/velp/reviewController";
import * as velpSelection from "tim/velp/velpSelection";
import {ParCompiler} from "./editor/parCompiler";
import {insertLogDivIfEnabled, timLogInit, timLogTime} from "./util/timTiming";

markAsUsed(
    annotation,
    answerbrowser,
    answerToQuestionController,
    bookmarkFolderBox,
    bookmarks,
    bootstrap,
    bootstrapPanel,
    breadcrumbs,
    createItem,
    createLectureCtrl,
    indexCtrl,
    lectureController,
    lectureInfoController,
    lectureMenu,
    loadMap,
    loginMenu,
    manageCtrl,
    pareditor,
    pluginUtil,
    qstController,
    questionAskController,
    questionController,
    reviewController,
    rightsEditor,
    searchBox,
    settingsCtrl,
    showStatisticsToQuestionController,
    sidebarMenuCtrl,
    startController,
    taggedDocumentList,
    tape,
    templateList,
    timTable,
    userlistController,
    velpSelection,
    viewctrl,
);

timLogInit(document.location.search.slice(1));

$(() => {
    timLogTime("DOM ready", "main.ts");
    insertLogDivIfEnabled();
    angular.bootstrap(document, ["timApp"], {strictDi: false});
    timLogTime("Angular bootstrap done", "main.ts");
    ParCompiler.processAllMathDelayed($("body"), 1500);

    // For some reason, anchor link in URL doesn't work when loading a page for the first time.
    // This is a workaround for it.
    if (location.hash && !location.hash.includes("/")) {
        try {
            const id = decodeURIComponent(location.hash).slice(1);

            // Don't use jQuery selector here because id would have to be escaped
            // and then jQuery would have to be updated to get escapeSelector method.
            const element = document.getElementById(id);
            if (element) {
                // Both with and without setTimeout are needed to get smooth experience.
                // Firefox and Chrome behave slightly differently.
                element.scrollIntoView();
                setTimeout(() => element.scrollIntoView());
            }
        } catch {
            // location.hash may still be invalid after decoding
        }
    }
});
