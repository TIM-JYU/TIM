import angular from "angular";
import ad from "bootstrap";
import $ from "jquery";
import * as q from "tim/answer/answerbrowser3";
import * as n from "tim/answer/userlistController";
import * as ah from "tim/bookmark/bookmarkFolderBox";
import * as r from "tim/bookmark/bookmarks";
import * as templateList from "tim/document/editing/templateList";
import * as g from "tim/document/question/questionController";
import * as o from "tim/document/viewctrl";
import * as v from "tim/editor/pareditor";
import * as d from "tim/folder/indexCtrl";
import * as m from "tim/frontpage/startController";
import * as b from "tim/item/breadcrumbs";
import * as t from "tim/item/createItem";
import * as ab from "tim/item/manageCtrl";
import * as x from "tim/item/rightsEditor";
import * as ag from "tim/item/taggedDocumentList";
import * as a from "tim/lecture/answerToQuestionController";
import * as c from "tim/lecture/createLectureCtrl";
import * as e from "tim/lecture/lectureController";
import * as f from "tim/lecture/lectureInfoController";
import * as lectureMenu from "tim/lecture/lectureMenu";
import * as h from "tim/lecture/questionAskController";
import * as j from "tim/lecture/showStatisticsToQuestionController";
import * as ae from "tim/plugin/qstController";
import * as af from "tim/plugin/timTable";
import * as ak from "tim/plugin/tape";
import * as ai from "tim/search/searchBox";
import * as aj from "tim/gamification/loadMap";
import * as k from "tim/sidebar/sidebarMenuCtrl";
import * as s from "tim/ui/bootstrapPanel";
import * as u from "tim/user/loginMenu";
import * as ac from "tim/user/settingsCtrl";
import {markAsUsed} from "tim/util/utils";
import * as p from "tim/velp/annotation";
import * as i from "tim/velp/reviewController";
import * as y from "tim/velp/velpSelection";
import {ParCompiler} from "./editor/parCompiler";
import {insertLogDivIfEnabled, timLogInit, timLogTime} from "./util/timTiming";

markAsUsed(a, b, c, d, e, f, g, h, i, j, k, m, n, o, p, q, r, s, t, u, v, x, y, ab, ac, ad, ae, ag, ah, ai, aj, lectureMenu, templateList, af, ak);

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
            const element = $(decodeURIComponent(location.hash))[0];
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
