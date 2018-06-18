import angular from "angular";
import ad from "bootstrap";
import $ from "jquery";
import * as lectureMenu from "tim/components/lectureMenu";
import * as templateList from "tim/components/templateList";
import * as a from "tim/controllers/answerToQuestionController";
import * as b from "tim/controllers/breadcrumbs";
import * as c from "tim/controllers/createLectureCtrl";
import * as d from "tim/controllers/indexCtrl";
import * as e from "tim/controllers/lectureController";
import * as f from "tim/controllers/lectureInfoController";
import * as ae from "tim/controllers/qstController";
import * as h from "tim/controllers/questionAskController";
import * as g from "tim/controllers/questionController";
import * as i from "tim/controllers/reviewController";
import * as j from "tim/controllers/showStatisticsToQuestionController";
import * as k from "tim/controllers/sidebarMenuCtrl";
import * as l from "tim/controllers/smallMenuCtrl";
import * as m from "tim/controllers/startController";
import * as n from "tim/controllers/userlistController";
import * as o from "tim/controllers/view/viewctrl";
import * as p from "tim/directives/annotation";
import * as q from "tim/directives/answerbrowser3";
import * as r from "tim/directives/bookmarks";
import * as s from "tim/directives/bootstrapPanel";
import * as t from "tim/directives/createItem";
import * as u from "tim/directives/loginMenu";
import * as v from "tim/directives/pareditor";
import * as x from "tim/directives/rightsEditor";
import * as y from "tim/directives/velpSelection";
import * as ab from "tim/manageView/manageCtrl";
import * as ac from "tim/settingsView/settingsCtrl";
import * as af from "tim/components/timTable";
import {markAsUsed} from "tim/utils";
import {ParCompiler} from "./services/parCompiler";
import {insertLogDivIfEnabled, timLogInit, timLogTime} from "./timTiming";

markAsUsed(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, x, y, ab, ac, ad, ae, lectureMenu, templateList, af);

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
