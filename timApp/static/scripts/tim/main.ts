import "./loadJQueryAndMomentGlobals";

import {enableProdMode, StaticProvider} from "@angular/core";
import angular from "angular";
import bootstrap from "bootstrap";
import "eonasdan-bootstrap-datetimepicker";
import $ from "jquery";
import * as answerbrowser from "tim/answer/answerbrowser3";
import * as userlistController from "tim/answer/userlistController";
import {timApp} from "tim/app";
import * as bookmarkFolderBox from "tim/bookmark/bookmarkFolderBox";
import * as bookmarks from "tim/bookmark/bookmarks";
import * as templateList from "tim/document/editing/templateList";
import * as questionController from "tim/document/question/questionController";
import * as viewctrl from "tim/document/viewctrl";
import * as viewRangeNavigation from "tim/document/viewRangeNavigation";
import * as pareditor from "tim/editor/pareditor";
import {environment} from "tim/environments/environment";
import * as indexCtrl from "tim/folder/indexCtrl";
import * as startController from "tim/frontpage/startController";
import * as loadMap from "tim/gamification/loadMap";
import * as manageCtrl from "tim/item/manageCtrl";
import * as relevanceEdit from "tim/item/relevanceEdit";
import * as rightsEditor from "tim/item/rightsEditor";
import * as taggedDocumentList from "tim/item/taggedDocumentList";
import * as answerToQuestionController from "tim/lecture/answerToQuestionController";
import * as createLectureCtrl from "tim/lecture/createLectureCtrl";
import * as lectureController from "tim/lecture/lectureController";
import * as lectureInfoController from "tim/lecture/lectureInfoController";
import * as lectureMenu from "tim/lecture/lectureMenu";
import * as questionAskController from "tim/lecture/questionAskController";
import * as showStatisticsToQuestionController from "tim/lecture/statisticsToQuestionController";
import * as searchBox from "tim/search/searchBox";
import * as sidebarMenuCtrl from "tim/sidebar/sidebarMenuCtrl";
import * as bootstrapPanel from "tim/ui/bootstrapPanel";
import * as logo from "tim/ui/logo";
import * as loginMenu from "tim/user/loginMenu";
import * as settingsCtrl from "tim/user/settingsCtrl";
import * as userMenu from "tim/user/userMenu";
import {markAsUsed, ModuleArray, StringArray} from "tim/util/utils";
import * as annotation from "tim/velp/annotation";
import * as reviewController from "tim/velp/reviewController";
import * as velpSelection from "tim/velp/velpSelection";
import {staticDynamicImport} from "tim/staticDynamicImport";
import {AppModule} from "tim/app.module";
import {HeaderComponent} from "tim/header/header.component";
import {CreateItemComponent} from "tim/item/create-item.component";
import {TimAlertComponent} from "tim/ui/tim-alert.component";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {setAngularJSGlobal} from "@angular/upgrade/static";
import {MarkupErrorComponent} from "tim/ui/markup-error.component";
import {LoadingComponent} from "tim/ui/loadingIndicator";
import {insertLogDivIfEnabled, timLogInit, timLogTime} from "./util/timTiming";
import {genericglobals} from "./util/globals";
import {ParCompiler} from "./editor/parCompiler";

if (environment.production) {
    enableProdMode();
}

markAsUsed(
    annotation,
    answerbrowser,
    answerToQuestionController,
    bookmarkFolderBox,
    bookmarks,
    bootstrap,
    bootstrapPanel,
    createLectureCtrl,
    indexCtrl,
    lectureController,
    lectureInfoController,
    lectureMenu,
    loadMap,
    loginMenu,
    logo,
    manageCtrl,
    pareditor,
    questionAskController,
    questionController,
    reviewController,
    relevanceEdit,
    rightsEditor,
    searchBox,
    settingsCtrl,
    showStatisticsToQuestionController,
    sidebarMenuCtrl,
    startController,
    taggedDocumentList,
    templateList,
    userlistController,
    userMenu,
    velpSelection,
    viewctrl,
    viewRangeNavigation,
);

const appBootstrap = (extraProviders: StaticProvider[]) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(AppModule);
};

setAngularJSGlobal(angular);

function createDowngradedAppModule() {
    const dg = createDowngradedModule(appBootstrap);
    doDowngrade(dg, "timHeader", HeaderComponent);
    doDowngrade(dg, "createItem", CreateItemComponent);
    doDowngrade(dg, "timAlert", TimAlertComponent);
    doDowngrade(dg, "timMarkupError", MarkupErrorComponent);
    doDowngrade(dg, "timLoading", LoadingComponent);
    return dg;
}

const downgradedModule = createDowngradedAppModule();

if (document.location) {
    timLogInit(document.location.search.slice(1));
}

const themeNameMap: Record<string, string | undefined> = {
    bluetheme: "theme-blue",
    reunukset: "theme-borders",
};

function applyThemeClasses() {
    for (const [name, used] of Object.entries(genericglobals().userPrefs.css_files)) {
        const classname = themeNameMap[name];
        if (classname) {
            document.body.classList.add(classname);
        }
    }
}

$(async () => {
    timLogTime("DOM ready", "main.ts");
    insertLogDivIfEnabled();
    applyThemeClasses();
    const jsmodules = genericglobals().JSMODULES;
    const moduleLoads = [];
    for (const mname of jsmodules) {
        const m = staticDynamicImport(mname);
        if (!m) {
            continue;
        }
        moduleLoads.push(m);
    }
    const angularModules: string[] = [];
    for (const m of moduleLoads) {
        const loaded = await m as {moduleDefs: unknown};
        const mods = loaded.moduleDefs;
        if (ModuleArray.is(mods)) {
            angularModules.push(...mods.map((mm) => mm.name));
        }
    }
    const extraAngularModules = genericglobals().ANGULARMODULES;
    if (StringArray.is(extraAngularModules)) {
        angularModules.push(...extraAngularModules);
    }
    angular.bootstrap(document, [timApp.name, downgradedModule.name, ...angularModules], {strictDi: false});
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
