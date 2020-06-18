import "./loadJQueryAndMomentGlobals";
import "reflect-metadata";

import {enableProdMode} from "@angular/core";
import angular from "angular";
import bootstrap from "bootstrap";
import "eonasdan-bootstrap-datetimepicker";
import $ from "jquery";
import * as answerbrowser from "tim/answer/answerbrowser3";
import * as userlistController from "tim/answer/userlistController";
import {timApp} from "tim/app";
import * as templateList from "tim/document/editing/templateList";
import * as questionController from "tim/document/question/questionController";
import * as viewctrl from "tim/document/viewctrl";
import * as viewRangeNavigation from "tim/document/viewRangeNavigation";
import {environment} from "tim/environments/environment";
import * as indexCtrl from "tim/folder/indexCtrl";
import {FrontPageComponent} from "tim/frontpage/front-page.component";
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
import * as bootstrapPanel from "tim/ui/bootstrap-panel.component";
import * as markAllAsRead from "tim/ui/markAllAsRead";
import {BootstrapPanelComponent} from "tim/ui/bootstrap-panel.component";
import {LogoComponent} from "tim/ui/logo.component";
import {LoginMenuComponent} from "tim/user/login-menu.component";
import * as timRoot from "tim/timRoot";
import {SettingsComponent} from "tim/user/settings.component";
import {markAsUsed, ModuleArray, StringArray} from "tim/util/utils";
import * as annotation from "tim/velp/annotation.component";
import {AnnotationComponent} from "tim/velp/annotation.component";
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
import {VelpSummaryComponent} from "tim/velp/velp-summary.component";
import {DialogComponent} from "tim/ui/dialog.component";
import {CloseButtonComponent} from "tim/ui/close-button.component";
import {DialogContainerComponent} from "tim/ui/angulardialog/dialog-container.component";
import {AddMemberComponent} from "tim/ui/add-member.component";
import {TimFooterComponent} from "tim/footer.component";
import {SiteHeaderComponent} from "tim/header/site-header.component";
import {TimeLeftComponent} from "tim/ui/time-left.component";
import {CountdownComponent} from "tim/ui/countdown.component";
import {AccessCountdownComponent} from "tim/item/access-countdown.component";
import {GotoLinkComponent} from "tim/ui/goto-link.component";
import {SidebarMenuComponent} from "tim/sidebarmenu/sidebar-menu.component";
import BackspaceDisabler from "backspace-disabler";
import {insertLogDivIfEnabled, timLogInit, timLogTime} from "./util/timTiming";
import {genericglobals} from "./util/globals";
import {ParCompiler} from "./editor/parCompiler";

BackspaceDisabler.disable();

if (environment.production) {
    enableProdMode();
}

markAsUsed(
    answerbrowser,
    answerToQuestionController,
    bootstrap,
    createLectureCtrl,
    indexCtrl,
    lectureController,
    lectureInfoController,
    lectureMenu,
    loadMap,
    manageCtrl,
    questionAskController,
    questionController,
    relevanceEdit,
    reviewController,
    rightsEditor,
    showStatisticsToQuestionController,
    taggedDocumentList,
    templateList,
    timRoot,
    userlistController,
    velpSelection,
    viewctrl,
    viewRangeNavigation,
    markAllAsRead,
);

setAngularJSGlobal(angular);

function createDowngradedAppModule() {
    const dg = createDowngradedModule((extraProviders) => {
        const platformRef = platformBrowserDynamic(extraProviders);
        return platformRef.bootstrapModule(AppModule);
    });
    doDowngrade(dg, "timHeader", HeaderComponent);
    doDowngrade(dg, "createItem", CreateItemComponent);
    doDowngrade(dg, "timAlert", TimAlertComponent);
    doDowngrade(dg, "timMarkupError", MarkupErrorComponent);
    doDowngrade(dg, "timLoading", LoadingComponent);
    doDowngrade(dg, "annotation", AnnotationComponent);
    doDowngrade(dg, "velpSummary", VelpSummaryComponent);
    doDowngrade(dg, "timDialog", DialogComponent);
    doDowngrade(dg, "timCloseButton", CloseButtonComponent);
    doDowngrade(dg, "timDialogContainer", DialogContainerComponent);
    doDowngrade(dg, "timAddMember", AddMemberComponent);
    doDowngrade(dg, "timFooter", TimFooterComponent);
    doDowngrade(dg, "timLoginMenu", LoginMenuComponent);
    doDowngrade(dg, "timLogo", LogoComponent);
    doDowngrade(dg, "bootstrapPanel", BootstrapPanelComponent);
    doDowngrade(dg, "timStart", FrontPageComponent);
    doDowngrade(dg, "timSiteHeader", SiteHeaderComponent);
    doDowngrade(dg, "timSettings", SettingsComponent);
    doDowngrade(dg, "timAccessCountdown", AccessCountdownComponent);
    doDowngrade(dg, "timGotoLink", GotoLinkComponent);
    doDowngrade(dg, "timTimeLeft", TimeLeftComponent);
    doDowngrade(dg, "timCountdown", CountdownComponent);
    doDowngrade(dg, "timSidebarMenu", SidebarMenuComponent);
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
        const loaded = await m as { moduleDefs: unknown };
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
