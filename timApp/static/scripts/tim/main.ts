import "tim/loadJQueryAndMomentGlobals";
import "reflect-metadata";

import {enableProdMode} from "@angular/core";
import angular from "angular";
import bootstrap from "bootstrap";
import "eonasdan-bootstrap-datetimepicker";
import $ from "jquery";
import * as userlistController from "tim/answer/userlistController";
import {timApp} from "tim/app";
import * as viewctrl from "tim/document/viewctrl";
import {ViewRangeNavigationComponent} from "tim/document/view-range-navigation.component";
import {environment} from "tim/environments/environment";
import {FrontPageComponent} from "tim/frontpage/front-page.component";
import * as loadMap from "tim/gamification/gamification-map.component";
import {GamificationMapComponent} from "tim/gamification/gamification-map.component";
import * as manageCtrl from "tim/item/manageCtrl";
import * as rightsEditor from "tim/item/rightsEditor";
import * as markAllAsRead from "tim/ui/mark-all-as-read.component";
import {MarkAllAsReadComponent} from "tim/ui/mark-all-as-read.component";
import {BootstrapPanelComponent} from "tim/ui/bootstrap-panel.component";
import {LogoComponent} from "tim/ui/logo.component";
import {LoginMenuComponent} from "tim/user/login-menu.component";
import * as timRoot from "tim/timRoot";
import {markAsUsed, ModuleArray, StringArray} from "tim/util/utils";
import {AnnotationComponent} from "tim/velp/annotation.component";
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
import {LoadingComponent} from "tim/ui/loading.component";
import {VelpSummaryComponent} from "tim/velp/velp-summary.component";
import {DialogComponent} from "tim/ui/dialog.component";
import {CloseButtonComponent} from "tim/ui/close-button.component";
import {DialogContainerComponent} from "tim/ui/angulardialog/dialog-container.component";
import {AddMemberComponent} from "tim/ui/add-member.component";
import {FooterComponent} from "tim/footer.component";
import {SiteHeaderComponent} from "tim/header/site-header.component";
import {TimeLeftComponent} from "tim/ui/time-left.component";
import {CountdownComponent} from "tim/ui/countdown.component";
import {AccessCountdownComponent} from "tim/item/access-countdown.component";
import {GotoLinkComponent} from "tim/ui/goto-link.component";
import {SidebarMenuComponent} from "tim/sidebarmenu/sidebar-menu.component";
import BackspaceDisabler from "backspace-disabler";
import {DrawToolbarComponent} from "tim/plugin/draw-canvas/draw-toolbar.component";
import {DrawCanvasComponent} from "tim/plugin/draw-canvas/draw-canvas.components";
import {DirectoryListComponent} from "tim/folder/directory-list.component";
import {TemplateListComponent} from "tim/document/editing/template-list.component";
import * as selfExpire from "tim/item/self-expire.component";
import {SelfExpireComponent} from "tim/item/self-expire.component";
import {HelpParContent} from "tim/document/editing/help-par-content.component";
import {DurationPickerComponent} from "tim/ui/duration-picker.component";
import {RelevanceEditComponent} from "tim/item/relevance-edit.component";
import {TimMessageViewComponent} from "tim/messaging/tim-message-view.component";
import {ManageReadReceiptComponent} from "tim/messaging/manage-read-receipt.component";
import {CopyFolderComponent} from "tim/folder/copy-folder.component";
import {NotificationOptionsComponent} from "tim/item/manage/notification-options.component";
import {ParRefComponent} from "tim/document/par-ref.component";
import {SearchButtonComponent} from "tim/search/search-button.component";
import {UserProfileComponent} from "tim/plugin/user-profile/user-profile.component";
import {ParticipantListComponent} from "tim/plugin/participant-list/participant-list.component";
import {CourseManagerComponent} from "tim/plugin/course-manager/course-manager.component";
import {
    handleExpiredSession,
    SESSION_VERIFICATION_NEEDED_CODE,
} from "tim/util/session-verify.interceptor";
import {RoleInfoComponent} from "tim/header/role-info.component";
import {PluginLoaderComponent} from "tim/plugin/plugin-loader.component";
import {
    insertLogDivIfEnabled,
    timLogInit,
    timLogTime,
} from "tim/util/timTiming";
import {genericglobals, isErrorGlobals} from "tim/util/globals";
import {ParCompiler} from "tim/editor/parCompiler";
import {PrintButtonComponent} from "tim/ui/print-button.component";
import {PointsDisplayComponent} from "tim/ui/points-display.component";
import {FormulaEditorLoaderComponent} from "../../../modules/cs/js/editor/math-editor/formula-editor-loader.component";

BackspaceDisabler.disable();

if (environment.production) {
    enableProdMode();
}

markAsUsed(
    bootstrap,
    loadMap,
    manageCtrl,
    rightsEditor,
    selfExpire,
    timRoot,
    userlistController,
    velpSelection,
    viewctrl,
    markAllAsRead
);

setAngularJSGlobal(angular);

function createDowngradedAppModule() {
    const dg = createDowngradedModule((extraProviders) => {
        const platformRef = platformBrowserDynamic(extraProviders);
        return platformRef.bootstrapModule(AppModule);
    });
    doDowngrade(dg, "timHeader", HeaderComponent);
    doDowngrade(dg, "createItem", CreateItemComponent);
    doDowngrade(dg, "timCopyFolder", CopyFolderComponent);
    doDowngrade(dg, "timAlert", TimAlertComponent);
    doDowngrade(dg, "timMarkupError", MarkupErrorComponent);
    doDowngrade(dg, "timLoading", LoadingComponent);
    doDowngrade(dg, "annotation", AnnotationComponent);
    doDowngrade(dg, "velpSummary", VelpSummaryComponent);
    doDowngrade(dg, "timDialog", DialogComponent);
    doDowngrade(dg, "timCloseButton", CloseButtonComponent);
    doDowngrade(dg, "timDialogContainer", DialogContainerComponent);
    doDowngrade(dg, "timAddMember", AddMemberComponent);
    doDowngrade(dg, "timFooter", FooterComponent);
    doDowngrade(dg, "timLoginMenu", LoginMenuComponent);
    doDowngrade(dg, "timLogo", LogoComponent);
    doDowngrade(dg, "bootstrapPanel", BootstrapPanelComponent);
    doDowngrade(dg, "timStart", FrontPageComponent);
    doDowngrade(dg, "timSiteHeader", SiteHeaderComponent);
    doDowngrade(dg, "timAccessCountdown", AccessCountdownComponent);
    doDowngrade(dg, "timGotoLink", GotoLinkComponent);
    doDowngrade(dg, "timPrintButton", PrintButtonComponent);
    doDowngrade(dg, "timTimeLeft", TimeLeftComponent);
    doDowngrade(dg, "timCountdown", CountdownComponent);
    doDowngrade(dg, "timSidebarMenu", SidebarMenuComponent);
    doDowngrade(dg, "timDrawToolbar", DrawToolbarComponent);
    doDowngrade(dg, "timPluginLoader", PluginLoaderComponent);
    doDowngrade(dg, "timDrawCanvas", DrawCanvasComponent);
    doDowngrade(dg, "timIndex", DirectoryListComponent);
    doDowngrade(dg, "timTemplateList", TemplateListComponent);
    doDowngrade(dg, "timViewRangeNavigation", ViewRangeNavigationComponent);
    doDowngrade(dg, "timHelpParContent", HelpParContent);
    doDowngrade(dg, "timDurationPicker", DurationPickerComponent);
    doDowngrade(dg, "timRelevanceEdit", RelevanceEditComponent);
    doDowngrade(dg, "timMessageView", TimMessageViewComponent);
    doDowngrade(dg, "manageReadReceipt", ManageReadReceiptComponent);
    doDowngrade(dg, "timNotificationOptions", NotificationOptionsComponent);
    doDowngrade(dg, "timParRef", ParRefComponent);
    doDowngrade(dg, "gamificationMap", GamificationMapComponent);
    doDowngrade(dg, "timMarkAllAsRead", MarkAllAsReadComponent);
    doDowngrade(dg, "timSelfExpire", SelfExpireComponent);
    doDowngrade(dg, "timSearchButton", SearchButtonComponent);
    doDowngrade(dg, "timRoleInfo", RoleInfoComponent);
    doDowngrade(dg, "csFormulaEditorLoader", FormulaEditorLoaderComponent);
    doDowngrade(dg, "timUserProfile", UserProfileComponent);
    doDowngrade(dg, "timParticipantList", ParticipantListComponent);
    doDowngrade(dg, "timCourseManager", CourseManagerComponent);
    doDowngrade(dg, "pointsDisplay", PointsDisplayComponent);
    return dg;
}

const downgradedModule = createDowngradedAppModule();

if (document.location) {
    timLogInit(document.location.search.slice(1));
}

$(async () => {
    timLogTime("DOM ready", "main.ts");
    insertLogDivIfEnabled();
    const globals = genericglobals();
    const jsmodules = globals.JSMODULES;
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
        const loaded = (await m) as {moduleDefs: unknown};
        const mods = loaded.moduleDefs;
        if (ModuleArray.is(mods)) {
            angularModules.push(...mods.map((mm) => mm.name));
        }
    }
    const extraAngularModules = genericglobals().ANGULARMODULES;
    if (StringArray.is(extraAngularModules)) {
        angularModules.push(...extraAngularModules);
    }
    angular.bootstrap(
        document,
        [timApp.name, downgradedModule.name, ...angularModules],
        {strictDi: false}
    );
    timLogTime("Angular bootstrap done", "main.ts");
    ParCompiler.processAllMathDelayed($("body"), 1500);

    if (
        isErrorGlobals(globals) &&
        globals.errorCode == SESSION_VERIFICATION_NEEDED_CODE
    ) {
        await handleExpiredSession();
        window.location.reload();
    }

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
