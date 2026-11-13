import "tim/loadJQueryAndMomentGlobals";
import "reflect-metadata";

import {
    enableProdMode,
    NgZone,
    provideZoneChangeDetection,
} from "@angular/core";
import $ from "jquery";
import {ViewCtrl} from "tim/document/viewctrl";
import {ViewRangeNavigationComponent} from "tim/document/view-range-navigation.component";
import {environment} from "tim/environments/environment";
import {FrontPageComponent} from "tim/frontpage/front-page.component";
import * as loadMap from "tim/gamification/gamification-map.component";
import {GamificationMapComponent} from "tim/gamification/gamification-map.component";
import {ManageComponent} from "tim/item/manage/manage.component";
import {UserListComponent} from "tim/answer/user-list.component";
import {SpellErrorComponent} from "tim/document/editing/spell-error.component";
import {RightsEditorComponent} from "tim/item/rights-editor.component";
import * as markAllAsRead from "tim/ui/mark-all-as-read.component";
import {MarkAllAsReadComponent} from "tim/ui/mark-all-as-read.component";
import {BootstrapPanelComponent} from "tim/ui/bootstrap-panel.component";
import {LogoComponent} from "tim/ui/logo.component";
import {LoginMenuComponent} from "tim/user/login-menu.component";
import {getVisibilityVars, RootCtrl} from "tim/timRoot";
import {attachDraggables} from "tim/ui/draggable";
import {markAsUsed} from "tim/util/utils";
import {
    createServerComponents,
    registerServerComponent,
} from "tim/serverComponents";
import {refreshUI} from "tim/util/refresh";
import {AnnotationComponent} from "tim/velp/annotation.component";
import {staticDynamicImport} from "tim/staticDynamicImport";
import {AppModule} from "tim/app.module";
import {HeaderComponent} from "tim/header/header.component";
import {CreateItemComponent} from "tim/item/create-item.component";
import {TimAlertComponent} from "tim/ui/tim-alert.component";
import {defineRegisteredElements, registerElement} from "tim/customElements";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {MarkupErrorComponent} from "tim/ui/markup-error.component";
import {LoadingComponent} from "tim/ui/loading.component";
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
import {TimMessageViewComponent} from "tim/messaging/tim-message-view.component";
import {ManageReadReceiptComponent} from "tim/messaging/manage-read-receipt.component";
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

BackspaceDisabler.disable();

if (environment.production) {
    enableProdMode();
}

markAsUsed(loadMap, selfExpire, markAllAsRead);

/**
 * Registers the components that can appear in HTML that comes from the server
 * (page templates and document paragraphs). See serverComponents.ts.
 */
function registerServerComponents() {
    registerServerComponent("tim-dialog-container", DialogContainerComponent);
    registerServerComponent("tim-header", HeaderComponent);
    registerServerComponent("create-item", CreateItemComponent);
    registerServerComponent("tim-alert", TimAlertComponent);
    registerServerComponent("tim-markup-error", MarkupErrorComponent);
    registerServerComponent("tim-loading", LoadingComponent);
    registerServerComponent("annotation", AnnotationComponent);
    registerServerComponent("tim-dialog", DialogComponent);
    registerServerComponent("tim-close-button", CloseButtonComponent);
    registerServerComponent("tim-footer", FooterComponent);
    registerServerComponent("tim-login-menu", LoginMenuComponent);
    registerServerComponent("tim-logo", LogoComponent);
    registerServerComponent("bootstrap-panel", BootstrapPanelComponent);
    registerServerComponent("tim-start", FrontPageComponent);
    registerServerComponent("tim-site-header", SiteHeaderComponent);
    registerServerComponent("tim-access-countdown", AccessCountdownComponent);
    registerServerComponent("tim-goto-link", GotoLinkComponent);
    registerServerComponent("tim-print-button", PrintButtonComponent);
    registerServerComponent("tim-time-left", TimeLeftComponent);
    registerServerComponent("tim-countdown", CountdownComponent);
    registerServerComponent("tim-sidebar-menu", SidebarMenuComponent);
    registerServerComponent("tim-draw-toolbar", DrawToolbarComponent);
    registerServerComponent("tim-draw-canvas", DrawCanvasComponent);
    registerServerComponent("tim-index", DirectoryListComponent);
    registerServerComponent("tim-template-list", TemplateListComponent);
    registerServerComponent(
        "tim-view-range-navigation",
        ViewRangeNavigationComponent
    );
    registerServerComponent("tim-help-par-content", HelpParContent);
    registerServerComponent("tim-duration-picker", DurationPickerComponent);
    registerServerComponent("tim-message-view", TimMessageViewComponent);
    registerServerComponent("gamification-map", GamificationMapComponent);
    registerServerComponent("tim-mark-all-as-read", MarkAllAsReadComponent);
    registerServerComponent("tim-self-expire", SelfExpireComponent);
    registerServerComponent("tim-search-button", SearchButtonComponent);
    registerServerComponent("tim-role-info", RoleInfoComponent);
    registerServerComponent("tim-participant-list", ParticipantListComponent);
    registerServerComponent("tim-course-manager", CourseManagerComponent);
    registerServerComponent("tim-rights-editor", RightsEditorComponent);
    registerServerComponent("tim-manage", ManageComponent);
    registerServerComponent("tim-user-list", UserListComponent);
}

/**
 * Registers the components that are used as custom elements.
 * The browser creates these whenever the element appears on the page.
 */
function registerCustomElements() {
    registerElement("tim-plugin-loader", PluginLoaderComponent);
    registerElement("tim-par-ref", ParRefComponent);
    registerElement("tim-add-member", AddMemberComponent);
    registerElement("manage-read-receipt", ManageReadReceiptComponent);
    registerElement("tim-user-profile", UserProfileComponent);
    registerElement("tim-spell-error", SpellErrorComponent);
}

registerServerComponents();
registerCustomElements();

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
    await Promise.all(moduleLoads);

    const moduleRef = await platformBrowserDynamic().bootstrapModule(
        AppModule,
        {applicationProviders: [provideZoneChangeDetection()]}
    );
    timLogTime("Angular bootstrap done", "main.ts");
    const zone = moduleRef.injector.get(NgZone);

    // Remove the parts of the page that are hidden in this view before anything is created in them.
    const hide = getVisibilityVars() as Record<string, boolean | undefined>;
    document.querySelectorAll("[data-tim-hide]").forEach((e) => {
        if (hide[e.getAttribute("data-tim-hide")!]) {
            e.remove();
        } else {
            e.removeAttribute("data-tim-hide");
        }
    });

    const itemRights = (globals.curr_item?.rights ?? {}) as unknown as Record<
        string,
        boolean | undefined
    >;
    document.querySelectorAll("[data-tim-require-right]").forEach((e) => {
        if (!itemRights[e.getAttribute("data-tim-require-right")!]) {
            e.remove();
        }
    });

    // The document view is plain TypeScript that runs outside the Angular zone;
    // it asks Angular to update the views with refreshUI when needed.
    new RootCtrl().$onInit();
    const isDocumentView = document.querySelector("tim-view") != null;
    let view: ViewCtrl | undefined;
    if (isDocumentView) {
        view = new ViewCtrl();
        view.$onInit();
        // The view is set up before any components are created in the document,
        // so the components can rely on it (e.g. on the paragraph structure).
        view.$postLink();
    }

    // The components of the server-rendered HTML must be created before the custom elements are defined:
    // after that, the page also has elements that Angular has rendered.
    zone.run(() => {
        createServerComponents(document.body);
        defineRegisteredElements(moduleRef.injector);
        attachDraggables(document.body);
    });
    refreshUI();
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
