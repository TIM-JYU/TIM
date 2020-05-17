import {getUrlParams} from "tim/util/utils";
import {IDocumentGlobals, someglobals, SomeGlobals} from "tim/util/globals";
import {Users} from "tim/user/userService";
import {timApp} from "./app";

export interface IVisibilityVars {
    footer?: boolean;
    siteheader?: boolean;
    sidebar?: boolean;
    header?: boolean;
    bookmarks?: boolean;
    getquestion?: boolean;
    hamburger?: boolean;
    index?: boolean;
    lecturer?: boolean;
    lecturetab?: boolean;
    links?: boolean;
    login?: boolean;
    logo?: boolean;
    search?: boolean;
    settings?: boolean;
    unilogo?: boolean;
    velps?: boolean;
    hakaLogin?: boolean;
    emailLogin?: boolean;
    signup?: boolean;
    passwordRecovery?: boolean;
    userMenuOptions?: boolean;
    editLine?: boolean;
    noteBadgeButton?: boolean;
    headerNav?: boolean;
    headerDocumentActions?: boolean;
}


function isDocumentGlobals(g: SomeGlobals): g is IDocumentGlobals {
    return "docSettings" in g;
}


function hideLinkStuff(hide: IVisibilityVars) {
    hide.links = true;
    hide.bookmarks = true;
    hide.settings = true;
    hide.hamburger = true;
    hide.header = true;
}

function hideParsOnlyStuff(hide: IVisibilityVars) {
    hide.bookmarks = true;
    hide.getquestion = true;
    hide.hamburger = true;
    hide.index = true;
    hide.lecturer = true;
    hide.lecturetab = true;
    hide.links = true;
    hide.login = Users.isLoggedIn();
    hide.logo = true;
    hide.search = true;
    hide.settings = true;
    hide.unilogo = true;
    hide.velps = true;
    hide.header = true;
    hide.sidebar = true;
    hide.siteheader = Users.isLoggedIn();
    hide.footer = true;
}

function hideTopButtonsStuff(hide: IVisibilityVars) {
    hide.logo = true;
    hide.search = true;
    hide.unilogo = true;
    hide.header = true;
    // hide.login = true; // TODO: Should login be hidden or not?
}

function hideExamModeElements(hide: IVisibilityVars) {
    hide.links = true;
    hide.bookmarks = true;
    hide.settings = true;
    hide.headerNav = true;
    hide.headerDocumentActions = true;
    hide.search = true;
    hide.userMenuOptions = true;
    hide.editLine = true;
    hide.noteBadgeButton = true;
    hide.footer = true;
}

function hideSideMenu(hide: IVisibilityVars) {
    hide.hamburger = true;
    hide.bookmarks = true;
    hide.index = true;
    hide.settings = true;
}

export function getVisibilityVars() {
    const params = getUrlParams();
    const g = someglobals();
    let hide: IVisibilityVars = {};
    if (isDocumentGlobals(g)) {
        if (g.hideLinks) {
            hideLinkStuff(hide);
        }
        if (g.parsOnly) {
            hideParsOnlyStuff(hide);
        }
        if (g.hideTopButtons) {
            hideTopButtonsStuff(hide);
        }
        if (g.docSettings.login) {
            hide = {...hide, ...g.docSettings.login?.hide};
        }
        if (g.exam_mode) {
            hideExamModeElements(hide);
        }
        if (g.hide_sidemenu) {
            hideSideMenu(hide);
        }
    }

    // If bookmarks are globally disabled, don't show the tab.
    if (g.bookmarks == null) {
        hide.bookmarks = true;
    }

    if (!g.config.hakaEnabled) {
        hide.hakaLogin = true;
    }

    if (!g.config.emailRegistrationEnabled) {
        hide.signup = true;
        hide.passwordRecovery = true;
    }

    if (params.get("hide_top_buttons")) {
        hideTopButtonsStuff(hide);
    }
    if (params.get("hide_links")) {
        hideLinkStuff(hide);
    }
    if (params.get("pars_only")) {
        hideParsOnlyStuff(hide);
    }
    return hide;
}

timApp.component("timRoot", {
    controller: class {
        private hide = getVisibilityVars();
    },
    template: `<div ng-transclude style="display: flex; min-height: 100vh; flex-direction: column"></div>`,
    transclude: true,
});
