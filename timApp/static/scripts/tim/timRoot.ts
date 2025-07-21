import {getUrlParams, to2} from "tim/util/utils";
import {
    documentglobals,
    genericglobals,
    isDocumentGlobals,
    someglobals,
} from "tim/util/globals";
import {Users} from "tim/user/userService";
import type {BookmarksComponent} from "tim/sidebarmenu/util/bookmarks.component";
import {setRoot} from "tim/rootinstance";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {timApp} from "tim/app";
import {showTosAgreementDialog} from "tim/ui/showTosAgreementDialog";
import type {IDocument} from "tim/item/IItem";

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
    scoreBoard?: boolean;
    messageListCreate?: boolean;
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
    hide.scoreBoard = true;
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
        if (g.requires_login) {
            hide.footer = false;
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
    }

    if (!g.config.messageListsEnabled) {
        hide.messageListCreate = true;
    }

    if (!g.config.passwordResetEnabled) {
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

    // Don't show Search UI if user is not logged in.
    if (!Users.isLoggedIn()) {
        hide.search = true;
    }

    return hide;
}

export class RootCtrl {
    public bookmarksCtrl: BookmarksComponent | undefined;
    private hide = getVisibilityVars();

    private tos_accepted_str: string | null = "";
    private tos_modified_date_str: string | null = "";
    private inTosPage: boolean = false;

    registerBookmarks(bookmarksCtrl: BookmarksComponent) {
        this.bookmarksCtrl = bookmarksCtrl;
    }

    $onInit() {
        setRoot(this);
        const g = genericglobals();
        if (g.config.hosts && Users.isLoggedIn()) {
            if (!g.config.hosts.allowed.includes(location.hostname)) {
                let message;
                for (const [key, val] of Object.entries(
                    g.config.hosts.warnings
                )) {
                    if (location.hostname.startsWith(key)) {
                        message = val;
                        break;
                    }
                }
                if (!message) {
                    message = g.config.hosts.defaultwarning;
                }
                const u = Users.getCurrent();
                const name = (
                    u.last_name ??
                    u.real_name?.split(" ")[0] ??
                    ""
                ).toLowerCase();
                message = message.replace(/LASTNAME/g, name);
                if (name) {
                    let letter;
                    if (name.length === 1) {
                        letter = name;
                    } else if (name.startsWith("k")) {
                        if (name.localeCompare("ko") < 0) {
                            letter = "ka";
                        } else {
                            letter = "ko";
                        }
                    } else {
                        letter = name[0];
                    }
                    message = message.replace(/LETTER/g, letter);
                    localStorage.clear(); // Make sure the dialog is shown in default position.
                    void showMessageDialog(message);
                }
            }
        }
        this.checkTosAcceptance();
    }

    private checkTosAcceptance() {
        this.tos_modified_date_str = documentglobals().latest_tos_date;
        this.tos_accepted_str = documentglobals().current_user.tos_accepted_at;
        const currentPage: IDocument | undefined = documentglobals().curr_item;
        if (currentPage) {
            this.inTosPage =
                documentglobals().curr_item.path ===
                documentglobals().footerDocs.termsOfService;
        }

        if (
            this.tos_modified_date_str &&
            Users.isRealUser() &&
            !this.inTosPage
        ) {
            if (!this.tos_accepted_str) {
                this.openTosDialog();
            } else if (
                Date.parse(this.tos_accepted_str) <
                Date.parse(this.tos_modified_date_str)
            ) {
                this.openTosDialog();
            }
        }
    }

    private async openTosDialog() {
        await to2(showTosAgreementDialog());
    }
}

timApp.component("timRoot", {
    controller: RootCtrl,
    template: `<div ng-transclude style="display: flex; min-height: 100vh; flex-direction: column"></div>`,
    transclude: true,
});
