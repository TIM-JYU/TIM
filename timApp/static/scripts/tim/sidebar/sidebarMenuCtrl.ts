import {IController} from "angular";
import angular from "angular";
import $ from "jquery";
import {timApp} from "tim/app";
import {showCourseDialog} from "../document/course/courseDialogCtrl";
import {showMergePdfDialog} from "../document/minutes/mergePdfCtrl";
import {ViewCtrl} from "../document/viewctrl";
import {showTagDialog} from "../item/tagCtrl";
import {showTagSearchDialog} from "../item/tagSearchCtrl";
import {ILecture, ILectureListResponse2} from "../lecture/lecturetypes";
import {ITemplate, showPrintDialog} from "../printing/printCtrl";
import {showMessageDialog} from "../ui/dialog";
import {ADMIN_GROUPNAME, TEACHERS_GROUPNAME} from "../user/IUser";
import {Users, UserService} from "../user/userService";
import {$http, $uibModal, $window} from "../util/ngimport";
import {Require, to} from "../util/utils";
import {getActiveDocument} from "../document/document";

/**
 * FILL WITH SUITABLE TEXT
 * @module sidebarMenuCtrl
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

export class SidebarMenuCtrl implements IController {
    private currentLecturesList: ILecture[];
    private futureLecturesList: ILecture[];
    private pastLecturesList: ILecture[];
    private users: UserService;
    private leftSide: JQuery;
    private active: number;
    private lastTab: number;
    private vctrl?: Require<ViewCtrl>;
    private bookmarks: {};
    private isDocumentMinutes: boolean = false;
    private docSettings?: {macros?: {knro?: string}};

    constructor() {
        this.currentLecturesList = [];
        this.futureLecturesList = [];
        this.pastLecturesList = [];
        this.users = Users;
        this.bookmarks = $window.bookmarks; // from base.html
        this.leftSide = $(".left-fixed-side");

        this.active = -1;
        if ($window.showIndex) {
            this.active = 0;
        } else if (Users.isLoggedIn()) {
            // make bookmarks tab active
            this.active = 6;
        }
        this.lastTab = this.active;

        this.updateLeftSide();
        $($window).resize(() => this.updateLeftSide());
    }

    $onInit() {
        this.isDocumentMinutes = $window.isMinutes;
        this.docSettings = $window.docSettings;
    }

    updateLeftSide() {
        if ($("#menuTabs").is(":visible")) {
            this.leftSide.css("min-width", "12em");
        } else {
            this.leftSide.css("min-width", "0");
        }
    }

    bookmarkTabSelected(isSelected: boolean) {
        const tabContent = $("#menuTabs").find(".tab-content");
        if (isSelected) {
            // The dropdown menu is clipped if it's near right side of the menu without applying this hack
            // Also the dropdown menu causes vertical scrollbar to appear without specifying height
            tabContent.css("height", "calc(100vh - 51.2833px)");
            tabContent.css("overflow-x", "visible");
            tabContent.css("overflow-y", "visible");
        } else {
            tabContent.css("height", "auto");
            tabContent.css("overflow-x", "hidden");
            tabContent.css("overflow-y", "auto");
        }
    }

    showSidebar() {
        const tabs = $("#menuTabs");
        if (tabs.is(":visible")) {
            if (this.active != null) {
                this.lastTab = this.active;
                this.active = -1; // this will set the value to null and remove the "selected" state from tab
                if ($(".device-xs").is(":visible") || $(".device-sm").is(":visible")) {
                    tabs.hide();
                    this.leftSide.css("min-width", "0");
                }
            } else {
                this.active = this.lastTab;
            }
        } else {
            tabs.show();
            this.leftSide.css("min-width", "12em");
            tabs.attr("class", "");
            if (this.active == null) {
                this.active = this.lastTab || 0;
            }
        }
    }

    async toggleLectures() {
        if (!this.vctrl) {
            await showMessageDialog("Not currently in a document view.");
            return;
        }
        const response = await $http<ILectureListResponse2>({
            url: "/getAllLecturesFromDocument",
            method: "GET",
            params: {doc_id: this.vctrl.docId},
        });
        const lectures = response.data;
        this.currentLecturesList = lectures.currentLectures;
        this.futureLecturesList = lectures.futureLectures;
        this.pastLecturesList = lectures.pastLectures;
    }

    /**
     *
     * @param settings_data : print settings
     */
    printDocument(settings_data: {}) {
        const api_address_for_templates = "/print/templates/" + $window.item.path;
        $http.get<ITemplate[]>(api_address_for_templates)
            .then((response) => {
                showPrintDialog({templates: response.data, document: $window.item});
            }, (response) => {
                console.log(response.toString());
            });
    }

    cssPrint() {
        // FOR DEBUGGING
        // AutoPageBreak();
        // TODO: Remove "any" after TypeScript 2.8.2 release
        (window as any).print();

        // FOR DEBUGGING
        // UndoAutoPageBreak();
    }

    createMinuteExtracts() {
        window.location.href = window.location.href.replace("/view/", "/minutes/createMinuteExtracts/" );
    }

    /**
     * Checks whether the side menu should have a button for creating extracts from minutes in this document.
     * @returns {boolean} Whether the button for creating extracts should be displayed.
     */
    enableCreateExtractsButton(): boolean {
        if (this.docSettings == null || this.docSettings.macros == null || this.vctrl == null) {
            return false;
        }

        return this.docSettings.macros.knro != null && this.isDocumentMinutes &&
            this.vctrl.item.rights.manage;
    }

    /**
     * Checks whether the side menu should have a button for creating minutes in this document.
     * @returns {boolean} Whether the button for creating minutes should be displayed.
     */
    enableCreateMinutesButton(): boolean {
        if (this.docSettings == null || this.docSettings.macros == null || this.vctrl == null) {
            return false;
        }

        return this.docSettings.macros.knro != null && !this.isDocumentMinutes &&
            this.vctrl.item.rights.manage;
    }

    /**
     * Checks if the document is faculty council minutes or a faculty council meeting invitation.
     * @returns {boolean} Whether the document is a faculty council meeting document.
     */
    isMinutesOrInvitation(): boolean {
        if (this.docSettings == null || this.docSettings.macros == null) {
            return false;
        }
        return this.docSettings.macros.knro != null;
    }

    /**
     * Creates minutes from a IT faculty council meeting invitation
     */
    createMinutes() {
        if (!this.vctrl) {
            void showMessageDialog("Not in a document");
            return;
        }

        if (this.docSettings == null || this.docSettings.macros == null || this.docSettings.macros.knro == null) {
            void showMessageDialog("The document has no 'knro' macro defined");
            return;
        }

        $http.post<{path: string}>("/minutes/createMinutes", angular.extend({
            item_path: this.vctrl.item.location + "/PK/PK" + this.docSettings.macros.knro,
            item_title: "PK" + this.docSettings.macros.knro,
            copy: this.vctrl.item.id,
        })).then((response) => {
            $window.location.href = "/view/" + response.data.path;
        }, (response) => {
            void showMessageDialog(response.data.error);
        });
    }

    stampPdf() {
    }

    mergePdf() {
        if (!this.vctrl) {
            return;
        }
        showMergePdfDialog({document: this.vctrl.item});
    }

    /**
     * Opens tag editing dialog.
     */
    addTag() {
        if (!this.vctrl) {
            return;
        }
        void showTagDialog(this.vctrl.item);
    }

    /**
     * Opens tag search dialog.
     */
    searchWithTags() {
        if (!this.vctrl) {
            return;
        }
        void showTagSearchDialog(this.vctrl.item);
    }

    /**
     * Start page specific version of the tag search opening.
     */
    searchWithTagsStart() {
        void showTagSearchDialog($window.item);
    }

    /**
     * Opens 'Set as a course' -dialog.
     */
    openCourseDialog() {
        if (!this.vctrl) {
            return;
        }
        void showCourseDialog(this.vctrl.item);
    }

    /**
     * Checks whether user belongs to teachers or admins group.
     * @returns {boolean}
     */
    userBelongsToTeachersOrIsAdmin() {
        if (Users.belongsToGroup(ADMIN_GROUPNAME)) {
            return true;
        }
        if (Users.belongsToGroup(TEACHERS_GROUPNAME)) {
            return true;
        }
        return false;
    }

    /**
     * Marks all paragraphs of the document as read.
     * @returns {Promise<void>}
     */
    private async markAllAsRead() {
        if (this.vctrl) {
                const [err, response] = await to($http.put("/read/" + this.vctrl.item.id, {}));
                if (err) {
                    $window.alert("Could not mark the document as read.");
                    return;
                }
                $(".readline").attr("class", "readline read");
                getActiveDocument().refreshSectionReadMarks();
        }
    }
}

timApp.component("timSidebarMenu", {
    controller: SidebarMenuCtrl,
    require: {
        lctrl: "?^timLecture",
        vctrl: "?^timView",
    },
    template: "<div ng-transclude></div>",
    transclude: true,
});
