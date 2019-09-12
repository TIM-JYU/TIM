/* tslint:disable:max-line-length */
import {IController} from "angular";
import $ from "jquery";
import {ngStorage} from "ngstorage";
import {timApp} from "tim/app";
import {showCourseDialog} from "../document/course/courseDialogCtrl";
import {getActiveDocument} from "../document/document";
import {IDocSettings} from "../document/IDocSettings";
import {showMergePdfDialog} from "../document/minutes/mergePdfCtrl";
import {ViewCtrl} from "../document/viewctrl";
import {DocumentOrFolder, IDocument, isRootFolder, redirectToItem} from "../item/IItem";
import {IRelevanceResponse} from "../item/relevanceEdit";
import {showRelevanceEditDialog} from "../item/relevanceEditDialog";
import {showTagDialog} from "../item/tagCtrl";
import {showTagSearchDialog} from "../item/tagSearchCtrl";
import {ILecture, ILectureListResponse2} from "../lecture/lecturetypes";
import {ITemplateParams, showPrintDialog} from "../printing/printCtrl";
import {showConsentDialog} from "../ui/consent";
import {showMessageDialog} from "../ui/dialog";
import {showInputDialog} from "../ui/inputDialog";
import {ADMIN_GROUPNAME, IGroup, TEACHERS_GROUPNAME} from "../user/IUser";
import {setConsent} from "../user/settingsCtrl";
import {Users, UserService} from "../user/userService";
import {someglobals} from "../util/globals";
import {$http, $localStorage} from "../util/ngimport";
import {IOkResponse, Require, to} from "../util/utils";

export interface IHeader {
    id: string;
    level: number;
    text: string;
}

export interface IHeaderDisplayIndexItem {
    h1: IHeader;
    h2List: IHeader[];
    closed: boolean;
}

export interface IGroupWithSisuPath extends IGroup {
    sisugroup_path: string | null;
}

export type HeaderIndexItem = [IHeader, IHeader[]];

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
    private documentMemoMinutes: string | undefined;
    private docSettings?: IDocSettings;
    private hideLinks: boolean = false;
    private hideTopButtons: boolean = false;
    private displayIndex?: IHeaderDisplayIndexItem[];
    // Consent types:
    // number corresponds to values of ConsentType
    // null means that the user has approved only cookies (but has not seen the data collection options)
    // undefined means that the user has not acknowledged anything yet
    private storage: ngStorage.StorageService & {consent: null | undefined | number};
    private currentRelevance?: number;
    private showRelevance: boolean = true;
    private showFolderSettings: boolean = false;
    private linkedGroups: IDocument[] = [];
    private item?: DocumentOrFolder;
    private sisugroupPath?: string;

    constructor() {
        const g = someglobals();
        this.item = g.item;
        this.currentLecturesList = [];
        this.futureLecturesList = [];
        this.pastLecturesList = [];
        this.users = Users;
        this.bookmarks = g.bookmarks; // from base.html
        this.leftSide = $(".left-fixed-side");
        this.hideLinks = "hideLinks" in g ? g.hideLinks : false;
        this.hideTopButtons = "hideTopButtons" in g ? g.hideTopButtons : false;
        this.displayIndex = "index" in g ? this.formDisplayIndex(g.index) : undefined;
        this.active = -1;
        if ("showIndex" in g ? g.showIndex : false) {
            this.active = 0;
        } else if (Users.isLoggedIn()) {
            // make bookmarks tab active
            this.active = 6;
        }
        this.lastTab = this.active;
        this.storage = $localStorage.$default({
            consent: undefined,
        });

        this.updateLeftSide();
        $(window).resize(() => this.updateLeftSide());
        if ("linked_groups" in g && g.linked_groups != null) {
            this.updateLinkedGroups(g.linked_groups);
        }
    }

    private updateLinkedGroups(groups: IGroupWithSisuPath[]) {
        this.linkedGroups = [];
        for (const g of groups) {
            if (g.admin_doc != null) {
                this.linkedGroups.push(g.admin_doc);
            }
        }
        // TODO: Theoretically there can be multiple different courses.
        //  Should display a list in that case.
        const gr = groups.find((g) => g.sisugroup_path != null);
        if (gr && gr.sisugroup_path != null) {
            this.sisugroupPath = gr.sisugroup_path;
        }
    }

    async $onInit() {
        const g = someglobals();
        this.documentMemoMinutes = "memoMinutes" in g ? g.memoMinutes : undefined;
        this.docSettings = "docSettings" in g ? g.docSettings : undefined;
        void this.getCurrentRelevance();
        if (this.item) {
            this.showFolderSettings = this.users.isLoggedIn() && this.item.isFolder;
        }
        // await this.processConsent();
    }

    private async processConsent() {
        const current = Users.getCurrent();
        if ((this.storage.consent === undefined && !Users.isLoggedIn()) || (Users.isLoggedIn() && current.consent == null)) {
            this.storage.consent = await showConsentDialog(Users.isLoggedIn());
        }
        if (this.storage.consent != null && current.consent == null && Users.isLoggedIn()) {
            await setConsent(this.storage.consent);
        }
        if (current.consent != null && this.storage.consent == null) {
            this.storage.consent = current.consent;
        }
    }

    updateLeftSide() {
        if ($("#menuTabs").is(":visible")) {
            this.leftSide.css("min-width", "12em");
        } else {
            this.leftSide.css("min-width", "0");
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
     * Opens print dialog.
     */
    async printDocument() {
        if (!this.vctrl) {
            return;
        }
        const r = await to($http.get<ITemplateParams>(`/print/templates/${this.vctrl.item.path}`));
        if (r.ok) {
            await showPrintDialog({params: r.result.data, document: this.vctrl.item});
        }
    }

    cssPrint() {
        // FOR DEBUGGING
        // AutoPageBreak();
        window.print();

        // FOR DEBUGGING
        // UndoAutoPageBreak();
    }

    createMinuteExtracts() {
        window.location.href = window.location.href.replace("/view/", "/minutes/createMinuteExtracts/");
    }

    /**
     * Checks whether the side menu should have a button for creating extracts from minutes in this document.
     * @returns {boolean} Whether the button for creating extracts should be displayed.
     */
    enableCreateExtractsButton(): boolean {
        if (this.docSettings == null || this.docSettings.macros == null || this.vctrl == null) {
            return false;
        }

        return this.docSettings.macros.knro != null && this.documentMemoMinutes == "minutes" &&
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

        return this.docSettings.macros.knro != null && this.documentMemoMinutes == "memo" &&
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
        return this.docSettings.macros.knro != null &&
            (this.documentMemoMinutes == "minutes" || this.documentMemoMinutes == "memo");
    }

    /**
     * Creates minutes from a IT faculty council meeting invitation
     */
    async createMinutes() {
        if (!this.vctrl) {
            await showMessageDialog("Not in a document");
            return;
        }

        if (this.docSettings == null || this.docSettings.macros == null || this.docSettings.macros.knro == null) {
            await showMessageDialog("The document has no 'knro' macro defined");
            return;
        }

        const r = await to($http.post<{path: string}>("/minutes/createMinutes", {
            item_path: this.vctrl.item.location + "/pk/pk" + this.docSettings.macros.knro,
            item_title: "pk" + this.docSettings.macros.knro,
            copy: this.vctrl.item.id,
        }));
        if (r.ok) {
            window.location.href = "/view/" + r.result.data.path;
        } else {
            await showMessageDialog(r.result.data.error);
        }
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
        void showTagSearchDialog();
    }

    /**
     * Open relevance edit dialog.
     */
    openRelevanceEditDialog() {
        if (this.item) {
            void showRelevanceEditDialog(this.item);
        }
    }

    /**
     * Opens 'Set as a course' -dialog.
     */
    async openCourseDialog() {
        if (!this.vctrl) {
            return;
        }
        await to(showCourseDialog(this.vctrl.item));
        const r = await to($http.get<IGroupWithSisuPath[]>(`/items/linkedGroups/${this.vctrl.item.id}`));
        if (r.ok) {
            this.updateLinkedGroups(r.result.data);
        } else {
            await showMessageDialog(r.result.data.error);
        }
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
     */
    private async markAllAsRead() {
        if (this.vctrl) {
            const r = await to($http.put("/read/" + this.vctrl.item.id, {}));
            if (!r.ok) {
                await showMessageDialog("Could not mark the document as read.");
                return;
            }
            $(".readline").attr("class", "readline read");
            getActiveDocument().refreshSectionReadMarks();
        }
    }

    /**
     * Choose header style class based on its contents and close state.
     * @param header Header containing h1 and possibly h2 list.
     * @returns {string} Header class.
     */
    private headerClass(header: IHeaderDisplayIndexItem) {
        if (header.h2List.length > 0) {
            if (header.closed) {
                return "exp";
            } else {
                return "col";
            }
        } else {
            return "basic";
        }
    }

    /**
     * Add closed states to header index.
     * @param index Index containing headers.
     * @returns {HeaderIndexItem[]} Index with added closed states.
     */
    private formDisplayIndex(index: HeaderIndexItem[]) {
        if (!index || !index[0]) {
            return [];
        }

        let closedState = true;
        const headerCount = this.getHeaderCount(index);
        if (index.length === 1 || headerCount < 40) {
            closedState = false;
        }

        const displayIndex: IHeaderDisplayIndexItem[] = [];
        for (const h of index) {
            if (!h[0]) {
                continue;
            }
            const h1: IHeader = h[0];
            let h2List: IHeader[] = [];
            if (h[1]) {
                h2List = h[1];
            }
            displayIndex.push({closed: closedState, h1: h1, h2List: h2List});
        }
        return displayIndex;
    }

    /**
     * Count the total number of headers and subheaders.
     * @param index Index containing the headers.
     * @returns {number} Total count of all headers.
     */
    private getHeaderCount(index: HeaderIndexItem[]) {
        let temp = 0;
        for (const h of index) {
            if (!h[0]) {
                continue;
            }
            temp += 1;
            if (!h[1]) {
                continue;
            }
            temp += h[1].length;
        }
        return temp;
    }

    /**
     * Fetches active relevance value. If root dir (id = -1), skip and hide relevance dir.
     */
    private async getCurrentRelevance() {
        if (this.item && !isRootFolder(this.item)) {
            const r = await to($http.get<IRelevanceResponse>(`/items/relevance/get/${this.item.id}`));
            if (r.ok) {
                this.currentRelevance = r.result.data.relevance.relevance;
            }
        } else {
            this.showRelevance = false; // Don't show in root folder.
        }
    }

    private async markTranslated() {
        if (this.vctrl && window.confirm("This will mark all paragraphs in this document as translated. Continue?")) {
            const r = await to($http.post<IOkResponse>(`/markTranslated/${this.vctrl.item.id}`, {}));
            if (r.ok) {
                window.location.reload();
            } else {
                void showMessageDialog(r.result.data.error);
            }
        }
    }

    private async createGroup() {
        const doc = await showInputDialog({
            defaultValue: "",
            text: "Enter name of the usergroup",
            title: "Create group",
            validator: async (s) => {
                const r = await to($http.get<IDocument>(`/groups/create/${s}`));
                if (r.ok) {
                    return {ok: true, result: r.result.data};
                } else {
                    return {ok: false, result: r.result.data.error};
                }
            },
        });
        redirectToItem(doc);
    }
}

timApp.component("timSidebarMenu", {
    controller: SidebarMenuCtrl,
    require: {
        lctrl: "?^timLecture",
        vctrl: "?^timView",
    },
    template: `<div class="btn btn-default btn-sm pull-left" ng-click="$ctrl.showSidebar()" title="Show menu"
     ng-if="!$ctrl.hideTopButtons">
    <i class="glyphicon glyphicon-menu-hamburger" title="Click to open sidebar-menu"></i>
</div>
<uib-tabset id="menuTabs" active="$ctrl.active" class="hidden-sm hidden-xs">
    <uib-tab ng-if="!$ctrl.hideLinks && $ctrl.users.isLoggedIn()" index="6">
        <uib-tab-heading title="bookmarks">
            <i class="glyphicon glyphicon-bookmark" title="bookmarks"></i>
        </uib-tab-heading>
        <h5>Bookmarks</h5>
        <bookmarks data="$ctrl.bookmarks"></bookmarks>
    </uib-tab>

    <uib-tab index="1" ng-if="!$ctrl.hideLinks">
        <uib-tab-heading>
            <i class="glyphicon glyphicon-cog" title="document settings"></i>
        </uib-tab-heading>
        <div>
            <h5>Help</h5>
            <a title="Open TIM-ohjeet" href="/view/tim/TIM-ohjeet">User guide</a>
        </div>
        <div ng-if="$ctrl.users.isLoggedIn()">
            <h5>Customize</h5>
            <a href="/settings">Customize TIM</a>
        </div>
        <div ng-if="$ctrl.showFolderSettings && $ctrl.showRelevance">
            <h5>Folder settings</h5>
            <button class="timButton btn-block" title="Set item relevance value" ng-if="$ctrl.showRelevance"
                    ng-click="$ctrl.openRelevanceEditDialog()">
                Edit relevance (<span uib-tooltip="Current relevance value">{{$ctrl.currentRelevance}}</span>)
            </button>
        </div>
        <div ng-show="!($ctrl.vctrl.item && !$ctrl.vctrl.item.isFolder)">
            <h5>Search</h5>
            <button class="timButton btn-block" title="Search with tags"
                    ng-click="$ctrl.searchWithTags()">Search with tags
            </button>
        </div>
        <div ng-if="$ctrl.users.isLoggedIn() && $ctrl.vctrl && !$ctrl.vctrl.item.isFolder">
            <h5>Document settings</h5>
            <button ng-if="$ctrl.vctrl.item.rights.editable"
                    class="timButton btn-block"
                    ng-click="$ctrl.vctrl.editingHandler.editSettingsPars()">Edit settings
            </button>
            <button class="timButton btn-block" ng-if="$ctrl.vctrl.item.rights.manage"
                    title="Set item relevance value"
                    ng-click="$ctrl.openRelevanceEditDialog()">
                Edit relevance (<span uib-tooltip="Current relevance value">{{$ctrl.currentRelevance}}</span>)
            </button>
            <button class="timButton btn-block"
                    ng-click="$ctrl.markAllAsRead()"
                    title="Mark all paragraphs of the document as read">Mark all as read
            </button>
            <button ng-if="$ctrl.vctrl.isTranslation()" class="timButton btn-block"
                    ng-click="$ctrl.markTranslated()"
                    title="Mark document as translated">Mark all as translated
            </button>
        </div>
        <div ng-show="$ctrl.lctrl.lectureSettings.inLecture">
            <h5>Lecture settings</h5>
            <div class="checkbox">
                <label>
                    <input type="checkbox" ng-model="$ctrl.lctrl.lectureSettings.useWall"> Show wall
                </label>
            </div>
            <div ng-show="!isLecturer" class="checkbox">
                <label>
                    <input type="checkbox" ng-model="$ctrl.lctrl.lectureSettings.useQuestions"> Show questions
                </label>
            </div>
            <div ng-show="$ctrl.lctrl.isLecturer" class="checkbox">
                <label>
                    <input type="checkbox" ng-model="$ctrl.lctrl.lectureSettings.useAnswers"> Show answers
                </label>
            </div>
            <div ng-show="$ctrl.lctrl.isLecturer" class="checkbox">
                <label>
                    <input type="checkbox" ng-model="$ctrl.lctrl.lectureSettings.useNotPollingDialog"> Show 'not
                    polling' dialog
                </label>
            </div>
        </div>
        <!-- TODO: check rights -->
        <div ng-show="$ctrl.vctrl.item && !$ctrl.vctrl.item.isFolder">
            <h5 style="display: inline-block">Print document</h5>
            <a style="display: inline-block" href="https://tim.jyu.fi/view/tim/ohjeita/tulostusohje">
                <span class="glyphicon glyphicon-question-sign" title="print help"></span>
            </a>
            <button class="timButton btn-block" title="Print using LaTeX => best quality"
                    ng-click="$ctrl.printDocument()">Print document
            </button>
            <button class="timButton btn-block" title="Print using Browser own printing capabilities"
                    ng-click="$ctrl.cssPrint()">Browser print
            </button>
            <h5 style="display: inline-block">Document tags</h5>
            <a style="display: inline-block"
               href="https://tim.jyu.fi/view/tim/ohjeita/opettajan-ohje#kurssikoodi">
                <span class="glyphicon glyphicon-question-sign" title="teachers help for couse code"></span>
            </a>
            <button class="timButton btn-block" ng-show="$ctrl.vctrl.item.rights.manage"
                    title="Add and remove document tags" ng-click="$ctrl.addTag()">Edit tags
            </button>
            <button class="timButton btn-block" title="Search with tags"
                    ng-click="$ctrl.searchWithTags()">Search with tags
            </button>
            <button class="timButton btn-block" ng-show="$ctrl.userBelongsToTeachersOrIsAdmin()"
                    title="Set document as a course main page"
                    ng-click="$ctrl.openCourseDialog()">Set as a course
            </button>
            <h5 style="display: inline-block" ng-show="$ctrl.isMinutesOrInvitation()">Memos/Minutes</h5>
            <button class="timButton btn-block" ng-show="$ctrl.enableCreateExtractsButton()"
                    ng-click="$ctrl.createMinuteExtracts()">Create extracts
            </button>
            <button class="timButton btn-block" ng-show="$ctrl.enableCreateMinutesButton()"
                    ng-click="$ctrl.createMinutes()">Create minutes
            </button>
            <button class="timButton btn-block" ng-show="$ctrl.isMinutesOrInvitation()"
                    ng-click="$ctrl.mergePdf()">Merge attachments
            </button>
        </div>
        <div ng-if="$ctrl.users.isGroupAdmin() || $ctrl.linkedGroups.length > 0">
            <h5>Groups</h5>
            <div ng-if="$ctrl.linkedGroups.length > 0">
                <h6 ng-if="!$ctrl.sisugroupPath">Linked course groups</h6>
                <h6 ng-if="$ctrl.sisugroupPath">Linked <a href="/view/{{$ctrl.sisugroupPath}}">course groups</a></h6>
                <ul class="list-unstyled">
                    <li ng-repeat="l in $ctrl.linkedGroups"><a href="/view/{{ l.path }}">{{ l.title }}</a></li>
                </ul>
            </div>
            <div ng-if="$ctrl.users.isGroupAdmin()">
                <button class="timButton btn-block" title="Create a new group"
                        ng-click="$ctrl.createGroup()">Create a new group
                </button>
                <a href="/view/groups">Browse existing groups</a>
            </div>
        </div>
    </uib-tab>

    <uib-tab ng-if="$ctrl.displayIndex.length > 0" index="0">
        <uib-tab-heading>
            <i class="glyphicon glyphicon-book" title="document index"></i>
        </uib-tab-heading>
        <h5>Index <a href="#" title="Go to top" class="pull-right">Go to top</a></h5>
        <ul class="subexp">
            <li ng-class="$ctrl.headerClass(h)" ng-repeat="h in ::$ctrl.displayIndex"
                ng-click="h.closed = !h.closed">
                <a class="a{{::h.h1.level}}" href="#{{::h.h1.id}}" target="_self" ng-click="$event.stopPropagation()">
                    {{::h.h1.text}}</a>
                <ul class="list-unstyled" ng-if="!h.closed" ng-click="$event.stopPropagation()">
                    <li class="basic" ng-repeat="h2 in h.h2List">
                        <a class="a{{::h2.level}}" href="#{{::h2.id}}" target="_self">{{::h2.text}}</a>
                    </li>
                </ul>
            </li>
        </ul>
    </uib-tab>

    <uib-tab index="2" ng-if="!$ctrl.hideLinks && $ctrl.lctrl.lectureSettings.lectureMode"
             select="$ctrl.toggleLectures()">
        <uib-tab-heading>
            <i class="glyphicon glyphicon-education" title="lecture"></i>
        </uib-tab-heading>
        <h5>Current Lectures</h5>
        <ul>
            <li ng-repeat="lecture in $ctrl.currentLecturesList">
                <a href="/showLectureInfo/{{ lecture.lecture_id }}">{{ lecture.lecture_code }}</a>
                <button class="timButton btn-xs" ng-if="$ctrl.lctrl.lecture.lecture_id != lecture.lecture_id"
                        value="Join"
                        ng-click="$ctrl.lctrl.joinLecture(lecture)">Join
                </button>
            </li>
            <li ng-show="$ctrl.currentLecturesList.length == 0"><p>No current lectures</p></li>
        </ul>
        <h5>Coming Lectures</h5>
        <ul>
            <li ng-repeat="lecture in $ctrl.futureLecturesList">
                <a href="/showLectureInfo/{{ lecture.lecture_id }}">{{ lecture.lecture_code }}</a>
            </li>
            <li ng-show="$ctrl.futureLecturesList.length == 0"><p>No coming lectures</p></li>
        </ul>
        <h5>Past Lectures</h5>
        <ul>
            <li ng-repeat="lecture in $ctrl.pastLecturesList">
                <a href="/showLectureInfo/{{ lecture.lecture_id }}">{{ lecture.lecture_code }}</a>
            </li>
            <li ng-show="$ctrl.pastLecturesList.length == 0"><p>No past lectures</p></li>
        </ul>
    </uib-tab>

    <uib-tab ng-if="!$ctrl.hideLinks && $ctrl.lctrl.lectureSettings.inLecture && !$ctrl.lctrl.isLecturer" index="4"
             select="$ctrl.lctrl.getQuestionManually()">
        <uib-tab-heading>
            <i class="glyphicon glyphicon-question-sign" title="get question"></i>
        </uib-tab-heading>
        Loading question manually...
    </uib-tab>

    <uib-tab ng-if="!$ctrl.hideLinks && $ctrl.lctrl.isLecturer && $ctrl.lctrl.lectureSettings.inLecture" index="5">
        <uib-tab-heading>
            <i class="glyphicon glyphicon-user" title="lecture participants"></i>
        </uib-tab-heading>
        <h5>People logged-in: <span
                ng-bind="$ctrl.lctrl.lecturerTable.length + $ctrl.lctrl.studentTable.length">None</span></h5>
        <h5>Lecturers (<span ng-bind="$ctrl.lctrl.lecturerTable.length">None</span>)</h5>
        <ul>
            <li ng-repeat="lecturer in $ctrl.lctrl.lecturerTable">
                {{ lecturer.user.name }} > {{ lecturer.active | timreldate }}
            </li>
        </ul>
        <h5>Students (<span ng-bind="$ctrl.lctrl.studentTable.length">None</span>)</h5>
        <p ng-show="$ctrl.lctrl.lecturerTable.length == 0">No lecturers</p>
        <ul>
            <li ng-repeat="person in $ctrl.lctrl.studentTable track by $index">
                {{ person.user.name }} > {{ person.active | timreldate }}
            </li>
        </ul>
        <p ng-show="$ctrl.lctrl.studentTable.length == 0">No students</p>
    </uib-tab>
</uib-tabset>`,
});
