import {Component, Input, OnInit} from "@angular/core";
import {IMenuTab, TabEntry} from "tim/sidebarmenu/menu-tab.directive";
import {Users, UserService} from "tim/user/userService";
import {ViewCtrl} from "tim/document/viewctrl";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {LectureController} from "tim/lecture/lectureController";
import {showTagDialog} from "tim/item/tagCtrl";
import {IDocument} from "tim/item/IItem";

@Component({
    selector: "settings-tab",
    template: `
        <ng-container>
            <h5 i18n>Help</h5>
            <a i18n-title title="Open TIM-guide" href="/view/tim/TIM-ohjeet" i18n>User guide</a>
        </ng-container>
        <ng-container *ngIf="users.isLoggedIn()">
            <h5 i18n>Customize</h5>
            <a href="/settings" i18n>Customize TIM</a>
        </ng-container>
        <ng-container *ngIf="showFolderSettings && showRelevance">
            <h5 i18n>Folder settings</h5>
            <button class="timButton btn-block"
                    i18n-title title="Set item relevance value"
                    (click)="openRelevanceEditDialog()"
                    i18n>
                Edit relevance (<span i18n-tooltip tooltip="Current relevance value">{{currentRelevance}}</span>)
            </button>
        </ng-container>
        <ng-container *ngIf="!(vctrl && vctrl.item && !vctrl.item.isFolder)">
            <h5 i18n>Search</h5>
            <button class="timButton btn-block"
                    i18n-title title="Search with tags"
                    (click)="searchWithTags()"
                    i18n>Search with tags
            </button>
        </ng-container>
        <ng-container *ngIf="users.isLoggedIn() && vctrl && !vctrl.item.isFolder">
            <h5 i18n>Document settings</h5>
            <a i18n-title title="Toggle between showing full and partitioned document"
               (click)="toggleViewRange()">
                <ng-container *ngIf="isFullPage; else showInFull" i18n>Show page in parts</ng-container>
                <ng-template #showInFull i18n>Show page in full</ng-template>
            </a>
            <a class="same-line" i18n-title title="Open document partitioning settings" (click)="openViewRangeMenu()">
                <span class="glyphicon glyphicon-cog"></span>
            </a>

            <button *ngIf="vctrl.item.rights.editable && isFullPage"
                    class="timButton btn-block"
                    (click)="vctrl.editingHandler.editSettingsPars()"
                    i18n>Edit settings
            </button>
            <button *ngIf="vctrl.item.rights.manage"
                    class="timButton btn-block"
                    title="Set item relevance value" i18n-title
                    (click)="openRelevanceEditDialog()"
                    i18n>
                Edit relevance (<span i18n-tooltip tooltip="Current relevance value">{{currentRelevance}}</span>)
            </button>
            <button class="timButton btn-block"
                    title="Mark all paragraphs of the document as read" i18n-title
                    (click)="markAllAsRead()"
                    i18n>Mark all as read
            </button>
            <button class="timButton btn-block"
                    title="Mark all paragraphs of the document as read" i18n-title
                    (click)="markTranslated()"
                    i18n>Mark all as translated
            </button>
        </ng-container>
        <ng-container *ngIf="lctrl.lectureSettings.inLecture">
            <h5 i18n>Lecture settings</h5>
            <div class="checkbox">
                <label i18n><input type="checkbox" [(ngModel)]="lctrl.lectureSettings.useWall"> Show wall</label>
            </div>
            <div *ngIf="!lctrl.isLecturer" class="checkbox">
                <label i18n><input type="checkbox" [(ngModel)]="lctrl.lectureSettings.useQuestions"> Show
                    questions</label>
            </div>
            <div *ngIf="lctrl.isLecturer" class="checkbox">
                <label i18n><input type="checkbox" [(ngModel)]="lctrl.lectureSettings.useAnswers"> Show answers</label>
            </div>
            <!--            <div class="checkbox">-->
            <!--                <label i18n><input type="checkbox" [(ngModel)]="lctrl.lectureSettings."> Show 'not polling' dialog</label>-->
            <!--            </div>-->
        </ng-container>

        <!--        TODO: check rights for given options-->
        <ng-container *ngIf="vctrl && vctrl.item && !vctrl.item.isFolder">
            <h5 class="same-line" i18n>Print document</h5>
            <a class="same-line" href="https://tim.jyu.fi/view/tim/ohjeita/tulostusohje">
                <span class="glyphicon glyphicon-question-sign" title="Printing help" i18n-title></span>
            </a>
            <button class="timButton btn-block"
                    title="Print using LaTeX (best quality)" i18n-title
                    (click)="printDocument()"
                    i18n>Print document
            </button>
            <button class="timButton btn-block"
                    title="Print via browser's print dialog" i18n-title
                    (click)="cssPrint()"
                    i18n>Browser print
            </button>

            <h5 class="same-line" i18n>Document tags</h5>
            <a class="same-line" href="https://tim.jyu.fi/view/tim/ohjeita/opettajan-ohje#kurssikoodi">
                    <span class="glyphicon glyphicon-question-sign"
                          title="Teachers' help for course code" i18n-title></span>
            </a>
            <button *ngIf="vctrl.item.rights.manage"
                    class="timButton btn-block"
                    title="Add and remove document tags" i18n-title
                    (click)="addTag()"
                    i18n>Edit tags
            </button>
            <button class="timButton btn-block"
                    title="Search with tags" i18n-title
                    (click)="searchWithTags()"
                    i18n>Search with tags
            </button>
            <button *ngIf="userBelongsToTeachersOrIsAdmin"
                    class="timButton btn-block"
                    title="Set document as a course main page" i18n-title
                    (click)="openCourseDialog()"
                    i18n>Set as course
            </button>

            <h5 *ngIf="isMinutesOrInvitation" class="same-line" i18n>Memos/Minutes</h5>
            <button *ngIf="enableCreateExtractsButton"
                    class="timButton btn-block"
                    title="Create extracts" i18n-title
                    (click)="createMinuteExtracts()"
                    i18n>Create extracts
            </button>
            <button *ngIf="enableCreateMinutesButton"
                    class="timButton btn-block"
                    title="Create minutes" i18n-title
                    (click)="createMinutes()"
                    i18n>Create minutes
            </button>
            <button *ngIf="isMinutesOrInvitation"
                    class="timButton btn-block"
                    title="Display attachments, check their validity, and merge them into single file." i18n-title
                    (click)="mergePdf()"
                    i18n>Merge attachments
            </button>
        </ng-container>

        <ng-container *ngIf="users.isGroupAdmin() || linkedGroups.length > 0">
            <h5 i18n>Groups</h5>
            <ng-container *ngIf="linkedGroups.length > 0">
                <h6 *ngIf="!sisugroupPath; else courseGroupsLink" i18n>Linked course groups</h6>
                <ng-template #courseGroupsLink i18n>Linked <a href="/view/{{sisugroupPath}}">course groups</a>
                </ng-template>
                <ul class="list-unstyled">
                    <li *ngFor="let group of linkedGroups"><a href="/view/{{group.path}}">{{group.title}}</a></li>
                </ul>
            </ng-container>
            <ng-container *ngIf="users.isGroupAdmin()">
                <button class="timButton btn-block"
                        title="Create a new group" i18n-title
                        (click)="createGroup()"
                        i18n>Create a new group
                </button>
                <a href="/view/groups" i18n>Browse existing groups</a>
            </ng-container>
        </ng-container>
    `,
})
export class SettingsTabComponent implements OnInit, IMenuTab {
    @Input() entry!: TabEntry;
    users: UserService = Users;
    showFolderSettings: boolean = false;
    showRelevance: boolean = true;
    currentRelevance?: number;
    vctrl?: ViewCtrl = vctrlInstance;
    isFullPage: boolean = true;
    lctrl: LectureController = this.vctrl?.lectureCtrl ?? LectureController.createAndInit(this.vctrl);
    linkedGroups: IDocument[] = [];
    sisugroupPath?: string;

    constructor() {
    }

    ngOnInit(): void {
    }

    /**
     * Open relevance edit dialog.
     */
    openRelevanceEditDialog() {
        // if (this.item) {
        //     void showRelevanceEditDialog(this.item);
        // }
    }

    /**
     * Opens tag search dialog.
     */
    searchWithTags() {
        // void showTagSearchDialog();
    }

    /**
     * Partition or unpartition document (starting from the beginning) using user defined piece size.
     */
    async toggleViewRange() {
        // if (!(this.vctrl && this.item)) {
        //     return;
        // }
        // await toggleViewRange(this.item.id, this.pieceSizeSetting);
        // this.currentViewRange = getCurrentViewRange();
        // this.updateIsFullRange();
    }

    /**
     * Open dialog for editing view range settings.
     */
    openViewRangeMenu() {
        // if (this.item) {
        //     void showViewRangeEditDialog(this.item);
        //     this.currentViewRange = getCurrentViewRange();
        //     this.updateIsFullRange();
        // }
    }

    /**
     * Marks all paragraphs of the document as read.
     */
    async markAllAsRead() {
        // if (this.vctrl) {
        //     const r = await to($http.put("/read/" + this.vctrl.item.id, {}));
        //     if (!r.ok) {
        //         await showMessageDialog("Could not mark the document as read.");
        //         return;
        //     }
        //     $(".readline").attr("class", "readline read");
        //     getActiveDocument().refreshSectionReadMarks();
        // }
    }

    async markTranslated() {
        // if (this.vctrl && window.confirm("This will mark all paragraphs in this document as translated. Continue?")) {
        //     const r = await to($http.post<IOkResponse>(`/markTranslated/${this.vctrl.item.id}`, {}));
        //     if (r.ok) {
        //         window.location.reload();
        //     } else {
        //         void showMessageDialog(r.result.data.error);
        //     }
        // }
    }

    /**
     * Opens print dialog.
     */
    async printDocument() {
        // if (!this.vctrl) {
        //     return;
        // }
        // const r = await to($http.get<ITemplateParams>(`/print/templates/${this.vctrl.item.path}`));
        // if (r.ok) {
        //     await showPrintDialog({params: r.result.data, document: this.vctrl.item});
        // }
    }

    cssPrint() {
        // FOR DEBUGGING
        // AutoPageBreak();
        window.print();

        // FOR DEBUGGING
        // UndoAutoPageBreak();
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
     * Opens 'Set as a course' -dialog.
     */
    async openCourseDialog() {
        // if (!this.vctrl) {
        //     return;
        // }
        // await to(showCourseDialog(this.vctrl.item));
        // const r = await to($http.get<IGroupWithSisuPath[]>(`/items/linkedGroups/${this.vctrl.item.id}`));
        // if (r.ok) {
        //     this.updateLinkedGroups(r.result.data);
        // } else {
        //     await showMessageDialog(r.result.data.error);
        // }
    }

    /**
     * Checks whether user belongs to teachers or admins group.
     * @returns {boolean}
     */
    get userBelongsToTeachersOrIsAdmin() {
        return false;
        // if (Users.belongsToGroup(ADMIN_GROUPNAME)) {
        //     return true;
        // }
        // if (Users.belongsToGroup(TEACHERS_GROUPNAME)) {
        //     return true;
        // }
        // return false;
    }

    /**
     * Checks whether the side menu should have a button for creating extracts from minutes in this document.
     * @returns {boolean} Whether the button for creating extracts should be displayed.
     */
    get enableCreateExtractsButton() {
        return false;
        // if (this.docSettings == null || this.docSettings.macros == null || this.vctrl == null) {
        //     return false;
        // }
        //
        // return this.docSettings.macros.knro != null && this.documentMemoMinutes == "minutes" &&
        //     this.vctrl.item.rights.manage;
    }

    createMinuteExtracts() {
        window.location.href = window.location.href.replace("/view/", "/minutes/createMinuteExtracts/");
    }

    /**
     * Checks whether the side menu should have a button for creating minutes in this document.
     * @returns {boolean} Whether the button for creating minutes should be displayed.
     */
    get enableCreateMinutesButton() {
        return false;
        // if (this.docSettings == null || this.docSettings.macros == null || this.vctrl == null) {
        //     return false;
        // }
        //
        // return this.docSettings.macros.knro != null && this.documentMemoMinutes == "memo" &&
        //     this.vctrl.item.rights.manage;
    }

    /**
     * Creates minutes from a IT faculty council meeting invitation
     */
    async createMinutes() {
        // if (!this.vctrl) {
        //     await showMessageDialog("Not in a document");
        //     return;
        // }
        //
        // if (this.docSettings == null || this.docSettings.macros == null || this.docSettings.macros.knro == null) {
        //     await showMessageDialog("The document has no 'knro' macro defined");
        //     return;
        // }
        //
        // const r = await to($http.post<{path: string}>("/minutes/createMinutes", {
        //     item_path: this.vctrl.item.location + "/pk/pk" + this.docSettings.macros.knro,
        //     item_title: "pk" + this.docSettings.macros.knro,
        //     copy: this.vctrl.item.id,
        // }));
        // if (r.ok) {
        //     window.location.href = "/view/" + r.result.data.path;
        // } else {
        //     await showMessageDialog(r.result.data.error);
        // }
    }

    /**
     * Checks if the document is faculty council minutes or a faculty council meeting invitation.
     * @returns {boolean} Whether the document is a faculty council meeting document.
     */
    get isMinutesOrInvitation() {
        return false;
        // if (this.docSettings == null || this.docSettings.macros == null) {
        //     return false;
        // }
        // return this.docSettings.macros.knro != null &&
        //     (this.documentMemoMinutes == "minutes" || this.documentMemoMinutes == "memo");
    }

    mergePdf() {
        // if (!this.vctrl) {
        //     return;
        // }
        // showMergePdfDialog({document: this.vctrl.item});
    }

    async createGroup() {
        // const doc = await showInputDialog({
        //     isInput: InputDialogKind.InputAndValidator,
        //     defaultValue: "",
        //     text: "Enter name of the usergroup",
        //     title: "Create group",
        //     validator: async (s) => {
        //         const r = await to($http.get<IDocument>(`/groups/create/${s}`));
        //         if (r.ok) {
        //             return {ok: true, result: r.result.data};
        //         } else {
        //             return {ok: false, result: r.result.data.error};
        //         }
        //     },
        // });
        // redirectToItem(doc);
    }
}
