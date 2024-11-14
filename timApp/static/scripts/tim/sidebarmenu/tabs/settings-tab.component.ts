import type {OnInit} from "@angular/core";
import {Component, EventEmitter} from "@angular/core";
import type {UserService} from "tim/user/userService";
import {Users} from "tim/user/userService";
import type {ViewCtrl} from "tim/document/viewctrl";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {LectureController} from "tim/lecture/lectureController";
import type {DocumentOrFolder, IDocument} from "tim/item/IItem";
import {isRootFolder, redirectToItem} from "tim/item/IItem";
import type {IMeetingMemoSettings, ISettings} from "tim/util/globals";
import {isDocumentGlobals, someglobals} from "tim/util/globals";
import type {IViewRange} from "tim/document/viewRangeInfo";
import {getCurrentViewRange, toggleViewRange} from "tim/document/viewRangeInfo";
import type {IOkResponse} from "tim/util/utils";
import {
    getTypedStorage,
    getViewName,
    replaceStyle,
    to2,
    toPromise,
} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import {getActiveDocument} from "tim/document/activedocument";
import type {ITemplateParams} from "tim/printing/print-dialog.component";
import type {IGroupWithSisuPath} from "tim/user/IUser";
import {ADMIN_GROUPNAME, TEACHERS_GROUPNAME} from "tim/user/IUser";
import type {IDocSettings} from "tim/document/IDocSettings";
import type {IRelevanceResponse} from "tim/item/relevance-edit.component";
import {showViewRangeEditDialog} from "tim/document/showViewRangeEditDialog";
import {showRelevanceEditDialog} from "tim/item/showRelevanceEditDialog";
import {showTagSearchDialog} from "tim/item/showTagSearchDialog";
import {showCourseDialog} from "tim/document/course/showCourseDialog";
import {showTagDialog} from "tim/item/showTagDialog";
import {showPrintDialog} from "tim/printing/showPrintDialog";
import {showMergePdfDialog} from "tim/document/minutes/showMergePdfDialog";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import * as t from "io-ts";
import {openScheduleDialog} from "tim/document/scheduling/openScheduleDialog";
import {showMessageListCreation} from "tim/messaging/showMessageListCreation";
import type {IVisibilityVars} from "tim/timRoot";
import {getVisibilityVars} from "tim/timRoot";
import {showUserGroupDialog} from "tim/user/showUserGroupDialog";

const DEFAULT_PIECE_SIZE = 20;

type QuickThemeEntry = {
    id: number;
    title: string;
    enabled: boolean;
};

const USER_PREFS_CHANGED: EventEmitter<ISettings> = new EventEmitter();

export function notifyUserPrefsChanged(settings: ISettings) {
    USER_PREFS_CHANGED.emit(settings);
}

@Component({
    selector: "settings-tab",
    template: `
        <ng-template i18n>Document settings</ng-template>
        <ng-container *ngIf="users.isRealUser() && quickThemes.length > 0">
            <h5 i18n>Quick themes</h5>
            <div class="flex flex-wrap gap-1 quick-theme-container">
                <button *ngFor="let theme of quickThemes"
                        class="btn btn-xs" 
                        [class.btn-primary]="theme.enabled"
                        [class.active]="theme.enabled"
                        [class.btn-default]="!theme.enabled"
                        (click)="toggleTheme(theme)"
                        >
                    {{ theme.title }}
                </button>
            </div>
        </ng-container>
        <ng-container>
            <h5 i18n>Help</h5>
            <div class="flex cl">
                <a i18n-title title="Open quick start guide" href="/view/tim/ohjeita/pikaohje" i18n>Quick start</a>
                <a i18n-title title="Open content creator guide" href="/view/tim/ohjeita/ohjeet" i18n>Content creator
                    guide</a>
                <a i18n-title title="Open all guides" href="/view/tim/TIM-ohjeet" i18n>All guides</a>
            </div>
        </ng-container>
        <ng-container *ngIf="users.isRealUser()">
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
        <ng-container *ngIf="item && item.isFolder">
            <h5 i18n>Search</h5>
            <button class="timButton btn-block"
                    i18n-title title="Search with tags"
                    (click)="searchWithTags()"
                    i18n>Search with tags
            </button>
        </ng-container>
        <ng-container *ngIf="users.isLoggedIn() && item && !item.isFolder">
            <h5 i18n>Document settings</h5>
            <a i18n-title title="Toggle between showing full and partitioned document"
               (click)="toggleViewRange()">
                <ng-container *ngIf="isFullPage; else showInFull" i18n>Show page in parts</ng-container>
                <ng-template #showInFull i18n>Show page in full</ng-template>
            </a>
            <a class="same-line spaced" i18n-title title="Open document partitioning settings"
               (click)="openViewRangeMenu()">
                <span class="glyphicon glyphicon-cog"></span>
            </a>
            <button *ngIf="vctrl && isFullPage && item.rights.editable"
                    class="timButton btn-block"
                    (click)="vctrl.editingHandler.editSettingsPars()"
                    i18n>Edit settings
            </button>
            <button *ngIf="item && item.rights.manage"
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
            <button *ngIf="vctrl?.isTranslation()"
                    class="timButton btn-block"
                    title="Mark document as translated" i18n-title
                    (click)="markTranslated()"
                    i18n>Mark all as translated
            </button>
            <button *ngIf="vctrl?.isTranslation()"
                    class="timButton btn-block"
                    title="Mark translations as checked" i18n-title
                    (click)="markTranslationChecked()"
                    i18n>Mark all translations as checked
            </button>
            <button *ngIf="docSettings?.exam_mode && item.rights.manage"
                    class="timButton btn-block"
                    title="Delete all read marks from all users who visited this document" i18n-title
                    (click)="markDocumentUnread()"
                    i18n>Delete all read marks
            </button>
            <span *ngIf="item.rights.editable">
                <a title="Refresh numbering" i18n-title
                   (click)="refreshNumbering(false)"
                   i18n>Refresh numbering
                </a> /
                <a title="Refresh also remote documents numbering" i18n-title
                   (click)="refreshNumbering(true)"
                   i18n>recurse
                </a>
            </span>
            <tim-loading *ngIf="isAutoCounterNumbering"></tim-loading>
        </ng-container>
        <ng-container *ngIf="lctrl.lectureSettings.inLecture">
            <h5 i18n>Lecture settings</h5>
            <div class="checkbox">
                <label i18n><input type="checkbox" [(ngModel)]="lctrl.lectureSettings.useWall"
                                   (ngModelChange)="lctrl.refreshWall()"> Show wall</label>
            </div>
            <div *ngIf="!lctrl.isLecturer" class="checkbox">
                <label i18n>
                    <input type="checkbox" [(ngModel)]="lctrl.lectureSettings.useQuestions"> Show questions
                </label>
            </div>
            <div *ngIf="lctrl.isLecturer" class="checkbox">
                <label i18n><input type="checkbox" [(ngModel)]="lctrl.lectureSettings.useAnswers"> Show answers</label>
            </div>
        </ng-container>

        <ng-container *ngIf="item && !item.isFolder">
            <div>
                <h5 class="same-line" i18n>Print document</h5>
                <a class="same-line spaced" href="https://tim.jyu.fi/view/tim/ohjeita/tulostusohje">
                    <span class="glyphicon glyphicon-question-sign" title="Printing help" i18n-title></span>
                </a>
            </div>
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
            <div>
                <h5 class="same-line" i18n>Document tags</h5>
                <a class="same-line spaced" href="https://tim.jyu.fi/view/tim/ohjeita/opettajan-ohje#kurssikoodi">
                    <span class="glyphicon glyphicon-question-sign"
                          title="Teachers' help for course code" i18n-title></span>
                </a>
            </div>
            <button *ngIf="item && item.rights.manage"
                    class="timButton btn-block"
                    title="Add or remove document tags" i18n-title
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

            <h5 *ngIf="isMinutesOrInvitation" i18n>Memo/Minutes</h5>
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
        <ng-container *ngIf="(users.isGroupAdmin() || users.isSisuTeacher()) && !hideVars.messageListCreate">
            <h5 i18n>Message lists</h5>
            <button class="timButton btn-block"
                    title="Create a new message list"
                    (click)="createMessagelist()"
                    i18n-title
                    i18n
            >Create a new message list
            </button>
            <a href="/view/messagelists" i18n>Browse existing message lists</a>
            <br/>
            <a href="/view/archives" i18n>Browse archives</a>
            <br/>
            <a href="/view/messages/tim-messages" i18n>Browse TIM messages</a>
        </ng-container>

        <ng-container *ngIf="docSettings?.cache && item?.rights?.manage">
            <h5 i18n>Cache</h5>
            <a href="/generateCache/{{ item?.path }}?caddy_nobuffering=1" target="_blank" i18n>Refresh cache</a>
        </ng-container>

        <ng-container *ngIf="users.canScheduleFunctions()">
            <h5 i18n>Scheduled functions</h5>
            <button (click)="openScheduleDialog()" class="timButton btn-block" i18n>Manage scheduled functions</button>
        </ng-container>
    `,
    styleUrls: ["./settings-tab.component.scss"],
})
export class SettingsTabComponent implements OnInit {
    hideVars: IVisibilityVars = getVisibilityVars();
    users: UserService = Users;
    showFolderSettings: boolean = false;
    showRelevance: boolean = true;
    currentRelevance?: number;
    vctrl?: ViewCtrl = vctrlInstance;
    lctrl: LectureController = LectureController.instance;
    isFullPage: boolean = true;
    linkedGroups: IDocument[] = [];
    sisugroupPath?: string;
    item?: DocumentOrFolder;
    docSettings?: IDocSettings;
    memoMinutesSettings?: IMeetingMemoSettings;
    isAutoCounterNumbering: boolean = false;
    quickThemes: QuickThemeEntry[] = [];
    private currentViewRange?: IViewRange;
    private documentMemoMinutes: string | undefined;

    constructor(private http: HttpClient) {
        const globals = someglobals();
        this.item = globals.curr_item;
        this.docSettings = isDocumentGlobals(globals)
            ? globals.docSettings
            : undefined;
        this.documentMemoMinutes = isDocumentGlobals(globals)
            ? globals.memoMinutes
            : undefined;
        this.memoMinutesSettings = isDocumentGlobals(globals)
            ? globals.memoMinutesSettings
            : undefined;
        if (isDocumentGlobals(globals) && globals.linked_groups) {
            this.updateLinkedGroups(globals.linked_groups);
        }
    }

    ngOnInit(): void {
        void this.getCurrentRelevance();
        if (this.item) {
            this.showFolderSettings =
                this.users.isLoggedIn() && this.item.isFolder;
        }
        if (!this.item?.isFolder) {
            this.loadViewRangeSettings();
        }
        if (this.users.isLoggedIn()) {
            void this.loadQuickThemesList();
        }

        USER_PREFS_CHANGED.subscribe(async () => {
            await this.loadQuickThemesList();
        });
    }

    private async loadQuickThemesList() {
        const r = await toPromise(
            this.http.get<QuickThemeEntry[]>("/settings/quickThemes")
        );
        if (r.ok) {
            this.quickThemes = r.result;
        }
    }

    /**
     * Open relevance edit dialog.
     */
    openRelevanceEditDialog() {
        if (this.item) {
            void to2(showRelevanceEditDialog(this.item));
        }
    }

    /**
     * Opens tag search dialog.
     */
    searchWithTags() {
        void to2(showTagSearchDialog());
    }

    /**
     * (Un)partition document (starting from the beginning) using user defined piece size.
     */
    async toggleViewRange() {
        if (!this.item) {
            return;
        }
        await toggleViewRange(
            this.item.id,
            getTypedStorage("pieceSize", t.number) ?? DEFAULT_PIECE_SIZE
        );
        this.currentViewRange = getCurrentViewRange();
        this.updateIsFullRange();
    }

    private updateIsFullRange() {
        this.isFullPage = this.currentViewRange?.is_full ?? true;
    }

    /**
     * Open dialog for editing view range settings.
     */
    openViewRangeMenu() {
        if (!this.item) {
            return;
        }
        void showViewRangeEditDialog(this.item);
        this.currentViewRange = getCurrentViewRange();
        this.updateIsFullRange();
    }

    /**
     * Marks all paragraphs of the document as read.
     */
    async markAllAsRead() {
        if (!this.item) {
            return;
        }
        const r = await toPromise(this.http.put(`/read/${this.item.id}`, {}));
        if (!r.ok) {
            await showMessageDialog(
                $localize`Could not mark the document as read.`
            );
            return;
        }
        const doc = getActiveDocument();
        doc.hideReadMarks();
        doc.refreshSectionReadMarks();
    }

    async markDocumentUnread() {
        if (!this.item) {
            return;
        }
        const r = await toPromise(
            this.http.get<number>(`/read/${this.item.id}/groupCount`)
        );
        let message = $localize`This document is in exam mode. Marking document unread will remove read marks from all users! Continue?`;
        if (r.ok) {
            message +=
                "\n" + $localize`This will affect ${r.result} users in total.`;
        }
        await this.confirmPost(message, `/markAllUnread/${this.item.id}`);
    }

    async markTranslated() {
        await this.confirmPost(
            $localize`This will mark all paragraphs in this document as translated. Continue?`,
            `/markTranslated/${this.item!.id}`
        );
    }

    async markTranslationChecked() {
        await this.confirmPost(
            $localize`This will mark the translations in all paragraphs in this document as checked. Continue?`,
            `/markChecked/${this.item!.id}`
        );
    }

    private async confirmPost(message: string, url: string) {
        if (!this.item) {
            return;
        }
        const shouldMark = window.confirm(message);
        if (!shouldMark) {
            return;
        }
        const r = await toPromise(this.http.post<IOkResponse>(url, {}));
        if (r.ok) {
            window.location.reload();
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }

    /**
     * Opens print dialog.
     */
    async printDocument() {
        if (!this.item) {
            return;
        }
        const r = await toPromise(
            this.http.get<ITemplateParams>(`/print/templates/${this.item.path}`)
        );
        if (r.ok) {
            await to2(showPrintDialog({document: this.item, params: r.result}));
        }
    }

    cssPrint() {
        window.print();
    }

    async refreshNumbering(recurse: boolean) {
        if (!this.item) {
            return;
        }
        this.isAutoCounterNumbering = true;
        const r = await toPromise(
            this.http.post(
                `/print/numbering/${this.item.path}`,
                {},
                {
                    params: {
                        recurse,
                    },
                }
            )
        );
        if (r.ok) {
            location.reload();
        } else {
            this.isAutoCounterNumbering = false;
            await showMessageDialog(r.result.error.error);
        }
    }

    /**
     * Opens tag editing dialog.
     */
    addTag() {
        if (!this.item) {
            return;
        }
        void to2(showTagDialog(this.item));
    }

    /**
     * Opens 'Set as a course' -dialog.
     */
    async openCourseDialog() {
        if (!this.item) {
            return;
        }
        await to2(showCourseDialog(this.item));
        const r = await toPromise(
            this.http.get<IGroupWithSisuPath[]>(
                `/items/linkedGroups/${this.item.id}`
            )
        );
        if (r.ok) {
            this.updateLinkedGroups(r.result);
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }

    private updateLinkedGroups(groups: IGroupWithSisuPath[]) {
        this.linkedGroups = [];
        for (const group of groups) {
            if (group.admin_doc) {
                this.linkedGroups.push(group.admin_doc);
            }
        }
        // TODO: Theoretically there can be multiple different courses.
        //  Should display a list in that case.
        const gr = groups.find((g) => g.sisugroup_path != null);
        if (gr?.sisugroup_path) {
            this.sisugroupPath = gr.sisugroup_path;
        }
    }

    /**
     * Checks whether user belongs to teachers or admins group.
     * @returns {boolean}
     */
    get userBelongsToTeachersOrIsAdmin() {
        return (
            this.users.belongsToGroup(ADMIN_GROUPNAME) ||
            this.users.belongsToGroup(TEACHERS_GROUPNAME)
        );
    }

    /**
     * Checks whether the side menu should have a button for creating extracts from minutes in this document.
     * @returns {boolean} Whether the button for creating extracts should be displayed.
     */
    get enableCreateExtractsButton() {
        return (
            this.memoMinutesSettings?.knro &&
            this.documentMemoMinutes == "minutes" &&
            this.item?.rights.manage
        );
    }

    createMinuteExtracts() {
        window.location.href = window.location.href.replace(
            "/view/",
            "/minutes/createMinuteExtracts/"
        );
    }

    /**
     * Checks whether the side menu should have a button for creating minutes in this document.
     * @returns {boolean} Whether the button for creating minutes should be displayed.
     */
    get enableCreateMinutesButton() {
        return (
            this.memoMinutesSettings?.knro &&
            this.documentMemoMinutes == "memo" &&
            this.item?.rights.manage
        );
    }

    /**
     * Creates minutes from a IT faculty council meeting invitation
     */
    async createMinutes() {
        if (!this.item) {
            await showMessageDialog($localize`Not in a document`);
            return;
        }

        if (!this.memoMinutesSettings?.knro) {
            await showMessageDialog(
                $localize`The document has no 'knro' macro defined`
            );
            return;
        }

        const r = await toPromise(
            this.http.post<{path: string}>("/minutes/createMinutes", {
                item_path: `${this.item.location}/pk/pk${this.memoMinutesSettings.knro}`,
                item_title: `pk${this.memoMinutesSettings.knro}`,
                copy: this.item.id,
            })
        );
        if (r.ok) {
            window.location.href = `/view/${r.result.path}`;
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }

    /**
     * Checks if the document is faculty council minutes or a faculty council meeting invitation.
     * @returns {boolean} Whether the document is a faculty council meeting document.
     */
    get isMinutesOrInvitation() {
        return (
            this.memoMinutesSettings?.knro &&
            this.item?.rights.manage &&
            (this.documentMemoMinutes == "minutes" ||
                this.documentMemoMinutes == "memo")
        );
    }

    mergePdf() {
        if (this.item) {
            void to2(showMergePdfDialog({document: this.item}));
        }
    }

    async createGroup(): Promise<void> {
        const doc = await to2(showUserGroupDialog());
        if (doc.ok) {
            redirectToItem(doc.result);
        }
    }

    createMessagelist() {
        void to2(showMessageListCreation());
    }

    /**
     * Fetches active relevance value. If root dir (id = -1), skip and hide relevance dir.
     */
    private async getCurrentRelevance() {
        if (this.item && !isRootFolder(this.item)) {
            const r = await toPromise(
                this.http.get<IRelevanceResponse>(
                    `/items/relevance/get/${this.item.id}`
                )
            );
            if (r.ok) {
                this.currentRelevance = r.result.relevance.relevance;
            }
        } else {
            this.showRelevance = false; // Don't show in root folder.
        }
    }

    /**
     * Get piece size from local storage and current view range from document globals.
     */
    private loadViewRangeSettings() {
        this.currentViewRange = getCurrentViewRange();
        this.updateIsFullRange();
    }

    openScheduleDialog() {
        if (!this.item) {
            return;
        }
        void to2(openScheduleDialog(this.item));
    }

    async toggleTheme(theme: QuickThemeEntry) {
        theme.enabled = !theme.enabled;

        const r = await toPromise(
            this.http.post<{style_path: string}>(`/settings/quickThemes`, {
                themes: this.quickThemes
                    .filter((qe) => qe.enabled)
                    .map((qe) => qe.id),
                view_route: getViewName(),
                doc_id: this.item?.id,
            })
        );

        if (r.ok) {
            const path = r.result.style_path;
            replaceStyle("user-prefs-style", path);
            replaceStyle("document-style", path);
        }
    }
}
