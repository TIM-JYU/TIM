import angular, {IController, IPromise, IScope} from "angular";
import $ from "jquery";
import moment from "moment";
import ngs, {ngStorage} from "ngstorage";
import {timApp} from "tim/app";
import {AreaHandler} from "tim/document/areas";
import {Document, setActiveDocument} from "tim/document/document";
import {ClipboardHandler, IClipboardMeta} from "tim/document/editing/clipboard";
import * as interceptor from "tim/document/interceptor";
import {NotesHandler} from "tim/document/notes";
import {getElementByParId, Paragraph, saveCurrentScreenPar} from "tim/document/parhelpers";
import {ParmenuHandler} from "tim/document/parmenu";
import * as popupMenu from "tim/document/popupMenu";
import {QuestionHandler} from "tim/document/question/questions";
import {initReadings} from "tim/document/readings";
import {timLogTime} from "tim/util/timTiming";
import {isPageDirty, markAsUsed, markPageNotDirty, to} from "tim/util/utils";
import {BookmarksController, IBookmarkGroup} from "../bookmark/bookmarks";
import {IPluginInfoResponse, ParCompiler} from "../editor/parCompiler";
import {IDocument, ITag, TagType} from "../item/IItem";
import {LectureController} from "../lecture/lectureController";
import {TimTableController} from "../plugin/timTable";
import {initCssPrint} from "../printing/cssPrint";
import {showMessageDialog} from "../ui/dialog";
import {IUser} from "../user/IUser";
import {Users} from "../user/userService";
import {$compile, $filter, $http, $interval, $localStorage, $timeout, $window} from "../util/ngimport";
import {ReviewController} from "../velp/reviewController";
import {EditingHandler} from "./editing/editing";
import {PendingCollection} from "./editing/edittypes";
import {onClick} from "./eventhandlers";
import {PopupMenuController} from "./popupMenu";
import {RefPopupHandler} from "./refpopup";
import {MenuFunctionEntry} from "./viewutils";

markAsUsed(ngs, popupMenu, interceptor);

export interface ITimComponent {
    getName: () => string | undefined;
    getContent: () => string;
    getGroups: () => string[];
    belongsToGroup(group: string): boolean;
    save: () => string | undefined;
    getPar: () => Paragraph
    setPluginWords?: (words: string[]) => void;
}

export interface IInsertDiffResult {
    type: "insert";
    after_id: string | null;
    content: IPluginInfoResponse;
}

export interface IReplaceDiffResult {
    type: "replace";
    content: IPluginInfoResponse;
    start_id: string;
    end_id: string | null;
}

export interface IDeleteDiffResult {
    type: "delete";
    content: IPluginInfoResponse;
    start_id: string;
    end_id: string | null;
}

export interface IChangeDiffResult {
    type: "change";
    content: IPluginInfoResponse;
    id: string;
}

export type DiffResult = IInsertDiffResult | IReplaceDiffResult | IDeleteDiffResult | IChangeDiffResult;

const courseFolder = "My courses";

export class ViewCtrl implements IController {
    private notification: string = "";
    private videoElements: {[name: string]: HTMLVideoElement | undefined} = {};
    clipMeta: IClipboardMeta = {allowPasteContent: false, allowPasteRef: false, empty: true};
    selection: {pars?: JQuery; start?: Paragraph; end?: Paragraph} = {};
    public par: JQuery | undefined;

    private static $inject = ["$scope"];

    public lectureMode: boolean;
    public inLecture: boolean;
    public item: IDocument;
    public docId: number;

    private hidePending: boolean;
    public group: any;
    public scope: IScope;
    public noBrowser: boolean;
    public docVersion: [number, number];
    private crumbs: any;
    private startIndex: number;
    public users: IUser[];
    public teacherMode: boolean;
    private velpMode: boolean;

    private timTables: {[parId: string]: TimTableController} = {};
    private timComponents: Map<string, ITimComponent> = new Map();

    private pendingUpdates: PendingCollection;
    private document: Document;
    private showRefresh: boolean;
    public selectedUser: IUser;
    public editing: boolean = false;
    public $storage: ngStorage.StorageService & {defaultAction: string | null; noteAccess: string};
    private liveUpdates: number;
    private oldWidth: number;
    public defaultAction: MenuFunctionEntry | undefined;
    public reviewCtrlScope?: IScope & {$ctrl: ReviewController};
    public lectureCtrl?: LectureController;
    public questionHandler: QuestionHandler;
    public areaHandler: AreaHandler;
    public clipboardHandler: ClipboardHandler;
    public editingHandler: EditingHandler;
    public notesHandler: NotesHandler;
    public parmenuHandler: ParmenuHandler;
    public refpopupHandler: RefPopupHandler;
    public popupmenu?: PopupMenuController;

    // To show a button that adds the document to bookmark folder 'My courses'.
    private taggedAsCourse: boolean = false;
    private bookmarksCtrl: BookmarksController | undefined;
    private bookmarked: boolean = false;
    private bookmarks: IBookmarkGroup[] = [];

    // For search box.
    private displaySearch = false;

    constructor(sc: IScope) {
        timLogTime("ViewCtrl start", "view");

        this.noBrowser = $window.noBrowser;
        this.docId = $window.item.id;
        this.docVersion = $window.docVersion;
        this.crumbs = $window.crumbs;
        this.item = $window.item;
        this.startIndex = $window.startIndex;
        this.users = $window.users;
        this.group = $window.group;
        this.teacherMode = $window.teacherMode;
        this.velpMode = $window.velpMode;
        this.lectureMode = $window.lectureMode;
        this.inLecture = $window.in_lecture;
        this.scope = sc;

        this.document = new Document(this.docId);
        setActiveDocument(this.document);

        if (this.users.length > 0) {
            this.selectedUser = this.users[0];
        } else {
            this.selectedUser = Users.getCurrent();
        }
        this.hidePending = false;
        this.pendingUpdates = {};

        $($window).resize((e) => {
            if (e.target === $window as any) {
                const newWidth = $($window).width();
                if (newWidth !== this.oldWidth && newWidth) {
                    this.oldWidth = newWidth;
                    const selected = $(".par.lightselect, .par.selected");
                    if (selected.length > 0) {
                        selected[0].scrollIntoView();
                    }
                }
            }
        });

        this.questionHandler = new QuestionHandler(sc, this);
        this.areaHandler = new AreaHandler(sc, this);
        this.clipboardHandler = new ClipboardHandler(sc, this);
        this.editingHandler = new EditingHandler(sc, this);
        this.notesHandler = new NotesHandler(sc, this);
        this.parmenuHandler = new ParmenuHandler(sc, this);
        this.refpopupHandler = new RefPopupHandler(sc, this);
        initReadings(this);
        initCssPrint();

        // from https://stackoverflow.com/a/7317311
        $(() => {
            this.questionHandler.processQuestions();
            this.setHeaderLinks();
            this.noBeginPageBreak();
            this.document.rebuildSections();
            window.addEventListener("beforeunload", (e) => {
                saveCurrentScreenPar();

                if (!this.editing) {
                    return undefined;
                }

                const msg = "You are currently editing something. Are you sure you want to leave the page?";

                (e || $window.event).returnValue = msg; // Gecko + IE
                return msg; // Gecko + Webkit, Safari, Chrome etc.
            });
        });

        onClick("html", ($this, e) => {
            // Clicking anywhere
            const tagName = (e.target as Element).tagName.toLowerCase();
            const jqTarget = $(e.target);
            const ignoreTags = ["button", "input", "label", "i"];
            const ignoreClasses = [
                "areaeditline",
                "draghandle",
                "editline",
                "menu-icon",
                "modal-dialog",
            ];

            let curElement = jqTarget;
            let limit = 10;
            while (curElement != null) {
                if (this.editing || ignoreTags.includes(tagName) || curElement.attr("position") === "absolute") {
                    return false;
                }

                for (const c of ignoreClasses) {
                    if (curElement.hasClass(c)) {
                        return false;
                    }
                }

                curElement = curElement.parent();
                if (--limit < 0) {
                    break;
                }
            }

            this.closePopupIfOpen();

            return false;

        }, true);

        // If you add 'mousedown' to bind, scrolling upon opening the menu doesn't work on Android
        $("body,html").bind("scroll wheel DOMMouseScroll mousewheel", (e) => {
            if (e.which > 0 || e.type === "mousedown" || e.type === "mousewheel") {
                $("html,body").stop();
            }
        });

        this.$storage = $localStorage.$default({
            defaultAction: "Close menu",
            noteAccess: "everyone",
        });

        $window.allowMove = false;
        this.oldWidth = $($window).width() || 500;
        this.showRefresh = isPageDirty();
        this.liveUpdates = $window.liveUpdates;

        if (Users.isLoggedIn() && this.liveUpdates) {
            this.startLiveUpdates();
        }

        try {
            const found = $filter("filter")(this.editingHandler.getEditorFunctions(),
                {desc: this.$storage.defaultAction, show: true}, true);
            if (found.length) {
                this.defaultAction = found[0];
            }
        } catch (e) {
        }
        timLogTime("ViewCtrl end", "view");
    }

    public isTranslation() {
        return this.item.src_docid != null && this.item.src_docid !== this.item.id;
    }

    getVideo(followid: string): HTMLVideoElement | undefined {
        return this.videoElements[followid];
    }

    registerVideo(followid: string, v: HTMLVideoElement): void {
        this.videoElements[followid] = v;
    }

    startLiveUpdates() {
        const sc = this.scope;
        const origLiveUpdates = this.liveUpdates;
        if (!origLiveUpdates) {
            return;
        }
        let stop: IPromise<any> | undefined;
        stop = $interval(async () => {
            const response = await $http.get<{version: [number, number], diff: DiffResult[], live: number}>("/getParDiff/" + this.docId + "/" + this.docVersion[0] + "/" + this.docVersion[1]);
            this.docVersion = response.data.version;
            this.liveUpdates = response.data.live; // TODO: start new loop by this or stop if None
            const replaceFn = async (d: DiffResult, parId: string) => {
                const compiled = await ParCompiler.compile(d.content, sc);
                const e = getElementByParId(parId);
                e.replaceWith(compiled);
            };
            const afterFn = async (d: DiffResult, parId: string) => {
                const compiled = await ParCompiler.compile(d.content, sc);
                const e = getElementByParId(parId);
                e.after(compiled);
            };
            const beforeFn = async (d: DiffResult, e: JQuery) => {
                const compiled = await ParCompiler.compile(d.content, sc);
                e.before(compiled);
            };
            for (let i = 0; i < response.data.diff.length; ++i) {
                const d = response.data.diff[i];
                if (d.type === "delete") {
                    if (d.end_id != null) {
                        getElementByParId(d.start_id).nextUntil(getElementByParId(d.end_id)).addBack().remove();
                    } else {
                        getElementByParId(d.start_id).nextAll(".par").addBack().remove();
                    }
                } else if (d.type === "replace") {
                    const first = getElementByParId(d.start_id);
                    if (d.start_id !== d.end_id) {
                        if (d.end_id != null) {
                            first.nextUntil(getElementByParId(d.end_id)).remove();
                        } else {
                            first.nextAll(".par").remove();
                        }
                    }
                    replaceFn(d, d.start_id);
                } else if (d.type === "insert") {
                    if (d.after_id == null) {
                        beforeFn(d, $(".par:first"));
                    } else {
                        afterFn(d, d.after_id);
                    }
                } else if (d.type === "change") {
                    replaceFn(d, d.id);
                }
            }
            $timeout(() => {
                this.document.rebuildSections();
            }, 1000);
            if (this.liveUpdates != origLiveUpdates) { // if value hase changes, stop and start new poll
                if (stop) {
                    $interval.cancel(stop);
                    stop = undefined;
                }
                $timeout(() => {
                    this.startLiveUpdates();
                }, 100);
            }
        }, Math.max(1000 * this.liveUpdates, 1000));
    }

    $onInit() {
        this.scope.$watchGroup([
            () => this.lectureMode,
            () => this.selection.start,
            () => this.selection.end,
            () => this.editing,
            () => this.getEditMode(),
            () => this.clipMeta.allowPasteContent,
            () => this.clipMeta.allowPasteRef,
            () => this.getAllowMove()], (newValues, oldValues, scope) => {
            const par = $(".editline.menuopen").parents(".par");
            this.parmenuHandler.updatePopupMenuIfOpen(this.parmenuHandler.getPopupAttrs(par.length > 0 ? par : undefined));
            if (this.editing) {
                this.notification = "Editor is already open.";
            } else {
                this.notification = "";
            }
        });
        void this.checkIfTaggedAsCourse();
        void this.checkIfBookmarked();
    }

    /**
     * Checks if the document has been tagged as a course and the tag hasn't expired.
     */
    private async checkIfTaggedAsCourse() {
        this.taggedAsCourse = false;
        const r = await to($http.get<ITag[]>(`/tags/getTags/${this.item.path}`));
        if (r.ok) {
            for (const tag of r.result.data) {
                if (isCourse(tag)) {
                    if (isExpired(tag)) {
                        return;
                    } else {
                        this.taggedAsCourse = true;
                        return;
                    }
                }
            }
        }
    }

    /**
     * Marks page as bookmarked if it's in the course bookmark folder.
     * @returns {Promise<void>}
     */
    private async checkIfBookmarked() {
        this.bookmarked = false;
        if (!Users.isLoggedIn()) {
            return;
        }
        const response = await $http.get<IBookmarkGroup[]>("/bookmarks/get");
        this.bookmarks = response.data;
        for (const folder of this.bookmarks) {
            if (folder.name === courseFolder) {
                for (const bookmark of folder.items) {
                    if (bookmark.link === "/view/" + this.item.path) {
                        this.bookmarked = true;
                        return;
                    }
                }
            }
        }
    }

    /**
     * Registers a table controller to the view controller.
     * All table controllers need to register for toggling edit mode of
     * the table to work.
     * @param {TimTableController} controller The table controller.
     * @param {string} parId The ID of the table paragraph.
     */
    public addTable(controller: TimTableController, parId: string) {
        this.timTables[parId] = controller;
    }

    /**
     * Returns a table controller related to a specific table paragraph.
     * @param {string} parId The paragraph's ID.
     * @returns {TimTableController} The table controller related to the given table paragraph, or undefined.
     */
    public getTableControllerFromParId(parId: string) {
        return this.timTables[parId];
    }

    /**
     * Registers an ITimComponent to the view controller by its name attribute if it has one
     * @param {ITimComponent} component The component to be registered
     */
    public addTimComponent(component: ITimComponent) {
        let name = component.getName();
        if (name) this.timComponents.set(name, component);
    }

    /**
     * Returns an ITimComponent where register ID matches the given string
     * @param {string} name The register ID of the ITimComponent
     * @returns {ITimComponent | undefined} Matching component if there was one
     */
    public getTimComponentByName(name: string): ITimComponent | undefined {
        return this.timComponents.get(name);
    }

    /**
     * Gets ITimComponents nested within specified area component
     * @param{string} group name of the area object
     * @returns {ITimComponent[]} List of ITimComponents nested within the area
     */
    public getTimComponentsByGroup(group: string): ITimComponent[] {
        let returnList: ITimComponent[] = [];
        for(const [k, v] of this.timComponents)
        {
            if (v.belongsToGroup(group)) returnList.push(v);
        }
        return returnList;
    }

    /**
     * Searches for registered ITimComponent whose ID matches the given regexp
     * @param {string} re The RegExp to be used in search
     * @returns {ITimComponent[]} List of ITimComponents where the ID matches the regexp
     */
    public getTimComponentsByRegex(re: string): ITimComponent[]{
        let returnList: ITimComponent[] = [];
        let reg = new RegExp(re)
        for(const [k, v] of this.timComponents)
        {
            if (reg.test(k)) returnList.push(v);
        }
        return returnList;
    }

    isEmptyDocument() {
        return this.docVersion[0] === 0 && this.docVersion[1] === 0; // TODO can be empty otherwise too
    }

    setReviewCtrlScope(scope: IScope & {$ctrl: ReviewController}) {
        this.reviewCtrlScope = scope;
    }

    reload() {
        markPageNotDirty();
        window.location.reload();
    }

    closeRefreshDlg() {
        this.showRefresh = false;
    }

    changeUser(user: IUser, updateAll: boolean) {
        this.selectedUser = user;
        this.scope.$broadcast("userChanged", {user, updateAll});
    }

    async beginUpdate() {
        const response = await $http.get<{changed_pars: PendingCollection}>("/getUpdatedPars/" + this.docId);
        this.updatePendingPars(response.data.changed_pars);
    }

    pendingUpdatesCount() {
        return Object.keys(this.pendingUpdates).length;
    }

    showUpdateDialog() {
        return !this.hidePending && this.pendingUpdatesCount() > 0;
    }

    updatePendingPars(pars: PendingCollection) {
        angular.extend(this.pendingUpdates, pars);
        this.hidePending = false;
        if (this.pendingUpdatesCount() < 10) {
            this.updatePending();
        }
    }

    updatePending() {
        for (const key in this.pendingUpdates) {
            if (this.pendingUpdates.hasOwnProperty(key)) {
                const par = getElementByParId(key);
                const n = $(this.pendingUpdates[key]);
                par.replaceWith(n);
                const compiled = $($compile(n)(this.scope));
                this.applyDynamicStyles(compiled);
                ParCompiler.processAllMathDelayed(compiled);
            }
        }
        this.document.rebuildSections();
        this.pendingUpdates = {};
        this.questionHandler.processQuestions();
    }

    applyDynamicStyles(par: Paragraph) {
        if ($window.editMode) {
            par.addClass("editmode");

            // Show hidden paragraphs if in edit mode
            par.find(".mdcontent").css("display", "initial");
        }
    }

    setHeaderLinks() {
        const pars = $(".parContent");
        pars.each((index, elem) => {
            const p = $(elem);
            p.find("h1, h2, h3, h4, h5, h6").each((i, e) => {
                const h = $(e);
                const id = h.attr("id");
                if (angular.isDefined(id)) {
                    h.append($("<a>", {
                        text: "#",
                        href: "#" + id,
                        class: "headerlink",
                        title: "Permanent link",
                    }));
                }
            });
        });
    }

    getEditMode() {
        return $window.editMode;
    }

    getAllowMove() {
        return $window.allowMove;
    }

    registerPopupMenu(param: PopupMenuController) {
        this.popupmenu = param;
    }

    async closePopupIfOpen() {
        if (this.popupmenu) {
            this.popupmenu.close();
            await this.popupmenu.closePromise();
            this.popupmenu = undefined;
        }
    }

    /**
     * Adds the current page to course bookmark folder.
     * @returns {Promise<void>}
     */
    async addToBookmarkFolder() {
        if (!Users.isLoggedIn()) {
            showMessageDialog("Log in to bookmark this course");
            return;
        }
        if (!this.bookmarksCtrl) {
            throw new Error("Bookmarkscontroller not registered");
        }
        const bookmark = {group: courseFolder, name: this.item.title, link: "/view/" + this.item.path};
        const response = await $http.post<IBookmarkGroup[]>("/bookmarks/add", bookmark);
        await this.bookmarksCtrl.refresh();
        this.checkIfBookmarked(); // Instead of directly changing boolean this checks if it really was added.
    }

    /**
     * Add bookmark controller.
     * @param {BookmarksController} bookmarksCtrl
     */
    registerBookmarks(bookmarksCtrl: BookmarksController) {
        this.bookmarksCtrl = bookmarksCtrl;
    }

    /**
     * Remove page break from the first header to avoid empty page in browser print.
     */
    private noBeginPageBreak() {
        const headers = $(".parContent h1");
        if (headers.length > 0) {
            headers.first().addClass("no-page-break-before");
            return;
        }
    }
}

/**
 * Checks if the tag type is course code.
 * @param {string} tag
 * @returns {boolean} Whether the tag has course code tag.
 */
function isCourse(tag: ITag) {
    return (tag.type === TagType.CourseCode);
}

/**
 * Checks if the tag has expired.
 * @param {string} tag
 * @returns {boolean} False if the tag has no expiration or hasn't yet expired.
 */
function isExpired(tag: ITag) {
    if (tag.expires) {
        if (tag.expires.diff(moment.now()) < 0) {
            return true;
        }
    } else {
        return false;
    }
}

timApp.component("timView", {
    controller: ViewCtrl,
    template: "<div ng-transclude></div>",
    transclude: true,
});
