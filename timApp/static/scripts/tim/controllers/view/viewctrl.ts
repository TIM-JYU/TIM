import angular, {IController, IPromise, IScope} from "angular";
import $ from "jquery";
import ngs, {ngStorage} from "ngstorage";
import {timApp} from "tim/app";
import {AreaHandler} from "tim/controllers/view/areas";
import {ClipboardHandler} from "tim/controllers/view/clipboard";
//noinspection TypeScriptPreferShortImport
import {initIndex} from "tim/controllers/view/index";
import * as interceptor from "tim/controllers/view/interceptor";
import {INoteEditorOptions, NotesHandler} from "tim/controllers/view/notes";
import {Area, getElementByParId, Paragraph, Paragraphs, ParOrArea} from "tim/controllers/view/parhelpers";
import {closeOptionsWindow, ParmenuHandler} from "tim/controllers/view/parmenu";
import {QuestionHandler} from "tim/controllers/view/questions";
import {initReadings} from "tim/controllers/view/readings";
import * as popupMenu from "tim/directives/popupMenu";
import {timLogTime} from "tim/timTiming";
import {applyMixins, Coords, isPageDirty, markAsUsed, markPageNotDirty, nameofFactoryCtrl} from "tim/utils";
import {initCssPrint} from "../../cssPrint";
import {INameAreaOptions} from "../../directives/nameArea";
import {IExtraData, IParResponse} from "../../edittypes";
import {IItem} from "../../IItem";
import {IUser} from "../../IUser";
import {$compile, $filter, $http, $interval, $localStorage, $timeout, $window} from "../../ngimport";
import {IPluginInfoResponse, ParCompiler} from "../../services/parCompiler";
import {Users} from "../../services/userService";
import {LectureController} from "../lectureController";
import * as printctrl from "../printCtrl";
import {ReviewController} from "../reviewController";
import {Document, setActiveDocument} from "./document";
import {EditingHandler, IParEditorAttrs, IParEditorOptions} from "./editing";
import {onClick} from "./eventhandlers";
import {MenuFunction, MenuFunctionCollection, MenuFunctionEntry} from "./IViewCtrl";
import {IPopupMenuAttrs} from "./parmenu";
import {IRefPopupAttrs, RefPopupHandler} from "./refpopup";

markAsUsed(ngs, popupMenu, interceptor);

markAsUsed(popupMenu, interceptor, printctrl);

export type PendingCollection = {[parId: string]: string};

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

export const viewCtrlDot = nameofFactoryCtrl<ViewCtrl>();

export class ViewCtrl implements QuestionHandler, AreaHandler, ClipboardHandler,
    EditingHandler, NotesHandler, ParmenuHandler, RefPopupHandler, IController {
    overReflink: boolean;
    overPopup: boolean;
    private notification: string;

    initRefPopup(sc: IScope, view: ViewCtrl): void {
        throw new Error(this.mixinMsg);
    }

    showRefPopup(e: Event, $ref: JQuery, coords: Coords, attrs: IRefPopupAttrs) {
        throw new Error(this.mixinMsg);
    }

    hideRefPopup(): void {
        throw new Error(this.mixinMsg);
    }

    lastclicktime: number;
    lastclickplace: Coords;

    initParMenu(sc: IScope, view: ViewCtrl): void {
        throw new Error(this.mixinMsg);
    }

    toggleActionButtons(e: Event, $par: Paragraph, toggle1: boolean, toggle2: boolean, coords: Coords): void {
        throw new Error(this.mixinMsg);
    }

    optionsWindowClosed(): void {
        throw new Error(this.mixinMsg);
    }

    updatePopupMenu($par?: Paragraph): void {
        throw new Error(this.mixinMsg);
    }

    noteBadgePar: JQuery;
    noteBadge: HTMLElement;

    initNotes(sc: IScope, view: ViewCtrl): void {
        throw new Error(this.mixinMsg);
    }

    toggleNoteEditor($parOrArea: ParOrArea, options: INoteEditorOptions): void {
        throw new Error(this.mixinMsg);
    }

    handleNoteCancel(): void {
        throw new Error(this.mixinMsg);
    }

    handleNoteDelete(saveData: IParResponse, extraData: IExtraData): void {
        throw new Error(this.mixinMsg);
    }

    handleNoteSave(saveData: IParResponse, extraData: IExtraData): void {
        throw new Error(this.mixinMsg);
    }

    createNoteBadge($par: Paragraph): HTMLElement {
        throw new Error(this.mixinMsg);
    }

    addNote(): void {
        throw new Error(this.mixinMsg);
    }

    setNotePadge($event: Event): void {
        throw new Error(this.mixinMsg);
    }

    updateNoteBadge($par: Paragraph): void {
        throw new Error(this.mixinMsg);
    }

    clearNoteBadge(e: Event): void {
        throw new Error(this.mixinMsg);
    }

    initQuestions(sc: IScope, view: ViewCtrl): void {
    }

    initClipboard(sc: IScope, view: ViewCtrl): void {
        throw new Error(this.mixinMsg);
    }

    initEditing(sc: IScope, view: ViewCtrl): void {
        throw new Error(this.mixinMsg);
    }

    initAreas(sc: IScope, view: ViewCtrl): void {
        throw new Error(this.mixinMsg);
    }

    toggleParEditor($pars: Paragraphs, options: IParEditorOptions): void {
        throw new Error(this.mixinMsg);
    }

    toggleEditor($par: Paragraph, options: IParEditorOptions, attrs: IParEditorAttrs, caption: string, directive: string): void {
        throw new Error(this.mixinMsg);
    }

    editSettingsPars(recursiveCall: boolean): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    showEditWindow(e: Event, $par: Paragraph): void {
        throw new Error(this.mixinMsg);
    }

    beginAreaEditing(e: Event, $par: Paragraph): void {
        throw new Error(this.mixinMsg);
    }

    handleCancel(extraData: IExtraData): void {
        throw new Error(this.mixinMsg);
    }

    showAddParagraphAbove(e: Event, $par: Paragraph): void {
        throw new Error(this.mixinMsg);
    }

    showAddParagraphBelow(e: Event, $par: Paragraph): void {
        throw new Error(this.mixinMsg);
    }

    goToEditor(): void {
        throw new Error(this.mixinMsg);
    }

    closeAndSave(e: Event, $par: Paragraph): void {
        throw new Error(this.mixinMsg);
    }

    closeWithoutSaving(e: Event, $par: Paragraph): void {
        throw new Error(this.mixinMsg);
    }

    getEditorFunctions($par?: Paragraph): MenuFunctionCollection {
        throw new Error(this.mixinMsg);
    }

    showAddParagraphMenu(e: Event, $parOrArea: ParOrArea, coords: Coords): void {
        throw new Error(this.mixinMsg);
    }

    getAddParagraphFunctions(): MenuFunctionCollection {
        throw new Error(this.mixinMsg);
    }

    removeDefaultPars(): void {
        throw new Error(this.mixinMsg);
    }

    showNoteWindow: MenuFunction;

    showOptionsWindow(e: Event, $par: Paragraph, coords?: Coords): void {
        throw new Error(this.mixinMsg);
    }

    addParagraphFunctions: MenuFunctionCollection;

    pasteFunctions: MenuFunctionCollection;
    allowPasteRef: boolean;
    allowPasteContent: boolean;

    showPasteMenu(e: Event, $parOrArea: ParOrArea, coords?: Coords): void {
        throw new Error(this.mixinMsg);
    }

    showMoveMenu(e: Event, $parOrArea: ParOrArea, coords?: Coords): void {
        throw new Error(this.mixinMsg);
    }

    pasteContentAbove(e: Event, $par: Paragraph): void {
        throw new Error(this.mixinMsg);
    }

    pasteRefAbove(e: Event, $par: Paragraph): void {
        throw new Error(this.mixinMsg);
    }

    pasteContentBelow(e: Event, $par: Paragraph): void {
        throw new Error(this.mixinMsg);
    }

    pasteRefBelow(e: Event, $par: Paragraph): void {
        throw new Error(this.mixinMsg);
    }

    deleteFromSource(): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    moveAbove(e: Event, $parOrArea: ParOrArea): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    moveBelow(e: Event, $parOrArea: ParOrArea): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    pasteAbove(e: Event, $parOrArea: ParOrArea, asRef: boolean): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    pasteBelow(e: Event, $parOrArea: ParOrArea, asRef: boolean): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    updateClipboardStatus(): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    getPasteFunctions(): MenuFunctionCollection {
        throw new Error(this.mixinMsg);
    }

    getMoveFunctions(): MenuFunctionCollection {
        throw new Error(this.mixinMsg);
    }

    cutPar(e: Event, $par: Paragraph): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    copyPar(e: Event, $par: Paragraph): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    copyArea(e: Event, $parOrArea: ParOrArea, overrideDocId?: number, cut?: boolean): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    cutArea(e: Event, $parOrArea: ParOrArea, cut?: boolean): void {
        throw new Error(this.mixinMsg);
    }

    handleDelete(param: IParResponse, param2: IExtraData): void {
        throw new Error(this.mixinMsg);
    }

    addSavedParToDom(data: IParResponse, extraData: IExtraData): void {
        throw new Error(this.mixinMsg);
    }

    selectedAreaName: string;
    popupMenuAttrs: IPopupMenuAttrs;
    selection: {pars: JQuery | null; start: Paragraph | null; end: Paragraph | null};

    onAreaEditClicked($this: JQuery, e: JQueryEventObject, className: string) {
        throw new Error(this.mixinMsg);
    }

    showAreaOptionsWindow(e: Event, $area: Area, $pars: Paragraphs, coords: Coords): void {
        throw new Error(this.mixinMsg);
    }

    startArea(e: Event, $par: Paragraph): void {
        throw new Error(this.mixinMsg);
    }

    nameArea(e: Event, $pars: Paragraph): Promise<void> {
        throw new Error(this.mixinMsg);
    }

    nameAreaOk($area: Area, areaName: string, options: INameAreaOptions): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    nameAreaCancel($area: Area): void {
        throw new Error(this.mixinMsg);
    }

    cancelArea(): void {
        throw new Error(this.mixinMsg);
    }

    removeAreaMarking(e: Event, $pars: Paragraphs): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    extendSelection($par: Paragraph, allowShrink?: boolean): void {
        throw new Error(this.mixinMsg);
    }

    showPopupMenu(e: Event, $pars: Paragraphs, coords: Coords, popupMenuAttrs: IPopupMenuAttrs, $area?: Area, s?: string): void {
        throw new Error(this.mixinMsg);
    }

    public noQuestionAutoNumbering: boolean;
    public par: JQuery;
    public viewctrl: ViewCtrl;
    private mixinMsg = "This method is implemented in a mixin.";

    showQuestion(questionId: string): void {
        throw new Error(this.mixinMsg);
    }

    showQuestionNew(parId: string): Promise<void> {
        throw new Error(this.mixinMsg);
    }

    addQuestionQst(e: Event, $par: Paragraph): Promise<void> {
        throw new Error(this.mixinMsg);
    }

    editQst(e: Event, $par: Paragraph): Promise<void> {
        throw new Error(this.mixinMsg);
    }

    addQuestion(e: Event, $par: Paragraph): Promise<void> {
        throw new Error(this.mixinMsg);
    }

    processQuestions(): void {
        throw new Error(this.mixinMsg);
    }

    showQuestions(): boolean {
        throw new Error(this.mixinMsg);
    }

    private static $inject = ["$scope"];

    public lectureMode: boolean;
    public inLecture: boolean;
    public item: IItem;
    public docId: number;

    public sc: IScope;
    private hidePending: boolean;
    public group: any;
    private scope: IScope;
    public noBrowser: boolean;
    private docName: string;
    public docVersion: [number, number];
    private crumbs: any;
    private startIndex: number;
    public users: IUser[];
    public teacherMode: boolean;
    private velpMode: boolean;

    private pendingUpdates: PendingCollection;
    private document: Document;
    private showRefresh: boolean;
    public selectedUser: IUser;
    public editing: boolean;
    public $storage: ngStorage.StorageService & {defaultAction: string | null; noteAccess: string};
    private liveUpdates: number;
    private oldWidth: number;
    public editorFunctions: MenuFunctionCollection;
    public defaultAction: MenuFunctionEntry | null;
    public reviewCtrlScope: IScope & {$ctrl: ReviewController};
    public lectureCtrl?: LectureController;

    constructor(sc: IScope) {
        timLogTime("ViewCtrl start", "view");

        this.noBrowser = $window.noBrowser;
        this.docId = $window.item.id;
        this.docName = $window.item.path;
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

        this.initQuestions(sc, this);
        this.initAreas(sc, this);
        this.initClipboard(sc, this);
        this.initEditing(sc, this);
        this.initNotes(sc, this);
        this.initParMenu(sc, this);
        this.initRefPopup(sc, this);
        initReadings(this);
        initIndex();
        initCssPrint();

        // from https://stackoverflow.com/a/7317311
        $(() => {
            this.processQuestions();
            this.setHeaderLinks();
            this.document.rebuildSections();
            $window.addEventListener("beforeunload", (e) => {
                if (!this.editing) {
                    return undefined;
                }

                const msg = "You are currently editing something. Are you sure you want to leave the page?";

                (e || $window.event).returnValue = msg; // Gecko + IE
                return msg; // Gecko + Webkit, Safari, Chrome etc.
            });
        });

        onClick("html.ng-scope", ($this, e) => {
            // Clicking anywhere
            const tagName = (e.target as Element).tagName.toLowerCase();
            const jqTarget = $(e.target);
            const ignoreTags = ["button", "input", "label", "i"];
            const ignoreClasses = ["menu-icon", "editline", "areaeditline", "draghandle", "actionButtons"];

            let curElement = jqTarget;
            let limit = 10;
            while (curElement != null) {
                if (this.editing || $.inArray(tagName, ignoreTags) >= 0 || curElement.attr("position") === "absolute") {
                    return false;
                }

                for (let i = 0; i < ignoreClasses.length; i++) {
                    if (curElement.hasClass(ignoreClasses[i])) {
                        return false;
                    }
                }

                curElement = curElement.parent();
                if (--limit < 0) {
                    break;
                }
            }

            closeOptionsWindow();

            if (tagName !== "p") {
                $(".selected").removeClass("selected");
                $(".lightselect").removeClass("lightselect");
            }

            return false;

        }, true);

        // If you add 'mousedown' to bind, scrolling upon opening the menu doesn't work on Android
        $("body,html").bind("scroll wheel DOMMouseScroll mousewheel", (e) => {
            if (e.which > 0 || e.type === "mousedown" || e.type === "mousewheel") {
                $("html,body").stop();
            }
        });

        this.$storage = $localStorage.$default({
            defaultAction: "Show options window",
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
            const found = $filter("filter")(this.editorFunctions,
                {desc: this.$storage.defaultAction}, true);
            if (found.length) {
                this.defaultAction = found[0];
            }
        } catch (e) {
        }
        timLogTime("ViewCtrl end", "view");
    }

    startLiveUpdates() {
        let sc = this.scope;
        let origLiveUpdates = this.liveUpdates;
        if (!origLiveUpdates) return;
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
            if ( this.liveUpdates != origLiveUpdates ) { // if value hase changes, stop and start new poll
                if (stop) {
                    $interval.cancel(stop);
                    stop = undefined;
                }
                $timeout(() => { this.startLiveUpdates(); }, 100);
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
            () => this.allowPasteContent,
            () => this.allowPasteRef,
            () => this.getAllowMove()], (newValues, oldValues, scope) => {
            const par = $(".actionButtons").parent(".par");
            this.updatePopupMenu(par.length > 0 ? par : undefined);
            if (this.editing) {
                this.notification = "Editor is already open.";
            } else {
                this.notification = "";
            }
        });
    }

    setReviewCtrlScope(scope: IScope & {$ctrl: ReviewController}) {
        this.reviewCtrlScope = scope;
    }

    reload() {
        markPageNotDirty();
        $window.location.reload();
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
                const $par = getElementByParId(key);
                const $newPar = $($compile(this.pendingUpdates[key])(this.scope));
                $par.replaceWith($newPar);
                this.applyDynamicStyles($newPar);
                ParCompiler.processAllMathDelayed($newPar);
            }
        }
        this.document.rebuildSections();
        this.pendingUpdates = {};
        this.processQuestions();
    }

    applyDynamicStyles($par: Paragraph) {
        if ($window.editMode) {
            $par.addClass("editmode");

            // Show hidden paragraphs if in edit mode
            $par.find(".mdcontent").css("display", "initial");
        }
    }

    setHeaderLinks() {
        const pars = $(".parContent");
        pars.each((index, elem) => {
            const $p = $(elem);
            $p.find("h1, h2, h3, h4, h5, h6").each((i, e) => {
                const $h = $(e);
                const id = $h.attr("id");
                if (angular.isDefined(id)) {
                    $h.append($("<a>", {
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

    nothing() {
    }
}

applyMixins(ViewCtrl, [QuestionHandler, AreaHandler, ClipboardHandler,
    EditingHandler, NotesHandler, ParmenuHandler, RefPopupHandler]);

timApp.component("timView", {
    controller: ViewCtrl,
    template: "<div ng-transclude></div>",
    transclude: true,
});
