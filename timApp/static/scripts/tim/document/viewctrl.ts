import {IController, IDeferred, IPromise, IScope} from "angular";
import $ from "jquery";
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
import {AnswerBrowserController, ITaskInfo, PluginLoaderCtrl} from "../answer/answerbrowser3";
import {IAnswer} from "../answer/IAnswer";
import {BookmarksController, IBookmarkGroup} from "../bookmark/bookmarks";
import {IPluginInfoResponse, ParCompiler} from "../editor/parCompiler";
import {IDocument} from "../item/IItem";
import {LectureController} from "../lecture/lectureController";
import {TimTableController} from "../plugin/timTable";
import {initCssPrint} from "../printing/cssPrint";
import {IUser} from "../user/IUser";
import {Users} from "../user/userService";
import {
    $compile,
    $filter,
    $http,
    $httpParamSerializer,
    $interval,
    $localStorage,
    $q,
    $timeout,
    $window,
} from "../util/ngimport";
import {AnnotationController} from "../velp/annotation";
import {ReviewController} from "../velp/reviewController";
import {DiffController} from "./diffDialog";
import {EditingHandler} from "./editing/editing";
import {PendingCollection} from "./editing/edittypes";
import * as helpPar from "./editing/helpPar";
import {onClick} from "./eventhandlers";
import {IDocSettings} from "./IDocSettings";
import {PopupMenuController} from "./popupMenu";
import {RefPopupHandler} from "./refpopup";
import {initSlideView} from "./slide";
import {IMenuFunctionEntry} from "./viewutils";

markAsUsed(ngs, popupMenu, interceptor, helpPar);

export interface ITimComponent {
    getName: () => string | undefined;
    getContent: () => string | undefined;
    getContentArray?: () => string[] | undefined;
    getGroups: () => string[];
    getTaskId: () => string | undefined;
    belongsToGroup(group: string): boolean;
    isUnSaved: () => boolean;
    save: () => Promise<{saved: boolean, message: (string | undefined)}>;
    getPar: () => Paragraph;
    setPluginWords?: (words: string[]) => void;
    setForceAnswerSave?: (force: boolean) => void;
    resetField: () => string | undefined;
    supportsSetAnswer: () => boolean;
    setAnswer: (content: {[index: string]: string}) => {ok: boolean, message: (string | undefined)};
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

export let vctrlInstance: ViewCtrl | undefined;

export class ViewCtrl implements IController {
    private notification: string = "";
    private videoElements = new Map<string, HTMLVideoElement>();
    clipMeta: IClipboardMeta = {allowPasteContent: false, allowPasteRef: false, empty: true};
    selection: { pars?: JQuery; start?: Paragraph; end?: Paragraph } = {};
    public par: JQuery | undefined;

    static $inject = ["$scope"];

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

    private timTables = new Map<string, TimTableController>();
    private timComponents: Map<string, ITimComponent> = new Map();

    private pendingUpdates: PendingCollection = new Map<string, string>();
    private document: Document;
    private showRefresh: boolean;
    public selectedUser: IUser;
    public editing: boolean = false;
    public $storage: ngStorage.StorageService & { defaultAction: string | null; noteAccess: string };
    private liveUpdates: number;
    private oldWidth: number;
    public defaultAction: IMenuFunctionEntry | undefined;
    public reviewCtrl: ReviewController;
    public lectureCtrl?: LectureController;
    public questionHandler: QuestionHandler;
    public areaHandler: AreaHandler;
    public clipboardHandler: ClipboardHandler;
    public editingHandler: EditingHandler;
    public notesHandler: NotesHandler;
    public parmenuHandler: ParmenuHandler;
    public refpopupHandler: RefPopupHandler;
    public popupmenu?: PopupMenuController;

    public bookmarksCtrl: BookmarksController | undefined;

    // For search box.
    private displaySearch = false;
    diffDialog?: DiffController;

    // To hide actions on both sides of the page.
    public actionsDisabled = false;

    // To give an alert if trying to go to another page when doing an adaptive feedback task.
    public doingTask = false;
    public docSettings: IDocSettings = $window.docSettings;

    // Form-mode related attributes.
    private formTaskInfosLoaded = false;

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
        if (!this.isSlideView()) {
            initReadings(this);
        } else {
            initSlideView(this.item);
        }
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
            if (this.diffDialog) {
                this.diffDialog.close();
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
        this.reviewCtrl = new ReviewController(this);
        timLogTime("ViewCtrl end", "view");
    }

    $postLink() {
        initCssPrint();

        this.questionHandler.processQuestions();
        this.setHeaderLinks();
        this.noBeginPageBreak();
        this.document.rebuildSections();
        // from https://stackoverflow.com/a/7317311
        window.addEventListener("beforeunload", (e) => {
            saveCurrentScreenPar();

            if ((!this.editing && !this.checkUnSavedTimComponents() && !this.doingTask) || $window.IS_TESTING) {
                return undefined;
            }

            const msg = "You are currently editing something. Are you sure you want to leave the page?";

            (e || $window.event).returnValue = msg; // Gecko + IE
            return msg; // Gecko + Webkit, Safari, Chrome etc.
        });
        // Change hash whenever user scrolls the document.
        window.addEventListener("scroll", (e) => {
            saveCurrentScreenPar();
        });
    }

    public isTranslation() {
        return this.item.src_docid != null && this.item.src_docid !== this.item.id;
    }

    getVideo(followid: string) {
        return this.videoElements.get(followid);
    }

    registerVideo(followid: string, v: HTMLVideoElement): void {
        this.videoElements.set(followid, v);
    }

    isSlideView() {
        const p = document.location.pathname;
        return p.startsWith("/slidefff/") || p.startsWith("/show_slide/");
    }

    startLiveUpdates() {
        const sc = this.scope;
        const origLiveUpdates = this.liveUpdates;
        if (!origLiveUpdates) {
            return;
        }
        let stop: IPromise<any> | undefined;
        stop = $interval(async () => {
            const response = await $http.get<{ version: [number, number], diff: DiffResult[], live: number }>("/getParDiff/" + this.docId + "/" + this.docVersion[0] + "/" + this.docVersion[1]);
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
            for (const d of response.data.diff) {
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
        vctrlInstance = this;
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
        this.reviewCtrl.loadDocumentAnnotations();
        this.editingHandler.insertHelpPar();
        // window.onbeforeunload = () => {
        //     const dirty = this.checkUnSavedTimComponents();
        //     if ( dirty ) { return "You have unsaved tasks!"; }  // IE shows this message
        //     // And for IE you can not return anything, otherwise it will show even null
        // };
    }

    /**
     * Registers a table controller to the view controller.
     * All table controllers need to register for toggling edit mode of
     * the table to work.
     * @param {TimTableController} controller The table controller.
     * @param {string} parId The ID of the table paragraph.
     */
    public addTable(controller: TimTableController, parId: string) {
        console.log("table added!");
        this.timTables.set(parId, controller);
    }

    /**
     * Returns a table controller related to a specific table paragraph.
     * @param {string} parId The paragraph's ID.
     * @returns {TimTableController} The table controller related to the given table paragraph, or undefined.
     */
    public getTableControllerFromParId(parId: string) {
        return this.timTables.get(parId);
    }

    /**
     * Registers an ITimComponent to the view controller by its name attribute if it has one.
     * @param {ITimComponent} component The component to be registered.
     */
    public addTimComponent(component: ITimComponent) {
        const name = component.getName();
        if (name) { this.timComponents.set(name, component); }
    }

    /**
     * Returns an ITimComponent where register ID matches the given string.
     * @param {string} name The register ID of the ITimComponent.
     * @returns {ITimComponent | undefined} Matching component if there was one.
     */
    public getTimComponentByName(name: string): ITimComponent | undefined {
        return this.timComponents.get(name);
    }

    /**
     * Gets ITimComponents nested within specified area component.
     * @param{string} group name of the area object.
     * @returns {ITimComponent[]} List of ITimComponents nested within the area.
     */
    public getTimComponentsByGroup(group: string): ITimComponent[] {
        const returnList: ITimComponent[] = [];
        for (const [k, v] of this.timComponents) {
            if (v.belongsToGroup(group)) { returnList.push(v); }
        }
        return returnList;
    }

    /**
     * Searches for registered ITimComponent whose ID matches the given regexp.
     * @param {string} re The RegExp to be used in search.
     * @returns {ITimComponent[]} List of ITimComponents where the ID matches the regexp.
     */
    public getTimComponentsByRegex(re: string): ITimComponent[] {
        const returnList: ITimComponent[] = [];
        const reg = new RegExp(re);
        for (const [k, v] of this.timComponents) {
            if (reg.test(k)) { returnList.push(v); }
        }
        return returnList;
    }

    /**
     * @returns {ITimComponent[]} List of all registered ITimComponents
     */
    public getAllTimComponents(): ITimComponent[] {
        return Array.from(this.timComponents.values());
    }

    /**
     * @returns {boolean} True if at least one registered ITimComponent was in unsaved state
     */
    public checkUnSavedTimComponents(): boolean {
        let unsavedTimComponents = false;
        for (const t of this.timComponents.values()) {
            if (t.isUnSaved()) {
                unsavedTimComponents = true;
                break;
            }
        }
        return unsavedTimComponents;
    }

    isEmptyDocument() {
        return this.docVersion[0] === 0 && this.docVersion[1] === 0; // TODO can be empty otherwise too
    }

    reload() {
        markPageNotDirty();
        window.location.reload();
    }

    closeRefreshDlg() {
        this.showRefresh = false;
    }

    async changeUser(user: IUser, updateAll: boolean) {
        this.selectedUser = user;
        if (updateAll) {
            for (const lo of this.ldrs.values()) {
                lo.loadPlugin();
                await lo.abLoad.promise;
            }
        }

        if (this.docSettings.form_mode) {
            const taskList = [];
            for (const fab of this.formAbs.values()) {
                taskList.push(fab.taskId);
            }
            // TODO: Query only one (first valid) answer
            const answerResponse = await $http.get<{ answers: {[index: string]: [IAnswer]}, userId: number}>("/multipluginanswer2?" + $httpParamSerializer({
                fields: taskList,
                user: user.id,
            }));
            if (!this.formTaskInfosLoaded) {
                const taskInfoResponse = await $http.get<{ [index: string]: ITaskInfo }>("/taskinfos?" + $httpParamSerializer({
                    tasks: taskList,
                }));
                this.formTaskInfosLoaded = true;
                for (const fab of this.formAbs.values()) {
                    fab.setInfo(taskInfoResponse.data[fab.taskId]);
                }
            }
            // UserId check might ensure the loaded response is from correct user
            // when using slow connections (or maybe not)
            if (answerResponse.data.userId == this.selectedUser.id) {
                for (const fab of this.formAbs.values()) {
                    fab.changeUserAndAnswers(user,
                        updateAll,
                        answerResponse.data.answers[fab.taskId],
                    );
                    const timComp = this.getTimComponentByName(fab.taskId.split(".")[1]);
                    if (timComp) {
                        if (fab.selectedAnswer) {
                            timComp.setAnswer(JSON.parse(fab.selectedAnswer.content));
                        } else {
                            timComp.resetField();
                        }
                    }
                }
            }
        }

        // TODO: do not call changeUser separately if updateAll enabled
        // - handle /answers as single request for all related plugins instead of separate requests
        // - do the same for /taskinfo and /getState requests
        // for (const ab of this.abs.values()) {
        //     ab.changeUser(user, updateAll);
        // }
        //
        // return;
        // TODO experiments below, uncomment 4 lines above
        // // Make a single request for all answerbrowsers in this.abs.values
        // const response = await $http.get<{ }>("/multipluginanswer?" + $httpParamSerializer({fields: taskList, user: user.id, doc: this.docId}));
        // console.log(response);
        // TODO: if answerbrowser had same user as before then it can be removed from the query
        //  Would it even happen if updateAll is enabled all the time when changing users
        if (updateAll) {
            if (this.abs.size == 0) {
                return;
            }
            const taskList2 = [];
            for (const ab of this.abs.values()) {
                taskList2.push(ab.taskId);
            }
            const answerResponse = await $http.get<{ [index: string]: [IAnswer] }>("/multipluginanswer2?" + $httpParamSerializer({
                fields: taskList2,
                user: user.id,
            }));
            console.log(answerResponse);
            // const selectedAnswers: {[index: string]: IAnswer} = {};
            // const absNeedAnswerchange = [];
            const answerIds: {[index: string]: AnswerBrowserController} = {};
            for (const [k, ab] of this.abs) {
                ab.changeUserAndAnswers(user, updateAll, answerResponse.data[ab.taskId]);
                if (ab.selectedAnswer !== undefined) {
                    answerIds[ab.selectedAnswer.id] = ab;
                }
                // if (needsAnswerChange) { absNeedAnswerchange.push(ab); }
            }
            console.log(answerIds);

            const r = await to($http.get<{ [index: number]: { html: string, reviewHtml: string } }>("/getMultiStates", {
                params: {
                    answer_ids: Object.keys(answerIds),
                    user_id: this.selectedUser.id,
                    doc_id: this.docId,
                },
            }));
            // console.log(stateResponse);
            if (!r.ok) { return; } // TODO: Notify user
            for (const [ansId, json] of Object.entries(r.result.data)) {
                answerIds[ansId].changeAnswerFromState(json.html, json.reviewHtml);
            }

            // const abParParams: Array<{
            //     answer_id: number; // IE shows this message
            //     review: boolean; // And for IE you can not return anything, otherwise it will show even null
            //     // And for IE you can not return anything, otherwise it will show even null
            //     user_id: number; doc_id: string | number | undefined; par_id: string | undefined; ref_from_doc_id: number; ref_from_par_id: string | undefined;
            // }>  = [];
            // const answerIds: number[] = [];
            // const answerIds: {[index: string]: AnswerBrowserController} = {};
            // absNeedAnswerchange.forEach((a) => {
            //     // const params = a.stateRequestPrep();
            //     // if (params != undefined) {
            //     //     abParParams.push(params);
            //     // }
            //     a.stateRequestPrep();
            //     if (a.selectedAnswer) {
            //         answerIds[a.selectedAnswer.id] = a;
            //     }
            //
            // });

            // make request for multiple states
            // need: userid, list of answerids, docid
            // maybe need: review (per task), par_id (per task)
            // const r = await to($http.get<{ [index: number]: { html: string, reviewHtml: string } }>("/getMultiStates", {
            //     params: {
            //         answer_ids: Object.keys(answerIds),
            //         user_id: this.selectedUser.id,
            //         doc_id: this.docId,
            //     },
            // }));
            // // console.log(stateResponse);
            // if (!r.ok) { return; } // TODO: Notify user
            // for (const [ansId, json] of Object.entries(r.result.data)) {
            //     answerIds[ansId].continueFromStateReq(json.html, json.reviewHtml);
            // }
        }
    }

    async beginUpdate() {
        const response = await $http.get<{ changed_pars: { [id: string]: string } }>("/getUpdatedPars/" + this.docId);
        this.updatePendingPars(new Map<string, string>(Object.entries(response.data.changed_pars)));
    }

    pendingUpdatesCount() {
        return this.pendingUpdates.size;
    }

    showUpdateDialog() {
        return !this.hidePending && this.pendingUpdatesCount() > 0;
    }

    updatePendingPars(pars: PendingCollection) {
        for (const [k, v] of pars) {
            this.pendingUpdates.set(k, v);
        }
        this.hidePending = false;
        if (this.pendingUpdatesCount() < 10) {
            this.updatePending();
        }
    }

    updatePending() {
        for (const [key, val] of this.pendingUpdates) {
            const par = getElementByParId(key);
            const n = $(val);
            par.replaceWith(n);
            const compiled = $($compile(n)(this.scope));
            this.applyDynamicStyles(compiled);
            ParCompiler.processAllMathDelayed(compiled);
        }
        this.document.rebuildSections();
        this.pendingUpdates.clear();
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
                if (id) {
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

    private abs = new Map<string, AnswerBrowserController>();
    private formAbs = new Map<string, AnswerBrowserController>();

    /**
     * Registers answerbrowser to related map
     * If form_mode is enabled and answerBrowser is from timComponent
     * that supports setting answer then add it to formAbs
     * else add it to regular ab map
     * @param ab AnswerBrowserController to be registered
     */
    registerAnswerBrowser(ab: AnswerBrowserController) {
        if (this.docSettings.form_mode) {
            const timComp = this.getTimComponentByName(ab.taskId.split(".")[1]);
            if (timComp && timComp.supportsSetAnswer()) {
                // TODO: Should propably iterate like below in case of duplicates
                this.formAbs.set(ab.taskId, ab);
                return;
            }
        }
        // TODO: Task can have two instances in same document (regular field and label version)
        // - for now just add extra answerbrowsers for them (causes unnecessary requests when changing user...)
        // - maybe in future answerbrowser could find all related plugin instances and update them when ab.changeuser gets called?
        // - fix registerPluginLoader too
        if (this.abs.has((ab.taskId))) {
            let index = 1;
            while (this.abs.has(ab.taskId + index)) {
                index++;
            }
            this.abs.set(ab.taskId + index, ab);
        } else { this.abs.set(ab.taskId, ab); }
    }

    getAnswerBrowser(taskId: string) {
        return (this.abs.get(taskId) || this.formAbs.get(taskId));
    }

    getFormAnswerBrowser(taskId: string) {
        return this.formAbs.get(taskId);
    }

    private ldrs = new Map<string, PluginLoaderCtrl>();

    registerPluginLoader(loader: PluginLoaderCtrl) {
        // TODO: see todos at registerAnswerBrowser
        if (this.ldrs.has((loader.taskId))) {
            let index = 1;
            while (this.ldrs.has(loader.taskId + index)) {
                index++;
            }
            this.ldrs.set(loader.taskId + index, loader);
        } else { this.ldrs.set(loader.taskId, loader); }
    }

    getPluginLoader(taskId: string) {
        return this.ldrs.get(taskId);
    }

    private anns = new Map<string, AnnotationController>();
    private annsDefers = new Map<string, IDeferred<AnnotationController>>();

    registerAnnotation(loader: AnnotationController) {
        // This assumes that the associated DOM element for annotation is attached in the page because we need to check
        // whether it's in the right margin or not (i.e. in text).
        const prefix = loader.getKeyPrefix();
        const key = prefix + loader.annotation.id;
        this.anns.set(key, loader);
        const defer = this.annsDefers.get(key);
        if (defer) {
            defer.resolve(loader);
        }
    }

    getAnnotation(id: string) {
        return this.anns.get(id);
    }

    getAnnotationAsync(id: string): IPromise<AnnotationController> {
        const a = this.getAnnotation(id);
        if (a) {
            return $q.resolve(a);
        }
        const value = $q.defer<AnnotationController>();
        this.annsDefers.set(id, value);
        return value.promise;
    }

    unRegisterAnnotation(a: AnnotationController) {
        const prefix = a.getKeyPrefix();
        const key = prefix + a.annotation.id;
        this.anns.delete(key);
        this.annsDefers.delete(key);
    }

}

class EntityRegistry<K, V> {
    private entities = new Map<K, V>();

    registerEntity(k: K, e: V) {
        this.entities.set(k, e);
    }

    getEntity(k: K) {
        return this.entities.get(k);
    }
}

timApp.component("timView", {
    controller: ViewCtrl,
    template: "<div ng-transclude></div>",
    transclude: true,
});
