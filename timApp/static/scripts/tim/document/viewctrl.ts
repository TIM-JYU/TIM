import {IController, IPromise, IScope} from "angular";
import $ from "jquery";
import ngs, {ngStorage} from "ngstorage";
import {timApp} from "tim/app";
import {setActiveDocument} from "tim/document/activedocument";
import {AreaHandler} from "tim/document/areas";
import {Document} from "tim/document/document";
import {ClipboardHandler, IClipboardMeta} from "tim/document/editing/clipboard";
import * as interceptor from "tim/document/interceptor";
import {NotesHandler} from "tim/document/notes";
import {getElementByParId, Paragraph, saveCurrentScreenPar} from "tim/document/parhelpers";
import {ParmenuHandler} from "tim/document/parmenu";
import * as popupMenu from "tim/document/popupMenu";
import {QuestionHandler} from "tim/document/question/questions";
import {initReadings} from "tim/document/readings";
import {setViewCtrl} from "tim/document/viewctrlinstance";
import {timLogTime} from "tim/util/timTiming";
import {isPageDirty, markAsUsed, markPageNotDirty, StringUnknownDict, to} from "tim/util/utils";
import {TimDefer} from "tim/util/timdefer";
import {AnswerBrowserController, PluginLoaderCtrl} from "../answer/answerbrowser3";
import {IAnswer} from "../answer/IAnswer";
import {BookmarksController} from "../bookmark/bookmarks";
import {IPluginInfoResponse, ParCompiler} from "../editor/parCompiler";
import {IDocument} from "../item/IItem";
import {LectureController} from "../lecture/lectureController";
import {IGenericPluginMarkup, IGenericPluginTopLevelFields} from "../plugin/attributes";
import {TableFormComponent} from "../plugin/tableForm";
import {TaskId} from "../plugin/taskid";
import {TimTableComponent} from "../plugin/timTable";
import {initCssPrint} from "../printing/cssPrint";
import {IUser, IUserListEntry} from "../user/IUser";
import {Users} from "../user/userService";
import {widenFields} from "../util/common";
import {documentglobals} from "../util/globals";
import {$compile, $filter, $http, $interval, $localStorage, $timeout} from "../util/ngimport";
import {AnnotationComponent} from "../velp/annotation.component";
import {ReviewController} from "../velp/reviewController";
import {diffDialog} from "./diffDialog";
import {EditingHandler} from "./editing/editing";
import {PendingCollection} from "./editing/edittypes";
import * as helpPar from "./editing/helpPar";
import {onClick} from "./eventhandlers";
import {IDocSettings} from "./IDocSettings";
import {ParRefController} from "./parRef";
import {PopupMenuController} from "./popupMenu";
import {initSlideView} from "./slide";
import {ViewRangeInfo} from "./viewRangeInfo";
import {IMenuFunctionEntry} from "./viewutils";

markAsUsed(ngs, popupMenu, interceptor, helpPar, ParRefController);

export interface ITimComponent {
    attrsall?: IGenericPluginTopLevelFields<IGenericPluginMarkup>; // TimTable does not have attrsall - that's why it's optional.
    getName: () => string | undefined;
    getContent: () => string | undefined;
    getContentArray?: () => string[] | undefined;
    getAreas: () => string[];
    getTaskId: () => TaskId | undefined;
    belongsToArea: (area: string) => boolean;
     // TODO: isForm could be integrated to supportsSetAnswer (supportsSetAnswer would be true if fieldplugin and form/form_mode)
    isForm: () => boolean;
    isUnSaved: (userChange?: boolean) => boolean;
    save: () => Promise<{saved: boolean, message: (string | undefined)}>;
    getPar: () => Paragraph;
    setPluginWords?: (words: string[]) => void;
    setForceAnswerSave?: (force: boolean) => void;
    resetField: () => string | undefined;
    supportsSetAnswer: () => boolean;
    setAnswer: (content: {[index: string]: unknown}) => {ok: boolean, message: (string | undefined)};
    setData?(data: unknown, save: boolean): void;
}

// TODO: import entire controller?
export interface IJsRunner {
    runScriptWithGroups: (groups: string[]) => void;
}

export interface IUserChanged {
    userChanged: (user: IUser) => void;
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

export enum RegexOption {
    PrependCurrentDocId,
    DontPrependCurrentDocId,
}

export class ViewCtrl implements IController {
    private notification: string = "";
    private videoElements = new Map<string, HTMLVideoElement>();
    clipMeta: IClipboardMeta = {allowPasteContent: false, allowPasteRef: false, empty: true};
    selection: { pars?: JQuery; start?: Paragraph; end?: Paragraph } = {};
    public par: JQuery | undefined;

    static $inject = ["$scope"];

    public item: IDocument;
    public docId: number;

    private hidePending: boolean;
    public group: unknown;
    public scope: IScope;
    public noBrowser: boolean;
    public docVersion: [number, number];
    private startIndex: number;
    public users: IUserListEntry[];
    public teacherMode: boolean;
    private velpMode: boolean;

    private timTables = new Map<string, TimTableComponent>();
    private tableForms = new Map<string, TableFormComponent>();
    private jsRunners = new Map<string, IJsRunner>();

    // TODO: Possibly redundant since same thing can be achieved by just using the array version
    private timComponents: Map<string, ITimComponent> = new Map();
    // Array to keep reference to possible duplicated fields
    private timComponentArrays: Map<string, ITimComponent[]> = new Map();
    private timComponentTags: Map<string, string[]> = new Map();
    private userChangeListeners: Map<string, IUserChanged> = new Map();

    private pendingUpdates: PendingCollection = new Map<string, string>();
    private document: Document;
    private showRefresh: boolean;
    public selectedUser: IUser;
    public editing: boolean = false;
    // public $storage: ngStorage.StorageService & { defaultAction: string | null; noteAccess: string };
    public $storage: ngStorage.StorageService & { defaultAction: string | null; noteAccess: string; };
    private liveUpdates: number;
    private oldWidth: number;
    public defaultAction: IMenuFunctionEntry | undefined;
    public reviewCtrl: ReviewController;
    public lectureCtrl: LectureController;
    public questionHandler: QuestionHandler;
    public areaHandler: AreaHandler;
    public clipboardHandler: ClipboardHandler;
    public editingHandler: EditingHandler;
    public notesHandler: NotesHandler;
    public parmenuHandler: ParmenuHandler;
    public popupmenu?: PopupMenuController;
    public viewRangeInfo: ViewRangeInfo;

    public bookmarksCtrl: BookmarksController | undefined;

    // For search box.
    private displaySearch = false;

    // To hide actions on both sides of the page.
    public actionsDisabled = false;

    // To give an alert if trying to go to another page when doing an adaptive feedback task.
    public doingTask = false;
    public docSettings: IDocSettings = documentglobals().docSettings;

    // Form-mode related attributes.
    private formTaskInfosLoaded = false;
    private hideLinks: boolean;
    private hideTopButtons: boolean;
    private parsOnly: boolean;

    constructor(sc: IScope) {
        timLogTime("ViewCtrl start", "view");
        const dg = documentglobals();
        this.noBrowser = dg.noBrowser;
        this.hideLinks = dg.hideLinks;
        this.hideTopButtons = dg.hideTopButtons;
        this.parsOnly = dg.parsOnly;
        this.docId = dg.curr_item.id;
        this.docVersion = dg.docVersion;
        this.item = dg.curr_item;
        this.startIndex = dg.startIndex;
        this.users = dg.users;
        this.group = dg.group;
        this.teacherMode = dg.teacherMode;
        this.velpMode = dg.velpMode;
        this.scope = sc;

        this.document = new Document(this.docId);
        setActiveDocument(this.document);

        if (this.users.length > 0) {
            this.selectedUser = this.users[0].user;
        } else {
            this.selectedUser = Users.getCurrent();
        }
        this.hidePending = false;

        $(window).resize((e) => {
            if (e.target === window) {
                const newWidth = $(window).width();
                if (newWidth !== this.oldWidth && newWidth) {
                    this.oldWidth = newWidth;
                    const selected = $(".par.lightselect, .par.selected");
                    if (selected.length > 0) {
                        selected[0].scrollIntoView();
                    }
                }
            }
        });

        this.lectureCtrl = LectureController.createAndInit(this);
        this.questionHandler = new QuestionHandler(sc, this);
        this.areaHandler = new AreaHandler(sc, this);
        this.clipboardHandler = new ClipboardHandler(sc, this);
        this.editingHandler = new EditingHandler(sc, this);
        this.notesHandler = new NotesHandler(sc, this);
        this.parmenuHandler = new ParmenuHandler(sc, this);
        this.viewRangeInfo = new ViewRangeInfo(this);
        if (!this.isSlideView()) {
            initReadings(this.item);
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
            if (diffDialog) {
                diffDialog.close();
            }

            return false;

        }, true);

        // If you add 'mousedown' to bind, scrolling upon opening the menu doesn't work on Android
        $("body,html").bind("scroll wheel DOMMouseScroll mousewheel", (e) => {
            if ((e.which && e.which > 0) || e.type === "mousedown" || e.type === "mousewheel") {
                $("html,body").stop();
            }
        });

        this.$storage = $localStorage.$default({
            defaultAction: "Close menu",
            noteAccess: "everyone",
        });

        dg.allowMove = false;
        this.oldWidth = $(window).width() ?? 500;
        this.showRefresh = isPageDirty();
        this.liveUpdates = dg.liveUpdates;

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

            if ((!this.editing && !this.checkUnSavedTimComponents() && !this.doingTask) || documentglobals().IS_TESTING) {
                return undefined;
            }

            const msg = "You are currently editing something. Are you sure you want to leave the page?";

            (e || window.event).returnValue = msg; // Gecko + IE
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

    // noinspection JSUnusedGlobalSymbols (used in view_html.html)
    showVelpSelection() {
        return (this.reviewCtrl.velpMode || (this.teacherMode && this.docSettings.show_velps)) && !this.parsOnly;
    }

    startLiveUpdates() {
        const sc = this.scope;
        const origLiveUpdates = this.liveUpdates;
        if (!origLiveUpdates) {
            return;
        }
        let stop: IPromise<unknown> | undefined;
        stop = $interval(async () => {
            const r = await to($http.get<{ version: [number, number], diff: DiffResult[], live: number }>("/getParDiff/" + this.docId + "/" + this.docVersion[0] + "/" + this.docVersion[1]));
            if (!r.ok) {
                return;
            }
            const response = r.result;
            this.docVersion = response.data.version;
            this.liveUpdates = response.data.live; // TODO: start new loop by this or stop if None
            const replaceFn = async (d: DiffResult, parId: string) => {
                const e = getElementByParId(parId);
                const compiled = await ParCompiler.compileAndReplace(e, d.content, sc);
            };
            const afterFn = async (d: DiffResult, parId: string) => {
                const e = getElementByParId(parId);
                const compiled = await ParCompiler.compileAndAfter(e, d.content, sc);
            };
            const beforeFn = async (d: DiffResult, e: JQuery) => {
                const compiled = await ParCompiler.compileAndBefore(e, d.content, sc);
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
        setViewCtrl(this);
        this.scope.$watchGroup([
            () => this.lectureCtrl.lectureSettings.lectureMode,
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
        this.viewRangeInfo.loadRanges();
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
     * @param {TimTableComponent} controller The table controller.
     * @param {string} parId The ID of the table paragraph.
     */
    public addTable(controller: TimTableComponent, parId: string) {
        // console.log("table added!");
        this.timTables.set(parId, controller);
    }

    /**
     * Returns a table controller related to a specific table paragraph.
     * @param {string} parId The paragraph's ID.
     * @returns {TimTableComponent} The table controller related to the given table paragraph, or undefined.
     */
    public getTableControllerFromParId(parId: string) {
        return this.timTables.get(parId);
    }

    // TODO: Refactor plugin interface for tableForm:
    //  - Export entire controller & keep own map vs implement ITimComponent
    public addTableForm(controller: TableFormComponent, taskId: string) {
        this.tableForms.set(taskId, controller);
    }

    public getTableForm(taskId: string) {
        return this.tableForms.get(taskId);
    }

    public addJsRunner(runner: IJsRunner, taskId: string) {
        this.jsRunners.set(taskId, runner);
    }

    public getJsRunner(taskId: string) {
        taskId = this.normalizeTaskId(taskId);
        return this.jsRunners.get(taskId);
    }

    public addUserChangeListener(name: string, listener: IUserChanged) {
        this.userChangeListeners.set(name, listener);
    }

    /**
     * Registers an ITimComponent to the view controller by its name attribute if it has one.
     * @param {ITimComponent} component The component to be registered.
     * @param {string | undefined} tag for accessing  group of ITimComponents
     */
    public addTimComponent(component: ITimComponent, tag?: (string | null)) {
        // Registering with any other name than docId.taskId breaks
        // form functionality
        const taskId = component.getTaskId();
        if (taskId) {
            const name = taskId.docTask();
            this.timComponents.set(name, component);
            if (tag) {
                const prev = this.timComponentTags.get(tag);
                if (prev != undefined) {
                    prev.push(name);
                    this.timComponentTags.set(tag, prev);
                } else {
                    this.timComponentTags.set(tag, [name]);
                }
            }
            const previousComps = this.getTimComponentArray(name);
            if (previousComps != undefined) {
                previousComps.push(component);
                this.timComponentArrays.set(name, previousComps);
            } else {
                this.timComponentArrays.set(name, [component]);
            }
        }
    }

    public getTimComponentArray(name: string): ITimComponent[] | undefined {
        if (!name) {
            return undefined;
        }
        name = this.normalizeTaskId(name);
        return this.timComponentArrays.get(name);
    }

    /**
     * Returns an ITimComponent where register ID matches the given string.
     * If docID is not present then automatically append with current docID
     * @param {string} name The register ID of the ITimComponent.
     * @returns {ITimComponent | undefined} Matching component if there was one.
     */
    public getTimComponentByName(name: string): ITimComponent | undefined {
        if (!name) { return undefined; }
        name = this.normalizeTaskId(name);
        return this.timComponents.get(name);
    }

    public getTimComponentsByTag(tag: string): ITimComponent[] {
        const returnList: ITimComponent[] = [];
        const arr = this.timComponentTags.get(tag);
        if (arr) {
            for (const name of arr) {
                const t = this.getTimComponentByName(name);
                if (t) {
                    returnList.push(t);
                }
            }
        }
        return returnList;
    }

    /**
     * Gets ITimComponents nested within specified area component.
     * @param{string} area name of the area object.
     * @returns {ITimComponent[]} List of ITimComponents nested within the area.
     */
    public getTimComponentsByArea(area: string): ITimComponent[] {
        const returnList: ITimComponent[] = [];
        for (const [k, v] of this.timComponents) {
            if (v.belongsToArea(area)) { returnList.push(v); }
        }
        return returnList;
    }

    /**
     * Searches for registered ITimComponent whose ID matches the given regexp.
     * @param {string} re The RegExp to be used in search.
     * @param opt Additional option how to interpret the regex.
     * @returns {ITimComponent[]} List of ITimComponents where the ID matches the regexp.
     */
    public getTimComponentsByRegex(re: string, opt: RegexOption): ITimComponent[] {
        const returnList: ITimComponent[] = [];
        let reg: RegExp;
        if (opt == RegexOption.DontPrependCurrentDocId) {
            reg = new RegExp(`^${re}$`);
        } else if (opt == RegexOption.PrependCurrentDocId) {
            reg = new RegExp(`^${this.item.id}\.${re}$`);
        } else {
            throw new Error("unreachable");
        }

        for (const [k, v] of this.timComponents) {
            if (reg.test(k)) {
                returnList.push(v);
            }
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
     * Check whether ITimComponents could lose data when leaving page or changing user
     * @param userChange true if changing user, otherwise undefined or false
     * @returns {boolean} True if at least one registered ITimComponent was in unsaved state
     */
    public checkUnSavedTimComponents(userChange?: boolean): boolean {
        let unsavedTimComponents = false;
        for (const t of this.timComponents.values()) {
            if (t.isUnSaved(userChange)) {
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
        for (const uc of this.userChangeListeners.values()) {
            uc.userChanged(user);
        }
        if (updateAll) {
            for (const lo of this.ldrs.values()) {
                lo.loadPlugin();
                // await lo.abLoad.promise;
            }
            for (const lo of this.ldrs.values()) {
                await lo.abLoad.promise;
            }
        } else {
            // Ensure form answerBrowsers are loaded even without updateAll/form_mode
            // TODO: Refactor: repeated lines from loader promise loop above
            // TODO: Keep track of formAb loders? Finding them here at every user change may be unnecessary
            // TODO: Or flag "loaded" and skip this part
            const loders: PluginLoaderCtrl[] = [];
            for (const loader of this.ldrs.values()) {
                if (loader.isInFormMode()) {
                    loders.push(loader);
                }
            }
            for (const lo of loders) {
                lo.loadPlugin();
                // await lo.abLoad.promise;
            }
            for (const lo of loders) {
                await lo.abLoad.promise;
            }
        }

        if (this.formAbs.getSize() > 0) {
            const taskList = [];
            for (const fab of this.formAbs.entities.values()) {
                taskList.push(fab.taskId);
            }
            const r = await to($http.post<{ answers: { [index: string]: IAnswer | undefined }, userId: number }>("/userAnswersForTasks", {
                tasks: taskList,
                user: user.id,
            }));
            if (!r.ok) {
                console.error("userAnswersForTasks failed");
                return;
            }
            const answerResponse = r.result;
            if (!this.formTaskInfosLoaded) {
                // TODO: answerLimit does not currently work with fields and point browser is not visible in forms
                //  - takes long time to load on pages with lots of form plugins
                // const taskInfoResponse = await to($http.post<{ [index: string]: ITaskInfo }>(
                //     "/infosForTasks",  // + window.location.search,  // done in interceptor
                //     {
                //     tasks: taskList,
                // }));
                this.formTaskInfosLoaded = true;
                for (const fab of this.formAbs.entities.values()) {
                    // fab.setInfo(taskInfoResponse.data[fab.taskId]);
                    fab.setInfo({userMin: 0,
                    userMax: 0,
                    answerLimit: 0,
                    });
                }
            }
            if (answerResponse.data.userId == this.selectedUser.id) {
                for (const fab of this.formAbs.entities.values()) {
                    const ans = answerResponse.data.answers[fab.taskId];
                    this.handleAnswerSet(ans, fab, user, true);
                }
            }
        }

        // TODO: do not call changeUser separately if updateAll enabled
        // - handle /answers as single request for all related plugins instead of separate requests
        // - do the same for /taskinfo and /getState requests
        for (const ab of this.abs.entities.values()) {
            ab.changeUser(user, updateAll);
        }
    }

    private handleAnswerSet(ans: IAnswer | undefined, fab: AnswerBrowserController, user: IUser, force: boolean) {
        if (ans === undefined) {
            fab.changeUserAndAnswers(user, []);
        } else {
            fab.changeUserAndAnswers(user, [ans]);
        }
        const timComps = this.getTimComponentArray(fab.taskId);
        if (timComps) {
            for (const timComp of timComps) {
                if (timComp.isUnSaved() && !force) {
                    continue;
                }
                if (fab.selectedAnswer) {
                    const parsed = JSON.parse(fab.selectedAnswer.content);
                    if (StringUnknownDict.is(parsed)) {
                        timComp.setAnswer(parsed);
                    } else {
                        console.warn("selectedAnswer content was not a dict with string keys:", parsed);
                    }
                } else {
                    timComp.resetField();
                }
            }
        }
    }

    public async updateFields(taskids: string[]) {
        // TODO: if(!taskids) use all formAbs / regular abs
        // TODO: Change regular answerBrowser's user and force update
        // TODO: Refactor (repeated lines from changeUser)
        taskids = widenFields(taskids);
        const formAbMap = new Map<string, AnswerBrowserController>();
        const fabIds: string[] = [];
        const regularAbMap = new Map<string, AnswerBrowserController>();
        for (const t of taskids) {
            const loader = this.getPluginLoader(t);
            if (!loader) {
                continue;
            }
            // TODO: Separate loads and awaits into own loops
            // No need to await before calling next load
            loader.loadPlugin();
            await loader.abLoad.promise;
            const fab = this.getFormAnswerBrowser(t);
            if (fab) {
                formAbMap.set(fab.taskId, fab);
                fabIds.push(fab.taskId);
            } else {
                const ab = this.getAnswerBrowser(t);
                if (ab) {
                    regularAbMap.set(ab.taskId, ab);
                }
            }
        }
        if (this.formAbs.getSize() > 0) {
            const r = await to($http.post<{ answers: { [index: string]: IAnswer | undefined }, userId: number }>("/userAnswersForTasks", {
                tasks: fabIds,
                user: this.selectedUser.id,
            }));
            if (!r.ok) {
                return;
            }
            const answerResponse = r.result;
            for (const fab of formAbMap.values()) {
                const ans = answerResponse.data.answers[fab.taskId];
                this.handleAnswerSet(ans, fab, this.selectedUser, false);
            }
        }
        for (const ab of regularAbMap.values()) {
            ab.getAnswersAndUpdate();
            ab.loadInfo();
        }
    }

    public updateTable(tableTaskId: string, fields?: string[]) {
        // TODO Refactor:
        //  - incorporate into updateFields (call all tables if updateFields was called with "auto" setting.
        //  - add support for multiple tables
        const tableForm = this.getTableForm(tableTaskId);
        if (!tableForm) {
            return;
        }
        if (fields) {
            tableForm.updateFields(fields);
        } else {
            tableForm.updateTable();
        }
    }

    public updateAllTables(fields?: string[]) {
        // TODO: Should probably check groups
        //  currently it is assumed that caller used same groups as tableForm
        for (const table of this.tableForms.values()) {
            const taskId = table.getTaskId();
            if (taskId) {
                const tid = taskId.docTask();
                const comptab = this.getTimComponentByName(tid);
                if (comptab?.isUnSaved()) {
                    continue;
                }
            }
            if (fields && fields.length > 0) {
                table.updateFields(fields);
            } else {
                table.updateTable();
            }
        }
    }

    public runJsRunner(runner: string, groups: string[]) {
        const jsRunner = this.getJsRunner(runner);
        if (jsRunner) {
            jsRunner.runScriptWithGroups(groups);
        }
    }

    async beginUpdate() {
        const response = await to($http.get<{ changed_pars: { [id: string]: string } }>("/getUpdatedPars/" + this.docId));
        if (!response.ok) {
            return;
        }
        this.updatePendingPars(new Map<string, string>(Object.entries(response.result.data.changed_pars)));
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
        if (documentglobals().editMode) {
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
        return documentglobals().editMode;
    }

    getAllowMove() {
        return documentglobals().allowMove;
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

    private abs = new EntityRegistry<string, AnswerBrowserController>();
    private formAbs = new EntityRegistry<string, AnswerBrowserController>();

    /**
     * Registers answerbrowser to related map
     * If ((form_mode is enabled or plugin wants to be form) and answerBrowser is from timComponent
     * that supports setting answer) then add it to formAbs
     * else add it to regular ab map
     * @param ab AnswerBrowserController to be registered
     */
    registerAnswerBrowser(ab: AnswerBrowserController) {
        const timComp = this.getTimComponentByName(ab.taskId);
        if (timComp) {
            if ((this.docSettings.form_mode || timComp.isForm()) && timComp.supportsSetAnswer()) {
                // TODO: Should propably iterate like below in case of duplicates
                this.formAbs.set(ab.taskId, ab);
                return;
            }
        }
        // TODO: Task can have two instances in same document (regular field and label version)
        // - for now just add extra answerbrowsers for them (causes unnecessary requests when changing user...)
        // - maybe in future answerbrowser could find all related plugin instances and update them when ab.changeuser gets called?
        // - fix registerPluginLoader too
        // if (this.abs.has((ab.taskId))) {
        //     let index = 1;
        //     while (this.abs.has(ab.taskId + index)) {
        //         index++;
        //     }
        //     this.abs.set(ab.taskId + index, ab);
        // } else { this.abs.set(ab.taskId, ab); }
        this.abs.set(ab.taskId, ab);
    }

    getAnswerBrowser(taskId: string) {
        taskId = this.normalizeTaskId(taskId);
        return (this.abs.get(taskId) ?? this.formAbs.get(taskId));
    }

    getAnswerBrowserAsync(taskId: string) {
        taskId = this.normalizeTaskId(taskId);
        const p1 = this.abs.getEntityAsync(taskId);
        const p2 = this.formAbs.getEntityAsync(taskId);
        // The getEntityAsync method never rejects the promise, so we can use Promise.race here.
        return Promise.race([p1, p2]);
    }

    private normalizeTaskId(taskId: string) {
        if (taskId.split(".").length < 2) {
            taskId = this.docId + "." + taskId;
        }
        return taskId;
    }

    getFormAnswerBrowser(taskId: string) {
        taskId = this.normalizeTaskId(taskId);
        return this.formAbs.get(taskId);
    }

    private ldrs = new Map<string, PluginLoaderCtrl>();

    registerPluginLoader(loader: PluginLoaderCtrl) {
        // // TODO: see todos at registerAnswerBrowser
        // if (this.ldrs.has((loader.taskId))) {
        //     let index = 1;
        //     while (this.ldrs.has(loader.taskId + index)) {
        //         index++;
        //     }
        //     this.ldrs.set(loader.taskId + index, loader);
        // } else { this.ldrs.set(loader.taskId, loader); }
        this.ldrs.set(loader.taskId, loader);
    }

    getPluginLoader(taskId: string) {
        taskId = this.normalizeTaskId(taskId);
        return this.ldrs.get(taskId);
    }

    private anns = new EntityRegistry<string, AnnotationComponent>();

    registerAnnotation(loader: AnnotationComponent) {
        // This assumes that the associated DOM element for annotation is attached in the page because we need to check
        // whether it's in the right margin or not (i.e. in text).
        const prefix = loader.getKeyPrefix();
        const key = prefix + loader.annotation.id;
        this.anns.set(key, loader);
    }

    rejectTextAnnotation(loader: AnnotationComponent) {
        // Informs that there won't be an InText annotation.
        this.anns.reject("t" + loader.annotation.id);
    }

    getAnnotation(id: string) {
        return this.anns.get(id);
    }

    getAnnotationAsync(id: string): Promise<AnnotationComponent> {
        return this.anns.getEntityAsync(id);
    }

    unRegisterAnnotation(a: AnnotationComponent) {
        const prefix = a.getKeyPrefix();
        const key = prefix + a.annotation.id;
        this.anns.delete(key);
    }
}

class EntityRegistry<K, V> {
    entities = new Map<K, V>();
    private entityDefers = new Map<K, TimDefer<V>>();

    set(k: K, e: V) {
        this.entities.set(k, e);
        const defer = this.entityDefers.get(k);
        if (defer) {
            defer.resolve(e);
        }
    }

    getSize() {
        return this.entities.size;
    }

    get(k: K) {
        return this.entities.get(k);
    }

    delete(k: K) {
        this.entities.delete(k);
        this.entityDefers.delete(k);
    }

    reject(k: K) {
        const defer = this.entityDefers.get(k);
        if (defer) {
            defer.reject();
        }
    }

    getEntityAsync(k: K): Promise<V> {
        const e = this.get(k);
        if (e) {
            return new Promise<V>((resolve) => resolve(e));
        }
        const value = new TimDefer<V>();
        this.entityDefers.set(k, value);
        return value.promise;
    }
}

timApp.component("timView", {
    controller: ViewCtrl,
    template: "<div ng-transclude></div>",
    transclude: true,
});
