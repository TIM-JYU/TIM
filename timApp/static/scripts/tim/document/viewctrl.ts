import {IController, IPromise, IScope} from "angular";
import $ from "jquery";
import {timApp} from "tim/app";
import {setActiveDocument} from "tim/document/activedocument";
import {AreaHandler} from "tim/document/areas";
import {TimDocument} from "tim/document/timDocument";
import {ClipboardHandler, IClipboardMeta} from "tim/document/editing/clipboard";
import * as interceptor from "tim/document/interceptor";
import {NotesHandler} from "tim/document/notes";
import {getElementByParId, saveCurrentScreenPar} from "tim/document/parhelpers";
import {ParmenuHandler} from "tim/document/parmenu";
import {QuestionHandler} from "tim/document/question/questions";
import {initReadings} from "tim/document/readings";
import {setViewCtrl} from "tim/document/viewctrlinstance";
import {timLogTime} from "tim/util/timTiming";
import {
    getURLParameter,
    isPageDirty,
    markAsUsed,
    markPageNotDirty,
    TimStorage,
    to,
    UnknownRecord,
} from "tim/util/utils";
import {TimDefer} from "tim/util/timdefer";
import {getVisibilityVars, IVisibilityVars} from "tim/timRoot";
import {DrawCanvasComponent} from "tim/plugin/drawCanvas";
import {diffDialog} from "tim/document/showDiffDialog";
import {showInputDialog} from "tim/ui/showInputDialog";
import {InputDialogKind} from "tim/ui/input-dialog.kind";
import * as t from "io-ts";
import {ParContext} from "tim/document/structure/parContext";
import {DerefOption} from "tim/document/structure/derefOption";
import {enumPars} from "tim/document/structure/iteration";
import {getParContainerElem} from "tim/document/structure/create";
import {
    AnswerBrowserController,
    PluginLoaderCtrl,
} from "../answer/answerbrowser3";
import {IAnswer} from "../answer/IAnswer";
import {IPluginInfoResponse, ParCompiler} from "../editor/parCompiler";
import {IDocument} from "../item/IItem";
import {LectureController} from "../lecture/lectureController";
import {
    IGenericPluginMarkup,
    IGenericPluginTopLevelFields,
    nullable,
} from "../plugin/attributes";
import {TableFormComponent} from "../plugin/tableForm";
import {DocIdDotName, TaskId} from "../plugin/taskid";
import {TimTableComponent} from "../plugin/timTable";
import {initCssPrint} from "../printing/cssPrint";
import {IUser, IUserListEntry} from "../user/IUser";
import {Users} from "../user/userService";
import {widenFields} from "../util/common";
import {documentglobals} from "../util/globals";
import {$compile, $http, $interval, $timeout} from "../util/ngimport";
import {AnnotationComponent} from "../velp/annotation.component";
import {ReviewController} from "../velp/reviewController";
import {EditingHandler} from "./editing/editing";
import {PendingCollection} from "./editing/edittypes";
import {onClick} from "./eventhandlers";
import {IDocSettings} from "./IDocSettings";
import {ParRefController} from "./parRef";
import {PopupMenuDialogComponent} from "./popup-menu-dialog.component";
import {initSlideView} from "./slide";
import {ViewRangeInfo} from "./viewRangeInfo";
import {ICtrlWithMenuFunctionEntry, IMenuFunctionEntry} from "./viewutils";

markAsUsed(interceptor, ParRefController);

export interface IChangeListener {
    informAboutChanges: (
        taskId: TaskId,
        state: ChangeType,
        tag?: string
    ) => void;
}

export interface ITimComponent {
    attrsall?: IGenericPluginTopLevelFields<IGenericPluginMarkup>; // TimTable does not have attrsall - that's why it's optional.
    getName: () => string | undefined;
    getContent: () => string | undefined;
    getContentArray?: () => string[] | undefined;
    getAreas: () => string[];
    getTaskId: () => TaskId | undefined;
    belongsToArea: (area: string) => boolean;
    formBehavior: () => FormModeOption;
    isUnSaved: (userChange?: boolean) => boolean;
    save: () => Promise<{saved: boolean; message: string | undefined}>;
    getPar: () => ParContext | undefined;
    setPluginWords?: (words: string[]) => void;
    setForceAnswerSave?: (force: boolean) => void;
    resetField: () => string | undefined;
    resetChanges: () => void;
    setAnswer: (content: Record<string, unknown>) => ISetAnswerResult;
    setData?(data: unknown, save: boolean): void;
}

export interface ISetAnswerResult {
    ok: boolean;
    message: string | undefined;
}

export interface IJsRunner {
    runScript: () => Promise<void>;
    runScriptWithUsers: (userNames: string[]) => Promise<void>;
    willAutoRefreshTables: () => boolean;
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

export type DiffResult =
    | IInsertDiffResult
    | IReplaceDiffResult
    | IDeleteDiffResult
    | IChangeDiffResult;

export enum RegexOption {
    PrependCurrentDocId,
    DontPrependCurrentDocId,
}

export enum ChangeType {
    Saved,
    Modified,
}

export enum FormModeOption {
    Undecided,
    IsForm,
    NoForm,
}

export class ViewCtrl implements IController {
    private hideVars: IVisibilityVars = getVisibilityVars();
    notification: string = "";
    private videoElements = new Map<string, HTMLVideoElement>();
    clipMeta: IClipboardMeta = {
        allowPasteContent: false,
        allowPasteRef: false,
        empty: true,
    };
    public par: JQuery | undefined;

    static $inject = ["$scope"];

    public item: IDocument;
    public docId: number;

    public group: unknown;
    public scope: IScope;
    public noBrowser: boolean;
    public docVersion: [number, number];
    private startIndex: number;
    public users: IUserListEntry[];
    public teacherMode: boolean;
    private velpMode: boolean;
    private editMenuOnLeftStorage = new TimStorage(
        "editMenu_openOnLeft",
        t.boolean
    );

    private timTables = new Map<string, TimTableComponent>();
    private tableForms = new Map<string, TableFormComponent>();
    private jsRunners = new Map<string, IJsRunner>();
    private velpCanvases = new Map<number, DrawCanvasComponent>();
    private parMenuEntries = new Map<string, ICtrlWithMenuFunctionEntry>();

    // TODO: Possibly redundant since same thing can be achieved by just using the array version
    private timComponents: Map<string, ITimComponent> = new Map();
    // Array to keep reference to possible duplicated fields
    private timComponentArrays: Map<string, ITimComponent[]> = new Map();
    private timComponentTags: Map<string, string[]> = new Map();
    private userChangeListeners: Map<string, IUserChanged> = new Map();

    private inputChangeListeners = new Set<IChangeListener>();

    private pendingUpdates: PendingCollection = new Map<string, string>();
    private document?: TimDocument;
    public selectedUser: IUser;
    public editing: boolean = false;
    public defaultActionStorage = new TimStorage(
        "defAction",
        nullable(t.string)
    );
    private liveUpdates: number;
    private oldWidth: number;
    public defaultAction: string | undefined;
    public reviewCtrl: ReviewController;
    public lectureCtrl: LectureController;
    public questionHandler: QuestionHandler;
    public areaHandler: AreaHandler;
    public clipboardHandler: ClipboardHandler;
    public editingHandler: EditingHandler;
    public notesHandler: NotesHandler;
    public parmenuHandler: ParmenuHandler;
    public popupmenu?: PopupMenuDialogComponent;
    public viewRangeInfo: ViewRangeInfo;

    private jsRunnersToRun: Set<string>;

    // For search box.
    private displaySearch = false;

    // To hide actions on both sides of the page.
    public actionsDisabled = false;

    // To give an alert if trying to go to another page when doing an adaptive feedback task.
    public doingTask = false;
    public docSettings: IDocSettings = documentglobals().docSettings;

    // Form-mode related attributes.
    private formTaskInfosLoaded = false;
    private hide = getVisibilityVars();

    constructor(sc: IScope) {
        timLogTime("ViewCtrl start", "view");
        const dg = documentglobals();
        this.noBrowser = dg.noBrowser;
        this.docId = dg.curr_item.id;
        this.docVersion = dg.docVersion;
        this.item = dg.curr_item;
        this.startIndex = dg.startIndex;
        this.users = dg.users;
        this.group = dg.group;
        this.teacherMode = dg.teacherMode;
        this.velpMode = dg.velpMode;
        this.scope = sc;

        const currSel = documentglobals().current_list_user;
        if (this.users.length > 0) {
            this.selectedUser = currSel
                ? this.findUserByName(currSel.name)!.user
                : this.users[0].user;
        } else {
            this.selectedUser = Users.getCurrent();
        }

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
        if (!this.isShowSlideView()) {
            initReadings(this.item);
        } else {
            initSlideView(this.item);
        }
        onClick(
            "html",
            ($this, e) => {
                // Clicking anywhere
                const tagName = e.target.tagName.toLowerCase();
                const jqTarget = $(e.target);
                const ignoreTags = ["button", "input", "label", "i"];
                const ignoreClasses = [
                    "draghandle",
                    "editline",
                    "menu-icon",
                    "modal-dialog",
                ];

                let curElement = jqTarget;
                let limit = 10;
                while (curElement != null) {
                    if (
                        this.editing ||
                        ignoreTags.includes(tagName) ||
                        curElement.attr("position") === "absolute"
                    ) {
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
            },
            true
        );

        // If you add 'mousedown' to bind, scrolling upon opening the menu doesn't work on Android
        $("body,html").bind("scroll wheel DOMMouseScroll mousewheel", (e) => {
            if (
                (e.which && e.which > 0) ||
                e.type === "mousedown" ||
                e.type === "mousewheel"
            ) {
                $("html,body").stop();
            }
        });

        dg.allowMove = false;
        this.oldWidth = $(window).width() ?? 500;
        if (isPageDirty()) {
            showInputDialog({
                isInput: InputDialogKind.NoValidator,
                text: "The page has been modified since the last reload. Refresh now?",
                title: "Page was modified - reload?",
                okValue: true,
            });
        }
        this.liveUpdates = dg.liveUpdates;

        if (Users.isLoggedIn() && this.liveUpdates) {
            this.startLiveUpdates();
        }

        this.defaultAction =
            this.defaultActionStorage.get() ?? "Show options window";
        this.reviewCtrl = new ReviewController(this);

        const runners = getURLParameter("run_jsrunners")
            ?.split(",")
            ?.map((s) => s.trim());
        this.jsRunnersToRun = new Set(runners ?? []);

        timLogTime("ViewCtrl end", "view");

        this.editingHandler.updateEditBarState();
    }

    get editMenuOnLeft() {
        return this.editMenuOnLeftStorage.get() ?? false;
    }

    set editMenuOnLeft(value: boolean) {
        this.editMenuOnLeftStorage.set(value);
    }

    getDefaultAction(par: ParContext): IMenuFunctionEntry | undefined {
        if (!this.defaultAction) {
            return undefined;
        }
        return this.editingHandler
            .getEditorFunctions(par)
            .find((f) => f.desc === this.defaultAction);
    }

    $postLink() {
        initCssPrint();
        this.document = new TimDocument(getParContainerElem()!);
        setActiveDocument(this.document);
        const parselem = getParContainerElem();
        if (this.item.lang_id && parselem) {
            // TODO document language ids should be validated
            parselem.lang = this.item.lang_id;
        }

        this.questionHandler.processQuestions();
        this.setHeaderLinks();
        this.noBeginPageBreak();
        if (!this.isSlideOrShowSlideView()) {
            this.document.rebuildSections();
            this.processAreaVisibility();
        }
        // from https://stackoverflow.com/a/7317311
        window.addEventListener("beforeunload", (e) => {
            saveCurrentScreenPar();

            if (
                (!this.editing &&
                    !this.checkUnSavedTimComponents() &&
                    !this.doingTask) ||
                documentglobals().IS_TESTING
            ) {
                return undefined;
            }

            const msg =
                "You are currently editing something. Are you sure you want to leave the page?";

            (e || window.event).returnValue = msg; // Gecko + IE
            return msg; // Gecko + Webkit, Safari, Chrome etc.
        });
        // Change hash whenever user scrolls the document.
        window.addEventListener("scroll", (e) => {
            saveCurrentScreenPar();
        });
    }

    public processAreaVisibility(newVisibility?: Record<string, boolean>) {
        const d = new TimStorage(
            "areaVisibility",
            t.record(t.string, t.record(t.string, t.boolean))
        );
        const currentVisibilities = d.get() ?? {};
        // io-ts doesn't automatically convert number keys to strings so we use docId as string directly
        const docId = `${this.docId}`;
        let currentDoc = currentVisibilities[docId] ?? {};

        if (newVisibility) {
            currentDoc = {...currentDoc, ...newVisibility};
        }

        let hasAreas = false;
        for (const [areaName, vis] of Object.entries(currentDoc)) {
            hasAreas = true;
            const area = document.querySelector(
                `div.area.area_${areaName} > .areaContent`
            );
            if (!area || !(area instanceof HTMLElement)) {
                continue;
            }
            area.style.setProperty(
                "display",
                vis ? "block" : "none",
                "important"
            );
        }

        if (hasAreas) {
            currentVisibilities[docId] = currentDoc;
        }
        d.set(currentVisibilities);
    }

    public findUserByName(userName: string) {
        return this.users.find((u) => u.user.name === userName);
    }

    public isTranslation() {
        return (
            this.item.src_docid != null && this.item.src_docid !== this.item.id
        );
    }

    getVideo(followid: string) {
        return this.videoElements.get(followid);
    }

    registerVideo(followid: string, v: HTMLVideoElement): void {
        this.videoElements.set(followid, v);
    }

    isShowSlideView() {
        const p = document.location.pathname;
        return p.startsWith("/show_slide/");
    }

    isSlideView() {
        const p = document.location.pathname;
        return p.startsWith("/slide/");
    }

    isSlideOrShowSlideView() {
        return this.isSlideView() || this.isShowSlideView();
    }

    // noinspection JSUnusedGlobalSymbols (used in view_html.jinja2)
    showVelpSelection() {
        return (
            (this.reviewCtrl.velpMode ||
                (this.teacherMode && this.docSettings.show_velps)) &&
            !this.hide.velps
        );
    }

    startLiveUpdates() {
        const sc = this.scope;
        const origLiveUpdates = this.liveUpdates;
        if (!origLiveUpdates) {
            return;
        }
        let stop: IPromise<unknown> | undefined;
        stop = $interval(async () => {
            const r = await to(
                $http.get<{
                    version: [number, number];
                    diff: DiffResult[];
                    live: number;
                }>(
                    "/getParDiff/" +
                        this.docId +
                        "/" +
                        this.docVersion[0] +
                        "/" +
                        this.docVersion[1]
                )
            );
            if (!r.ok) {
                return;
            }
            const response = r.result;
            this.docVersion = response.data.version;
            this.liveUpdates = response.data.live; // TODO: start new loop by this or stop if None
            const replaceFn = async (d: DiffResult, parId: string) => {
                const e = getElementByParId(parId);
                const _ = await ParCompiler.compileAndReplace(e, d.content, sc);
            };
            const afterFn = async (d: DiffResult, parId: string) => {
                const e = getElementByParId(parId);
                const _ = await ParCompiler.compileAndAfter(e, d.content, sc);
            };
            const beforeFn = async (d: DiffResult, e: JQuery) => {
                const _ = await ParCompiler.compileAndBefore(e, d.content, sc);
            };
            for (const d of response.data.diff) {
                if (d.type === "delete") {
                    if (d.end_id != null) {
                        getElementByParId(d.start_id)
                            .nextUntil(getElementByParId(d.end_id))
                            .addBack()
                            .remove();
                    } else {
                        getElementByParId(d.start_id)
                            .nextAll(".par")
                            .addBack()
                            .remove();
                    }
                } else if (d.type === "replace") {
                    const first = getElementByParId(d.start_id);
                    if (d.start_id !== d.end_id) {
                        if (d.end_id != null) {
                            first
                                .nextUntil(getElementByParId(d.end_id))
                                .remove();
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
                this.document!.rebuildSections();
                this.processAreaVisibility();
            }, 1000);
            if (this.liveUpdates != origLiveUpdates) {
                // if value hase changes, stop and start new poll
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
        if (documentglobals().show_unpublished_bg) {
            document.body.classList.add("bg-unpublished");
        }
        this.scope.$watchGroup(
            [
                () => this.lectureCtrl.lectureSettings.lectureMode,
                () => this.editing,
                () => this.getEditMode(),
                () => this.getAllowMove(),
            ],
            (newValues, oldValues, scope) => {
                if (this.popupmenu && this.parmenuHandler.currCtx) {
                    this.popupmenu.updateAttrs(
                        this.parmenuHandler.getPopupAttrs(
                            this.parmenuHandler.currCtx
                        )
                    );
                }
                if (this.editing) {
                    this.notification = "Editor is already open.";
                } else {
                    this.notification = "";
                }
            }
        );
        this.reviewCtrl.loadDocumentAnnotations();

        // TODO: Inserting help par is currently independent of hideVars.editLine.
        //  The non-existence of the help par causes at least some plugins
        //  to not update properly (for example, "Saved" text will not appear when saving qst).
        if (!this.isSlideOrShowSlideView()) {
            this.editingHandler.insertHelpPar();
        }
        this.viewRangeInfo.loadRanges();

        if (this.teacherMode) {
            document.addEventListener("keydown", async (e) => {
                const activeElement = document.activeElement;
                if (!activeElement) {
                    return;
                }
                if (!e.ctrlKey) {
                    return;
                }
                let abElem: Element | null =
                    $(activeElement).parents("tim-plugin-loader")[0];
                if (!abElem) {
                    abElem = $(activeElement)
                        .parents(".par")[0]
                        ?.querySelector("tim-plugin-loader");
                }
                if (abElem) {
                    const tid = abElem.getAttribute("task-id");
                    if (tid) {
                        const p = TaskId.tryParse(tid);
                        if (p.ok) {
                            const ab = this.getAnswerBrowser(
                                p.result.docTask().toString()
                            );
                            if (ab) {
                                const changed = await ab.checkKeyPress(e);

                                // If the focus was in a textarea element, it means the focus was inside the plugin
                                // area (because answerbrowser itself doesn't have any textareas), and so we need to
                                // re-focus it.
                                if (
                                    changed &&
                                    activeElement.tagName === "TEXTAREA"
                                ) {
                                    abElem.querySelector("textarea")?.focus();
                                }
                            }
                        }
                    }
                }
            });
        }
    }

    /**
     * Registers a table controller to the view controller.
     * All table controllers need to register for toggling edit mode of
     * the table to work.
     * @param {TimTableComponent} controller The table controller.
     * @param {string} par The par context where the table is.
     */
    public addTable(controller: TimTableComponent, par: ParContext) {
        // console.log("table added!");
        this.timTables.set(par.originalPar.id, controller);
    }

    /**
     * Returns a table controller related to a specific table paragraph.
     * @param par The paragraph context.
     * @returns The table controller related to the given table paragraph, or undefined.
     */
    public getTableControllerFromParId(par: ParContext) {
        return this.timTables.get(par.originalPar.id);
    }

    public addParMenuEntry(
        controller: ICtrlWithMenuFunctionEntry,
        par: ParContext
    ) {
        this.parMenuEntries.set(par.originalPar.id, controller);
    }

    public getParMenuEntry(par: ParContext) {
        return this.parMenuEntries.get(par.originalPar.id);
    }

    // TODO: Refactor plugin interface for tableForm:
    //  - Export entire controller & keep own map vs implement ITimComponent
    public addTableForm(controller: TableFormComponent, taskId: DocIdDotName) {
        this.tableForms.set(taskId.toString(), controller);
    }

    public getTableForm(taskId: string) {
        return this.tableForms.get(taskId);
    }

    public informChangeListeners(
        taskId: TaskId,
        state: ChangeType,
        tag?: string
    ) {
        this.inputChangeListeners.forEach((listener) => {
            listener.informAboutChanges(taskId, state, tag);
        });
    }

    public addChangeListener(controller: IChangeListener) {
        this.inputChangeListeners.add(controller);
    }

    public async addJsRunner(
        runner: IJsRunner,
        taskId: DocIdDotName,
        name: string
    ) {
        const taskIdStr = taskId.toString();
        this.jsRunners.set(taskIdStr, runner);
        if (
            this.jsRunnersToRun.delete(taskIdStr) ||
            this.jsRunnersToRun.delete(name)
        ) {
            await runner.runScript();
        }
    }

    public getJsRunner(taskId: string) {
        taskId = this.normalizeTaskId(taskId);
        return this.jsRunners.get(taskId);
    }

    public addUserChangeListener(name: DocIdDotName, listener: IUserChanged) {
        this.userChangeListeners.set(name.toString(), listener);
    }

    public addVelpCanvas(id: number, canvas: DrawCanvasComponent) {
        this.velpCanvases.set(id, canvas);
    }

    public getVelpCanvas(id: number) {
        return this.velpCanvases.get(id);
    }

    /**
     * Registers an ITimComponent to the view controller by its name attribute if it has one.
     * @param {ITimComponent} component The component to be registered.
     * @param {string | undefined} tag for accessing  group of ITimComponents
     */
    public addTimComponent(component: ITimComponent, tag?: string | null) {
        // Registering with any other name than docId.taskId breaks
        // form functionality
        const taskId = component.getTaskId();
        if (taskId) {
            const name = taskId.docTaskField();
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
            if (component.attrsall?.markup.hideBrowser) {
                const ldr = this.getPluginLoader(name);
                if (ldr) {
                    ldr.hideBrowser = true;
                }
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
        if (!name) {
            return undefined;
        }
        name = this.normalizeTaskId(name);
        return this.timComponents.get(name);
    }

    public getTimComponentsByTag(tag: string): ITimComponent[] {
        const returnList: ITimComponent[] = [];
        const arr = this.timComponentTags.get(tag);
        if (arr) {
            for (const name of arr) {
                const component = this.getTimComponentByName(name);
                if (component) {
                    returnList.push(component);
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
        for (const [_, v] of this.timComponents) {
            if (v.belongsToArea(area)) {
                returnList.push(v);
            }
        }
        return returnList;
    }

    /**
     * Searches for registered ITimComponent whose ID matches the given regexp.
     * @param {string} re The RegExp to be used in search.
     * @param opt Additional option how to interpret the regex.
     * @returns {ITimComponent[]} List of ITimComponents where the ID matches the regexp.
     */
    public getTimComponentsByRegex(
        re: string,
        opt: RegexOption
    ): ITimComponent[] {
        const returnList: ITimComponent[] = [];
        let reg: RegExp;
        if (opt == RegexOption.DontPrependCurrentDocId) {
            reg = new RegExp(`^${re}$`);
        } else if (opt == RegexOption.PrependCurrentDocId) {
            // If the current document is a translation, the plugin IDs always have the doc id of the original document.
            // So we need to check for src_docid first.
            reg = new RegExp(`^${this.item.src_docid ?? this.item.id}\.${re}$`);
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
        for (const component of this.timComponents.values()) {
            if (component.isUnSaved(userChange)) {
                unsavedTimComponents = true;
                break;
            }
        }
        return unsavedTimComponents;
    }

    isEmptyDocument() {
        if (this.isSlideOrShowSlideView()) {
            return false;
        }
        for (const p of enumPars(DerefOption.NoDeref)) {
            if (!p.preamblePath) {
                return false;
            }
        }
        return true;
    }

    reload() {
        markPageNotDirty();
        window.location.reload();
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
            const fieldsToChange = new Map<
                DocIdDotName,
                AnswerBrowserController
            >();
            for (const fab of this.formAbs.entities.values()) {
                if (!fab.isUseCurrentUser()) {
                    fieldsToChange.set(fab.taskId.docTask(), fab);
                }
            }
            this.getFormAnswersAndUpdate(fieldsToChange, this.selectedUser);
        }

        // TODO: do not call changeUser separately if updateAll enabled
        // - handle /answers as single request for all related plugins instead of separate requests
        // - do the same for /taskinfo and /getState requests
        for (const ab of this.abs.entities.values()) {
            ab.changeUser(user, updateAll);
        }
    }

    private handleAnswerSet(
        ans: IAnswer | undefined,
        fab: AnswerBrowserController,
        user: IUser,
        force: boolean
    ) {
        if (ans === undefined) {
            fab.changeUserAndAnswers(user, []);
        } else {
            fab.changeUserAndAnswers(user, [ans]);
        }
        const timComps = this.getTimComponentArray(fab.taskId.docTaskField());
        if (timComps) {
            for (const timComp of timComps) {
                if (timComp.isUnSaved() && !force) {
                    continue;
                }
                if (fab.selectedAnswer) {
                    const tid = fab.taskId;
                    let parsed;
                    if (tid.field) {
                        if (tid.field === "points") {
                            // We assume that the plugin is textfield/numericfield, so it understands "c".
                            parsed = {c: fab.selectedAnswer.points};
                        } else {
                            // TODO grab custom field inside answer content?
                        }
                    } else {
                        parsed = JSON.parse(fab.selectedAnswer.content);
                    }

                    if (UnknownRecord.is(parsed)) {
                        timComp.setAnswer(parsed);
                    } else {
                        console.warn(
                            "selectedAnswer content was not a dict with string keys:",
                            parsed
                        );
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
        const formAbMap = new Map<DocIdDotName, AnswerBrowserController>();
        const regularAbMap = new Map<string, AnswerBrowserController>();
        const currentUserFormAbs = new Map<
            DocIdDotName,
            AnswerBrowserController
        >();
        for (const tid of taskids) {
            const loader = this.getPluginLoader(tid);
            if (!loader) {
                continue;
            }
            // TODO: Separate loads and awaits into own loops
            // No need to await before calling next load
            loader.loadPlugin();
            await loader.abLoad.promise;
            const fab = this.getFormAnswerBrowser(tid);
            if (fab) {
                if (!fab.isUseCurrentUser()) {
                    formAbMap.set(fab.taskId.docTask(), fab);
                } else {
                    currentUserFormAbs.set(fab.taskId.docTask(), fab);
                }
            } else {
                const ab = this.getAnswerBrowser(tid);
                if (ab) {
                    regularAbMap.set(ab.taskId.docTaskField(), ab);
                }
            }
        }
        if (formAbMap.size > 0) {
            this.getFormAnswersAndUpdate(formAbMap, this.selectedUser);
        }
        if (currentUserFormAbs.size > 0) {
            this.getFormAnswersAndUpdate(
                currentUserFormAbs,
                Users.getCurrent()
            );
        }
        for (const ab of regularAbMap.values()) {
            ab.getAnswersAndUpdate();
            ab.loadInfo();
        }
    }

    async getFormAnswersAndUpdate(
        formAnswerBrowsers: Map<DocIdDotName, AnswerBrowserController>,
        user: IUser
    ) {
        const r = await to(
            $http.post<{
                answers: Record<string, IAnswer | undefined>;
                userId: number;
            }>("/userAnswersForTasks", {
                tasks: Array.from(formAnswerBrowsers.keys()).map((task) =>
                    task.toString()
                ),
                user_id: user.id,
            })
        );
        if (!r.ok) {
            console.error("userAnswersForTasks failed");
            return;
        }
        const answerResponse = r.result;
        if (answerResponse.data.userId == user.id) {
            for (const fab of formAnswerBrowsers.values()) {
                // Note: here docTask() is correct, not docTaskField().
                const ans =
                    answerResponse.data.answers[
                        fab.taskId.docTask().toString()
                    ];
                this.handleAnswerSet(ans, fab, user, true);
            }
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
            tableForm.tryUpdateTable();
        }
    }

    public updateAllTables(fields?: string[], callerTaskId?: TaskId) {
        // TODO: Should probably check groups
        //  currently it is assumed that caller used same groups as tableForm
        for (const table of this.tableForms.values()) {
            const taskId = table.getTaskId();
            let updateAll = false;
            if (taskId) {
                if (callerTaskId) {
                    if (callerTaskId == taskId) {
                        continue;
                    }
                    const callerTaskIdStr = callerTaskId.docTaskField();
                    updateAll = table.refreshScripts
                        .map((s) => this.normalizeTaskId(s))
                        .some((s) => s == callerTaskIdStr);
                }
                const tid = taskId.docTask();
                const comptab = this.getTimComponentByName(tid.toString());
                if (comptab?.isUnSaved()) {
                    continue;
                }
            }
            if (fields && fields.length > 0 && !updateAll) {
                void table.updateFields(fields);
            } else {
                void table.tryUpdateTable();
            }
        }
    }

    /**
     * Run jsrunner in document for selected users
     * @param runner taskid of the runner
     * @param userNames list of user names to use in jsrunner
     * @return JSRunner that was run
     */
    public async runJsRunner(runner: string, userNames: string[]) {
        const jsRunner = this.getJsRunner(runner);
        if (jsRunner) {
            await jsRunner.runScriptWithUsers(userNames);
        }
        return jsRunner;
    }

    async beginUpdate() {
        const response = await to(
            $http.get<{changed_pars: Record<string, string>}>(
                "/getUpdatedPars/" + this.docId
            )
        );
        if (!response.ok) {
            return;
        }
        this.updatePendingPars(
            new Map<string, string>(
                Object.entries(response.result.data.changed_pars)
            )
        );
    }

    pendingUpdatesCount() {
        return this.pendingUpdates.size;
    }

    async updatePendingPars(pars: PendingCollection) {
        for (const [k, v] of pars) {
            this.pendingUpdates.set(k, v);
        }
        if (this.pendingUpdatesCount() < 10) {
            this.updatePending();
        } else {
            const _ = await showInputDialog({
                cancelText: "Dismiss",
                isInput: InputDialogKind.NoValidator,
                okText: "Update",
                okValue: true,
                text: `There are ${this.pendingUpdatesCount()} pending paragraph updates.`,
                title: "Updates pending",
            });
            this.updatePending();
        }
    }

    updatePending() {
        for (const [key, val] of this.pendingUpdates) {
            const par = getElementByParId(key);
            const n = $(val);
            par.replaceWith(n);
            const compiled = $($compile(n)(this.scope));
            ParCompiler.processAllMathDelayed(compiled);
        }
        this.document!.rebuildSections();
        this.pendingUpdates.clear();
        this.questionHandler.processQuestions();
        this.processAreaVisibility();
    }

    setHeaderLinks() {
        if (documentglobals().exam_mode) {
            return;
        }
        const pars = $(".par");
        // Generate selector like .parContent h1, .parContent h2, .parContent h3, ...
        const headerSelector = new Array(6)
            .fill(".parContent h")
            .map((h, i) => `${h as string}${i + 1}`)
            .join(", ");
        pars.each((index, elem) => {
            const p = $(elem);
            if (p.children("span.headerlink").length > 0) {
                return;
            }
            p.find(headerSelector).each((i, e) => {
                const h = $(e);
                const id = h.attr("id");
                if (id) {
                    h.append(
                        $(`<span class="headerlink"><a href="#${id}" class="anchor" title="Header anchor">
                                <span class="header-anchor">#</span>
                           </a>`)
                    );
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

    registerPopupMenu(param: PopupMenuDialogComponent) {
        this.popupmenu = param;
    }

    closePopupIfOpen() {
        if (this.popupmenu) {
            this.popupmenu.close();
            // await this.popupmenu.closePromise();
            this.popupmenu = undefined;
        }
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
     * Returns true if ITimComponent wants to be a form, or is in undecided state and document is in form mode
     * @param timComp ITimComponent to inspect
     */
    isTimComponentInFormMode(timComp: ITimComponent): boolean {
        return (
            (this.docSettings.form_mode &&
                timComp.formBehavior() == FormModeOption.Undecided) ||
            timComp.formBehavior() == FormModeOption.IsForm
        );
    }

    /**
     * Registers answerbrowser to related map
     * If ((form_mode is enabled or plugin wants to be form) and answerBrowser is from timComponent
     * that supports setting answer) then add it to formAbs
     * else add it to regular ab map
     * @param ab AnswerBrowserController to be registered
     */
    registerAnswerBrowser(ab: AnswerBrowserController) {
        const dtf = ab.taskId.docTaskField();
        const timComp = this.getTimComponentByName(dtf);
        if (timComp) {
            if (this.isTimComponentInFormMode(timComp)) {
                // TODO: Should probably iterate like below in case of duplicates
                this.formAbs.set(dtf, ab);
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
        this.abs.set(dtf, ab);
    }

    getAnswerBrowser(taskId: string) {
        taskId = this.normalizeTaskId(taskId);
        return this.abs.get(taskId) ?? this.formAbs.get(taskId);
    }

    getAnswerBrowserAsync(tid: DocIdDotName) {
        let taskId = tid.toString();
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
