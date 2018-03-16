import angular, {IController, IPromise, IScope} from "angular";
import $ from "jquery";
import ngs, {ngStorage} from "ngstorage";
import {timApp} from "tim/app";
import {AreaHandler} from "tim/controllers/view/areas";
import {ClipboardHandler} from "tim/controllers/view/clipboard";
//noinspection TypeScriptPreferShortImport
import {initIndex} from "tim/controllers/view/index";
import * as interceptor from "tim/controllers/view/interceptor";
import {NotesHandler} from "tim/controllers/view/notes";
import {getElementByParId, Paragraph} from "tim/controllers/view/parhelpers";
import {closeOptionsWindow, ParmenuHandler} from "tim/controllers/view/parmenu";
import {QuestionHandler} from "tim/controllers/view/questions";
import {initReadings} from "tim/controllers/view/readings";
import * as popupMenu from "tim/directives/popupMenu";
import {timLogTime} from "tim/timTiming";
import {isPageDirty, markAsUsed, markPageNotDirty} from "tim/utils";
import {initCssPrint} from "../../cssPrint";
import {IItem} from "../../IItem";
import {IUser} from "../../IUser";
import {$compile, $filter, $http, $interval, $localStorage, $timeout, $window} from "../../ngimport";
import {IPluginInfoResponse, ParCompiler} from "../../services/parCompiler";
import {Users} from "../../services/userService";
import {LectureController} from "../lectureController";
import {ReviewController} from "../reviewController";
import {Document, setActiveDocument} from "./document";
import {EditingHandler} from "./editing";
import {onClick} from "./eventhandlers";
import {IPopupMenuAttrs} from "./parmenu";
import {RefPopupHandler} from "./refpopup";
import {MenuFunctionCollection, MenuFunctionEntry} from "./viewutils";

markAsUsed(ngs, popupMenu, interceptor);

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

export class ViewCtrl implements IController {
    private notification: string;
    addParagraphFunctions: MenuFunctionCollection;
    pasteFunctions: MenuFunctionCollection;
    allowPasteRef: boolean;
    allowPasteContent: boolean;
    popupMenuAttrs: IPopupMenuAttrs;
    selection: {pars: JQuery | null; start: Paragraph | null; end: Paragraph | null};
    public par: JQuery;

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
    public questionHandler: QuestionHandler;
    public areaHandler: AreaHandler;
    public clipboardHandler: ClipboardHandler;
    public editingHandler: EditingHandler;
    public notesHandler: NotesHandler;
    public parmenuHandler: ParmenuHandler;
    public refpopupHandler: RefPopupHandler;

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
        initIndex();
        initCssPrint();

        // from https://stackoverflow.com/a/7317311
        $(() => {
            this.questionHandler.processQuestions();
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
            this.parmenuHandler.updatePopupMenu(par.length > 0 ? par : undefined);
            if (this.editing) {
                this.notification = "Editor is already open.";
            } else {
                this.notification = "";
            }
        });
    }

    isEmptyDocument() {
        return this.docVersion[0] === 0 && this.docVersion[1] === 0; // TODO can be empty otherwise too
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
        this.questionHandler.processQuestions();
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
}

timApp.component("timView", {
    controller: ViewCtrl,
    template: "<div ng-transclude></div>",
    transclude: true,
});
