import angular, {IScope} from "angular";
import $ from "jquery";
import ngs, {ngStorage} from "ngstorage";
import {timApp} from "tim/app";
import {AreaHandler} from "tim/controllers/view/areas";
import {ClipboardHandler} from "tim/controllers/view/clipboard";
//noinspection TypeScriptPreferShortImport
import {initIndex} from "tim/controllers/view/index";
import * as interceptor from "tim/controllers/view/interceptor";
import {NotesHandler} from "tim/controllers/view/notes";
import {getElementByParId} from "tim/controllers/view/parhelpers";
import {closeOptionsWindow, ParmenuHandler} from "tim/controllers/view/parmenu";
import {QuestionHandler} from "tim/controllers/view/questions";
import {initReadings} from "tim/controllers/view/readings";
import * as popupMenu from "tim/directives/popupMenu";
import {timLogTime} from "tim/timTiming";
import {applyMixins, isPageDirty, markAsUsed, markPageNotDirty} from "tim/utils";
import {ParCompiler} from "../../services/parCompiler";
import {$compile, $http, $window, $localStorage, $interval, $timeout, $filter} from "../../ngimport";
import {Users} from "../../services/userService";
import {Document, setActiveDocument} from "./document";
import {onClick} from "./eventhandlers";
import {MenuFunction, MenuFunctionCollection} from "./IViewCtrl";
import {EditingHandler} from "./editing";
import {RefPopupHandler} from "./refpopup";

markAsUsed(ngs, popupMenu, interceptor);

export class ViewCtrl implements QuestionHandler, AreaHandler, ClipboardHandler,
    EditingHandler, NotesHandler, ParmenuHandler, RefPopupHandler {
    overReflink: boolean;
    overPopup: boolean;

    initRefPopup(sc: IScope, view: ViewCtrl): any {
        return undefined;
    }

    showRefPopup(e, $ref, coords, attrs): JQuery {
        return undefined;
    }

    hideRefPopup(): any {
        return undefined;
    }

    lastclicktime: number;
    lastclickplace: {left; top};

    initParMenu(sc: IScope, view: ViewCtrl): any {
        return undefined;
    }

    toggleActionButtons(e, $par, toggle1, toggle2, coords): any {
        return undefined;
    }

    optionsWindowClosed(): any {
        return undefined;
    }

    updatePopupMenu(): any {
        return undefined;
    }

    noteBadgePar: JQuery;
    noteBadge: HTMLElement;

    initNotes(sc: IScope, view: ViewCtrl): any {
        return undefined;
    }

    toggleNoteEditor($parOrArea, options): any {
        return undefined;
    }

    handleNoteCancel(): any {
        return undefined;
    }

    handleNoteDelete(saveData, extraData): any {
        return undefined;
    }

    handleNoteSave(saveData, extraData): any {
        return undefined;
    }

    createNoteBadge($par): HTMLElement {
        return undefined;
    }

    addNote(): any {
        return undefined;
    }

    setNotePadge($event): any {
        return undefined;
    }

    updateNoteBadge($par): any | any {
        return undefined;
    }

    clearNoteBadge(e): any {
        return undefined;
    }

    initQuestions(sc: IScope, view: ViewCtrl): void {
    }

    initClipboard(sc: IScope, view: ViewCtrl): any {
        return undefined;
    }

    initEditing(sc: IScope, view: ViewCtrl): any {
        return undefined;
    }

    initAreas(sc: IScope, view: ViewCtrl): any {
        return undefined;
    }

    toggleParEditor($pars, options): any {
        return undefined;
    }

    toggleEditor($par, options, attrs: Object, caption, directive): any {
        return undefined;
    }

    editSettingsPars(recursiveCall): Promise<any> {
        return undefined;
    }

    showEditWindow(e, $par): any {
        return undefined;
    }

    beginAreaEditing(e, $par): any {
        return undefined;
    }

    handleCancel(extraData): any {
        return undefined;
    }

    showAddParagraphAbove(e, $par): any {
        return undefined;
    }

    showAddParagraphBelow(e, $par): any {
        return undefined;
    }

    goToEditor(): any {
        return undefined;
    }

    closeAndSave(e, $par): any {
        return undefined;
    }

    closeWithoutSaving(e, $par): any {
        return undefined;
    }

    getEditorFunctions(): MenuFunctionCollection {
        return undefined;
    }

    showAddParagraphMenu(e, $parOrArea, coords): any {
        return undefined;
    }

    getAddParagraphFunctions(): MenuFunctionCollection {
        return undefined;
    }

    removeDefaultPars(): any {
        return undefined;
    }

    showNoteWindow: MenuFunction;

    showOptionsWindow(e: any, $par: any, coords?): void {
    }

    addParagraphFunctions: MenuFunctionCollection;

    pasteFunctions: MenuFunctionCollection;
    allowPasteRef: boolean;
    allowPasteContent: boolean;

    showPasteMenu(e, $parOrArea, coords?): any {
        throw new Error(this.mixinMsg);
    }

    showMoveMenu(e, $parOrArea, coords?): any {
        throw new Error(this.mixinMsg);
    }

    pasteContentAbove(e, $par): any {
        throw new Error(this.mixinMsg);
    }

    pasteRefAbove(e, $par): any {
        throw new Error(this.mixinMsg);
    }

    pasteContentBelow(e, $par): any {
        throw new Error(this.mixinMsg);
    }

    pasteRefBelow(e, $par): any {
        throw new Error(this.mixinMsg);
    }

    deleteFromSource(): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    moveAbove(e, $parOrArea): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    moveBelow(e, $parOrArea): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    pasteAbove(e, $parOrArea, asRef): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    pasteBelow(e, $parOrArea, asRef): Promise<any> {
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

    cutPar(e, $par): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    copyPar(e, $par): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    copyArea(e, $parOrArea, overrideDocId?, cut?): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    cutArea(e, $parOrArea, cut?): any {
        throw new Error(this.mixinMsg);
    }

    handleDelete(param: {version: any}, param2: {par: any; area_start: any; area_end: any}): void {
    }

    addSavedParToDom(data, extraData: {docId: number; par: string; par_next}): void {
    }

    selectedAreaName: string;
    popupMenuAttrs: any;
    selection: {pars; start; end};

    onAreaEditClicked($this, e, className): boolean {
        throw new Error(this.mixinMsg);
    }

    showAreaOptionsWindow(e, $area, $pars, coords): any {
        throw new Error(this.mixinMsg);
    }

    startArea(e, $par): any {
        throw new Error(this.mixinMsg);
    }

    nameArea(e, $pars): any {
        throw new Error(this.mixinMsg);
    }

    nameAreaOk($area, areaName, options): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    nameAreaCancel($area): any {
        throw new Error(this.mixinMsg);
    }

    cancelArea(): any {
        throw new Error(this.mixinMsg);
    }

    removeAreaMarking(e, $pars): Promise<any> {
        throw new Error(this.mixinMsg);
    }

    extendSelection($par: any, allowShrink?: boolean): void {
        throw new Error(this.mixinMsg);
    }

    showPopupMenu(e: any, $pars: any, coords: any, popupMenuAttrs: any, $area?: any, s?: string): void {
        throw new Error(this.mixinMsg);
    }

    public questionShown: boolean;
    public firstTimeQuestions: boolean;
    public noQuestionAutoNumbering: boolean;
    public par: JQuery;
    public markup: {json?; points?; expl?};
    public questionParId: string;
    public qId: string;
    public json: {questionTitle};
    public viewctrl: ViewCtrl;
    public showQuestionPreview: boolean;
    public questionParIdNext: string;
    private mixinMsg = "This method is implemented in a mixin.";

    showQuestion(questionId: string): void {
        throw new Error(this.mixinMsg);
    }

    showQuestionNew(parId: string, parIdNext: string): void {
        throw new Error(this.mixinMsg);
    }

    addQuestionQst(e: any, $par: any): void {
        throw new Error(this.mixinMsg);
    }

    editQst(e: any, $par: any): Promise<void> {
        throw new Error(this.mixinMsg);
    }

    addQuestion(e: any, $par: any): void {
        throw new Error(this.mixinMsg);
    }

    getQuestionHtml(questions: any): JQuery {
        throw new Error(this.mixinMsg);
    }

    processQuestions(): void {
        throw new Error(this.mixinMsg);
    }

    getQuestions(): Promise<void> {
        throw new Error(this.mixinMsg);
    }

    showQuestions(): boolean {
        throw new Error(this.mixinMsg);
    }

    private static $inject = ["$scope"];

    public isLecturer: boolean;
    public lectureId: number;
    public lectureMode: boolean;
    public inLecture: boolean;
    public item: any;
    public docId: number;

    public sc: IScope;
    private hidePending: boolean;
    private group: any;
    private scope: IScope;
    private noBrowser: boolean;
    private docName: string;
    public docVersion: any;
    private crumbs: any;
    private startIndex: number;
    private users: any[];
    private teacherMode: boolean;
    private velpMode: boolean;

    private pendingUpdates: {};
    private document: Document;
    private showRefresh: boolean;
    private selectedUser: {};
    public editing: boolean;
    public $storage: ngStorage.StorageService & {defaultAction: string; noteAccess: string};
    private liveUpdates: number;
    private oldWidth: number;
    public editorFunctions: any[];
    public defaultAction: any;

    constructor(sc: IScope) {
        timLogTime("ViewCtrl start", "view");
        this.initQuestions(sc, this);
        this.initAreas(sc, this);
        this.initClipboard(sc, this);
        this.initEditing(sc, this);
        this.initNotes(sc, this);
        this.initParMenu(sc, this);
        this.initRefPopup(sc, this);
        initReadings(sc);

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
                if (newWidth !== sc.oldWidth) {
                    sc.oldWidth = newWidth;
                    const selected = $(".par.lightselect, .par.selected");
                    if (selected.length > 0) {
                        selected[0].scrollIntoView();
                    }
                }
            }
        });

        initIndex();

        // from https://stackoverflow.com/a/7317311
        $(() => {
            this.processQuestions();
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
            while (curElement !== null) {
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

        this.setHeaderLinks();
        this.document.rebuildSections();

        // If you add 'mousedown' to bind, scrolling upon opening the menu doesn't work on Android
        $("body,html").bind("scroll wheel DOMMouseScroll mousewheel", (e) => {
            if (e.which > 0 || e.type === "mousedown" || e.type === "mousewheel") {
                $("html,body").stop();
            }
        });

        this.scope.$watchGroup([
            () => this.lectureMode,
            () => this.selection.start,
            () => this.selection.end,
            () => this.editing,
            () => this.getEditMode(),
            () => this.allowPasteContent,
            () => this.allowPasteRef,
            () => this.getAllowMove()], function(newValues, oldValues, scope) {
            this.updatePopupMenu();
            if (this.editing) {
                this.notification = "Editor is already open.";
            } else {
                this.notification = "";
            }
        });

        this.$storage = $localStorage.$default({
            defaultAction: "Show options window",
            noteAccess: "everyone",
        });

        $window.allowMove = false;
        this.oldWidth = $($window).width();
        this.showRefresh = isPageDirty();
        this.liveUpdates = $window.liveUpdates;

        if (Users.isLoggedIn() && this.liveUpdates) {
            $interval(async () => {
                const response = await $http.get<{version: any, diff: any[]}>("/getParDiff/" + this.docId + "/" + this.docVersion[0] + "/" + this.docVersion[1]);
                this.docVersion = response.data.version;
                const replaceFn = async (d, parId) => {
                    const compiled = await ParCompiler.compile(d.content, sc);
                    const e = getElementByParId(parId);
                    e.replaceWith(compiled);
                };
                const afterFn = async (d, parId) => {
                    const compiled = await ParCompiler.compile(d.content, sc);
                    const e = getElementByParId(parId);
                    e.after(compiled);
                };
                const beforeFn = async (d, e) => {
                    const compiled = await ParCompiler.compile(d.content, sc);
                    e.before(compiled);
                };
                for (let i = 0; i < response.data.diff.length; ++i) {
                    const d = response.data.diff[i];
                    if (d.type === "delete") {
                        if (d.end_id !== null) {
                            getElementByParId(d.start_id).nextUntil(getElementByParId(d.end_id)).addBack().remove();
                        } else {
                            getElementByParId(d.start_id).nextAll(".par").addBack().remove();
                        }
                    } else if (d.type === "replace") {
                        const first = getElementByParId(d.start_id);
                        if (d.start_id !== d.end_id) {
                            if (d.end_id !== null) {
                                first.nextUntil(getElementByParId(d.end_id)).remove();
                            } else {
                                first.nextAll(".par").remove();
                            }
                        }
                        replaceFn(d, d.start_id);
                    } else if (d.type === "insert") {
                        if (d.after_id === null) {
                            beforeFn(d, $(".par:first"));
                        } else {
                            afterFn(d, d.after_id);
                        }
                    } else if (d.type === "change") {
                        replaceFn(d, d.id);
                    }
                }
                $timeout(function() {
                    this.document.rebuildSections();
                }, 1000);
            }, 1000 * this.liveUpdates);
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

    reload() {
        markPageNotDirty();
        $window.location.reload();
    }

    closeRefreshDlg() {
        this.showRefresh = false;
    }

    changeUser(user, updateAll) {
        this.selectedUser = user;
        this.scope.$broadcast("userChanged", {user, updateAll});
    }

    async beginUpdate() {
        const response = await $http.get<{changed_pars: any[]}>("/getUpdatedPars/" + this.docId);
        this.updatePendingPars(response.data.changed_pars);
    }

    pendingUpdatesCount() {
        return Object.keys(this.pendingUpdates).length;
    }

    showUpdateDialog() {
        return !this.hidePending && this.pendingUpdatesCount() > 0;
    }

    updatePendingPars(pars) {
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

    applyDynamicStyles($par) {
        if ($window.editMode) {
            $par.addClass("editmode");

            // Show hidden paragraphs if in edit mode
            $par.find(".mdcontent").css("display", "initial");
        }
    }

    setHeaderLinks() {
        const pars = $(".parContent");
        pars.each(function() {
            const $p = $(this);
            $p.find("h1, h2, h3, h4, h5, h6").each(function() {
                const $h = $(this);
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

timApp.controller("ViewCtrl", ViewCtrl);
