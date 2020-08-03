import angular, {IController, IScope, ITranscludeFunction} from "angular";
import * as allanswersctrl from "tim/answer/allAnswersController";
import {timApp} from "tim/app";
import {timLogTime} from "tim/util/timTiming";
import {TimDefer} from "tim/util/timdefer";
import {TaskId} from "tim/plugin/taskid";
import {dereferencePar, getParId} from "../document/parhelpers";
import {FormModeOption, ITimComponent, ViewCtrl} from "../document/viewctrl";
import {getRangeBeginParam} from "../document/viewRangeInfo";
import {compileWithViewctrl, ParCompiler} from "../editor/parCompiler";
import {IAnswerBrowserMarkupSettings, IGenericPluginMarkup} from "../plugin/attributes";
import {DestroyScope} from "../ui/destroyScope";
import {showMessageDialog} from "../ui/dialog";
import {IUser} from "../user/IUser";
import {isAdmin, userBelongsToGroupOrIsAdmin, Users} from "../user/userService";
import {documentglobals} from "../util/globals";
import {KEY_DOWN, KEY_LEFT, KEY_RIGHT, KEY_UP} from "../util/keycodes";
import {$filter, $http, $httpParamSerializer, $timeout} from "../util/ngimport";
import {Binding, getURLParameter, getUrlParams, markAsUsed, Require, to} from "../util/utils";
import {showAllAnswers} from "./allAnswersController";
import {IAnswer} from "./IAnswer";

/*
 * TODO: if forceBrowser and formMode, now does not show the browser after refresh in view-mode.
 *
 * globalField and useCurrentUser logic:
 * - Do not set showBrowser: false in loader - hidden answerBrowser is still needed to show the server responses for the
 *   user (e.g when answers are invalid after answering limits have passed). Instead use hideBrowser if needed.
 *
 * - if useCurrentUser:
 *     - save to current user always
 *     - take current user answers
 *     - do not show the answerBrowser unless forceBrowser // TODO: forceBrowser seems redundant, same can be achieved with form: false and hidebrowser settings
 *     - do not change users in hidden answerBrowser
 *
 * - if globalField:
 *     - pick the answer from anybody (wins useCurrentUser)
 *     - save to current user
 *     - use hidden answerBrowser without changing users
 *     - TODO: if hideBrowser: false, then show browser
 *
 */

markAsUsed(allanswersctrl);

timLogTime("answerbrowser3 load", "answ");

const LAZY_MARKER = "lazy";
const LAZY_MARKER_LENGTH = LAZY_MARKER.length;

function isElement(n: Node): n is Element {
    return n.nodeType === Node.ELEMENT_NODE;
}

const angularPlugins = new Set(["timTable", "tableForm", "pali"]);

async function loadPlugin(html: string, plugin: JQuery, scope: IScope, viewctrl: ViewCtrl) {
    const elementToCompile = $(html);
    const plugintype = plugin.attr("data-plugin");
    elementToCompile.attr("plugintype", plugintype ?? null);
    const taskidstr = plugin.attr("id");
    const sc = scope.$new(true) as {taskid: TaskId} & IScope;
    if (taskidstr) {
        if (plugintype && angularPlugins.has(plugintype.slice(1))) {
            // For Angular plugins (e.g. timTable and tableForm), we must pass the parsed TaskId.
            const p = TaskId.tryParse(taskidstr);
            if (p.ok) {
                sc.taskid = p.result;
                elementToCompile.attr("bind-taskid", "taskid");
                scope = sc;
            }
        } else {
            elementToCompile.attr("taskid", taskidstr);
        }
    }
    const compiled = await compileWithViewctrl(elementToCompile, scope, viewctrl);
    await ParCompiler.processAllMath(compiled);
    plugin.empty().append(compiled);
    plugin.css("opacity", "1.0");
}

export class PluginLoaderCtrl extends DestroyScope implements IController {
    static $inject = ["$element", "$scope", "$transclude"];
    private compiled = false;
    private viewctrl?: Require<ViewCtrl>;
    public taskId!: Binding<string, "@">;
    private type!: Binding<string, "@">;
    private showBrowser: boolean = false;
    public hideBrowser: boolean = false;
    private forceBrowser: boolean = false;
    private pluginElement?: JQuery;
    public showPlaceholder = true;
    public abLoad = new TimDefer<AnswerBrowserController | null>();

    constructor(private element: JQLite, private scope: IScope, private transclude: ITranscludeFunction) {
        super(scope, element);
        transclude((clone, _) => {
            const c = clone!.filter("[data-plugin]");
            this.pluginElement = c;
            element.append(c);
        });
        timLogTime("timPluginLoader constructor", "answ", 1);
    }

    $onInit() {
        if (this.viewctrl) {
            this.viewctrl.registerPluginLoader(this);
        }
        if (this.isValidTaskId(this.taskId)) {
            const [id, name] = this.taskId.split(".");
            if (getURLParameter("task") === name) {
                this.loadPlugin();
            }
        }
        const m = this.pluginMarkup();
        if (m?.hideBrowser || this.viewctrl?.docSettings.hideBrowser
            || this.isUseCurrentUser() || this.isGlobal()
            || this.isInFormMode()) {
            this.hideBrowser = true;
            this.showPlaceholder = false;
        }
        if (m?.forceBrowser) { this.forceBrowser = true; }

        this.showPlaceholder = !this.isInFormMode() && !this.hideBrowser;
    }

    $postLink() {
        this.element.on("mouseenter touchstart", this.loadPlugin);
    }

    $onDestroy() {
        this.removeActivationHandler();
    }

    private removeActivationHandler() {
        this.element.off("mouseenter touchstart", this.loadPlugin);
    }

    /**
     * Returns whether the given task id is valid.
     * A valid task id is of the form '1.taskname'.
     * @param taskId {string} The task id to validate.
     * @returns {boolean} True if the task id is valid, false otherwise.
     */
    isValidTaskId(taskId: string) {
        return taskId && !taskId.endsWith("."); // TODO should check more accurately
    }

    public pluginObject(): ITimComponent | undefined {
        if (!this.viewctrl || !this.taskId) {
            return undefined;
        }
        return this.viewctrl.getTimComponentByName(this.taskId);
    }

    public pluginMarkup(): IGenericPluginMarkup | undefined {
        const c = this.pluginObject();
        if (!c) {
            return undefined;
        }
        const a = c.attrsall;
        return a?.markup;
    }

    public isUseCurrentUser() {
        const m = this.pluginMarkup();
        return m?.useCurrentUser ?? false;
    }

    public isGlobal(): boolean {
        return this.taskId.includes("GLO_");
        // return this.pluginMarkup().globalField;
    }

    loadPlugin = () => {
        if (this.compiled) {
            return;
        }
        if (this.viewctrl) {
            const ab = this.viewctrl.getAnswerBrowser(this.taskId);
            if (ab) {
                this.compiled = true;
                this.abLoad.resolve(ab);
            }
        }

        this.scope.$evalAsync(async () => {
            const plugin = this.getPluginElement();

            this.compiled = true;
            if (this.viewctrl &&
                (!this.viewctrl.noBrowser || this.forceBrowser) &&
                this.isValidTaskId(this.taskId) &&
                this.type !== "lazyonly" && Users.isLoggedIn()) {
                this.showBrowser = true;
            } else {
                this.abLoad.resolve(null); // this plugin instance doesn't have answer browser
            }
            const h = this.getNonLazyHtml();
            if (h && this.viewctrl) {
                await loadPlugin(h, plugin, this.viewctrl.scope, this.viewctrl);
            }
            this.removeActivationHandler();
        });
    };

    /**
     * Returns the non-lazy HTML for the plugin if the plugin is lazy.
     * If the plugin is not lazy, returns nothing.
     */
    getNonLazyHtml() {
        const pe = this.getPluginElement();
        const cns = pe[0].childNodes;
        let nonLazyHtml;
        for (const n of cns) {
            if (n.nodeType === Node.COMMENT_NODE &&
                n.nodeValue &&
                n.nodeValue.startsWith(LAZY_MARKER) &&
                n.nodeValue.endsWith(LAZY_MARKER)) {
                nonLazyHtml = n.nodeValue.slice(LAZY_MARKER_LENGTH, n.nodeValue.length - LAZY_MARKER_LENGTH);
                break;
            } else if (isElement(n) && n.tagName === "DIV" && n.className === LAZY_MARKER) {
                nonLazyHtml = n.getAttribute("data-html");
                break;
            }
        }
        return nonLazyHtml;
    }

    getPluginElement(): JQuery {
        return this.element.find("[data-plugin]");
    }

    unDimPlugin() {
        const e = this.getPluginElement();
        e.css("opacity", "1");
        e.removeClass("hidden-print");
    }

    dimPlugin() {
        if (!this.isInFormMode()) {
            const e = this.getPluginElement();
            e.css("opacity", "0.3");
            e.addClass("hidden-print");
        }
    }

    isPreview() {
        return this.element.parents(".previewcontent").length > 0;
    }

    isInFormMode() {
        if (this.viewctrl) {
            const timComp = this.viewctrl.getTimComponentByName(this.taskId);
            if (timComp) {
                return this.viewctrl.isTimComponentInFormMode(timComp);
            }
        }
        return false;
    }
}

timApp.component("timPluginLoader", {
    bindings: {
        answerId: "<?",
        taskId: "@",
        type: "@",
    },
    controller: PluginLoaderCtrl,
    require: {
        viewctrl: "?^timView",
    },
    template: `
<div ng-focus="$ctrl.loadPlugin()"
     ng-if="!$ctrl.compiled"
     title="This loads the plugin if it's not loaded"
     tabindex="0"></div>
<div class="answerBrowserPlaceholder"
     ng-if="$ctrl.answerId && $ctrl.showPlaceholder && !$ctrl.isPreview()"
     style="width: 1px; height: 23px;"></div>
<answerbrowser ng-class="{'has-answers': ($ctrl.answerId && !$ctrl.hideBrowser)}"
               ng-if="$ctrl.showBrowser && !$ctrl.isPreview()"
               task-id="$ctrl.taskId"
               answer-id="$ctrl.answerId">
</answerbrowser>
    `,
    transclude: true,
});

export interface ITaskInfo {
    userMin: number;
    userMax: number;
    answerLimit: number;
    showPoints: boolean;
}

export interface IAnswerSaveEvent {
    savedNew: number | false;
    error?: string;
}

type AnswerLoadCallback = (a: IAnswer) => void;

export type AnswerBrowserData =
    { saveAnswer: boolean; teacher: boolean; saveTeacher: false }
    | { saveAnswer: boolean; teacher: boolean; saveTeacher: boolean; answer_id: number | undefined; userId: number; points: number | undefined; giveCustomPoints: boolean };

const DEFAULT_MARKUP_CONFIG: IAnswerBrowserMarkupSettings = {
    pointsStep: 0,
};

export class AnswerBrowserController extends DestroyScope implements IController {
    static $inject = ["$scope", "$element"];
    public taskId!: Binding<string, "<">;
    private loading: number;
    private viewctrl!: Require<ViewCtrl>;
    private user: IUser | undefined;
    private fetchedUser: IUser | undefined;
    private saveTeacher: boolean = false;
    private users: IUser[] | undefined;
    private answers: IAnswer[] = [];
    private filteredAnswers: IAnswer[] = [];
    private onlyValid: boolean = true;
    public selectedAnswer: IAnswer | undefined;
    private anyInvalid: boolean = false;
    private giveCustomPoints: boolean = false;
    public review: boolean = false;
    public oldreview: boolean = false;
    private shouldFocus: boolean = false;
    private alerts: Array<unknown> = [];
    private taskInfo: ITaskInfo | undefined;
    private points: number | undefined;
    private loadedAnswer: { id: number | undefined } = {id: undefined};
    private answerId?: Binding<number, "<?">;
    private loader!: PluginLoaderCtrl;
    private reviewHtml?: string;
    private answerLoader?: AnswerLoadCallback;
    private pointsStep: number = 0.01;
    private markupSettings: IAnswerBrowserMarkupSettings = DEFAULT_MARKUP_CONFIG;
    private isValidAnswer = false;
    private hidden: boolean = false;
    private showDelete = false;
    private formMode = false;

    constructor(private scope: IScope, private element: JQLite) {
        super(scope, element);
        this.loading = 0;
    }

    registerNewAnswer(args: IAnswerSaveEvent) {
        if (args.savedNew) {
            this.loadedAnswer.id = args.savedNew;
            this.review = false;
            this.oldreview = false;
            if (this.answers.length == 0 && this.viewctrl.teacherMode) {
                this.getAvailableUsers();
            }
            this.getAnswersAndUpdate();
            // HACK: for some reason the math mode is lost because of the above call, so we restore it here
            ParCompiler.processAllMathDelayed(this.loader.getPluginElement());
        }
        if (args.error) {
            this.alerts.push({msg: args.error, type: "warning"});
        }
    }

    async $onInit() {
        this.formMode = (this.loader.isInFormMode() && !this.forceBrowser());
        if (this.loader.hideBrowser || this.formMode) {
            this.hidden = true;
        }

        this.viewctrl.registerAnswerBrowser(this);
        this.scope.$watch(() => this.taskId, (newValue, oldValue) => {
            if (newValue === oldValue) {
                return;
            }
            if (this.viewctrl.teacherMode) {
                this.getAvailableUsers();
            }
            this.getAnswersAndUpdate();
        });

        if (this.isUseCurrentUser() || this.isGlobal()) {
            this.user = Users.getCurrent();
        } else if (this.viewctrl.selectedUser) {
            this.user = this.viewctrl.selectedUser;
        } else if (this.viewctrl.users && this.viewctrl.users.length > 0) {
            this.user = this.viewctrl.users[0].user;
        } else {
            this.user = Users.getCurrent();
        }
        this.loadedAnswer.id = this.getPluginHtmlAnswerId();

        // TODO: This condition is inaccurate because we don't really know who the initially loaded user is.
        if (this.viewctrl.users.length > 0 && this.user !== this.viewctrl.users[0].user
            && !this.isUseCurrentUser() && !this.isGlobal()) {
            this.dimPlugin();
        }
        this.saveTeacher = (this.viewctrl.docSettings.save_teacher && this.viewctrl.teacherMode) ?? false;
        this.showDelete = isAdmin() && getURLParameter("showAdmin") != null;
        this.users = undefined;
        this.answers = [];
        this.filteredAnswers = [];
        this.anyInvalid = false;
        this.giveCustomPoints = false;
        this.review = false;
        this.shouldFocus = false;
        this.alerts = [];

        const markup = this.loader.pluginMarkup();
        if (markup?.answerBrowser) {
            this.markupSettings = markup.answerBrowser;
        }

        // Ensure the point step is never zero because some browsers don't like step="0" value in number inputs.
        if (this.markupSettings.pointsStep) {
            this.pointsStep = this.markupSettings?.pointsStep;
        }

        this.scope.$watch(() => this.review, (newValue, oldValue) => {
            if (newValue == oldValue) {
                return;
            }
            // TODO: Separate function and route for just review
            this.changeAnswer(true);
        });
        this.scope.$watch(
            () => this.onlyValid,
            (newValue, oldValue, scope) => {
                if (newValue == oldValue) {
                    return;
                }
                this.updateFilteredAndSetNewest();
            });

        // If task is in form_mode, only last (already loaded) answer should matter.
        // Answer changes are handled by viewctrl, so don't bother querying them here
        // TODO: Make a route to handle getAnswers on global task. Currently global task has its' answerBrowser
        //  always hidden and queries as they are now do not handle global task correctly, causing useless plugin update
        if (!this.formMode && !this.isGlobal()) {
            const answs = await this.getAnswers();
            if (answs && (answs.length > 0)) {
                this.answers = answs;
                const updated = this.updateAnswerFromURL();
                if (!updated) {
                    this.handleAnswerFetch(this.answers);
                }
            }
            await this.checkUsers(); // load users, answers have already been loaded for the currently selected user

            this.loader.getPluginElement().on("mouseenter touchstart", () => {
                void this.checkUsers();
            });
        }
        if (!this.formMode) {
            await this.loadInfo();
        }
        // TODO: Angular throws error if many answerbrowsers resolve around the same time
        //  (e.g awaits above don't happen if were in form mode and don't want separate info reqs)
        await $timeout(0);
        this.loader.abLoad.resolve(this);
    }

    async changeUser(user: IUser, updateAll: boolean) {
        if (this.isGlobal() || this.isUseCurrentUser()) {
            return;
        }
        this.user = user;
        if (updateAll) {
            await this.loadUserAnswersIfChanged();
        } else if (this.hasUserChanged()) {
            this.dimPlugin();
        } else {
            this.unDimPlugin();
        }
    }

    changeUserAndAnswers(user: IUser, answers: IAnswer[]) {
        if (!this.isGlobal() && !this.isUseCurrentUser()) {
            this.user = user;
            this.fetchedUser = this.user;
        }
        this.answers = answers;
        this.updateFiltered();
        this.selectedAnswer = this.filteredAnswers.length > 0 ? this.filteredAnswers[0] : undefined;
    }

    private unDimPlugin() {
        this.loader.unDimPlugin();
    }

    urlParamMatchesThisTask() {
        return getURLParameter("task") === this.getTaskName();
    }

    async $postLink() {
        this.loader.showPlaceholder = false;
        if (this.urlParamMatchesThisTask()) {
            await $timeout(0);
            this.element[0].scrollIntoView();
        }
    }

    async saveValidity() {
        if (!this.selectedAnswer) {
            return;
        }
        await to($http.put("/answer/saveValidity", {
            valid: this.isValidAnswer,
            answer_id: this.selectedAnswer.id,
        }));
        this.selectedAnswer.valid = this.isValidAnswer;
    }

    trySavePoints(updateAnswers: boolean = false) {
        if (!this.selectedAnswer || !this.user) {
            return true;
        }

        const formEl = this.element.find(".point-form");
        if (!formEl.length) {
            return true;
        }

        const formElement = formEl[0] as HTMLFormElement;
        if (!formElement.reportValidity()) {
            return false;
        }

        if (this.points == this.selectedAnswer.points) {
            return true;
        }

        const doSavePoints = async () => {
          await this.savePoints(false);
          if (updateAnswers) {
              await this.getAnswersAndUpdate();
          }
        };
        doSavePoints();
        return true;
    }

    async savePoints(updatePointsState: boolean = true) {
        if (!this.selectedAnswer || !this.user) {
            return;
        }
        const r = await to($http.put("/savePoints/" + this.user.id + "/" + this.selectedAnswer.id,
            {points: this.points}));
        this.shouldFocus = true;
        if (!r.ok) {
            this.showError(r.result);
            return;
        }
        if (!updatePointsState || !this.selectedAnswer) {
            return;
        }
        this.selectedAnswer.points = this.points;
    }

    updatePoints() {
        if (!this.selectedAnswer) {
            this.points = undefined;
            return;
        }
        this.points = this.selectedAnswer.points;
        this.isValidAnswer = this.selectedAnswer.valid;
        if (this.points != null) {
            this.giveCustomPoints = this.selectedAnswer.last_points_modifier != null;
        } else {
            this.giveCustomPoints = false;
        }
    }

    setFocus() {
        this.element.focus();
    }

    setAnswerLoader(ac: AnswerLoadCallback) {
        if (this.answerLoader) {
            console.warn(`answerListener was already set for task ${this.taskId}`);
        }
        this.answerLoader = ac;
    }

    /**
     * Handles loading up selected answer:
     * Loads plugin html with selected answer (or user default state if no answer)
     * also sets up points, fetches review data if needed and dims the plugin if teacher-mode and no answers
     * @param changeReviewOnly only fetch and set the plugin review html, without touching anything else
     * // TODO: Separate function for just fetching the review html
     */
    async changeAnswer(changeReviewOnly = false) {
        if (!changeReviewOnly) {
            this.updatePoints();
        }
        if (!this.user) {
            return;
        }
        const par = this.element.parents(".par");
        const ids = dereferencePar(par);
        if (!ids) {
            return;
        }
        const parParams = {
            doc_id: ids[0],
            par_id: ids[1],
            ref_from_doc_id: this.viewctrl.docId,
            ref_from_par_id: getParId(par),
        };
        const preParams = {
            ...parParams,
            review: this.review,
            user_id: this.user.id,
        };
        let taskOrAnswer = {};
        if (this.selectedAnswer) {
            taskOrAnswer = {answer_id: this.selectedAnswer.id};
        } else {
            taskOrAnswer = {task_id: this.taskId};
        }
        // get new state as long as the previous answer was not explicitly the same as the one before
        // otherwise we might not see the unanswered plugin exactly how the user sees it
        if (!(this.selectedAnswer && this.loadedAnswer
            && this.selectedAnswer.id == this.loadedAnswer.id
            && this.oldreview == this.review)) {
            this.loading++;
            const r = await to($http.get<{ html: string, reviewHtml: string }>("/getState", {
                params: {
                    ...preParams,
                    ...taskOrAnswer,
                },
            }));
            this.loading--;
            if (!r.ok) {
                this.showError(r.result);
                return;
            }

            this.oldreview = this.review;

            if (!changeReviewOnly) {
                this.loadedAnswer.id = this.selectedAnswer?.id;
                // Plugins with an iframe usually set their own callback for loading an answer so that the iframe doesn't
                // have to be fully reloaded every time.
                if (this.answerLoader && this.selectedAnswer) {
                    this.answerLoader(this.selectedAnswer);
                } else {
                    await loadPlugin(r.result.data.html, this.loader.getPluginElement(), this.scope, this.viewctrl);
                }
            }
            if (this.review) {
                this.reviewHtml = r.result.data.reviewHtml;
                await $timeout();
            }
        }
        if (this.selectedAnswer) {
            this.viewctrl.reviewCtrl.loadAnnotationsToAnswer(this.selectedAnswer.id, par[0]);
        }
        if (!changeReviewOnly) {
            if (this.viewctrl.teacherMode && !this.selectedAnswer && !this.saveTeacher) {
                this.dimPlugin();
            } else {
                this.unDimPlugin();
            }
        }
    }

    async changeAnswerTo(dir: (-1 | 1)) {
        if (!this.trySavePoints(true)) {
            return;
        }

        let newIndex = this.findSelectedAnswerIndex() + dir;
        if (newIndex < 0) {
            newIndex = this.filteredAnswers.length - 1;
        } else if (newIndex >= this.filteredAnswers.length) {
            newIndex = 0;
        }
        this.selectedAnswer = this.filteredAnswers[newIndex];
        await this.changeAnswer();
    }

    findSelectedUserIndex() {
        if (!this.users || !this.user) {
            return -1;
        }
        for (let i = 0; i < this.users.length; i++) {
            if (this.users[i].id === this.user.id) {
                return i;
            }
        }
        return -1;
    }

    handlePointScroll(e: KeyboardEvent) {
        if (this.loading > 0 || this.markupSettings.pointsStep) {
            return;
        }
        if ((e.key === "ArrowUp" || e.which === KEY_UP) ||
            (e.key === "ArrowDown" || e.which === KEY_DOWN)) {
            e.preventDefault();
        }
    }

    async checkKeyPress(e: KeyboardEvent) {
        if (this.loading > 0) {
            return false;
        }
        if (e.ctrlKey) {
            // e.key does not work on IE but it is more readable, so let's use both
            if ((e.key === "ArrowUp" || e.which === KEY_UP)) {
                e.preventDefault();
                await this.changeStudent(-1);
                return true;
            } else if ((e.key === "ArrowDown" || e.which === KEY_DOWN)) {
                e.preventDefault();
                await this.changeStudent(1);
                return true;
            } else if ((e.key === "ArrowLeft" || e.which === KEY_LEFT)) {
                e.preventDefault();
                await this.changeAnswerTo(-1);
                return true;
            } else if ((e.key === "ArrowRight" || e.which === KEY_RIGHT)) {
                e.preventDefault();
                await this.changeAnswerTo(1);
                return true;
            }
        }
        return false;
    }

    async changeStudentToIndex(newIndex: number) {
        if (this.isGlobal() || this.isUseCurrentUser()) {
            return;
        }
        if (!this.users) {
            return;
        }
        if (this.users.length <= 0) {
            return;
        }
        if (!this.trySavePoints()) {
            return;
        }
        const shouldRefocusPoints = this.shouldFocus;
        this.shouldFocus = false;
        if (newIndex >= this.users.length) {
            newIndex = 0;
        }
        if (newIndex < 0) {
            newIndex = this.users.length - 1;
        }
        if (newIndex < 0) {
            return;
        }
        this.user = this.users[newIndex];
        await this.getAnswersAndUpdate();

        if (shouldRefocusPoints) {
            this.shouldFocus = shouldRefocusPoints;
        } else {
            this.setFocus();
        }
    }

    shouldFocusIfSelectedAnswer() {
        if (this.selectedAnswer) {
            this.shouldFocus = true;
        }
    }

    async changeStudent(dir: 1 | -1) {
        const newIndex = this.findSelectedUserIndex() + dir;
        await this.changeStudentToIndex(newIndex);
    }

    async randomStudent() {
        if (!this.users) {
            return;
        }
        const newIndex = Math.floor(Math.random() * this.users.length);
        await this.changeStudentToIndex(newIndex);
    }

    async setNewest() {
        if (this.filteredAnswers.length > 0) {
            this.selectedAnswer = this.filteredAnswers[0];
        }
        await this.changeAnswer();
    }

    async setAnswerById(id: number, updateImmediately: boolean) {
        for (const f of this.filteredAnswers) {
            if (f.id === id) {
                this.selectedAnswer = f;
                if (updateImmediately) {
                    await this.changeAnswer();
                }
                break;
            }
        }
    }

    getBrowserData(): AnswerBrowserData {
        const common = {
            teacher: this.viewctrl.teacherMode,
            saveAnswer: !this.viewctrl.noBrowser,
        };
        if (this.user) {
            let userId = this.user.id;
            if (this.isGlobal()) {
                userId = Users.getCurrent().id;
            }
            return {
                answer_id: this.selectedAnswer ? this.selectedAnswer.id : undefined,
                saveTeacher: this.saveTeacher ||
                    (this.viewctrl.teacherMode && (this.loader.isInFormMode() || this.isGlobal())) , // TODO: Check if correct
                points: this.points,
                giveCustomPoints: this.giveCustomPoints,
                userId: userId,
                ...common,
            };
        } else {
            return {
                saveTeacher: false,
                ...common,
            };
        }
    }

    async getAvailableUsers() {
        this.loading++;
        const r = await to($http.get<IUser[]>("/getTaskUsers/" + this.taskId, {params: {group: this.viewctrl.group}}));
        this.loading--;
        if (!r.ok) {
            this.showError(r.result);
            return;
        }
        this.users = r.result.data;
    }

    showError(response: { data: { error: string } }) {
        this.alerts.push({msg: "Error: " + response.data.error, type: "danger"});
    }

    getAnswerLink(single = false) {
        if (!this.user || !this.selectedAnswer) {
            return undefined;
        }
        let newroute = "answers";
        if (single && location.href.includes("/view/")) { newroute = "view"; }  // TODO: think other routes also?
        const par = this.element.parents(".par");
        const parId = getParId(par);
        const currBegin = getRangeBeginParam() ?? 0;
        const params = getUrlParams();
        const group = params.get("group"); // TODO: should we save other params also?
        const rangeParams = single ? {
            b: parId,
            size: 1,
            group: group,
        } : {};
        const link = `/${newroute}/${this.viewctrl.item.path}?${$httpParamSerializer({
            answerNumber: this.answers.length - this.findSelectedAnswerIndexFromUnFiltered(),
            task: this.getTaskName(),
            user: this.user.name,
            ...rangeParams,
        })}`;
        return link;
    }

    updateAnswerFromURL() {
        const answerNumber = getURLParameter("answerNumber");
        const index = this.answers.length - parseInt(answerNumber ?? "1", 10);
        if (answerNumber != null && this.urlParamMatchesThisTask()) {
            if (index >= 0 && index < this.answers.length) {
                this.onlyValid = false;
                this.updateFiltered();
                this.selectedAnswer = this.filteredAnswers.length > 0 ? this.filteredAnswers[index] : undefined;
                this.changeAnswer();
                return true;
            } else {
                void showMessageDialog(`Answer number ${answerNumber} is out of range for this task and user.`);
            }
        }
        return false;
    }

    async getAnswersAndUpdate() {
        // if ( this.isUseCurrentUser(this.taskId) ) { return null; }

        if (!this.viewctrl.item.rights || !this.viewctrl.item.rights.browse_own_answers) {
            return;
        }

        const data = await this.getAnswers();
        if (!data) {
            return;
        }
        await this.handleAnswerFetch(data);
        return data;
    }

    private async handleAnswerFetch(data: IAnswer[]) {
        if (data.length > 0 && (this.hasUserChanged() || data.length !== this.answers.length) || this.forceBrowser()) {
            this.answers = data;
            this.updateFiltered();
            this.selectedAnswer = this.filteredAnswers.length > 0 ? this.filteredAnswers[0] : undefined;
            if (!this.selectedAnswer && !this.forceBrowser()) {
                this.dimPlugin();
            } else {
                await this.changeAnswer();
            }
        } else {
            this.answers = data;
            if (this.answers.length === 0 && this.viewctrl.teacherMode) {
                this.dimPlugin();
            }
            await this.updateFilteredAndSetNewest();
        }
    }

    private getUserOrCurrentUserForAnswers(taskId: string): IUser | undefined {
        let user: IUser | undefined;
        if (this.user) { user = this.user; }
        // TODO: refactor to use pluginMarkup()
        const c = this.viewctrl.getTimComponentByName(this.taskId.split(".")[1]);
        if (!c) { return user; }
        const a = c.attrsall;
        if (a?.markup?.useCurrentUser) {
            this.user = Users.getCurrent();  // TODO: looks bad when function has a side effect?
            return Users.getCurrent();
        }
        return user;
    }

    public pluginMarkup(): IGenericPluginMarkup | undefined {
        if (!this.viewctrl || !this.taskId) {
            return undefined;
        }
        const c = this.viewctrl.getTimComponentByName(this.taskId); // TODO: why here is no split?
        if (!c) {
            return undefined;
        }
        const a = c.attrsall;
        return a?.markup;
    }

    public isUseCurrentUser(): boolean {
        const m = this.pluginMarkup();
        return m?.useCurrentUser ?? false;
    }

    public isGlobal() {
        return this.taskId.includes("GLO_");
        // return this.pluginMarkup().globalField;
    }

    public forceBrowser() {
        const m = this.pluginMarkup();
        return m?.forceBrowser;
    }

    /* Return user answers, null = do not care */
    private async getAnswers() {
        const user = this.getUserOrCurrentUserForAnswers(this.taskId);
        if (!user) {
            return undefined;
        }
        this.loading++;

        const r = await to($http.get<IAnswer[]>(`/getAnswers/${this.taskId}/${user.id}`, {
            params: {
                _: Date.now(),
            },
        }));

        this.loading--;
        if (!r.ok) {
            this.showError(r.result);
            return undefined;
        }
        this.fetchedUser = user;
        return r.result.data;
    }

    getPluginHtmlAnswerId() {
        return this.answerId;
    }

    hasUserChanged() {
        return (this.user ?? {id: null}).id !== (this.fetchedUser ?? {id: null}).id;
    }

    dimPlugin() {
        this.loader.dimPlugin();
    }

    allowCustomPoints() {
        if (this.taskInfo == null) {
            return false;
        }
        return this.taskInfo.userMin != null && this.taskInfo.userMax != null;
    }

    // TODO: Delete? Possibly only plugins registered as form plugins should reset,
    //  and that is handled by viewctrl
    resetITimComponent() {
        const c = this.viewctrl.getTimComponentByName(this.taskId.split(".")[1]);
        if (c) {
            c.resetField();
        }
    }

    async loadUserAnswersIfChanged() {
        if (this.hasUserChanged()) {
            const answers = await this.getAnswersAndUpdate();
            // if (!answers || answers.length === 0) {
            //     // if ( answers != null ) { this.resetITimComponent(); }
            // } else {
            //     this.loadedAnswer = {review: false, id: undefined};
            //     this.changeAnswer();
            // }
            await this.loadInfo();
        }
    }

    showTeacher() {
        return this.viewctrl.teacherMode && this.viewctrl.item.rights.teacher;
    }

    showVelpsCheckBox() {
        // return this.$parent.teacherMode || $window.velpMode; // && this.$parent.item.rights.teacher;
        return documentglobals().velpMode || this.element.parents(".par").hasClass("has-annotation");
    }

    getTriesLeft() {
        if (this.taskInfo == null) {
            return null;
        }
        return Math.max(this.taskInfo.answerLimit - this.answers.length, 0);
    }

    public setInfo(info: ITaskInfo) {
        this.taskInfo = info;
    }

    async loadInfo() {
        if (this.taskInfo != null) {
            return;
        }
        this.loading++;
        const r = await to($http.get<ITaskInfo>("/taskinfo/" + this.taskId));
        this.loading--;
        if (!r.ok) {
            this.showError(r.result);
            return;
        }
        this.taskInfo = r.result.data;
    }

    async checkUsers() {
        // TODO: Changing user from sidebar could change to user's answer on global field
        // for now just skip the fetches (firefox throws error in their current state)
        if (this.loading > 0 || this.isGlobal() || this.isUseCurrentUser()) {
            return;
        }
        await this.loadUserAnswersIfChanged();
        if (this.viewctrl.teacherMode && this.users == null) {
            this.users = [];
            if (this.viewctrl.users.length > 0) {
                await this.getAvailableUsers();
            }
        }
    }

    async getAllAnswers() {
        await showAllAnswers({
            url: "/allAnswersPlain/" + this.taskId,
            identifier: this.taskId,
            allTasks: false,
        });
    }

    findSelectedAnswerIndex() {
        if (this.filteredAnswers == null || this.selectedAnswer == null) {
            return -1;
        }
        for (let i = 0; i < this.filteredAnswers.length; i++) {
            if (this.filteredAnswers[i].id === this.selectedAnswer.id) {
                return i;
            }
        }
        return -1;
    }

    private findSelectedAnswerIndexFromUnFiltered() {
        if (this.selectedAnswer == null) {
            return -1;
        }
        for (let i = 0; i < this.answers.length; i++) {
            if (this.answers[i].id === this.selectedAnswer.id) {
                return i;
            }
        }
        return -1;
    }

    updateFiltered() {
        this.anyInvalid = false;
        this.filteredAnswers = $filter("filter")<IAnswer>(this.answers, (value, index, array) => {
            if (value.valid) {
                return true;
            }
            this.anyInvalid = true;
            return !this.onlyValid;
        });
    }

    async updateFilteredAndSetNewest() {
        this.updateFiltered();
        // updateFilteredAndSetNewest is called if "Show valid only" checkbox changes.
        // If the rest of the answerbrowser is hidden, the user doesn't have a way to change the answer,
        // so we do it whenever the checkbox changes.
        if (this.findSelectedAnswerIndex() < 0 || this.hidden) {
            await this.setNewest();
        }
    }

    closeAlert(index: number) {
        this.alerts.splice(index, 1);
    }

    $onDestroy() {
    }

    private getTaskName() {
        const [id, name] = this.taskId.split(".");
        return name;
    }

    private toggleInput() {
        if (this.saveTeacher) {
            this.unDimPlugin();
        } else if (!this.selectedAnswer) {
            this.dimPlugin();
        }
    }

    async deleteAnswer() {
        if (!this.selectedAnswer) {
            return;
        }
        if (!window.confirm("Delete this answer?")) {
            return;
        }
        const aid = this.selectedAnswer.id;
        await to($http.post("/answer/delete", {
            answer_id: aid,
        }));
        const index = this.answers.findIndex((a) => a.id === aid);
        if (index < 0) {
            return;
        }
        this.answers.splice(index, 1);
        this.updateFiltered();
        if (index < this.filteredAnswers.length) {
            this.selectedAnswer = this.filteredAnswers[index];
        } else if (this.filteredAnswers.length > 0) {
            this.selectedAnswer = this.filteredAnswers[0];
        }
        await this.changeAnswer();
    }
}

timApp.component("answerbrowser", {
    bindings: {
        answerId: "<?",
        taskId: "<",
    },
    controller: AnswerBrowserController,
    require: {
        loader: "^timPluginLoader",

        // This is not a real component but a workaround for getting reference to the correct paragraph.
        // Answerbrowser gets compiled before being attached to DOM to avoid too early rendering,
        // so we can't use "this.element.parents()" to look for parent elements, so instead, we pass
        // the paragraph to the compile call.
        // par: "^timPar",

        viewctrl: "^timView",
    },
    templateUrl: "/static/templates/answerBrowser.html",
});
