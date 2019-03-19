import {IController, IRootElementService, IScope, ITranscludeFunction} from "angular";
import * as allanswersctrl from "tim/answer/allAnswersController";
import {timApp} from "tim/app";
import {timLogTime} from "tim/util/timTiming";
import {dereferencePar, getParId} from "../document/parhelpers";
import {ViewCtrl} from "../document/viewctrl";
import {compileWithViewctrl, ParCompiler} from "../editor/parCompiler";
import {DestroyScope} from "../ui/destroyScope";
import {showMessageDialog} from "../ui/dialog";
import {IUser} from "../user/IUser";
import {Users} from "../user/userService";
import {KEY_DOWN, KEY_LEFT, KEY_RIGHT, KEY_UP} from "../util/keycodes";
import {$filter, $http, $httpParamSerializer, $timeout, $window} from "../util/ngimport";
import {Binding, getURLParameter, markAsUsed, Require, to} from "../util/utils";
import {ReviewController} from "../velp/reviewController";
import {showAllAnswers} from "./allAnswersController";
import {IAnswer} from "./IAnswer";

markAsUsed(allanswersctrl);

timLogTime("answerbrowser3 load", "answ");

const LAZYWORD = "lazylazylazy";
const LAZYSTART = "<!--lazy ";
const LAZYEND = " lazy-->";
const RLAZYSTART = new RegExp(LAZYSTART, "g");
const RLAZYEND = new RegExp(LAZYEND, "g");

function makeNotLazy(html: string) {
    let s = html.replace(RLAZYSTART, "");
    const i = s.lastIndexOf(LAZYEND);
    if (i >= 0) {
        s = s.substring(0, i);
    }
    s = s.replace(RLAZYEND, "");
    s = s.replace(/-LAZY->/g, "-->");
    s = s.replace(/<!-LAZY-/g, "<!--");
    return s;
}

async function loadPlugin(html: string, plugin: JQuery, scope: IScope, viewctrl: ViewCtrl) {
    const newhtml = makeNotLazy(html);
    const compiled = compileWithViewctrl(newhtml, scope, viewctrl);
    await $timeout(); // let AngularJS finish its processing
    await ParCompiler.processAllMath(compiled);
    plugin.empty().append(compiled);
    plugin.css("opacity", "1.0");
}

export class PluginLoaderCtrl extends DestroyScope implements IController {
    private static $inject = ["$element", "$scope", "$transclude"];
    private compiled = false;
    private viewctrl!: Require<ViewCtrl>;
    private taskId!: Binding<string, "@">;
    private type!: Binding<string, "@">;
    private showBrowser: boolean = false;
    private pluginElement?: JQuery;
    public showPlaceholder = true;

    constructor(private element: IRootElementService, private scope: IScope, private transclude: ITranscludeFunction) {
        super(scope, element);
        this.loadPlugin = this.loadPlugin.bind(this);
        transclude((clone, s) => {
            const c = clone!.filter("[data-plugin]");
            this.pluginElement = c;
            element.append(c);
        });
        timLogTime("timPluginLoader constructor", "answ", 1);
    }

    $onInit() {
        if (this.isValidTaskId(this.taskId)) {
            const [id, name] = this.taskId.split(".");
            if (getURLParameter("task") === name) {
                this.loadPlugin();
            }
        }
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
        return taskId && taskId.slice(-1) !== "."; // TODO should check more accurately
    }

    loadPlugin() {
        this.scope.$evalAsync(async () => {
            if (this.compiled) {
                console.warn(`Plugin ${this.taskId} was already compiled`);
                return;
            }
            const plugin = this.getPluginElement();
            this.compiled = true;
            if (!this.viewctrl.noBrowser && this.isValidTaskId(this.taskId) && this.type !== "lazyonly") {
                this.showBrowser = true;
            }
            const origHtml = plugin[0].innerHTML;
            if (origHtml.indexOf(LAZYSTART) < 0) {
                // plugin is not lazy; it is already loaded
            } else {
                await loadPlugin(origHtml, plugin, this.viewctrl.scope, this.viewctrl);
            }
            this.removeActivationHandler();
        });
    }

    getPluginElement(): JQuery {
        return this.element.find("[data-plugin]");
    }

    unDimPlugin() {
        this.getPluginElement().css("opacity", "1");
    }

    dimPlugin() {
        this.getPluginElement().css("opacity", "0.3");
    }

    isPreview() {
        return this.element.parents(".previewcontent").length > 0;
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
        viewctrl: "^timView",
    },
    template: `
<div ng-if="$ctrl.answerId && $ctrl.showPlaceholder && !$ctrl.isPreview()" style="width: 1px; height: 23px;"></div>
<answerbrowser ng-class="{'has-answers': $ctrl.answerId}"
               ng-if="$ctrl.showBrowser && !$ctrl.isPreview()"
               task-id="$ctrl.taskId"
               answer-id="$ctrl.answerId">
</answerbrowser>
    `,
    transclude: true,
});

interface ITaskInfo {
    userMin: number;
    userMax: number;
    answerLimit: number;
}

interface IAnswerSaveEvent {
    taskId: string;
    savedNew: number | false;
    error?: string;
}

export class AnswerBrowserController extends DestroyScope implements IController {
    private static $inject = ["$scope", "$element"];
    private taskId!: Binding<string, "<">;
    private loading: number;
    private viewctrl!: Require<ViewCtrl>;
    private rctrl!: Require<ReviewController>;
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
    private review: boolean = false;
    private shouldFocus: boolean = false;
    private alerts: Array<{}> = [];
    private taskInfo: ITaskInfo | undefined;
    private points: number | undefined;
    private loadedAnswer: {id: number | undefined, review: boolean} = {review: false, id: undefined};
    private answerId?: Binding<number, "<?">;
    private loader!: PluginLoaderCtrl;

    constructor(private scope: IScope, private element: IRootElementService) {
        super(scope, element);
        this.loading = 0;
        this.checkKeyPress = this.checkKeyPress.bind(this);
    }

    async $onInit() {
        this.scope.$watch(() => this.taskId, (newValue, oldValue) => {
            if (newValue === oldValue) {
                return;
            }
            if (this.viewctrl.teacherMode) {
                this.getAvailableUsers();
            }
            this.getAnswersAndUpdate();
        });

        if (this.viewctrl.teacherMode) {
            // ng-keypress will not fire if the textbox has focus
            this.element[0].addEventListener("keydown", this.checkKeyPress);
        }

        this.scope.$on("answerSaved", (event, args: IAnswerSaveEvent) => {
            if (args.taskId === this.taskId) {
                if (args.savedNew) {
                    this.loadedAnswer.id = args.savedNew;
                    this.review = false;
                    this.loadedAnswer.review = false;
                    this.getAnswersAndUpdate();
                    // HACK: for some reason the math mode is lost because of the above call, so we restore it here
                    ParCompiler.processAllMathDelayed(this.loader.getPluginElement());
                }
                if (args.error) {
                    this.alerts.push({msg: args.error, type: "warning"});
                }
            }
        });

        if (this.viewctrl.selectedUser) {
            this.user = this.viewctrl.selectedUser;
        } else if (this.viewctrl.users && this.viewctrl.users.length > 0) {
            this.user = this.viewctrl.users[0];
        } else {
            this.user = Users.getCurrent();
        }
        this.loadedAnswer.id = this.getPluginHtmlAnswerId();

        // TODO: This condition is inaccurate because we don't really know who the initially loaded user is.
        if (this.viewctrl.users.length > 0 && this.user !== this.viewctrl.users[0]) {
            this.dimPlugin();
        }
        this.saveTeacher = false;
        this.users = undefined;
        this.answers = [];
        this.filteredAnswers = [];
        this.onlyValid = true;
        this.anyInvalid = false;
        this.giveCustomPoints = false;
        this.review = false;
        this.shouldFocus = false;
        this.alerts = [];

        this.scope.$watch(() => this.review, () => this.changeAnswer());
        this.scope.$watchGroup([
            () => this.onlyValid,
        ], (newValues, oldValues, scope) => this.updateFilteredAndSetNewest());

        const answs = await this.getAnswers();
        if (answs) {
            this.answers = answs;
            const updated = this.updateAnswerFromURL();
            if (!updated) {
                this.handleAnswerFetch(this.answers);
            }
        }
        await this.loadInfo();
        await this.checkUsers(); // load users, answers have already been loaded for the currently selected user

        this.loader.getPluginElement().on("mouseenter touchstart", () => {
            void this.checkUsers();
        });

        this.scope.$on("userChanged", async (event, args) => {
                this.user = args.user;
                if (args.updateAll) {
                    await this.loadUserAnswersIfChanged();
                } else if (this.hasUserChanged()) {
                    this.dimPlugin();
                } else {
                    this.unDimPlugin();
                }
            },
        );
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

    async savePoints() {
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
        if (!this.selectedAnswer) {
            return;
        }
        this.selectedAnswer.points = this.points;
    }

    updatePoints() {
        if (!this.selectedAnswer) {
            return;
        }
        this.points = this.selectedAnswer.points;
        if (this.points != null) {
            this.giveCustomPoints = this.selectedAnswer.last_points_modifier != null;
        } else {
            this.giveCustomPoints = false;
        }
    }

    setFocus() {
        this.element.focus();
    }

    async changeAnswer() {
        if (this.selectedAnswer == null || !this.user) {
            return;
        }
        this.unDimPlugin();
        this.updatePoints();
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
        if (this.selectedAnswer.id !== this.loadedAnswer.id || this.loadedAnswer.review !== this.review) {
            this.loading++;
            const r = await to($http.get<{html: string, reviewHtml: string}>("/getState", {
                params: {
                    ...parParams,
                    answer_id: this.selectedAnswer.id,
                    review: this.review,
                    user_id: this.user.id,
                },
            }));
            this.loading--;
            if (!r.ok) {
                this.showError(r.result);
                return;
            }
            this.loadedAnswer.id = this.selectedAnswer.id;
            this.loadedAnswer.review = this.review;
            void loadPlugin(r.result.data.html, this.loader.getPluginElement(), this.scope, this.viewctrl);
            if (this.review) {
                this.element.find(".review").html(r.result.data.reviewHtml);
            }
        }
        this.rctrl.loadAnnotationsToAnswer(this.selectedAnswer.id, par[0], this.review);
    }

    nextAnswer() {
        let newIndex = this.findSelectedAnswerIndex() - 1;
        if (newIndex < 0) {
            newIndex = this.filteredAnswers.length - 1;
        }
        this.selectedAnswer = this.filteredAnswers[newIndex];
        this.changeAnswer();
    }

    previousAnswer() {
        let newIndex = this.findSelectedAnswerIndex() + 1;
        if (newIndex >= this.filteredAnswers.length) {
            newIndex = 0;
        }
        this.selectedAnswer = this.filteredAnswers[newIndex];
        this.changeAnswer();
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

    checkKeyPress(e: KeyboardEvent) {
        if (this.loading > 0) {
            return;
        }
        if (e.ctrlKey) {
            // e.key does not work on IE but it is more readable, so let's use both
            if ((e.key === "ArrowUp" || e.which === KEY_UP)) {
                e.preventDefault();
                this.changeStudent(-1);
            } else if ((e.key === "ArrowDown" || e.which === KEY_DOWN)) {
                e.preventDefault();
                this.changeStudent(1);
            } else if ((e.key === "ArrowLeft" || e.which === KEY_LEFT)) {
                e.preventDefault();
                this.previousAnswer();
            } else if ((e.key === "ArrowRight" || e.which === KEY_RIGHT)) {
                e.preventDefault();
                this.nextAnswer();
            }
        }
    }

    changeStudentToIndex(newIndex: number) {
        if (!this.users) {
            return;
        }
        if (this.users.length <= 0) {
            return;
        }
        const shouldRefocusPoints = this.shouldFocus;
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
        this.getAnswersAndUpdate();

        // Be careful when modifying the following code. All browsers (IE/Chrome/FF)
        // behave slightly differently when it comes to (de-)focusing something.
        $timeout(() => {
            if (shouldRefocusPoints) {
                this.shouldFocus = shouldRefocusPoints;
            } else {
                this.setFocus();
            }
        }, 200);
    }

    changeStudent(dir: 1 | -1) {
        const newIndex = this.findSelectedUserIndex() + dir;
        this.changeStudentToIndex(newIndex);
    }

    randomStudent() {
        if (!this.users) {
            return;
        }
        const newIndex = Math.floor(Math.random() * this.users.length);
        this.changeStudentToIndex(newIndex);
    }

    setNewest() {
        if (this.filteredAnswers.length > 0) {
            this.selectedAnswer = this.filteredAnswers[0];
            this.changeAnswer();
        }
    }

    setAnswerById(id: number) {
        for (let i = 0; i < this.filteredAnswers.length; i++) {
            if (this.filteredAnswers[i].id === id) {
                this.selectedAnswer = this.filteredAnswers[i];
                this.changeAnswer();
                break;
            }
        }
    }

    getBrowserData() {
        if (this.answers.length > 0 && this.selectedAnswer && this.user) {
            return {
                answer_id: this.selectedAnswer.id,
                saveTeacher: this.saveTeacher,
                teacher: this.viewctrl.teacherMode,
                points: this.points,
                giveCustomPoints: this.giveCustomPoints,
                userId: this.user.id,
                saveAnswer: !this.viewctrl.noBrowser,
            };
        } else {
            return {
                saveTeacher: false,
                teacher: this.viewctrl.teacherMode,
                saveAnswer: !this.viewctrl.noBrowser,
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

    showError(response: {data: {error: string}}) {
        this.alerts.push({msg: "Error: " + response.data.error, type: "danger"});
    }

    getAnswerLink() {
        if (!this.user || !this.selectedAnswer) {
            return undefined;
        }
        return `/answers/${this.viewctrl.item.path}?${$httpParamSerializer({
            answerNumber: this.answers.length - this.findSelectedAnswerIndexFromUnFiltered(),
            task: this.getTaskName(),
            user: this.user.name,
        })}`;
    }

    updateAnswerFromURL() {
        const answerNumber = getURLParameter("answerNumber");
        const index = this.answers.length - parseInt(answerNumber || "1", 10);
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
        if (!this.viewctrl.item.rights || !this.viewctrl.item.rights.browse_own_answers) {
            return;
        }
        const data = await this.getAnswers();
        if (!data) {
            return;
        }
        this.handleAnswerFetch(data);
    }

    private handleAnswerFetch(data: IAnswer[]) {
        if (data.length > 0 && (this.hasUserChanged() || data.length !== this.answers.length)) {
            this.answers = data;
            this.updateFiltered();
            this.selectedAnswer = this.filteredAnswers.length > 0 ? this.filteredAnswers[0] : undefined;
            if (!this.selectedAnswer) {
                this.dimPlugin();
            } else {
                this.changeAnswer();
            }
        } else {
            this.answers = data;
            if (this.answers.length === 0 && this.viewctrl.teacherMode) {
                this.dimPlugin();
            }
            this.updateFilteredAndSetNewest();
        }
    }

    private async getAnswers() {
        if (!this.user) {
            return undefined;
        }
        this.loading++;
        const r = await to($http.get<IAnswer[]>(`/answers/${this.taskId}/${this.user.id}`));
        this.loading--;
        if (!r.ok) {
            this.showError(r.result);
            return undefined;
        }
        this.fetchedUser = this.user;
        return r.result.data;
    }

    getPluginHtmlAnswerId() {
        return this.answerId;
    }

    hasUserChanged() {
        return (this.user || {id: null}).id !== (this.fetchedUser || {id: null}).id;
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

    async loadUserAnswersIfChanged() {
        if (this.hasUserChanged()) {
            await this.getAnswersAndUpdate();
            await this.loadInfo();
        }
    }

    showTeacher() {
        return this.viewctrl.teacherMode && this.viewctrl.item.rights.teacher;
    }

    showVelpsCheckBox() {
        // return this.$parent.teacherMode || $window.velpMode; // && this.$parent.item.rights.teacher;
        return $window.velpMode || this.element.parents(".par").hasClass("has-annotation");
    }

    getTriesLeft() {
        if (this.taskInfo == null) {
            return null;
        }
        return Math.max(this.taskInfo.answerLimit - this.answers.length, 0);
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
        if (this.loading > 0) {
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

    updateFilteredAndSetNewest() {
        this.updateFiltered();
        if (this.findSelectedAnswerIndex() < 0) {
            this.setNewest();
        }
    }

    closeAlert(index: number) {
        this.alerts.splice(index, 1);
    }

    $onDestroy() {
        this.element[0].removeEventListener("keydown", this.checkKeyPress);
    }

    private getTaskName() {
        const [id, name] = this.taskId.split(".");
        return name;
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

        rctrl: "^timReview",
        viewctrl: "^timView",
    },
    templateUrl: "/static/templates/answerBrowser.html",
});
