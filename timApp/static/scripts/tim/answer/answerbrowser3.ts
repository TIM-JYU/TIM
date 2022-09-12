import {
    AfterViewInit,
    ApplicationRef,
    ChangeDetectorRef,
    Component,
    DoBootstrap,
    ElementRef,
    Inject,
    Input,
    NgModule,
    OnDestroy,
    OnInit,
    SimpleChanges,
    ViewChild,
} from "@angular/core";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {IController, IScope} from "angular";
import {timLogTime} from "tim/util/timTiming";
import {TaskId} from "tim/plugin/taskid";
import {DrawCanvasComponent, DrawCanvasModule} from "tim/plugin/drawCanvas";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {showAllAnswersDialog} from "tim/answer/showAllAnswersDialog";
import {tryCreateParContextOrHelp} from "tim/document/structure/create";
import {ParContext} from "tim/document/structure/parContext";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {showResetTaskLock} from "tim/answer/showResetTaskLock";
import {loadPlugin, PluginLoaderComponent} from "tim/answer/pluginLoader";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {PurifyModule} from "tim/util/purify.module";
import {isVelpable, ViewCtrl} from "../document/viewctrl";
import {ParCompiler} from "../editor/parCompiler";
import {
    IAnswerBrowserSettings,
    IGenericPluginMarkup,
    IGenericPluginTopLevelFields,
} from "../plugin/attributes";
import {IUser, sortByRealName} from "../user/IUser";
import {isAdmin, Users} from "../user/userService";
import {documentglobals} from "../util/globals";
import {KEY_DOWN, KEY_LEFT, KEY_RIGHT, KEY_UP} from "../util/keycodes";
import {$filter, $http, $httpParamSerializer, $timeout} from "../util/ngimport";
import {
    getURLParameter,
    getUrlParams,
    getUrlParamsJSON,
    getViewName,
    Require,
    to,
    to2,
} from "../util/utils";
import {IAnswer, IAnswerWithUsers, IModelAnswerSettings} from "./IAnswer";

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

timLogTime("answerbrowser3 load", "answ");

// TODO: make PluginLoaderCtrl and AnswerBrowserController to implement this so
//    many this.showFeedback(txt) implementations could be replaced just on one call:
//    showFeedback(this: IFeedback, txt:string = '', scrollToView: boolean=false);
/*
export interface IFeedback {
   scope: IScope;
   viewctrl?: Require<ViewCtrl>;
   feedback?: string;
}
 */

export interface ITaskInfo {
    userMin: number;
    userMax: number;
    answerLimit: number;
    showPoints: boolean;
    newtask?: boolean;
    buttonNewTask: string;
    modelAnswer?: IModelAnswerSettings;
    starttime?: string;
    deadline?: string;
    triesText?: string;
    maxPoints?: string | number;
    pointsText?: string;
}

export interface IAnswerSaveEvent {
    savedNew: number | false;
    error?: string;
    topfeedback?: string;
    feedback?: string;
    valid?: boolean;
}

type AnswerLoadCallback = (a: IAnswer) => void;

export type AnswerBrowserData =
    | {saveAnswer: boolean; teacher: boolean; saveTeacher: false}
    | {
          saveAnswer: boolean;
          teacher: boolean;
          saveTeacher: boolean;
          answer_id: number | undefined;
          userId: number;
          points: number | undefined;
          giveCustomPoints: boolean;
          answernr: number | undefined;
      };

const DEFAULT_MARKUP_CONFIG: IAnswerBrowserSettings = {
    pointsStep: 0,
    validOnlyText: $localize`Show valid only`,
};

@Component({
    selector: "answerbrowser",
    templateUrl: "../../../templates/answerBrowser.html",
    styles: [],
})
export class AnswerBrowserComponent
    implements AfterViewInit, IController, OnDestroy, OnInit
{
    @Input() public taskId!: TaskId;
    @ViewChild("modelAnswerDiv") modelAnswerRef?: ElementRef<HTMLDivElement>;
    @ViewChild("feedback") feedBackElement?: ElementRef<HTMLDivElement>;
    loading: number;
    viewctrl!: Require<ViewCtrl>;
    user: IUser | undefined;
    private fetchedUser: IUser | undefined;
    reviewerUser: IUser | undefined;
    saveTeacher: boolean = false;
    users: IUser[] | undefined;
    reviewerUsers: IUser[] = [];
    answers: IAnswer[] = [];
    filteredAnswers: IAnswer[] = [];
    onlyValid: boolean = true;
    public selectedAnswer: IAnswer | undefined;
    private selectedAnswerCanvas?: DrawCanvasComponent;
    anyInvalid: boolean = false;
    giveCustomPoints: boolean = false;
    public review: boolean = false;
    imageReview: boolean = false;
    private imageReviewUrls: string[] = [];
    imageReviewDatas: string[] = [];
    private imageReviewUrlIndex = 0;
    public oldreview: boolean = false;
    private shouldFocus: boolean = false;
    alerts: Array<{msg: string; type: "warning" | "danger"}> = [];
    feedback?: string;
    taskInfo: ITaskInfo | undefined;
    points: number | undefined;
    private loadedAnswer: {
        id: number | undefined;
        valid: boolean | undefined;
    } = {id: undefined, valid: undefined};
    // private answerId?: Binding<number, "<?">;
    @Input() public answerId?: number;
    // @Input() public loader!: PluginLoaderComponent;
    public loader!: PluginLoaderComponent;

    reviewHtml?: string;
    private answerLoader?: AnswerLoadCallback;
    pointsStep: number = 0.01;
    markupSettings: IAnswerBrowserSettings = DEFAULT_MARKUP_CONFIG;
    modelAnswer?: IModelAnswerSettings;
    private modelAnswerFetched = false;
    modelAnswerHtml?: string;
    modelAnswerVisible = false;
    isValidAnswer = false;
    hidden: boolean = false;
    showDelete = false;
    formMode = false;
    showBrowseAnswers = true;
    isPeerReview = false;
    peerReviewEnabled = false;
    showNewTask = false;
    buttonNewTask = $localize`New task`;

    constructor(
        private element: ElementRef<HTMLElement>,
        @Inject("$scope") private scope: IScope,
        public cdr: ChangeDetectorRef
    ) {
        this.loading = 0;
    }

    registerNewAnswer(args: IAnswerSaveEvent) {
        if (args.savedNew || this.saveTeacher) {
            if (args.savedNew) {
                this.loadedAnswer.id = args.savedNew;
                this.loadedAnswer.valid = args.valid;
            }
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
            const markup = this.pluginMarkup();
            let displayAlert = true;
            if (markup?.warningFilter) {
                const pattern = new RegExp(markup.warningFilter, "i");
                displayAlert = !pattern.test(args.error);
            }
            if (displayAlert) {
                this.alerts.push({msg: args.error, type: "warning"});
            }
        }
        this.showFeedback(args.topfeedback);
        this.loader.showFeedback(args.feedback);
        this.cdr.detectChanges();
        // TODO Check if redundant
        this.scope.$evalAsync(); // required because this method may be called from Angular context
    }

    async ngOnInit() {
        this.viewctrl = vctrlInstance!;
        this.loader = this.viewctrl.getPluginLoader(
            this.taskId.docTask().toString()
        )!;
        this.formMode = this.loader.isInFormMode() && !this.forceBrowser();
        if (this.loader.hideBrowser || this.formMode) {
            this.hidden = true;
        }
        const isPeerReview = getViewName() == "review";
        this.isPeerReview = isPeerReview;
        this.review = isPeerReview;
        this.peerReviewEnabled = this.viewctrl.peerReviewInProcess() ?? false;
        if (!this.viewctrl.item.rights.teacher && isPeerReview) {
            this.showBrowseAnswers = false;
        }

        this.viewctrl.registerAnswerBrowser(this);
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
        this.loadedAnswer.valid = true;

        // TODO: This condition is inaccurate because we don't really know who the initially loaded user is.
        if (
            this.viewctrl.users.length > 0 &&
            this.user !== this.viewctrl.users[0].user &&
            !this.isUseCurrentUser() &&
            !this.isGlobal()
        ) {
            this.dimPlugin();
        }
        this.saveTeacher =
            (this.viewctrl.docSettings.save_teacher &&
                this.viewctrl.teacherMode) ??
            false;
        this.showDelete = isAdmin() && getURLParameter("showAdmin") != null;
        this.users = undefined;
        this.answers = [];
        this.filteredAnswers = [];
        this.anyInvalid = false;
        this.giveCustomPoints = false;
        this.shouldFocus = false;
        this.alerts = [];
        this.feedback = "";
        this.showNewTask = this.isAndSetShowNewTask();

        if (this.viewctrl?.docSettings.answerBrowser) {
            this.markupSettings = {
                ...this.markupSettings,
                ...this.viewctrl.docSettings.answerBrowser,
            };
        }
        const markup = this.loader.pluginMarkup();
        if (markup?.answerBrowser) {
            this.markupSettings = {
                ...this.markupSettings,
                ...markup.answerBrowser,
            };
        }

        // Ensure the point step is never zero because some browsers don't like step="0" value in number inputs.
        if (this.markupSettings.pointsStep) {
            this.pointsStep = this.markupSettings?.pointsStep;
        }
        if (this.markupSettings.showValidOnly != undefined) {
            this.onlyValid = this.markupSettings.showValidOnly;
        }

        // If task is in form_mode, only last (already loaded) answer should matter.
        // Answer changes are handled by viewctrl, so don't bother querying them here
        // TODO: Make a route to handle getAnswers on global task. Currently global task has its' answerBrowser
        //  always hidden and queries as they are now do not handle global task correctly, causing useless plugin update
        if (!this.formMode && !this.isGlobal()) {
            const answs = await this.getAnswers();
            if (answs && answs.length > 0) {
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
        if (this.viewctrl?.teacherMode) {
            this.getAvailableUsers();
        }
        if (!this.formMode) {
            await this.loadInfo();
        }
        // TODO: Angular throws error if many answerbrowsers resolve around the same time
        //  (e.g awaits above don't happen if were in form mode and don't want separate info reqs)
        await $timeout(0);
        this.loader.abLoad.resolve(this);
    }

    reviewToggled(): void {
        // TODO: Separate function and route for just review
        this.changeAnswer(true);
    }

    userSelected() {
        this.tryChangeDocumentSelectedUser();
        this.getAnswersAndUpdate();
    }

    /**
     * Inform viewcontrol about new selected user
     */
    tryChangeDocumentSelectedUser() {
        // TODO:
        //  corner case: this.getAvailableUsers may have user not in current userList or related entries
        if (this.user) {
            this.viewctrl.changeUserFromAb(this.user, this.taskId);
        }
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
            const par = this.getPar();
            if (par) {
                this.viewctrl.reviewCtrl.clearAnswerAnnotationsFromParMargin(
                    par
                );
            }
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
        this.selectedAnswer =
            this.filteredAnswers.length > 0
                ? this.filteredAnswers[0]
                : undefined;
    }

    private unDimPlugin() {
        this.loader.unDimPlugin();
    }

    urlParamMatchesThisTask() {
        return getURLParameter("task") === this.getTaskName();
    }

    async ngAfterViewInit() {
        this.loader.showPlaceholder = false;
        if (this.urlParamMatchesThisTask()) {
            await $timeout(0);
            this.element.nativeElement.scrollIntoView();
        }
    }

    async saveValidity() {
        if (!this.selectedAnswer) {
            return;
        }
        await to(
            $http.put("/answer/saveValidity", {
                valid: this.isValidAnswer,
                answer_id: this.selectedAnswer.id,
            })
        );
        this.selectedAnswer.valid = this.isValidAnswer;
    }

    trySavePoints(updateAnswers: boolean = false) {
        if (!this.selectedAnswer || !this.user) {
            return true;
        }

        const formEl =
            this.element.nativeElement.querySelectorAll(".point-form");
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
        const r = await to(
            $http.put(`/savePoints/${this.user.id}/${this.selectedAnswer.id}`, {
                points: this.points,
            })
        );
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
            this.giveCustomPoints =
                this.selectedAnswer.last_points_modifier != null;
        } else {
            this.giveCustomPoints = false;
        }
    }

    setFocus() {
        this.element.nativeElement.focus();
    }

    setAnswerLoader(ac: AnswerLoadCallback) {
        if (this.answerLoader) {
            console.warn(
                `answerListener was already set for task ${this.taskId.docTaskField()}`
            );
        }
        this.answerLoader = ac;
    }

    /**
     * Handles loading up selected answer:
     * Loads plugin html with selected answer (or user default state if no answer)
     * also sets up points, fetches review data if needed and dims the plugin if teacher-mode and no answers
     * @param changeReviewOnly only fetch and set the plugin review html, without touching anything else
     * @param askNew true if new task is asked to generate
     * // TODO: Separate function for just fetching the review html
     */
    async changeAnswer(changeReviewOnly = false, askNew: boolean = false) {
        if (!changeReviewOnly) {
            this.updatePoints();
        }
        if (!this.user) {
            return;
        }
        const par = this.getPar();
        if (!par) {
            return;
        }
        const orig = par.originalPar;
        const parParams = {
            doc_id: par.par.docId,
            par_id: par.par.id,
            ref_from_doc_id: orig.docId,
            ref_from_par_id: orig.id,
        };
        const preParams = {
            ...parParams,
            review: this.review,
            user_id: this.user.id,
        };
        let taskOrAnswer: Record<string, string | number | boolean>;
        if (this.selectedAnswer) {
            const idx = this.findSelectedAnswerIndexRevFromUnFiltered();
            taskOrAnswer = {answer_id: this.selectedAnswer.id};
            if (idx >= 0) {
                taskOrAnswer.answernr = idx;
            }
            if (this.answerLoader) {
                this.answerLoader(this.selectedAnswer);
            }
        } else {
            taskOrAnswer = {task_id: this.taskId.docTask().toString()};
            taskOrAnswer.answernr = this.filteredAnswers.length;
        }
        if (askNew) {
            taskOrAnswer.ask_new = true;
        }
        // get new state as long as the previous answer was not explicitly the same as the one before
        // otherwise we might not see the unanswered plugin exactly how the user sees it
        if (
            !(
                this.selectedAnswer &&
                this.loadedAnswer &&
                this.selectedAnswer.id == this.loadedAnswer.id &&
                this.oldreview == this.review
            )
        ) {
            this.loading++;
            const r = await to(
                $http.get<{html: string; reviewHtml: string}>("/getState", {
                    params: {
                        ...preParams,
                        ...taskOrAnswer,
                        ...getUrlParamsJSON(),
                    },
                })
            );
            this.loading--;
            if (!r.ok) {
                this.showError(r.result);
                return;
            }

            this.oldreview = this.review;

            if (!changeReviewOnly) {
                if (
                    this.loadedAnswer.id != this.selectedAnswer?.id &&
                    this.loadedAnswer.valid
                ) {
                    this.showFeedback("");
                    this.loader.showFeedback("");
                }
                this.loadedAnswer.id = this.selectedAnswer?.id;
                this.loadedAnswer.valid = this.selectedAnswer?.valid;
                // Plugins with an iframe usually set their own callback for loading an answer so that the iframe doesn't
                // have to be fully reloaded every time.
                if (this.answerLoader && this.selectedAnswer) {
                    // Do nothing; the answerLoader gets called earlier in this method.
                } else {
                    await loadPlugin(r.result.data.html, this.loader);
                }
            }
            if (this.review) {
                this.reviewerUser = undefined;
                let newReviewHtml = r.result.data.reviewHtml;
                // Check if component itself provides review data and try to load it instead
                const comp = this.viewctrl.getTimComponentByName(
                    this.taskId.docTaskField()
                );
                if (isVelpable(comp)) {
                    const velpImages = await comp.getVelpImages();
                    if (velpImages) {
                        this.imageReview = true;
                        this.imageReviewDatas = velpImages;
                    }
                } else if (newReviewHtml.startsWith("data:image")) {
                    this.imageReview = true;
                    this.imageReviewDatas = [newReviewHtml];
                } else if (newReviewHtml.startsWith("imageurls:")) {
                    this.imageReview = true;
                    const fetchedImages: string[] = [];
                    this.imageReviewUrls = newReviewHtml
                        .substring(10)
                        .split(";");
                    const promises = this.imageReviewUrls.map((url) =>
                        to($http.get<Blob>(url, {responseType: "blob"}))
                    );
                    const ress = await Promise.all(promises);
                    for (const blobRes of ress) {
                        if (!blobRes.ok) {
                            // Push some invalid image src to show it as 404
                            fetchedImages.push("error");
                            continue;
                        }
                        const base64Data = await this.readAsDataUrl(
                            blobRes.result.data
                        );
                        fetchedImages.push(base64Data as string);
                    }
                    this.imageReviewDatas = fetchedImages;
                }

                if (this.selectedAnswer) {
                    if (newReviewHtml === this.reviewHtml) {
                        // It's possible that the user was changed but the user has exactly the same
                        // answer as the current one. In that case, we still want to refresh the DOM
                        // because we need to clear the annotations of the previous user.
                        // That's why we append a space here so that Angular will always update the DOM.
                        newReviewHtml += " ";
                    }
                    this.reviewHtml = newReviewHtml;
                } else {
                    this.reviewHtml = undefined;
                }
                this.setReviewerUsers();
                await $timeout();
            }
        }
        // if reviewhtml is image-based, then the image load callback should handle annotation loading
        if (!(this.imageReview && this.review)) {
            this.viewctrl.reviewCtrl.loadAnnotationsToAnswer(
                this.selectedAnswer?.id,
                par,
                this.reviewerUser
            );
        }
        if (!changeReviewOnly) {
            if (
                this.viewctrl.teacherMode &&
                !this.selectedAnswer &&
                !this.saveTeacher
            ) {
                this.dimPlugin();
            } else {
                this.unDimPlugin();
            }
        }
        this.isAndSetShowNewTask();
    }

    readAsDataUrl(input: Blob) {
        return new Promise((resolve, reject) => {
            const reader = new FileReader();

            reader.onerror = () => {
                reader.abort();
                reject(new DOMException("Problem parsing input blob."));
            };

            reader.onload = () => {
                resolve(reader.result);
            };

            reader.readAsDataURL(input);
        });
    }

    /**
     * Function to be passed for canvas on image-based reviews which is called when the canvas is ready and loaded.
     * @param canvas DrawCanvasComponent who made the callback
     */
    public setImageReview = (canvas: DrawCanvasComponent) => {
        if (this.selectedAnswer && this.reviewHtml) {
            const par = this.getPar();
            if (!par) {
                return;
            }
            this.selectedAnswerCanvas = canvas;
            this.viewctrl.reviewCtrl.setCanvas(this.selectedAnswer.id, canvas);
            this.viewctrl.reviewCtrl.loadAnnotationsToAnswer(
                this.selectedAnswer.id,
                par,
                this.reviewerUser
            );
        } else {
            this.selectedAnswerCanvas = undefined;
        }
    };

    /**
     * Moves selected answer to newer or older answer. Rolls over if at the end/start.
     *
     * @param dir -1 for older answer, 1 for newer answer
     */
    async changeAnswerTo(dir: -1 | 1) {
        if (!this.trySavePoints(true)) {
            return;
        }
        if (!(await this.checkUnsavedAnnotations())) {
            return;
        }

        // Note: filteredAnswers is always a descending list of answer IDs (i.e. from newest to oldest)
        // As such, flip the dir to make more sense
        dir *= -1;

        let newIndex = this.findSelectedAnswerIndex() + dir;
        if (newIndex < 0) {
            newIndex = this.filteredAnswers.length - 1;
        } else if (newIndex >= this.filteredAnswers.length) {
            newIndex = 0;
        }
        this.selectedAnswer = this.filteredAnswers[newIndex];
        await this.changeAnswer();
    }

    async newTask() {
        if (!this.trySavePoints(true)) {
            return;
        }

        this.selectedAnswer = undefined;
        this.showNewTask = false;
        await this.changeAnswer(false, true);
        this.cdr.detectChanges();
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
        if (
            e.key === "ArrowUp" ||
            e.which === KEY_UP ||
            e.key === "ArrowDown" ||
            e.which === KEY_DOWN
        ) {
            e.preventDefault();
        }
    }

    async checkKeyPress(e: KeyboardEvent) {
        if (this.loading > 0 || this.hidden) {
            return false;
        }
        if (e.ctrlKey) {
            // e.key does not work on IE but it is more readable, so let's use both
            if (e.key === "ArrowUp" || e.which === KEY_UP) {
                e.preventDefault();
                await this.changeStudent(-1);
                return true;
            } else if (e.key === "ArrowDown" || e.which === KEY_DOWN) {
                e.preventDefault();
                await this.changeStudent(1);
                return true;
            } else if (e.key === "ArrowLeft" || e.which === KEY_LEFT) {
                e.preventDefault();
                await this.changeAnswerTo(-1);
                return true;
            } else if (e.key === "ArrowRight" || e.which === KEY_RIGHT) {
                e.preventDefault();
                await this.changeAnswerTo(1);
                return true;
            }
        }
        return false;
    }

    async changeStudentToIndex(newIndex: number) {
        if (!(await this.checkUnsavedAnnotations())) {
            return;
        }
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
        this.tryChangeDocumentSelectedUser();
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

    private async checkUnsavedAnnotations() {
        if (this.selectedAnswerCanvas?.isUnSaved()) {
            return await showConfirm(
                $localize`Unsaved velps`,
                $localize`You have unsaved velps. Changing the answer will discard them. Continue?`
            );
        }
        return true;
    }

    async setNewest() {
        if (this.filteredAnswers.length > 0) {
            this.selectedAnswer = this.filteredAnswers[0];
        }
        await this.changeAnswer();
    }

    async setAnswerById(answerId: number) {
        // TODO: Global / useCurrentUser probably fails here
        if (this.hasUserChanged()) {
            const answers = await this.getAnswers();
            if (!answers) {
                return;
            }
            await this.handleAnswerFetch(answers, answerId);
        } else {
            const ans = this.filteredAnswers.find((f) => f.id === answerId);
            if (ans) {
                this.selectedAnswer = ans;
                await this.changeAnswer();
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
            const answernr = this.findSelectedAnswerIndexRevFromUnFiltered();
            return {
                answer_id: this.selectedAnswer
                    ? this.selectedAnswer.id
                    : undefined,
                saveTeacher:
                    this.saveTeacher ||
                    (this.viewctrl.teacherMode &&
                        (this.loader.isInFormMode() || this.isGlobal())), // TODO: Check if correct
                points: this.points,
                giveCustomPoints: this.giveCustomPoints,
                userId: userId,
                answernr: answernr,
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
        // No need to poll task users for global tasks as global answers are the same for all
        if (this.isGlobal()) {
            return;
        }
        this.loading++;
        const r = await to(
            $http.get<IUser[]>(
                `/getTaskUsers/${this.taskId.docTask().toString()}`,
                {params: {group: this.viewctrl.group}}
            )
        );
        this.loading--;
        if (!r.ok) {
            this.showError(r.result);
            return;
        }
        this.users = r.result.data;
        this.users.sort(sortByRealName);
        // force ui to show selected
        if (this.user) {
            const found = this.users?.find((u) => u.id == this.user?.id);
            if (found) {
                this.user = found;
            }
        }
    }

    showError(response: {data: {error: string}}) {
        this.alerts.push({
            msg: "Error: " + response.data.error,
            type: "danger",
        });
    }

    getAnswerLink(single = false) {
        if (!this.user || !this.selectedAnswer) {
            return undefined;
        }
        let newroute = "answers";
        if (single && location.href.includes("/view/")) {
            newroute = "view";
        } // TODO: think other routes also?
        const par = this.getPar();
        if (!par) {
            return;
        }
        const parId = par.originalPar.id;
        const rangeParams = single
            ? {
                  b: parId,
                  size: 1,
              }
            : {};
        return `/${newroute}/${this.viewctrl.item.path}?${$httpParamSerializer({
            answerNumber:
                this.answers.length -
                this.findSelectedAnswerIndexFromUnFiltered(),
            task: this.getTaskName(),
            user: this.user.name,
            ...getUrlParamsJSON(),
            ...rangeParams,
        })}`;
    }

    getReviewLink() {
        return `/review/${this.viewctrl.item.path}?${$httpParamSerializer({
            b: this.getPar()?.originalPar.id ?? "",
            size: 1,
            group: getUrlParams().get("group"),
        })}`;
    }

    getModelAnswerLink() {
        return `/getModelAnswer/${this.taskId.docTask().toString()}`;
    }

    getModelAnswerLinkText() {
        if (
            this.modelAnswer?.alreadyLocked &&
            this.modelAnswer.lockedLinkText
        ) {
            return this.modelAnswer.lockedLinkText;
        }
        if (this.modelAnswerVisible) {
            return this.modelAnswer?.hideText ?? $localize`Hide model answer`;
        }
        return this.modelAnswer?.linkText ?? $localize`Show model answer`;
    }

    showModelAnswerLink() {
        if (!this.viewctrl?.item.rights.teacher) {
            if (
                this.modelAnswer?.linkTextCount &&
                this.modelAnswer?.linkTextCount - this.answers.length > 0
            ) {
                return false;
            }
            if (this.modelAnswer?.revealDate) {
                const endTime = new Date(
                    this.modelAnswer?.revealDate
                ).toISOString();
                const currentTime = new Date().toISOString();
                return endTime <= currentTime;
            }
        }
        return true;
    }

    showClearModelAnswerLockLink() {
        return this.viewctrl?.item.rights.teacher && this.modelAnswer?.lock;
    }

    async clearModelAnswerLock() {
        if (!this.user) {
            return;
        }
        // TODO: Fetch users who have taskBlocks to show as dropdown list in resetTaskLockDialog
        await showResetTaskLock({
            currentUser: this.user.name,
            taskId: this.taskId.docTask().toString(),
        });
    }

    async showModelAnswer() {
        if (this.modelAnswerFetched) {
            this.modelAnswerVisible = !this.modelAnswerVisible;
            if (this.modelAnswerVisible && this.modelAnswerRef) {
                ParCompiler.processAllMathDelayed(
                    $(this.modelAnswerRef.nativeElement)
                );
            }
            return;
        }
        if (!this.viewctrl?.item.rights.teacher) {
            if (
                this.modelAnswer?.count &&
                Math.max(this.modelAnswer?.count - this.answers.length, 0) > 0
            ) {
                this.alerts.push({
                    msg: $localize`You need to attempt at least ${this.modelAnswer.count} times before viewing the model answer`,
                    type: "warning",
                });
                return;
            }
            if (this.modelAnswer?.lock && !this.modelAnswer?.alreadyLocked) {
                const defaultLockText = $localize`Lock the task and view the model answer?`;
                if (
                    !(await showConfirm(
                        this.modelAnswer?.lockConfirmation ?? defaultLockText,
                        this.modelAnswer?.lockConfirmation ?? defaultLockText
                    ))
                ) {
                    return;
                }
            }
        }
        this.loading++;
        const r = await to(
            $http.get<{answer: string}>(this.getModelAnswerLink())
        );
        this.loading--;
        if (!r.ok) {
            this.showError(r.result);
            return;
        }
        if (this.modelAnswer?.lock) {
            this.modelAnswer.alreadyLocked = true;
            this.viewctrl.informAboutLock(this.taskId);
        }
        this.modelAnswerFetched = true;
        this.modelAnswerVisible = true;
        this.modelAnswerHtml = r.result.data.answer;
        if (this.modelAnswerRef) {
            ParCompiler.processAllMathDelayed(
                $(this.modelAnswerRef.nativeElement)
            );
        }
    }

    updateAnswerFromURL() {
        const answerNumber = getURLParameter("answerNumber");

        if (answerNumber == null && this.isAskNew()) {
            this.updateFiltered();
            this.selectedAnswer = undefined;
            this.changeAnswer(false, true);
            return true;
        }

        if (answerNumber != null && this.urlParamMatchesThisTask()) {
            const index =
                this.answers.length - parseInt(answerNumber ?? "1", 10);
            if (index >= 0 && index < this.answers.length) {
                this.onlyValid = false;
                this.updateFiltered();
                this.selectedAnswer =
                    this.filteredAnswers.length > 0
                        ? this.filteredAnswers[index]
                        : undefined;
                this.changeAnswer();
                return true;
            } else {
                void showMessageDialog(
                    $localize`Answer number ${answerNumber} is out of range for this task and user.`
                );
            }
        }
        return false;
    }

    async getAnswersAndUpdate() {
        // if ( this.isUseCurrentUser(this.taskId) ) { return null; }

        if (
            !this.viewctrl.item.rights ||
            !this.viewctrl.item.rights.browse_own_answers
        ) {
            return;
        }

        const data = await this.getAnswers();
        if (!data) {
            return;
        }
        await this.handleAnswerFetch(data);
        this.isAndSetShowNewTask();
        return data;
    }

    private async handleAnswerFetch(data: IAnswer[], newSelectedId?: number) {
        if (
            (data.length > 0 &&
                (this.hasUserChanged() ||
                    data.length !== this.answers.length)) ||
            this.forceBrowser() ||
            this.saveTeacher ||
            newSelectedId
        ) {
            this.answers = data;
            this.updateFiltered();
            if (newSelectedId) {
                this.selectedAnswer = this.filteredAnswers.find(
                    (ans) => ans.id == newSelectedId
                );
            } else {
                this.selectedAnswer =
                    this.filteredAnswers.length > 0
                        ? this.filteredAnswers[0]
                        : undefined;
                if (this.saveTeacher) {
                    this.updatePoints();
                }
            }
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
        this.cdr.detectChanges();
    }

    // noinspection JSUnusedLocalSymbols,JSUnusedLocalSymbols
    private getUserOrCurrentUserForAnswers(): IUser | undefined {
        let user: IUser | undefined;
        if (this.user) {
            user = this.user;
        }
        // TODO: refactor to use pluginMarkup()
        const c = this.viewctrl.getTimComponentByName(
            this.taskId.docTaskField()
        );
        if (!c) {
            return user;
        }
        const a = c.attrsall;
        if (a?.markup?.useCurrentUser) {
            this.user = Users.getCurrent(); // TODO: looks bad when function has a side effect?
            return Users.getCurrent();
        }
        return user;
    }

    public pluginAttrs():
        | IGenericPluginTopLevelFields<IGenericPluginMarkup>
        | undefined {
        if (!this.viewctrl || !this.taskId) {
            return undefined;
        }
        const c = this.viewctrl.getTimComponentByName(
            this.taskId.docTaskField()
        );
        if (!c) {
            return undefined;
        }
        return c.attrsall;
    }

    public pluginMarkup(): IGenericPluginMarkup | undefined {
        const a = this.pluginAttrs();
        return a?.markup;
    }

    public pluginInfo() {
        const a = this.pluginAttrs();
        return a?.info;
    }

    public isUseCurrentUser(): boolean {
        const m = this.pluginMarkup();
        return m?.useCurrentUser ?? false;
    }

    public isGlobal() {
        return this.taskId.name.startsWith("GLO_");
        // return this.pluginMarkup().globalField;
    }

    public forceBrowser() {
        const m = this.pluginMarkup();
        return m?.forceBrowser;
    }

    /* Return user answers, null = do not care */
    private async getAnswers() {
        const user = this.getUserOrCurrentUserForAnswers();
        if (!user) {
            return undefined;
        }
        this.loading++;

        const r = await to(
            $http.get<IAnswer[]>(
                `/getAnswers/${this.taskId.docTask().toString()}/${user.id}`,
                {
                    params: {
                        _: Date.now(),
                    },
                }
            )
        );

        this.loading--;
        if (!r.ok) {
            this.showError(r.result);
            return undefined;
        }
        this.fetchedUser = user;
        this.isAndSetShowNewTask();
        return r.result.data;
    }

    getPluginHtmlAnswerId() {
        return this.answerId;
    }

    getUser(): IUser | undefined {
        return this.user;
    }

    hasUserChanged() {
        return (
            (this.user ?? {id: null}).id !== (this.fetchedUser ?? {id: null}).id
        );
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

    isAndSetShowNewTask() {
        const result = this.taskInfo?.newtask ?? false;
        if (!result) {
            this.showNewTask = false;
            return false;
        }
        const selidx = this.findSelectedAnswerIndexFromUnFiltered();
        this.showNewTask = selidx >= 0;
        return this.showNewTask;
    }

    /* If seed=="answernr" show task first time as a new task */
    isAskNew() {
        if (this.viewctrl.teacherMode) {
            return false;
        }
        const info = this.pluginInfo();
        return info?.askNew ?? false;
    }

    setReviewerUsers() {
        if (!this.selectedAnswer) {
            this.reviewerUser = undefined;
            return;
        }
        this.reviewerUsers =
            this.viewctrl.reviewCtrl.getAnnotationersByAnswerId(
                this.selectedAnswer.id
            );
    }

    updateReviewers() {
        this.setReviewerUsers();
    }

    getReviewerUser() {
        return this.reviewerUser;
    }

    changeReviewerUser() {
        if (this.selectedAnswer && this.reviewHtml) {
            const par = this.getPar();
            if (!par) {
                return;
            }
            this.viewctrl.reviewCtrl.loadAnnotationsToAnswer(
                this.selectedAnswer.id,
                par,
                this.reviewerUser
            );
        }
    }

    showVelpsCheckBox() {
        // return this.$parent.teacherMode || $window.velpMode; // && this.$parent.item.rights.teacher;
        return (
            documentglobals().velpMode ||
            (this.getPar()?.hasClass("has-annotation") ?? false)
        );
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
        const r = await to(
            $http.get<ITaskInfo>(
                `/taskinfo/${this.taskId.docTask().toString()}`,
                {
                    params: getUrlParamsJSON(),
                }
            )
        );
        this.loading--;
        if (!r.ok) {
            this.showError(r.result);
            return;
        }
        this.taskInfo = r.result.data;
        if (r.result.data.modelAnswer) {
            this.modelAnswer = r.result.data.modelAnswer;
            this.onlyValid = false;
        }
        this.showNewTask = this.isAndSetShowNewTask();
        if (this.taskInfo.buttonNewTask) {
            this.buttonNewTask = this.taskInfo.buttonNewTask;
        }
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

    getAllAnswers() {
        void to2(
            showAllAnswersDialog({
                url: `/allAnswersPlain/${this.taskId.docTask().toString()}`,
                identifier: this.taskId.docTask().toString(),
                allTasks: false,
            })
        );
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

    findSelectedAnswerIndexRevFromUnFiltered() {
        const idx = this.findSelectedAnswerIndex();
        if (idx < 0) {
            return idx;
        }
        return this.filteredAnswers.length - idx - 1;
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
        // noinspection JSUnusedLocalSymbols,JSUnusedLocalSymbols
        this.filteredAnswers = $filter("filter")<IAnswer>(
            this.answers,
            (value, index, array) => {
                if (value.valid) {
                    return true;
                }
                this.anyInvalid = true;
                return !this.onlyValid;
            }
        );
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

    showFeedback(txt?: string) {
        this.feedback = txt;
        if (this.feedBackElement) {
            ParCompiler.processAllMathDelayed(
                $(this.feedBackElement.nativeElement)
            );
        }
    }

    onOnlyValidChanged() {
        this.updateFilteredAndSetNewest();
    }

    ngOnChanges(changes: SimpleChanges) {
        if (changes.taskId && !changes.taskId.firstChange) {
            if (this.viewctrl?.teacherMode) {
                this.getAvailableUsers();
            }
            this.getAnswersAndUpdate();
        }
    }

    ngOnDestroy() {
        this.viewctrl.unregisterAnswerBrowser(this);
    }

    private getTaskName() {
        return this.taskId.name;
    }

    // noinspection JSUnusedLocalSymbols
    toggleInput() {
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
        if (!window.confirm($localize`Delete this answer?`)) {
            return;
        }
        const aid = this.selectedAnswer.id;
        await to(
            $http.post("/answer/delete", {
                answer_id: aid,
            })
        );
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

    async deleteCollab(userId: number) {
        if (!this.selectedAnswer) {
            return;
        }
        const sa = this.selectedAnswer as IAnswerWithUsers; // TODO fix selectedAnswer type
        if (sa.users.length === 1) {
            await showMessageDialog(
                $localize`Cannot delete the only collaborator.`
            );
            return;
        }
        const index = sa.users.findIndex((u) => u.id === userId);
        if (index < 0) {
            return;
        }
        const user = sa.users[index];
        if (
            !window.confirm(
                `Delete collaborator ${user.real_name ?? "(null)"} (${
                    user.name
                })?`
            )
        ) {
            return;
        }
        await to(
            $http.post("/answer/deleteCollaborator", {
                answer_id: sa.id,
                user_id: userId,
            })
        );
        sa.users.splice(index, 1);
    }

    getPar() {
        const parEl = this.element.nativeElement.closest(".par")!;
        const ctx = tryCreateParContextOrHelp(parEl);
        if (ctx instanceof ParContext) {
            return ctx;
        }
        return undefined;
    }

    get selectedAnswerWithUsers(): IAnswerWithUsers | undefined {
        const ret = this.selectedAnswer as IAnswerWithUsers;
        return ret;
    }
}
@NgModule({
    declarations: [AnswerBrowserComponent],
    imports: [
        CommonModule,
        FormsModule,
        TimUtilityModule,
        DrawCanvasModule,
        PurifyModule,
    ],
    exports: [AnswerBrowserComponent],
})
export class AnswerBrowserModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}
