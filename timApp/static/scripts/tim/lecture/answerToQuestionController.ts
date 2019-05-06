import {IRootElementService, IScope} from "angular";
import moment, {Moment} from "moment";
import {IPreviewParams, makePreview} from "../document/question/dynamicAnswerSheet";
import {fetchAskedQuestion, showQuestionEditDialog} from "../document/question/questionController";
import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "../ui/dialog";
import {$http, $timeout} from "../util/ngimport";
import {
    AnswerTable,
    IAskedQuestion,
    IGetNewQuestionResponse,
    IQuestionAnswer,
    isAskedQuestion,
    questionAnswerReceived,
    questionAsked,
    QuestionOrAnswer,
} from "./lecturetypes";
import {showStatisticsDialog} from "./statisticsToQuestionController";
import {getStorage, setStorage} from "../util/utils";

/**
 * Created by hajoviin on 22.4.2015
 * FILL WITH SUITABLE TEXT
 * @module answerToQuestionController
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

export interface IAnswerQuestionParams {
    qa: QuestionOrAnswer;
    isLecturer: boolean;
}

export function getAskedQuestionFromQA(qa: QuestionOrAnswer): IAskedQuestion {
    if (isAskedQuestion(qa)) {
        return qa;
    } else {
        return qa.asked_question;
    }
}

export type IAnswerQuestionResult =
    {type: "pointsclosed", askedId: number}
    | {type: "closed"}
    | {type: "answered"}
    | {type: "reask"}
    | {type: "reask_as_new"};

export let currentQuestion: AnswerToQuestionController | undefined;

export const QUESTION_STORAGE = "lectureQuestion";

export class AnswerToQuestionController extends DialogController<{params: IAnswerQuestionParams}, IAnswerQuestionResult> {
    static component = "timAnswerQuestion";
    static $inject = ["$element", "$scope"] as const;
    private barFilled?: number;
    private progressText?: string;
    private isLecturer = false;
    private askedTime?: Moment;
    private endTime?: Moment;
    private progressMax?: number;
    private answered = false;
    private buttonText: string = "Answer";
    private result?: boolean;
    private question!: IAskedQuestion; // $onInit
    private preview!: IPreviewParams; // $onInit
    private questionEnded: boolean = false;
    private answer?: AnswerTable;

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
        this.updateAnswer = this.updateAnswer.bind(this);
    }

    $onInit() {
        super.$onInit();
        if (currentQuestion) {
            throw new Error("Question window was already open.");
        }
        currentQuestion = this;
        this.setData(this.resolve.params.qa);
    }

    public getQuestion() {
        return this.question;
    }

    public hasResult() {
        return this.result;
    }

    public getTitle() {
        return "Answer question";
    }

    private $onDestroy() {
        currentQuestion = undefined;
        setStorage(QUESTION_STORAGE, null);
    }

    private async answerToQuestion() {
        const response = await $http<{questionLate?: string}>({
            url: "/answerToQuestion",
            method: "PUT",
            params: {
                asked_id: this.question.asked_id,
                buster: new Date().getTime(),
                input: {answers: this.answer},
            },
        });
        const answer = response.data;
        if (answer.questionLate) {
            await showMessageDialog(answer.questionLate);
        }
        this.answered = true;
        this.preview = makePreview(this.preview.markup, {answerTable: this.answer});
        if (!this.isLecturer) {
            super.close({type: "answered"});
        }
    }

    public close() {
        if (this.isLecturer) {
            this.stopQuestion();
        }
        if (this.result) {
            super.close({type: "pointsclosed", askedId: this.question.asked_id});
        } else {
            super.close({type: "closed"});
        }
    }

    private async stopQuestion() {
        const response = await $http({
            url: "/stopQuestion",
            method: "POST",
            params: {
                asked_id: this.question.asked_id,
            },
        });
        // Don't call endQuestion here; it will come from lectureController.
    }

    private async showAnswers() {
        await showStatisticsDialog(this.question);
    }

    private reAsk() {
        super.close({type: "reask"});
    }

    private askAsNew() {
        super.close({type: "reask_as_new"});
    }

    private async edit() {
        const asked = await fetchAskedQuestion(this.question.asked_id);
        const r = await showQuestionEditDialog(asked);
        if (isAskedQuestion(this.resolve.params.qa)) {
            this.setData(await fetchAskedQuestion(this.question.asked_id));
        } else {
            this.setData((await $http.get<IQuestionAnswer>("/getQuestionAnswer", {
                params: {id: this.resolve.params.qa.answer_id},
            })).data);
        }
    }

    private async showPoints() {
        const response = await $http<IGetNewQuestionResponse>({
            url: "/showAnswerPoints",
            method: "POST",
            params: {
                asked_id: this.question.asked_id,
                current_question_id: this.question.asked_id, // TODO useless parameter
            },
        });
        const d = response.data;
        if (questionAnswerReceived(d)) {
            this.result = true;
            this.preview = makePreview(d.data.asked_question.json.json, {
                answerTable: d.data.answer,
                showCorrectChoices: true,
                showExplanations: true,
                userpoints: d.data.points,
            });
        } else if (questionAsked(d)) {
            this.result = true;
            this.preview = makePreview(d.data.json.json, {
                showCorrectChoices: true,
                showExplanations: true,
            });
        } else {
            // This case happens if the lecturer did not answer the question himself.
            this.result = true;
            this.preview = makePreview(this.question.json.json, {
                showCorrectChoices: true,
                showExplanations: true,
            });
        }
    }

    /**
     * Changes question end time.
     */
    public updateEndTime(time: Moment) {
        this.endTime = time.clone();
        this.updateMaxProgress();
    }

    private start(updateInterval: number) {
        void this.updateBar(updateInterval);
    }

    /**
     * Updates progressbar and time left text
     */
    private async updateBar(updateInterval: number) {
        // TODO: Problem with inactive tab.
        if (!this.endTime || !this.progressMax) {
            return;
        }
        while (!this.closed && !this.questionEnded) {
            const now = moment();
            const timeLeft = this.endTime.diff(now);
            this.barFilled = (this.endTime.diff(this.askedTime)) - timeLeft;
            this.progressText = Math.max(timeLeft / 1000, 0).toFixed(0) + " s";
            if (this.barFilled >= this.progressMax) {
                await this.endQuestion();
            }
            await $timeout(updateInterval);
        }
    }

    private updateAnswer(at: AnswerTable) {
        this.answer = at;
    }

    public async endQuestion() {
        this.endTime = moment();
        this.updateMaxProgress();
        this.barFilled = this.progressMax;
        this.progressText = "Time's up";
        this.questionEnded = true;
        if (!this.answered && !this.isLecturer) {
            await this.answerToQuestion();
        }
    }

    public setData(data: QuestionOrAnswer) {
        if (isAskedQuestion(data)) {
            this.question = data;
            this.result = false;
            this.preview = makePreview(this.question.json.json, {
                enabled: true,
            });
            this.questionEnded = false;
            this.answered = false;
        } else {
            this.question = data.asked_question;
            this.result = true;
            this.preview = makePreview(
                this.question.json.json, {
                    answerTable: data.answer,
                    showCorrectChoices: true,
                    showExplanations: true,
                    userpoints: data.points,
                },
            );
            this.questionEnded = true;
            this.answered = true;
        }
        this.askedTime = this.question.asked_time.clone();

        if (this.question.json.json.timeLimit) {
            this.endTime = this.askedTime.clone().add(this.question.json.json.timeLimit, "seconds");
        }
        this.isLecturer = this.resolve.params.isLecturer;

        if (!this.result) {
            this.barFilled = 0;
            if (this.endTime) {
                this.progressText = "";
                this.updateMaxProgress();
                this.start(500);
            }
        }
    }

    private updateMaxProgress() {
        if (this.endTime) {
            this.progressMax = this.endTime.diff(this.askedTime);
        } else {
            this.progressMax = undefined;
        }
    }
}

registerDialogComponent(AnswerToQuestionController,
    {
        template: `
<tim-dialog>
    <dialog-header>
        Answer question
    </dialog-header>
    <dialog-body>
        <dynamic-answer-sheet
                questiondata="$ctrl.preview"
                on-answer-change="$ctrl.updateAnswer">
        </dynamic-answer-sheet>
        <uib-progressbar
                max="$ctrl.progressMax"
                value="$ctrl.barFilled">
            {{ $ctrl.progressText }}
        </uib-progressbar>
    </dialog-body>
    <dialog-footer>
        <button ng-show="$ctrl.isLecturer && $ctrl.questionEnded" class="timButton" ng-click="$ctrl.edit()">
            Edit points
        </button>
        <button ng-show="$ctrl.isLecturer && $ctrl.questionEnded" class="timButton" ng-click="$ctrl.askAsNew()">
            Ask as new
        </button>
        <button ng-show="$ctrl.isLecturer && $ctrl.questionEnded" class="timButton" ng-click="$ctrl.reAsk()">
            Reask
        </button>
        <button ng-show="!$ctrl.questionEnded && !$ctrl.answered && !$ctrl.result && $ctrl.answer"
                class="answerButton timButton"
                ng-click="$ctrl.answerToQuestion()">{{$ctrl.buttonText}}
        </button>

        <br/>

        <button ng-show="$ctrl.isLecturer" class="timButton" ng-click="$ctrl.showAnswers()">Show answers</button>
        <button ng-show="$ctrl.isLecturer && $ctrl.questionEnded" class="timButton" ng-click="$ctrl.showPoints()">
            Show points
        </button>
        <button ng-show="($ctrl.isLecturer && $ctrl.questionEnded) || $ctrl.result" class="timButton"
                ng-click="$ctrl.close()">Close
        </button>
        <button ng-show="$ctrl.isLecturer && !$ctrl.questionEnded" class="timButton" ng-click="$ctrl.stopQuestion()">
            End question
        </button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showQuestionAnswerDialog(p: IAnswerQuestionParams) {
    return await showDialog(AnswerToQuestionController, {params: () => p}).result;
}

export function isOpenInAnotherTab(qa: QuestionOrAnswer) {
    return getStorage(QUESTION_STORAGE) === getAskedQuestionFromQA(qa).asked_id;
}
