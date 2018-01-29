import moment, {Moment} from "moment";
import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "../dialog";
import {IPreviewParams, makePreview} from "../directives/dynamicAnswerSheet";
import {
    AnswerTable, IAskedQuestion, IGetNewQuestionResponse, IQuestionAnswer, isAskedQuestion,
    questionAnswerReceived, questionAsked,
} from "../lecturetypes";
import {$http, $timeout} from "../ngimport";
import {fetchAskedQuestion, showQuestionEditDialog} from "./questionController";
import {showStatisticsDialog} from "./showStatisticsToQuestionController";

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
    qa: IAskedQuestion | IQuestionAnswer;
    isLecturer: boolean;
}

export type IAnswerQuestionResult =
    {type: "pointsclosed", askedId: number}
    | {type: "closed"}
    | {type: "reask"}
    | {type: "reask_as_new"};

let currentQuestion: AnswerToQuestionController | undefined;

export class AnswerToQuestionController extends DialogController<{params: IAnswerQuestionParams}, IAnswerQuestionResult, "timAnswerQuestion"> {
    private barFilled: number;
    private progressText: string;
    private timeLeft: number;
    private isLecturer = false;
    private askedTime: Moment;
    private endTime?: Moment;
    private progressMax: number;
    private answered = false;
    private buttonText: string;
    private result: boolean;
    private question: IAskedQuestion;
    private clockOffset = moment.duration(0);
    private preview: IPreviewParams;
    private questionEnded: boolean;
    private answer: AnswerTable;

    constructor() {
        super();
        this.updateAnswer = this.updateAnswer.bind(this);
    }

    private $onInit() {
        if (currentQuestion) {
            throw new Error("Question window was already open.");
        }
        currentQuestion = this;
        if (isAskedQuestion(this.resolve.params.qa)) {
            this.question = this.resolve.params.qa;
            this.result = false;
            this.preview = makePreview(this.question.json.json, [], false);
            this.questionEnded = false;
            this.answered = false;
        } else {
            this.question = this.resolve.params.qa.asked_question;
            this.result = true;
            this.preview = makePreview(
                this.question.json.json,
                this.resolve.params.qa.answer,
                false,
                true,
                this.resolve.params.qa.points,
            );
            this.questionEnded = true;
            this.answered = true;
        }
        this.askedTime = this.question.asked_time.subtract(this.clockOffset);

        // clockOffset usually in range [-100, -25], so it's almost meaningless?
        this.endTime = this.askedTime.add(this.question.json.json.timeLimit!, "seconds").subtract(this.clockOffset);
        this.isLecturer = this.resolve.params.isLecturer;
        this.buttonText = "Answer"; // TODO: Make configurable

        if (!this.result) {
            const now = moment();
            this.timeLeft = this.endTime.diff(now);
            this.barFilled = 0;
            if (this.endTime && this.question.json.json.timeLimit) {
                this.progressText = "";
                this.start(500);
            }
        }
    }

    private $onDestroy() {
        currentQuestion = undefined;
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
        super.close({type: "closed"});
    }

    protected close() {
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
        this.endQuestion();
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
        // TODO: update state?
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
            this.preview = makePreview(d.data.asked_question.json.json, d.data.answer, false, true, d.data.points);
        } else if (questionAsked(d)) {
            this.result = true;
            this.preview = makePreview(d.data.json.json, [], false, true);
        } else {
            await showMessageDialog("Did not receive result to show points.");
        }
        // TODO: Is it necessary to update current_points_id here?
        // this.current_points_id = this.question.asked_id;
    }

    /**
     * Use time parameter to either close question/points window or extend question end time.
     * If time is null, question/points is closed.
     * Else time is set as questions new end time.
     * Event: update_end_time
     */
    private updateEndTime(time: Moment) {
        if (time != null) {
            this.endTime = time.subtract(this.clockOffset);
            this.progressMax = this.endTime.diff(this.askedTime);
        } else {
            if (!this.isLecturer) {
                this.close();
            }
        }
    }

    private start(timeBetween: number) {
        void this.updateBar(timeBetween);
    }

    /**
     * Updates progressbar and time left text
     */
    private async updateBar(timeBetween: number) {
        // TODO: Problem with inactive tab.
        const now = moment();
        if (!this.endTime) {
            return;
        }
        while (true) {
            this.timeLeft = this.endTime.diff(now);
            this.barFilled = (this.endTime.diff(this.askedTime)) - this.timeLeft;
            this.progressText = Math.max(this.timeLeft / 1000, 0).toFixed(0) + " s";
            if (this.barFilled >= this.progressMax) {
                if (!this.isLecturer && !this.questionEnded) {
                    this.answerToQuestion();
                } else {
                    this.progressText = "Time's up";
                }
                this.questionEnded = true;
                return;
            }
            await $timeout(timeBetween);
        }
    }

    private updateAnswer(at: AnswerTable) {
        this.answer = at;
    }

    private endQuestion() {
        this.endTime = moment();
        this.barFilled = this.progressMax;
        this.progressText = "Time's up";
        this.questionEnded = true;
    }
}

registerDialogComponent("timAnswerQuestion",
    AnswerToQuestionController,
    {
        template: `
<div class="popUpWindow">
    <div class="questionAskedWindow">
        <dynamic-answer-sheet
        questiondata="$ctrl.preview"
        on-answer-change="$ctrl.updateAnswer">
</dynamic-answer-sheet>
    </div>
    <div class="buttons">
        <button ng-show="$ctrl.isLecturer && $ctrl.questionEnded" class="timButton" ng-click="$ctrl.edit()">
            Edit points
        </button>
        <button ng-show="$ctrl.isLecturer && $ctrl.questionEnded" class="timButton" ng-click="$ctrl.askAsNew()">
            Ask as new
        </button>
        <button ng-show="$ctrl.isLecturer && $ctrl.questionEnded" class="timButton" ng-click="$ctrl.reAsk()">
            Reask
        </button>
        <button ng-show="!$ctrl.questionEnded && !$ctrl.answered && !$ctrl.result" class="answerButton timButton"
                ng-click="$ctrl.answer()">{{$ctrl.buttonText}}
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
    </div>
</div>
`,
    });

export async function showQuestionAnswerDialog(p: IAnswerQuestionParams) {
    return await showDialog<AnswerToQuestionController>("timAnswerQuestion", {params: () => p});
}
