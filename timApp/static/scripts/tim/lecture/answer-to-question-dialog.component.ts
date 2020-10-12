import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {Component, OnDestroy} from "@angular/core";
import moment, {Moment} from "moment";
import deepEqual from "deep-equal";
import {
    IPreviewParams,
    makePreview,
} from "../document/question/answer-sheet.component";
import {
    fetchAskedQuestion,
    showQuestionEditDialog,
} from "../document/question/question-edit-dialog.component";
import {showMessageDialog} from "../ui/dialog";
import {$http} from "../util/ngimport";
import {getStorage, setStorage, timeout, to} from "../util/utils";
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

/**
 * Created by hajoviin on 22.4.2015
 * FILL WITH SUITABLE TEXT
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
    | {type: "pointsclosed"; askedId: number}
    | {type: "closed"}
    | {type: "answered"}
    | {type: "reask"}
    | {type: "reask_as_new"};

export let currentQuestion: AnswerToQuestionDialogComponent | undefined;

export const QUESTION_STORAGE = "lectureQuestion";

@Component({
    selector: "tim-answer-question-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <tim-answer-sheet
                        [questiondata]="preview"
                        (onAnswerChange)="updateAnswer($event)">
                </tim-answer-sheet>
                <progressbar
                        *ngIf="progressMax"
                        [max]="progressMax"
                        [value]="barFilled">
                    {{ progressText }}
                </progressbar>
            </ng-container>
            <ng-container footer>
                <button *ngIf="isLecturer && questionEnded" class="timButton" (click)="edit()">
                    Edit points
                </button>
                <button *ngIf="isLecturer && questionEnded" class="timButton" (click)="askAsNew()">
                    Ask as new
                </button>
                <button *ngIf="isLecturer && questionEnded" class="timButton" (click)="reAsk()">
                    Reask
                </button>
                <button *ngIf="!questionEnded && !answered && !gotResult && answer"
                        class="answerButton timButton"
                        (click)="answerToQuestion()">
                    Answer
                </button>

                <br/>

                <button *ngIf="isLecturer" class="timButton" (click)="showAnswers()">Show answers</button>
                <button *ngIf="isLecturer && questionEnded" class="timButton" (click)="showPoints()">
                    Show points
                </button>
                <button *ngIf="(isLecturer && questionEnded) || result" class="timButton"
                        (click)="close()">Close
                </button>
                <button *ngIf="isLecturer && !questionEnded" class="timButton" (click)="stopQuestion()">
                    End question
                </button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class AnswerToQuestionDialogComponent
    extends AngularDialogComponent<IAnswerQuestionParams, IAnswerQuestionResult>
    implements OnDestroy {
    protected dialogName = "AnswerQuestion";
    barFilled = 0;
    progressText?: string;
    isLecturer = false;
    private askedTime?: Moment;
    private endTime?: Moment;
    progressMax?: number;
    answered = false;
    gotResult?: boolean;
    private question!: IAskedQuestion; // ngOnInit
    preview!: IPreviewParams; // ngOnInit
    questionEnded: boolean = false;
    answer?: AnswerTable;
    private defaultUpdateInterval = 500;

    ngOnInit() {
        if (currentQuestion) {
            throw new Error("Question window was already open.");
        }
        currentQuestion = this;
        this.setData(this.data.qa);
    }

    public getQuestion() {
        return this.question;
    }

    public hasResult() {
        return this.gotResult;
    }

    public getTitle() {
        return "Answer question";
    }

    ngOnDestroy() {
        currentQuestion = undefined;
        setStorage(QUESTION_STORAGE, null);
    }

    async answerToQuestion() {
        const response = await to(
            $http<{questionLate?: string}>({
                url: "/answerToQuestion",
                method: "PUT",
                params: {
                    asked_id: this.question.asked_id,
                    buster: new Date().getTime(),
                    input: {answers: this.answer},
                },
            })
        );
        if (!response.ok) {
            return;
        }
        const answer = response.result.data;
        if (answer.questionLate) {
            await showMessageDialog(answer.questionLate);
        }
        this.answered = true;
        this.disableAnswerSheet();
        if (!this.isLecturer) {
            super.close({type: "answered"});
        }
    }

    private disableAnswerSheet() {
        this.preview = makePreview(this.preview.markup, {
            answerTable: this.answer,
            enabled: false,
        });
    }

    public close() {
        if (this.isLecturer && !this.questionEnded) {
            this.stopQuestion();
        }
        if (this.gotResult) {
            super.close({
                type: "pointsclosed",
                askedId: this.question.asked_id,
            });
        } else {
            super.close({type: "closed"});
        }
    }

    public dismiss() {
        this.close();
    }

    async stopQuestion() {
        const _ = await to(
            $http({
                url: "/stopQuestion",
                method: "POST",
                params: {
                    asked_id: this.question.asked_id,
                },
            })
        );
        // Don't call endQuestion here; it will come from lectureController.
    }

    async showAnswers() {
        await showStatisticsDialog(this.question);
    }

    reAsk() {
        super.close({type: "reask"});
    }

    askAsNew() {
        super.close({type: "reask_as_new"});
    }

    async edit() {
        const asked = await fetchAskedQuestion(this.question.asked_id);
        const _ = await showQuestionEditDialog(asked);
        if (isAskedQuestion(this.data.qa)) {
            this.setData(await fetchAskedQuestion(this.question.asked_id));
        } else {
            const resp = await to(
                $http.get<IQuestionAnswer>("/getQuestionAnswer", {
                    params: {id: this.data.qa.answer_id},
                })
            );
            if (!resp.ok) {
                return;
            }
            this.setData(resp.result.data);
        }
    }

    async showPoints() {
        const response = await to(
            $http<IGetNewQuestionResponse>({
                url: "/showAnswerPoints",
                method: "POST",
                params: {
                    asked_id: this.question.asked_id,
                    current_question_id: this.question.asked_id, // TODO useless parameter
                },
            })
        );
        if (!response.ok) {
            return;
        }
        const d = response.result.data;
        if (questionAnswerReceived(d)) {
            this.gotResult = true;
            this.preview = makePreview(d.data.asked_question.json.json, {
                answerTable: d.data.answer,
                showCorrectChoices: true,
                showExplanations: true,
                userpoints: d.data.points,
            });
        } else if (questionAsked(d)) {
            this.gotResult = true;
            this.preview = makePreview(d.data.json.json, {
                showCorrectChoices: true,
                showExplanations: true,
            });
        } else {
            // This case happens if the lecturer did not answer the question himself.
            this.gotResult = true;
            this.preview = makePreview(this.question.json.json, {
                showCorrectChoices: true,
                showExplanations: true,
            });
        }
    }

    /**
     * Changes question end time.
     */
    public updateEndTime(time: Moment | null) {
        if (time != null) {
            this.endTime = time.clone();
        } else {
            this.endTime = undefined;
        }
        const maxProgressBeforeUpdate = this.progressMax;
        this.updateMaxProgress();
        if (maxProgressBeforeUpdate == null && this.progressMax != null) {
            this.start(this.defaultUpdateInterval);
        }
        // TODO: controller has no option to call /extendQuestion
        // else if (maxProgressBeforeUpdate != null && this.progressMax == null) {
        //     // reset bar here
        // }
    }

    private start(updateInterval: number) {
        void this.updateBar(updateInterval);
    }

    /**
     * Updates progressbar and time left text
     */
    private async updateBar(updateInterval: number) {
        // TODO: Problem with inactive tab.
        while (
            !this.closed &&
            !this.questionEnded &&
            !(!this.endTime || !this.progressMax)
        ) {
            const now = moment();
            const timeLeft = this.endTime.diff(now);
            this.barFilled = this.endTime.diff(this.askedTime) - timeLeft;
            this.progressText = Math.max(timeLeft / 1000, 0).toFixed(0) + " s";
            if (this.barFilled >= this.progressMax) {
                await this.endQuestion();
            }
            await timeout(updateInterval);
        }
    }

    updateAnswer(at: AnswerTable) {
        this.answer = at;
    }

    public async endQuestion() {
        this.endTime = moment();
        this.updateMaxProgress();
        this.barFilled = this.progressMax ?? 0;
        this.progressText = "Time's up";
        this.questionEnded = true;
        if (!this.isLecturer) {
            if (!this.answered) {
                await this.answerToQuestion();
            }
        }
        this.disableAnswerSheet();
    }

    public setData(data: QuestionOrAnswer) {
        if (isAskedQuestion(data)) {
            if (deepEqual(data, this.question) && this.preview) {
                return;
            } else {
                this.question = data;
                this.gotResult = false;
                this.preview = makePreview(this.question.json.json, {
                    enabled: true,
                });
                this.questionEnded = false;
                this.answered = false;
            }
        } else {
            this.question = data.asked_question;
            this.gotResult = true;
            this.preview = makePreview(this.question.json.json, {
                answerTable: data.answer,
                showCorrectChoices: true,
                showExplanations: true,
                userpoints: data.points,
            });
            this.questionEnded = true;
            this.answered = true;
        }
        this.askedTime = this.question.asked_time.clone();

        if (this.question.json.json.timeLimit) {
            this.endTime = this.askedTime
                .clone()
                .add(this.question.json.json.timeLimit, "seconds");
        }
        this.isLecturer = this.data.isLecturer;

        if (!this.gotResult) {
            this.barFilled = 0;
            if (this.endTime) {
                this.progressText = "";
                this.updateMaxProgress();
                this.start(this.defaultUpdateInterval);
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

export async function showQuestionAnswerDialog(p: IAnswerQuestionParams) {
    return await (await angularDialog.open(AnswerToQuestionDialogComponent, p))
        .result;
}

export function isOpenInAnotherTab(qa: QuestionOrAnswer) {
    return getStorage(QUESTION_STORAGE) === getAskedQuestionFromQA(qa).asked_id;
}
