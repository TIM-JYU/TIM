import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule, NgZone, OnDestroy, ViewChild} from "@angular/core";
import deepEqual from "deep-equal";
import {CountdownComponent} from "tim/ui/countdown.component";
import {HttpClient} from "@angular/common/http";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BrowserModule} from "@angular/platform-browser";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {ProgressbarModule} from "ngx-bootstrap/progressbar";
import {
    currentQuestion,
    QUESTION_STORAGE,
    setCurrentQuestion,
} from "tim/lecture/currentQuestion";
import {
    AnswerSheetModule,
    IPreviewParams,
    makePreview,
} from "../document/question/answer-sheet.component";
import {
    fetchAskedQuestion,
    showQuestionEditDialog,
} from "../document/question/question-edit-dialog.component";
import {showMessageDialog} from "../ui/dialog";
import {ReadonlyMoment, setStorage, to2} from "../util/utils";
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
import {showStatisticsDialog} from "./showLectureDialogs";

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

export type IAnswerQuestionResult =
    | {type: "pointsclosed"; askedId: number}
    | {type: "closed"}
    | {type: "answered"}
    | {type: "reask"}
    | {type: "reask_as_new"};

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
                <div class="flex cl align-center">
                    <tim-countdown (onTick)="timerTick($event)"
                                   [endTime]="endTime"
                                   [displayUnits]="['s']"
                                   [autoStart]="false"
                                   template="{0} s"
                    ></tim-countdown>
                    <progressbar
                            *ngIf="progressMax"
                            [max]="progressMax"
                            [value]="barFilled">
                        <ng-container *ngIf="questionEnded">Time's up</ng-container>
                    </progressbar>
                </div>
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
    endTime?: ReadonlyMoment;
    progressMax?: number;
    answered = false;
    gotResult?: boolean;
    private question!: IAskedQuestion; // ngOnInit
    preview!: IPreviewParams; // ngOnInit
    questionEnded: boolean = false;
    answer?: AnswerTable;
    @ViewChild(CountdownComponent) private countdownTimer?: CountdownComponent;

    constructor(private http: HttpClient, private zone: NgZone) {
        super();
    }

    ngOnInit() {
        if (currentQuestion) {
            throw new Error("Question window was already open.");
        }
        setCurrentQuestion(this);
        this.setData(this.data.qa);
    }

    ngAfterViewInit() {
        super.ngAfterViewInit();
        this.maybeStartTimer();
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
        setCurrentQuestion(undefined);
        setStorage(QUESTION_STORAGE, null);
    }

    async answerToQuestion() {
        const response = await to2(
            this.http
                .put<{questionLate?: string}>("/answerToQuestion", {
                    asked_id: this.question.asked_id,
                    buster: new Date().getTime(),
                    input: this.answer,
                })
                .toPromise()
        );
        if (!response.ok) {
            return;
        }
        const answer = response.result;
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
            void this.stopQuestion();
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
        const _ = await to2(
            this.http
                .post("/stopQuestion", {
                    asked_id: this.question.asked_id,
                })
                .toPromise()
        );
        // Don't call endQuestion here; it will come from timerTick.
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
            const resp = await to2(
                this.http
                    .get<IQuestionAnswer>("/getQuestionAnswer", {
                        params: {
                            id: this.data.qa.answer_id.toString(),
                        },
                    })
                    .toPromise()
            );
            if (!resp.ok) {
                return;
            }
            this.setData(resp.result);
        }
    }

    async showPoints() {
        const response = await to2(
            this.http
                .post<IGetNewQuestionResponse>("/showAnswerPoints", {
                    asked_id: this.question.asked_id,
                    current_question_id: this.question.asked_id, // TODO useless parameter
                })
                .toPromise()
        );
        if (!response.ok) {
            return;
        }
        const d = response.result;
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
            // This case happens if the lecturer did not answer the question.
            this.gotResult = true;
            this.preview = makePreview(this.question.json.json, {
                showCorrectChoices: true,
                showExplanations: true,
            });
        }
    }

    private checkChanges() {
        this.zone.run(() => {});
    }

    public updateEndTime(time: ReadonlyMoment | null) {
        this.endTime = time?.clone();
        const maxProgressBeforeUpdate = this.progressMax;
        this.updateMaxProgress();
        if (maxProgressBeforeUpdate == null && this.progressMax != null) {
            this.countdownTimer!.start();
        }
        this.checkChanges();
        // TODO: controller has no option to call /extendQuestion
        // else if (maxProgressBeforeUpdate != null && this.progressMax == null) {
        //     // reset bar here
        // }
    }

    updateAnswer(at: AnswerTable) {
        this.answer = at;
    }

    private async endQuestion() {
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

        if (this.question.json.json.timeLimit) {
            this.endTime = this.question.asked_time
                .clone()
                .add(this.question.json.json.timeLimit, "seconds");
        }
        this.maybeStartTimer();
    }

    get isLecturer() {
        return this.data.isLecturer;
    }

    private maybeStartTimer() {
        if (!this.gotResult && this.countdownTimer) {
            this.barFilled = 0;
            if (this.endTime) {
                this.updateMaxProgress();
                this.countdownTimer.start();
            }
        }
    }

    private updateMaxProgress() {
        if (this.endTime) {
            this.progressMax = this.endTime.diff(
                this.question.asked_time,
                "s",
                true
            );
        } else {
            this.progressMax = undefined;
        }
    }

    timerTick(remaining: number) {
        const max = this.progressMax!;
        this.barFilled = max - remaining;
        if (this.barFilled >= max) {
            void this.endQuestion();
        }
    }
}

@NgModule({
    declarations: [AnswerToQuestionDialogComponent],
    imports: [
        BrowserModule,
        TimUtilityModule,
        DialogModule,
        AnswerSheetModule,
        ProgressbarModule.forRoot(),
    ],
})
export class AnswerToQuestionDialogModule {}
