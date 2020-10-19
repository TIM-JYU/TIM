import moment from "moment";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {timeout, to2} from "tim/util/utils";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {FormsModule} from "@angular/forms";
import {BrowserModule} from "@angular/platform-browser";
import {AnswerChartModule} from "tim/lecture/answer-chart.component";
import {IAskedQuestion, IQuestionAnswerPlain} from "./lecturetypes";

export type IStatisticsParams = IAskedQuestion;

export interface IStatisticsResult {}

function getQuestionEndTime(q: IAskedQuestion) {
    return q.asked_time
        .clone()
        .add(moment.duration(q.json.json.timeLimit ?? 999999, "seconds"));
}

@Component({
    selector: "tim-question-statistics-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <div class="flex" style="height: 100%; flex-direction: column">
                    <tim-answer-chart
                            [question]="data"
                            [answers]="answers"></tim-answer-chart>
                    <p>Question <span *ngIf="ended">has ended</span><span *ngIf="!ended">is running</span>.</p>
                </div>
            </ng-container>
            <ng-container footer>
                <button class="timButton" (click)="close({})">Close</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class QuestionStatisticsDialogComponent extends AngularDialogComponent<
    IStatisticsParams,
    IStatisticsResult
> {
    protected dialogName = "QuestionStatistics";
    answers: IQuestionAnswerPlain[] = [];
    ended = false;
    private lastFetch = moment({year: 1900});

    public getTitle() {
        return `Question ${this.data.json.json.questionTitle} statistics`;
    }

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit() {
        void this.getLectureAnswers();
    }

    /**
     * Gets answers from the current lecture to current question.
     */
    private async getLectureAnswers() {
        while (!this.closed) {
            const now = moment();
            const r = await to2(
                this.http
                    .get<IQuestionAnswerPlain[]>("/getLectureAnswers", {
                        params: {
                            after: this.lastFetch.toISOString(),
                            asked_id: this.data.asked_id.toString(),
                            buster: new Date().getTime().toString(),
                        },
                    })
                    .toPromise()
            );
            if (r.ok) {
                const data = r.result;
                for (const ans of data) {
                    this.answers.push(ans);
                }
                if (this.answers.length > 0) {
                    this.lastFetch = this.answers[
                        this.answers.length - 1
                    ].answered_on
                        .clone()
                        .add(1, "ms");
                }
                if (getQuestionEndTime(this.data) < now) {
                    this.ended = true;
                    return;
                }
            } else {
                this.ended = true;
                return;
            }
            await timeout(1000);
        }
    }
}

@NgModule({
    declarations: [QuestionStatisticsDialogComponent],
    imports: [
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        DialogModule,
        AnswerChartModule,
    ],
})
export class QuestionStatisticsDialogModule {}
