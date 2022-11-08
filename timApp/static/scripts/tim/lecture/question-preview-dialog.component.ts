import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import type {AskParams, IShowAsk} from "tim/lecture/askQuestion";
import {askQuestion, isReasking} from "tim/lecture/askQuestion";
import {
    deleteQuestionWithConfirm,
    fetchAndEditQuestion,
    fetchAskedQuestion,
    fetchQuestion,
} from "tim/document/question/fetchQuestion";
import {showQuestionEditDialog} from "tim/document/question/showQuestionEditDialog";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {to2} from "tim/util/utils";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import type {IQuestionDialogResult} from "tim/document/question/question-edit-dialog.component";
import type {IPreviewParams} from "tim/document/question/answer-sheet.component";
import {
    AnswerSheetModule,
    makePreview,
} from "tim/document/question/answer-sheet.component";
import type {IAskedQuestion} from "tim/lecture/lecturetypes";
import {CommonModule} from "@angular/common";

export type QuestionPreviewParams = AskParams & IShowAsk;

export interface IQuestionPreviewDialogResult {
    question?: IAskedQuestion;
    editResult?: IQuestionDialogResult;
}

@Component({
    selector: "tim-ask-question-dialog",
    template: `
        <tim-dialog-frame [dialogOptions]="dialogOptions" [dialogName]="dialogName" [align]="'center'">
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body *ngIf="questiondata">
            <span *ngIf="getTimeLimit()">
            Time limit: {{ getTimeLimit() }} seconds
            </span>
                <span *ngIf="!questiondata.markup.timeLimit">
            No time limit.
            </span>
                <p *ngIf="questiondata.markup.randomizedRows !== undefined">
                    {{printRandomizationInfo()}}
                </p>
                <tim-answer-sheet [questiondata]="questiondata"></tim-answer-sheet>
            </ng-container>
            <ng-container footer>
                <!-- <button (click)="deleteQuestion()" class="btn btn-danger pull-left">Delete</button> -->
                <button *ngIf="showAsk()" (click)="ask()" class="timButton">Ask</button>&nbsp;&nbsp;
                <button (click)="editQuestion()" class="timButton">Edit</button>
                <button (click)="dismiss()" class="timButton">Close</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class QuestionPreviewDialogComponent extends AngularDialogComponent<
    QuestionPreviewParams,
    IQuestionPreviewDialogResult
> {
    protected dialogName = "askQuestion";
    questiondata?: IPreviewParams;

    public getTitle() {
        return "Ask a question";
    }

    ngOnInit() {
        (async () => {
            if (!isReasking(this.data)) {
                const data = await fetchQuestion(
                    this.data.docId,
                    this.data.parId,
                    false
                );
                this.questiondata = makePreview(data.markup, {
                    enabled: false,
                    showCorrectChoices: false,
                    showExplanations: false,
                });
            } else {
                const data = await fetchAskedQuestion(this.data.askedId);
                this.questiondata = makePreview(data.json.json, {
                    enabled: false,
                    showCorrectChoices: false,
                    showExplanations: false,
                });
            }
        })();
    }

    showAsk() {
        return this.data.showAsk;
    }

    async editQuestion() {
        let res;
        if (!isReasking(this.data)) {
            res = await to2(
                fetchAndEditQuestion(this.data.docId, this.data.parId)
            );
        } else {
            res = await to2(
                showQuestionEditDialog(
                    await fetchAskedQuestion(this.data.askedId)
                )
            );
        }
        await res;
        if (res.ok) {
            this.close({editResult: res.result});
        } else {
            this.dismiss();
        }
    }

    async ask() {
        if (!this.questiondata) {
            await showMessageDialog("Question has not been loaded yet.");
            return;
        }
        const p = this.data;
        const question = await askQuestion(p);
        this.close({question});
    }

    private async deleteQuestion() {
        if (!isReasking(this.data)) {
            await deleteQuestionWithConfirm(this.data.docId, this.data.parId);
            this.dismiss();
        }
    }

    getTimeLimit() {
        if (!this.questiondata) {
            return undefined;
        }
        return this.questiondata.markup.timeLimit;
    }

    printRandomizationInfo(): string {
        if (!this.questiondata || !this.questiondata.markup.randomizedRows) {
            return "";
        }
        let locks: number;
        if (this.questiondata.markup.doNotMove) {
            if (typeof this.questiondata.markup.doNotMove == "number") {
                locks = 1;
            } else {
                locks = this.questiondata.markup.doNotMove.length;
            }
            locks = Math.min(locks, this.questiondata.markup.rows.length);
        } else {
            locks = 0;
        }
        const randomRows = Math.min(
            this.questiondata.markup.randomizedRows,
            this.questiondata.markup.rows.length - locks
        );
        let ret = `Answerers will see ${randomRows} randomly picked rows`;
        if (locks) {
            ret += ` and ${locks} guaranteed rows`;
        }
        return ret + ".";
    }
}

@NgModule({
    declarations: [QuestionPreviewDialogComponent],
    imports: [CommonModule, DialogModule, AnswerSheetModule, TimUtilityModule],
})
export class QuestionPreviewDialogModule {}
