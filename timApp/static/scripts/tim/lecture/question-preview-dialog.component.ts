import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {Component} from "@angular/core";
import {to} from "tim/util/utils";
import {
    IPreviewParams,
    makePreview,
} from "../document/question/answer-sheet.component";
import {
    deleteQuestionWithConfirm,
    fetchAndEditQuestion,
    fetchAskedQuestion,
    fetchQuestion,
    showQuestionEditDialog,
} from "../document/question/question-edit-dialog.component";
import {showMessageDialog} from "../ui/dialog";
import {$http} from "../util/ngimport";
import {IAskedQuestion, IUniqueParId} from "./lecturetypes";

/**
 * FILL WITH SUITABLE TEXT
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

export interface IShowAsk {
    showAsk: boolean;
}

export interface IAskNew extends IUniqueParId {}

export interface IReAsk {
    askedId: number;
}

export type AskParams = IAskNew | IReAsk;

function isReasking(p: AskParams): p is IReAsk {
    return (p as IReAsk).askedId != null;
}

export type QuestionPreviewParams = AskParams & IShowAsk;

export async function askQuestion(p: AskParams) {
    const args = isReasking(p)
        ? {
              asked_id: p.askedId,
          }
        : {
              doc_id: p.docId,
              par_id: p.parId,
          };
    const response = await to(
        $http.post<IAskedQuestion>(
            "/askQuestion",
            {},
            {
                params: {buster: new Date().getTime(), ...args},
            }
        )
    );
    if (!response.ok) {
        throw Error("askQuestion failed");
    }
    return response.result.data;
}

@Component({
    selector: "tim-ask-question-dialog",
    template: `
        <tim-dialog-frame>
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
    IAskedQuestion
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
        if (!isReasking(this.data)) {
            await fetchAndEditQuestion(this.data.docId, this.data.parId);
        } else {
            await showQuestionEditDialog(
                await fetchAskedQuestion(this.data.askedId)
            );
        }
        this.dismiss();
    }

    async ask() {
        if (!this.questiondata) {
            await showMessageDialog("Question has not been loaded yet.");
            return;
        }
        const p = this.data;
        const question = await askQuestion(p);
        this.close(question);
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

export async function showQuestionAskDialog(p: QuestionPreviewParams) {
    return await (await angularDialog.open(QuestionPreviewDialogComponent, p))
        .result;
}
