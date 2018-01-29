import * as answerSheet from "tim/directives/dynamicAnswerSheet";
import {markAsUsed} from "tim/utils";
import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "../dialog";
import {IPreviewParams, makePreview} from "../directives/dynamicAnswerSheet";
import {IAskedQuestion, IUniqueParId} from "../lecturetypes";
import {$http} from "../ngimport";
import {deleteQuestionWithConfirm, fetchAndEditQuestion, fetchQuestion} from "./questionController";

markAsUsed(answerSheet);

/**
 * FILL WITH SUITABLE TEXT
 * @module questionPreviewController
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

export interface IAskNew extends IUniqueParId, IShowAsk {
}

export interface IReAsk extends IShowAsk {
    askedId: number;
}

function isReasking(p: QuestionPreviewParams): p is IReAsk {
    return (p as IReAsk).askedId != null;
}

export type QuestionPreviewParams = IAskNew | IReAsk;

export class QuestionPreviewController extends DialogController<{params: QuestionPreviewParams}, IAskedQuestion, "timAskQuestion"> {
    private questiondata?: IPreviewParams;
    private showAsk: boolean;

    constructor() {
        super();
    }

    private async $onInit() {
        if (!isReasking(this.resolve.params)) {
            const data = await fetchQuestion(this.resolve.params.docId, this.resolve.params.parId, false);
            this.questiondata = makePreview(data.markup);
        } else {
            // TODO
        }
        this.showAsk = this.resolve.params.showAsk;
    }

    private async editQuestion() {
        if (!isReasking(this.resolve.params)) {
            await fetchAndEditQuestion(this.resolve.params.docId, this.resolve.params.parId);
        } else {
            // TODO
        }
        this.dismiss();
    }

    private async ask() {
        if (!this.questiondata) {
            await showMessageDialog("Question has not been loaded yet.");
            return;
        }
        const p = this.resolve.params;
        const args = isReasking(p) ? {
            asked_id: p.askedId,
        } : {
            doc_id: p.docId,
            par_id: p.parId,
        };
        const response = await $http.post<IAskedQuestion>("/askQuestion", {}, {
            params: {buster: new Date().getTime(), ...args},
        });
        const question = response.data;
        this.close(question);
    }

    private async deleteQuestion() {
        if (!isReasking(this.resolve.params)) {
            await deleteQuestionWithConfirm(this.resolve.params.docId, this.resolve.params.parId);
            this.dismiss();
        }
    }
}

registerDialogComponent("timAskQuestion", QuestionPreviewController, {
    template: `
<div class="popUpWindow questionPopUp">
    <div class="questionPreview">
        <div id="questionTypeAndTimeLimit">
            <span ng-if="$ctrl.questiondata.markup.timeLimit > 0">
            Time limit: {{ $ctrl.markup.json.timeLimit }} seconds
            </span>
            <span ng-if="!$ctrl.questiondata.markup.timeLimit">
            No time limit.
            </span>
        </div>
        <dynamic-answer-sheet questiondata="$ctrl.questiondata"></dynamic-answer-sheet>
    </div>
    <div class="buttons">
        <!-- <button ng-click="deleteQuestion()" class="btn btn-danger pull-left">Delete</button> -->
        <button ng-show="$ctrl.showAsk" ng-click="$ctrl.ask()" class="timButton">Ask</button>&nbsp;&nbsp;
        <button ng-click="$ctrl.editQuestion()" class="timButton">Edit</button>
        <button ng-click="$ctrl.close()" class="timButton">Close</button>
    </div>
</div>
`,
});

export async function showQuestionAskDialog(p: QuestionPreviewParams) {
    return await showDialog<QuestionPreviewController>("timAskQuestion", {params: () => p});
}
