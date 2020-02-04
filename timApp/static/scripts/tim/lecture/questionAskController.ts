import {IScope} from "angular";
import * as answerSheet from "tim/document/question/dynamicAnswerSheet";
import {markAsUsed, to} from "tim/util/utils";
import {IPreviewParams, makePreview} from "../document/question/dynamicAnswerSheet";
import {
    deleteQuestionWithConfirm,
    fetchAndEditQuestion,
    fetchAskedQuestion,
    fetchQuestion,
    showQuestionEditDialog,
} from "../document/question/questionController";
import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "../ui/dialog";
import {$http} from "../util/ngimport";
import {IAskedQuestion, IUniqueParId} from "./lecturetypes";

markAsUsed(answerSheet);

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

export interface IAskNew extends IUniqueParId {
}

export interface IReAsk {
    askedId: number;
}

export type AskParams = IAskNew | IReAsk;

function isReasking(p: AskParams): p is IReAsk {
    return (p as IReAsk).askedId != null;
}

export type QuestionPreviewParams = AskParams & IShowAsk;

export async function askQuestion(p: AskParams) {
    const args = isReasking(p) ? {
        asked_id: p.askedId,
    } : {
        doc_id: p.docId,
        par_id: p.parId,
    };
    const response = await to($http.post<IAskedQuestion>("/askQuestion", {}, {
        params: {buster: new Date().getTime(), ...args},
    }));
    if (!response.ok) {
        throw Error("askQuestion failed");
    }
    return response.result.data;
}

export class QuestionPreviewController extends DialogController<{params: QuestionPreviewParams}, IAskedQuestion> {
    static component = "timAskQuestion";
    static $inject = ["$element", "$scope"] as const;
    private questiondata?: IPreviewParams;

    constructor(protected element: JQLite, protected scope: IScope) {
        super(element, scope);
    }

    public getTitle() {
        return "Ask a question";
    }

    $onInit() {
        super.$onInit();
        (async () => {
            if (!isReasking(this.resolve.params)) {
                const data = await fetchQuestion(this.resolve.params.docId, this.resolve.params.parId, false);
                this.questiondata = makePreview(data.markup, {
                    enabled: false,
                    showCorrectChoices: false,
                    showExplanations: false,
                });
            } else {
                const data = await fetchAskedQuestion(this.resolve.params.askedId);
                this.questiondata = makePreview(data.json.json, {
                    enabled: false,
                    showCorrectChoices: false,
                    showExplanations: false,
                });
            }
        })();
    }

    private showAsk() {
        return this.resolve.params.showAsk;
    }

    private async editQuestion() {
        if (!isReasking(this.resolve.params)) {
            await fetchAndEditQuestion(this.resolve.params.docId, this.resolve.params.parId);
        } else {
            await showQuestionEditDialog(await fetchAskedQuestion(this.resolve.params.askedId));
        }
        this.dismiss();
    }

    private async ask() {
        if (!this.questiondata) {
            await showMessageDialog("Question has not been loaded yet.");
            return;
        }
        const p = this.resolve.params;
        const question = await askQuestion(p);
        this.close(question);
    }

    private async deleteQuestion() {
        if (!isReasking(this.resolve.params)) {
            await deleteQuestionWithConfirm(this.resolve.params.docId, this.resolve.params.parId);
            this.dismiss();
        }
    }

    private getTimeLimit() {
        if (!this.questiondata) {
            return undefined;
        }
        return this.questiondata.markup.timeLimit;
    }
}

registerDialogComponent(QuestionPreviewController, {
    template: `
<tim-dialog>
    <dialog-header>
        Question
    </dialog-header>
    <dialog-body>
<span ng-if="$ctrl.getTimeLimit()">
            Time limit: {{ $ctrl.getTimeLimit() }} seconds
            </span>
        <span ng-if="!$ctrl.questiondata.markup.timeLimit">
            No time limit.
            </span>
        <dynamic-answer-sheet questiondata="$ctrl.questiondata"></dynamic-answer-sheet>
    </dialog-body>
    <dialog-footer>
        <!-- <button ng-click="deleteQuestion()" class="btn btn-danger pull-left">Delete</button> -->
        <button ng-show="$ctrl.showAsk()" ng-click="$ctrl.ask()" class="timButton">Ask</button>&nbsp;&nbsp;
        <button ng-click="$ctrl.editQuestion()" class="timButton">Edit</button>
        <button ng-click="$ctrl.dismiss()" class="timButton">Close</button>
    </dialog-footer>
</tim-dialog>
`,
});

export async function showQuestionAskDialog(p: QuestionPreviewParams) {
    return await showDialog(QuestionPreviewController, {params: () => p}).result;
}
