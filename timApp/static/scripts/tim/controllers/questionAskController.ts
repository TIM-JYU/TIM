import * as answerSheet from "tim/directives/dynamicAnswerSheet";
import {markAsUsed} from "tim/utils";
import {DialogController, registerDialogComponent, showDialog} from "../dialog";
import {makePreview} from "../directives/dynamicAnswerSheet";
import {IPreviewParams} from "../directives/dynamicAnswerSheet";
import {IUniqueParId} from "../lecturetypes";
import {$http, $rootScope, $timeout} from "../ngimport";
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

export interface IAskNew extends IUniqueParId {
    lectureId: number;
}

export interface IReAsk {
    askedId: number;
    lectureId: number;
}

function isReasking(p: QuestionPreviewParams): p is IReAsk {
    return (p as IReAsk).askedId !== undefined;
}

export type QuestionPreviewParams = IAskNew | IReAsk;

export class QuestionPreviewController extends DialogController<{params: QuestionPreviewParams}, number> {
    private questiondata: IPreviewParams = null;

    constructor() {
        super();
    }

    async $onInit() {
        if (!isReasking(this.resolve.params)) {
            const data = await fetchQuestion(this.resolve.params.docId, this.resolve.params.parId, false);
            this.questiondata = makePreview(data);
        } else {
            // TODO
        }
    }

    async editQuestion() {
        if (!isReasking(this.resolve.params)) {
            await fetchAndEditQuestion(this.resolve.params.docId, this.resolve.params.parId);
        } else {
            // TODO
        }
        this.close(null);
    }

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:questionPreviewController
     */
    async ask() {
        const markup = this.questiondata.markup;
        const args = !isReasking(this.resolve.params) ? {
            buster: new Date().getTime(),
            doc_id: this.resolve.params.docId,
            lecture_id: this.resolve.params.lectureId,
            par_id: this.resolve.params.parId,
        } : {
            asked_id: this.resolve.params.askedId,
            buster: new Date().getTime(),
            lecture_id: this.resolve.params.lectureId,
        };
        const response = await $http.post<number>("/askQuestion", {}, {
            params: args,
        });
        const id = response.data;
        if (this.lectureSettings.useAnswers) {
            // Because of dynamic creation needs to wait 1ms to ensure that the directive is made(maybe?)
            $timeout(() => {
                $rootScope.$broadcast("createChart", markup.json);
            }, 1);

            const answer = {askedId: id};
            this.currentQuestionId = id;
            this.getLectureAnswers(answer);
        }
        $rootScope.$broadcast("setQuestionJson", {
            markup: markup,
            questionParId: data.par_id,
            askedId: id,
            isLecturer: this.isLecturer,
            askedTime: new Date().valueOf() + this.clockOffset,
            clockOffset: this.clockOffset,
        });
        // TODO: show statistics window
        this.close(id);
    }

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:questionPreviewController
     */
    async deleteQuestion() {
        if (!isReasking(this.resolve.params)) {
            await deleteQuestionWithConfirm(this.resolve.params.docId, this.resolve.params.parId);
        }
    }
}

registerDialogComponent("timAskQuestion", QuestionPreviewController, {
    template: `
<div class="popUpWindow questionPopUp">
    <div class="questionPreview">
        <div id="questionTypeAndTimeLimit">
            <span ng-if="$ctrl.markup.json.timeLimit > 0">Time limit: {{ $ctrl.markup.json.timeLimit }} seconds</span>
            <span ng-if="!$ctrl.markup.json.timeLimit">No time limit.</span>
        </div>
        <dynamic-answer-sheet preview="true" questiondata="$ctrl.questiondata"></dynamic-answer-sheet>
    </div>
    <div class="buttons">
        <!-- <button ng-click="deleteQuestion()" class="btn btn-danger pull-left">Delete</button> -->
        <button ng-show="lctrl.inLecture" ng-click="$ctrl.ask()" class="timButton">Ask</button>&nbsp;&nbsp;
        <button ng-click="$ctrl.editQuestion()" class="timButton">Edit</button>
        <button ng-click="$ctrl.close()" class="timButton">Close</button>
    </div>
</div>
`,
});

export async function showQuestionAskDialog(p: QuestionPreviewParams) {
    return await showDialog<QuestionPreviewController, {params: QuestionPreviewParams}, number>("timAskQuestion", {params: () => p});
}
