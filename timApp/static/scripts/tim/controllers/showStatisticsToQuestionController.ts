import * as chart from "tim/directives/showChartDirective";
import {markAsUsed} from "tim/utils";
import {DialogController, registerDialogComponent, showDialog} from "../dialog";
import {IAskedQuestion, IQuestionAnswer} from "../lecturetypes";
import {$http, $timeout} from "../ngimport";

markAsUsed(chart);

export type IStatisticsParams = IAskedQuestion;

export interface IStatisticsResult {
}
export class ShowStatisticsToQuestionController extends DialogController<{params: IStatisticsParams}, IStatisticsResult, "timQuestionStatistics"> {
    private answers: IQuestionAnswer[] = [];

    constructor() {
        super();
    }

    public getTitle() {
        return `Question ${this.resolve.params.json.json.questionTitle} statistics`;
    }

    private $onInit() {
        void this.getLectureAnswers();
    }

    /**
     * Gets answers from the current lecture to current question.
     */
    private async getLectureAnswers() {
        while (!this.closed) {
            const response = await $http.get<IQuestionAnswer[]>("/getLectureAnswers", {
                params: {
                    asked_id: this.resolve.params.asked_id,
                    buster: new Date().getTime(),
                },
            });
            const data = response.data;
            for (const ans of data) {
                this.answers.push(ans);
            }
            await $timeout(1000);
        }
    }
}

registerDialogComponent("timQuestionStatistics",
    ShowStatisticsToQuestionController,
    {
        template: `
<div class="modal-header">
    <h4 class="modal-title" id="modal-title" ng-bind-html="$ctrl.getTitle()"></h4>
</div>
<div class="modal-body" id="modal-body">
    <show-chart-directive
            divresize="true"
            question="$ctrl.resolve.params"
            answers="$ctrl.answers"></show-chart-directive>
</div>
<div class="modal-footer">
    <button class="timButton" ng-click="$ctrl.close()">Close</button>
</div>
`,
    });

export async function showStatisticsDialog(p: IStatisticsParams) {
    return await showDialog<ShowStatisticsToQuestionController>("timQuestionStatistics", {params: () => p});
}
