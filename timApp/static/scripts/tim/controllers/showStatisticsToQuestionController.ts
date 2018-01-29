import {Moment} from "moment";
import * as chart from "tim/directives/showChartDirective";
import {markAsUsed} from "tim/utils";
import {DialogController, registerDialogComponent, showDialog} from "../dialog";
import {IAskedQuestion} from "../lecturetypes";
import {$http, $timeout} from "../ngimport";

markAsUsed(chart);

export type IStatisticsParams = IAskedQuestion;

export interface IStatisticsResult {
}

/**
 * Created by hajoviin on 6.5.2015.
 * FILL WITH SUITABLE TEXT
 * @module showStatisticsToQuestionController
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

interface IQuestionAnswer {
    answer_id: number;
    answer: string;
    points: number;
}

interface ILectureAnswersResponse {
    answers: IQuestionAnswer[];
    askedId: number;
    latestAnswer: Moment;
}

export class ShowStatisticsToQuestionController extends DialogController<{params: IStatisticsParams}, IStatisticsResult, "timQuestionStatistics"> {
    private answers: IQuestionAnswer[] = [];

    constructor() {
        super();
    }

    private $onInit() {
        void this.getLectureAnswers();
    }

    /**
     * Gets answers from the current lecture to current question.
     */
    private async getLectureAnswers() {
        while (true) {
            const response = await $http.get<ILectureAnswersResponse>("/getLectureAnswers", {
                params: {
                    asked_id: this.resolve.params.asked_id,
                    buster: new Date().getTime(),
                },
            });
            const respData = response.data;
            for (const ans of respData.answers) {
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
<div class="show-answer">
    <div class="statisticsWindow">
        <h2 ng-bind-html="$ctrl.questionTitle" ng-click="$ctrl.dynamicAnswerShowControl.toggle()"></h2>
        <div id="chartDiv" class="chartDiv">
            <show-chart-directive
            divresize="true"
            question="$ctrl.resolve.params"
            answers="$ctrl.answers"></show-chart-directive>
        </div>
    </div>
    <div class="buttons" style="margin-top: -35px;">
        <p ng-show="!$ctrl.dynamicAnswerShowControl.isText" class="chart-menu" style="display: inline;">
            <span ng-click="$ctrl.dynamicAnswerShowControl.toggle()">Bar</span>&nbsp;&nbsp;
        </p>
        <button class="timButton" ng-click="$ctrl.hide()">Close</button>
    </div>
</div>
`,
    });

export async function showStatisticsDialog(p: IStatisticsParams) {
    return await showDialog<ShowStatisticsToQuestionController>("timQuestionStatistics", {params: () => p});
}
