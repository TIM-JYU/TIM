import {IScope} from "angular";
import moment from "moment";
import * as chart from "tim/lecture/showChartDirective";
import {markAsUsed, to} from "tim/util/utils";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {$http, $timeout} from "../util/ngimport";
import {IAskedQuestion, IQuestionAnswer} from "./lecturetypes";

markAsUsed(chart);

export type IStatisticsParams = IAskedQuestion;

export interface IStatisticsResult {
}

function getQuestionEndTime(q: IAskedQuestion) {
    return q.asked_time.clone().add(moment.duration(q.json.json.timeLimit || 999999, "seconds"));
}

export class StatisticsToQuestionController extends DialogController<{params: IStatisticsParams}, IStatisticsResult> {
    static component = "timQuestionStatistics";
    static $inject = ["$element", "$scope"] as const;
    private answers: IQuestionAnswer[] = [];
    private ended = false;
    private lastFetch = moment({year: 1900});

    constructor(protected element: JQLite, protected scope: IScope) {
        super(element, scope);
    }

    public getTitle() {
        return `Question ${this.resolve.params.json.json.questionTitle} statistics`;
    }

    $onInit() {
        super.$onInit();
        void this.getLectureAnswers();
    }

    /**
     * Gets answers from the current lecture to current question.
     */
    private async getLectureAnswers() {
        while (!this.closed) {
            const now = moment();
            const r = await to($http.get<IQuestionAnswer[]>("/getLectureAnswers", {
                params: {
                    after: this.lastFetch.toISOString(),
                    asked_id: this.resolve.params.asked_id,
                    buster: new Date().getTime(),
                },
            }));
            if (r.ok) {
                const data = r.result.data;
                for (const ans of data) {
                    this.answers.push(ans);
                }
                if (this.answers.length > 0) {
                    this.lastFetch = this.answers[this.answers.length - 1].answered_on.clone().add(1, "ms");
                }
                if (getQuestionEndTime(this.resolve.params) < now) {
                    this.ended = true;
                    return;
                }
            } else {
                this.ended = true;
                return;
            }
            await $timeout(1000);
        }
    }
}

registerDialogComponent(StatisticsToQuestionController,
    {
        template: `
<tim-dialog>
    <dialog-header ng-bind-html="$ctrl.getTitle()">

    </dialog-header>
    <dialog-body>
        <show-chart-directive
                divresize="true"
                question="$ctrl.resolve.params"
                answers="$ctrl.answers"></show-chart-directive>
        <p ng-show="$ctrl.ended">Question has ended.</p>
    </dialog-body>
    <dialog-footer>
        <button class="timButton" ng-click="$ctrl.close()">Close</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showStatisticsDialog(p: IStatisticsParams) {
    return await showDialog(StatisticsToQuestionController, {params: () => p}).result;
}
