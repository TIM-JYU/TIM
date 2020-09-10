import {IChangesObject, IController, IOnChangesObject} from "angular";
import Chart, {ChartData, ChartType} from "chart.js";
import $ from "jquery";
import {timApp} from "tim/app";
import {fixQuestionJson} from "tim/document/question/dynamicAnswerSheet";
import {Overwrite} from "type-zoo";
import {assertNotNull, clone, truncate} from "../util/utils";
import {
    IAskedQuestion,
    IQuestionAnswer,
    IQuestionAnswerPlain,
} from "./lecturetypes";

/**
 * Created by hajoviin on 13.5.2015.
 * FILL WITH SUITABLE TEXT
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

function timStripHtml(s: string) {
    s = s.replace(/<[^>]*>?/gm, ""); // problem: <img src=http://www.google.com.kh/images/srpr/nav_logo27.png onload="alert(42)" >
    s = s.replace("\\(", "");
    s = s.replace("\\)", "");
    s = s.replace("&lt;", "<");
    s = s.replace("&gt;", ">");
    return s;
}

function timFillArray<T>(len: number, value: T) {
    return Array.apply(null, new Array(len)).map(() => value);
}

function qstCleanHtml(s: string) {
    s = timStripHtml(s);
    return s.replace("&amp;", "&").trim();
}

/* takes a string phrase and breaks it into separate phrases
 no bigger than 'maxwidth', breaks are made at complete words.*/

function qstFormatLabel(str: string, maxwidth: number, maxrows: number) {
    if (str.length <= maxwidth) {
        return str;
    }
    const sections: string[] = [];
    const words = str.split(" ");
    let temp = "";

    words.forEach((item, index) => {
        if (sections.length >= maxrows) {
            return;
        }
        if (temp.length > 0) {
            const concat = temp + " " + item;

            if (concat.length > maxwidth) {
                sections.push(temp);
                temp = "";
            } else {
                if (index == words.length - 1) {
                    sections.push(concat);
                    return;
                } else {
                    temp = concat;
                    return;
                }
            }
        }

        if (index == words.length - 1) {
            sections.push(item);
            return;
        }

        if (item.length < maxwidth) {
            temp = item;
        } else {
            sections.push(item);
        }
    });

    return sections;
}

function qstShortText(s: string): string | string[] {
    const parts = s.split("!!");
    let text = "";
    if (parts.length >= 2) {
        text = qstCleanHtml(parts[1]);
    }
    if (text === "") {
        text = qstCleanHtml(parts[0]);
    }
    let max = 25;
    if (parts.length >= 3) {
        max = parseInt(parts[2], 10);
        if (isNaN(max)) {
            max = 25;
        }
    }
    let maxrows = 3;
    if (parts.length >= 4) {
        maxrows = parseInt(parts[3], 10);
        if (isNaN(maxrows)) {
            maxrows = 3;
        }
    }
    const result = qstFormatLabel(text, max, maxrows);
    if (typeof result === "string") {
        return truncate(text, max);
    } else {
        return result.map((row) => truncate(row, max));
    }
}

function timGetLSIntValue(key: string, def: number): number {
    let val: string | null | number = window.localStorage.getItem(key);
    if (val == null) {
        val = def;
    }
    val = parseInt(val.toString(), 10);
    if (isNaN(val)) {
        val = def;
    }
    return val;
}

let qstChartIndex = timGetLSIntValue("qstChartIndex", 0);

type AnswerList = (IQuestionAnswer | IQuestionAnswerPlain)[];

function* enumAnswers(answers: AnswerList): Iterable<[number, string[]]> {
    for (const answ of answers) {
        const onePersonAnswers = answ.answer;
        for (let a = 0; a < onePersonAnswers.length; a++) {
            const singleAnswers = onePersonAnswers[a];
            yield [a, singleAnswers];
        }
    }
}

interface IDataSet {
    label: string | string[];
    fill: string;
    borderColor: string | string[];
    pointBackgroundColor: string;
    pointBorderColor: string;
    pointHoverBackgroundColor: string;
    pointHoverBorderColor: string;
    data: number[];
    backgroundColor?: string | string[];
    borderWidth?: number;
}

type TimChartData = Overwrite<ChartData, {datasets: IDataSet[]}>;
type ChartConfig = Overwrite<Chart.ChartConfiguration, {data: TimChartData}>;
type TimChart = Overwrite<Chart, {data: TimChartData}>;

const chartTypes: ChartType[] = ["bar", "horizontalBar"];

class ChartController implements IController {
    static $inject = ["$element"];
    private isText = false;
    private div?: JQuery<HTMLDivElement>;
    private chartIndex: number;
    private question?: IAskedQuestion;
    private answers?: AnswerList;
    private chartConfig?: ChartConfig;
    private answerChart?: TimChart;
    private lastAnswerCount = 0;

    // TODO: If more than 12 choices this will break. Refactor to better format.
    private basicSets: IDataSet[] = [
        {
            label: "", // "Answer",
            fill: "rgba(0,220,0,0.2)",
            borderColor: "rgba(0,220,0,1)",
            pointBackgroundColor: "rgba(0,220,0,1)",
            pointBorderColor: "#fff",
            pointHoverBackgroundColor: "#fff",
            pointHoverBorderColor: "rgba(0,220,0,1)",
            data: [],
        },
        {
            label: "Answers",
            fill: "rgba(220,0,0,0.2)",
            borderColor: "rgba(220,0,0,1)",
            pointBackgroundColor: "rgba(220,0,0,1)",
            pointBorderColor: "#fff",
            pointHoverBackgroundColor: "#fff",
            pointHoverBorderColor: "rgba(220,0,0,1)",
            data: [],
        },

        {
            label: "Answers",
            fill: "rgba(0,0,220,0.2)",
            borderColor: "rgba(0,0,220,1)",
            pointBackgroundColor: "rgba(0,0,220,1)",
            pointBorderColor: "#fff",
            pointHoverBackgroundColor: "#fff",
            pointHoverBorderColor: "rgba(0,0,220,1)",
            data: [],
        },
        {
            label: "Answers",
            fill: "rgba(0,220,220,0.2)",
            borderColor: "rgba(0,220,220,1)",
            pointBackgroundColor: "rgba(0,220,220,1)",
            pointBorderColor: "#fff",
            pointHoverBackgroundColor: "#fff",
            pointHoverBorderColor: "rgba(0,220,220,1)",
            data: [],
        },
        {
            label: "Answers",
            fill: "rgba(220,0,220,0.2)",
            borderColor: "rgba(220,0,220,1)",
            pointBackgroundColor: "rgba(220,0,220,1)",
            pointBorderColor: "#fff",
            pointHoverBackgroundColor: "#fff",
            pointHoverBorderColor: "rgba(0,0,220,1)",
            data: [],
        },
        {
            label: "Answers",
            fill: "rgba(220,220,220,0.2)",
            borderColor: "rgba(220,220,220,1)",
            pointBackgroundColor: "rgba(220,220,220,1)",
            pointBorderColor: "#fff",
            pointHoverBackgroundColor: "#fff",
            pointHoverBorderColor: "rgba(220,220,220,1)",
            data: [],
        },
        {
            label: "Answers",
            fill: "rgba(165,220,0,0.2)",
            borderColor: "rgba(165,220,0,1)",
            pointBackgroundColor: "rgba(165,220,0,1)",
            pointBorderColor: "#fff",
            pointHoverBackgroundColor: "#fff",
            pointHoverBorderColor: "rgba(165,220,0,1)",
            data: [],
        },
        {
            label: "Answers",
            fill: "rgba(220,165,0,0.2)",
            borderColor: "rgba(220,165,0,1)",
            pointBackgroundColor: "rgba(220,165,0,1)",
            pointBorderColor: "#fff",
            pointHoverBackgroundColor: "#fff",
            pointHoverBorderColor: "rgba(220,165,0,1)",
            data: [],
        },
        {
            label: "Answers",
            fill: "rgba(0,165,220,0.2)",
            borderColor: "rgba(220,165,0,1)",
            pointBackgroundColor: "rgba(220,165,0,1)",
            pointBorderColor: "#fff",
            pointHoverBackgroundColor: "#fff",
            pointHoverBorderColor: "rgba(220,165,0,1)",
            data: [],
        },
        {
            label: "Answers",
            fill: "rgba(220,0,165,0.2)",
            borderColor: "rgba(220,0,165,1)",
            pointBackgroundColor: "rgba(220,0,165,1)",
            pointBorderColor: "#fff",
            pointHoverBackgroundColor: "#fff",
            pointHoverBorderColor: "rgba(220,0,165,1)",
            data: [],
        },
        {
            label: "Answers",
            fill: "rgba(30,0,75,0.2)",
            borderColor: "rgba(30,0,75,1)",
            pointBackgroundColor: "rgba(30,0,75,1)",
            pointBorderColor: "#fff",
            pointHoverBackgroundColor: "#fff",
            pointHoverBorderColor: "rgba(30,0,75,1)",
            data: [],
        },
        {
            label: "Answers",
            fill: "rgba(75,75,180,0.2)",
            borderColor: "rgba(75,75,180,1)",
            pointBackgroundColor: "rgba(75,75,180,1)",
            pointBorderColor: "#fff",
            pointHoverBackgroundColor: "#fff",
            pointHoverBorderColor: "rgba(75,75,180,1)",
            data: [],
        },
    ];
    private element: JQLite;
    private textAnswers: string[] = [];

    constructor(element: JQLite) {
        this.chartIndex = qstChartIndex;
        this.element = element;
    }

    $onInit() {}

    $onChanges(onChangesObj: IOnChangesObject) {
        const qdata = onChangesObj.question as
            | IChangesObject<IAskedQuestion>
            | undefined;
        const adata = onChangesObj.answers as
            | IChangesObject<AnswerList>
            | undefined;
        if (qdata) {
            this.createChart();
        } else if (adata) {
            this.update();
        }
    }

    $doCheck() {
        if (this.answers && this.lastAnswerCount !== this.answers.length) {
            this.lastAnswerCount = this.answers.length;
            this.update();
        }
    }

    $postLink() {
        this.div = this.element.find(".canvasContainer") as JQuery<
            HTMLDivElement
        >;
        assertNotNull(this.div);
        this.createChart();
    }

    intScale(value: number, axis: number): number | undefined {
        if (axis == this.chartIndex) {
            return value;
        }
        if (value % 1 === 0) {
            return value;
        }
    }

    fillValues(emptyData: number[], dataSets: IDataSet[], index: number) {
        dataSets.push(this.basicSets[index % this.basicSets.length]);
        dataSets[index].data = emptyData.slice();
        const color = this.basicSets[index % this.basicSets.length].fill;
        dataSets[index].backgroundColor = timFillArray(emptyData.length, color);
        const bordercolor = this.basicSets[index % this.basicSets.length]
            .borderColor;
        dataSets[index].borderColor = timFillArray(
            emptyData.length,
            bordercolor as string
        );
        dataSets[index].borderWidth = 1;
    }

    async createChart() {
        if (!this.question || !this.div) {
            return;
        }

        const j = this.question.json.json;

        if (j.answerFieldType === "text") {
            this.isText = true;
            return;
        }
        const data = fixQuestionJson(j);
        this.isText = false;
        const showLegend = data.headers.length > 1;

        const labels: Array<string | string[]> = [];
        const emptyData: number[] = [];
        for (const row of data.rows) {
            const text = qstShortText(row.text);
            labels.push(text);
            emptyData.push(0);
        }
        if (!(j.questionType === "matrix" || j.questionType === "true-false")) {
            labels.push("No answer");
            emptyData.push(0);
        }
        const usedDataSets: IDataSet[] = [];
        if (j.questionType === "matrix" || j.questionType === "true-false") {
            for (let i = 0; i < data.rows[0].columns.length; i++) {
                this.fillValues(emptyData, usedDataSets, i);
            }

            const noAnswerIndex = usedDataSets.length; // for no answer
            this.fillValues(emptyData, usedDataSets, noAnswerIndex);
            usedDataSets[noAnswerIndex].label = "No answer";
            for (let i = 0; i < data.headers.length; i++) {
                usedDataSets[i].label = qstShortText(data.headers[i].text);
            }
        } else {
            this.fillValues(emptyData, usedDataSets, 0);
        }

        this.chartConfig = {
            type: "horizontalBar",
            data: {
                labels: labels,
                datasets: usedDataSets,
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                animation: {
                    duration: 0,
                },
                legend: {
                    display: showLegend,
                    labels: {},
                },
                scales: {
                    xAxes: [
                        {
                            ticks: {
                                min: 0,
                                callback: (value) => {
                                    // According to http://www.chartjs.org/docs/latest/axes/labelling.html#creating-custom-tick-formats,
                                    // this callback is allowed to return undefined. Type definition is not accurate,
                                    // so we use "as number".
                                    return this.intScale(
                                        value as number,
                                        0
                                    ) as number;
                                },
                            },
                        },
                    ],
                    yAxes: [
                        {
                            ticks: {
                                min: 0,
                                callback: (value) => {
                                    return this.intScale(
                                        value as number,
                                        1
                                    ) as number;
                                },
                            },
                        },
                    ],
                },
            },
        };

        await this.changeType();
    }

    toggle() {
        qstChartIndex = timGetLSIntValue("qstChartIndex", 0);
        if (qstChartIndex != this.chartIndex) {
            qstChartIndex = this.chartIndex;
        }
        qstChartIndex = (qstChartIndex + 1) % chartTypes.length;
        this.chartIndex = qstChartIndex;
        window.localStorage.setItem("qstChartIndex", qstChartIndex.toString());
        this.changeType();
    }

    async changeType() {
        if (!this.chartConfig || !this.div) {
            return;
        }
        if (this.answerChart) {
            this.answerChart.destroy();
        }
        this.div.empty();
        if (!this.isText) {
            const newType = chartTypes[qstChartIndex];
            const ctx: JQuery<HTMLCanvasElement> = $(`<canvas><canvas>`);
            this.div.append(ctx);
            const config = clone(this.chartConfig);
            config.type = newType;
            const chart = (await import("chart.js")).Chart;
            this.answerChart = new chart(ctx, config) as TimChart;
        }
        this.update();
    }

    update() {
        const answers = this.answers;
        if (!answers) {
            return;
        }
        if (this.isText) {
            this.textAnswers = [];
            for (const [_, singleAnswers] of enumAnswers(answers)) {
                for (const singleAnswer of singleAnswers) {
                    this.textAnswers.push(singleAnswer);
                }
            }
        } else {
            if (!this.answerChart || !this.chartConfig) {
                return;
            }
            const datasets = clone(this.chartConfig.data.datasets);

            // The "no answer" situation has two different formats:
            // Old format [[""]] (has an empty string)
            // New format [[]] (is an empty array)
            const firstSet = datasets[0].data;
            for (const [a, singleAnswers] of enumAnswers(answers)) {
                if (singleAnswers.length === 0) {
                    if (datasets.length === 1) {
                        firstSet[firstSet.length - 1]++; // no answer (new format)
                    } else {
                        datasets[datasets.length - 1].data[a]++; // no answer (new format)
                    }
                }
                for (const singleAnswer of singleAnswers) {
                    const index = parseInt(singleAnswer, 10) - 1;
                    if (datasets.length === 1) {
                        if (index >= 0 && index < firstSet.length) {
                            firstSet[index]++;
                        } else {
                            firstSet[firstSet.length - 1]++; // no answer (old format)
                        }
                    } else {
                        if (index >= 0 && index < datasets.length) {
                            datasets[index].data[a]++;
                        } else {
                            datasets[datasets.length - 1].data[a]++; // no answer (old format)
                        }
                    }
                }
            }
            this.answerChart.data.datasets = datasets;
            this.answerChart.update();
        }
    }

    getTotalPoints() {
        if (!this.answers) {
            return undefined;
        }
        return this.answers.reduce((prev, curr) => curr.points + prev, 0);
    }
}

timApp.component("showChartDirective", {
    bindings: {
        answers: "<",
        question: "<",
    },
    controller: ChartController,
    template: `
<div ng-show="!$ctrl.isText" class="canvasContainer" style="flex: 1; min-height: 0">

</div>
<div ng-show="$ctrl.isText">
    <p ng-repeat="t in $ctrl.textAnswers" ng-bind="t"></p>
</div>
<p ng-show="$ctrl.answers">Total points: {{ $ctrl.getTotalPoints() }}</p>
<div ng-show="!$ctrl.isText">
    <button class="timButton btn-xs" ng-click="$ctrl.toggle()">Change chart orientation</button>
</div>
    `,
});
