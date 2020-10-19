import {Component, Input, NgModule, OnChanges} from "@angular/core";
import {ChartData, ChartDataSets, ChartOptions, ChartType} from "chart.js";
import {fixQuestionJson} from "tim/document/question/answer-sheet.component";
import {Overwrite} from "type-zoo";
import {Changes} from "tim/util/angularchanges";
import {BrowserModule} from "@angular/platform-browser";
import {ChartsModule} from "ng2-charts";
import {clone, truncate} from "../util/utils";
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

function qstShortText(s: string): string {
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
        return result.map((row) => truncate(row, max)).join(" ");
    }
}

function timGetLSIntValue(key: string, def: number): number {
    const valStr = window.localStorage.getItem(key);
    if (valStr == null) {
        return def;
    }
    let val = parseInt(valStr.toString(), 10);
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
    label: string;
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

interface ChartConfig {
    type: ChartType;
    data: Overwrite<
        ChartData,
        {datasets: IDataSet[]; labels: Array<string | string[]>}
    >;
    options: ChartOptions;
}

@Component({
    selector: "tim-answer-chart",
    template: `
        <div *ngIf="!isText && chartData" style="flex: 1; min-height: 0">
            <!--suppress TypeScriptUnresolvedVariable -->
            <canvas baseChart
                    [datasets]="chartData.datasets"
                    [labels]="chartData.config.data.labels"
                    [options]="chartData.config.options"
                    [legend]="chartData.config.options.legend?.display || false"
                    [chartType]="chartTypes[chartIndex]">
            </canvas>
        </div>
        <div *ngIf="isText">
            <p *ngFor="let t of textAnswers" [innerText]="t"></p>
        </div>
        <p *ngIf="answers">Total points: {{ getTotalPoints() }}</p>
        <div *ngIf="!isText">
            <button class="timButton btn-xs" (click)="toggle()">Change chart orientation</button>
        </div>
    `,
    styleUrls: ["./answer-chart.component.scss"],
})
export class AnswerChartComponent implements OnChanges {
    isText = false;
    chartIndex = qstChartIndex;
    @Input() question?: IAskedQuestion;
    @Input() answers?: AnswerList;
    private chartConfig?: ChartConfig;
    chartData?: {config: ChartConfig; datasets: ChartDataSets[]};
    private lastAnswerCount = 0;
    chartTypes: ChartType[] = ["bar", "horizontalBar"];

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
    textAnswers: string[] = [];

    ngOnChanges(onChangesObj: Changes<this, "question" | "answers">) {
        const qdata = onChangesObj.question;
        const adata = onChangesObj.answers;
        if (qdata) {
            this.createChart();
        } else if (adata) {
            this.update();
        }
    }

    ngDoCheck() {
        if (this.answers && this.lastAnswerCount !== this.answers.length) {
            this.lastAnswerCount = this.answers.length;
            this.update();
        }
    }

    intScale(
        value: number | string,
        axis: number
    ): string | number | undefined {
        if (axis == this.chartIndex) {
            return value;
        }
        if (typeof value === "number" && value % 1 === 0) {
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

    createChart() {
        if (!this.question) {
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
                                    return this.intScale(value, 0);
                                },
                            },
                        },
                    ],
                    yAxes: [
                        {
                            ticks: {
                                min: 0,
                                callback: (value) => {
                                    return this.intScale(value, 1);
                                },
                            },
                        },
                    ],
                },
            },
        };

        this.update();
    }

    toggle() {
        qstChartIndex = timGetLSIntValue("qstChartIndex", 0);
        if (qstChartIndex != this.chartIndex) {
            qstChartIndex = this.chartIndex;
        }
        qstChartIndex = (qstChartIndex + 1) % this.chartTypes.length;
        this.chartIndex = qstChartIndex;
        window.localStorage.setItem("qstChartIndex", qstChartIndex.toString());
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
            if (!this.chartConfig) {
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
            this.chartData = {config: this.chartConfig, datasets: datasets};
        }
    }

    getTotalPoints() {
        if (!this.answers) {
            return undefined;
        }
        return this.answers.reduce((prev, curr) => curr.points + prev, 0);
    }
}

@NgModule({
    declarations: [AnswerChartComponent],
    imports: [BrowserModule, ChartsModule],
    exports: [AnswerChartComponent],
})
export class AnswerChartModule {}
