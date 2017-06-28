import angular from "angular";
import {ChartData} from "chart.js";
import * as chartmodule from "chart.js";
import $ from "jquery";
import {timApp} from "tim/app";
import {getJsonAnswers} from "tim/directives/dynamicAnswerSheet";
import {lazyLoad} from "../lazyLoad";
import {$compile, $log} from "../ngimport";

/**
 * Created by hajoviin on 13.5.2015.
 * FILL WITH SUITABLE TEXT
 * @module showChartDirective
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

function timStripHtml(s) {
    s = s.replace(/<[^>]*>?/gm, "");  // problem: <img src=http://www.google.com.kh/images/srpr/nav_logo27.png onload="alert(42)" >
    s = s.replace("\\(", "");
    s = s.replace("\\)", "");
    return s;
}

function timFillArray(len, value) {
    return Array.apply(null, new Array(len)).map(() => value);
}

function qstCleanHtml(s) {
    s = timStripHtml(s);
    return s.replace("&amp;", "&").trim();
}

/* takes a string phrase and breaks it into separate phrases
 no bigger than 'maxwidth', breaks are made at complete words.*/

function qstFormatLabel(str, maxwidth, maxrows) {
    if (str.length <= maxwidth) {
        return str;
    }
    const sections = [];
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
                if (index == (words.length - 1)) {
                    sections.push(concat);
                    return;
                } else {
                    temp = concat;
                    return;
                }
            }
        }

        if (index == (words.length - 1)) {
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

function qstShortText(s) {
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
        max = parseInt(parts[2]);
        if (isNaN(max)) {
            max = 25;
        }
    }
    let maxrows = 3;
    if (parts.length >= 4) {
        maxrows = parseInt(parts[3]);
        if (isNaN(maxrows)) {
            maxrows = 3;
        }
    }
    text = qstFormatLabel(text, max, maxrows);
    if (text.length > max) {
        text = text.substring(0, max - 1) + "...";
    }
    return text;
}

function timGetLSIntValue(key, def: number): number {
    let val: any = window.localStorage.getItem(key);
    if (val === undefined) {
        val = def;
    }
    val = parseInt(val);
    if (isNaN(val)) {
        val = def;
    }
    return val;
}

let qstChartIndex = timGetLSIntValue("qstChartIndex", 0);

class ShowChartController {
    private static $inject = ["$scope", "$element"];
    private canvasId: string;
    private canvas: string;
    private isText: boolean;
    private div: JQuery;
    private charts: [string, string];
    private chartIndex: number;
    private canvasw = 400;
    private canvash = 300;
    private divresize: boolean;
    private x: number;
    private y: number;
    private chartConfig: Chart.ChartConfiguration;
    private answerChart: Chart;
    private ctx: JQuery;

    //TODO: If more than 12 choices this will break. Refactor to better format.
    private basicSets = [
        {
            label: "", // "Answer",
            fillColor: "rgba(0,220,0,0.2)",
            strokeColor: "rgba(0,220,0,1)",
            pointColor: "rgba(0,220,0,1)",
            pointStrokeColor: "#fff",
            pointHighlightFill: "#fff",
            pointHighlightStroke: "rgba(0,220,0,1)",
            data: [],
        },
        {
            label: "Answers",
            fillColor: "rgba(220,0,0,0.2)",
            strokeColor: "rgba(220,0,0,1)",
            pointColor: "rgba(220,0,0,1)",
            pointStrokeColor: "#fff",
            pointHighlightFill: "#fff",
            pointHighlightStroke: "rgba(220,0,0,1)",
            data: [],
        },

        {
            label: "Answers",
            fillColor: "rgba(0,0,220,0.2)",
            strokeColor: "rgba(0,0,220,1)",
            pointColor: "rgba(0,0,220,1)",
            pointStrokeColor: "#fff",
            pointHighlightFill: "#fff",
            pointHighlightStroke: "rgba(0,0,220,1)",
            data: [],
        },
        {
            label: "Answers",
            fillColor: "rgba(0,220,220,0.2)",
            strokeColor: "rgba(0,220,220,1)",
            pointColor: "rgba(0,220,220,1)",
            pointStrokeColor: "#fff",
            pointHighlightFill: "#fff",
            pointHighlightStroke: "rgba(0,220,220,1)",
            data: [],
        },
        {
            label: "Answers",
            fillColor: "rgba(220,0,220,0.2)",
            strokeColor: "rgba(220,0,220,1)",
            pointColor: "rgba(220,0,220,1)",
            pointStrokeColor: "#fff",
            pointHighlightFill: "#fff",
            pointHighlightStroke: "rgba(0,0,220,1)",
            data: [],
        },
        {
            label: "Answers",
            fillColor: "rgba(220,220,220,0.2)",
            strokeColor: "rgba(220,220,220,1)",
            pointColor: "rgba(220,220,220,1)",
            pointStrokeColor: "#fff",
            pointHighlightFill: "#fff",
            pointHighlightStroke: "rgba(220,220,220,1)",
            data: [],
        },
        {
            label: "Answers",
            fillColor: "rgba(165,220,0,0.2)",
            strokeColor: "rgba(165,220,0,1)",
            pointColor: "rgba(165,220,0,1)",
            pointStrokeColor: "#fff",
            pointHighlightFill: "#fff",
            pointHighlightStroke: "rgba(165,220,0,1)",
            data: [],
        },
        {
            label: "Answers",
            fillColor: "rgba(220,165,0,0.2)",
            strokeColor: "rgba(220,165,0,1)",
            pointColor: "rgba(220,165,0,1)",
            pointStrokeColor: "#fff",
            pointHighlightFill: "#fff",
            pointHighlightStroke: "rgba(220,165,0,1)",
            data: [],
        },
        {
            label: "Answers",
            fillColor: "rgba(0,165,220,0.2)",
            strokeColor: "rgba(220,165,0,1)",
            pointColor: "rgba(220,165,0,1)",
            pointStrokeColor: "#fff",
            pointHighlightFill: "#fff",
            pointHighlightStroke: "rgba(220,165,0,1)",
            data: [],
        },
        {
            label: "Answers",
            fillColor: "rgba(220,0,165,0.2)",
            strokeColor: "rgba(220,0,165,1)",
            pointColor: "rgba(220,0,165,1)",
            pointStrokeColor: "#fff",
            pointHighlightFill: "#fff",
            pointHighlightStroke: "rgba(220,0,165,1)",
            data: [],
        },
        {
            label: "Answers",
            fillColor: "rgba(30,0,75,0.2)",
            strokeColor: "rgba(30,0,75,1)",
            pointColor: "rgba(30,0,75,1)",
            pointStrokeColor: "#fff",
            pointHighlightFill: "#fff",
            pointHighlightStroke: "rgba(30,0,75,1)",
            data: [],
        },
        {
            label: "Answers",
            fillColor: "rgba(75,75,180,0.2)",
            strokeColor: "rgba(75,75,180,1)",
            pointColor: "rgba(75,75,180,1)",
            pointStrokeColor: "#fff",
            pointHighlightFill: "#fff",
            pointHighlightStroke: "rgba(75,75,180,1)",
            data: [],
        },
    ];

    constructor(scope, element) {
        this.canvasId = "#" + this.canvas || "";
        this.isText = false;
        this.div = $(element).parent();
        this.charts = ["bar", "horizontalBar"];
        this.chartIndex = qstChartIndex;
        scope.$on("resizeElement", (event, data) => {
            // var w = this.div.width();  // data.size.width;
            // var h = this.div.height(); //  data.size.height)
            if (this.isText) {
                return;
            }
            this.resizeDiv();
        });
    }

    intScale(value, axis) {
        if (axis == this.chartIndex) {
            return value;
        }
        if (value % 1 === 0) {
            return value;
        }
    }

    fillValues(emptyData: any[], dataSets, index: number) {
        dataSets.push(this.basicSets[index % this.basicSets.length]);
        dataSets[index].data = emptyData.slice();
        let color = this.basicSets[index % this.basicSets.length].fillColor;
        dataSets[index].backgroundColor = timFillArray(emptyData.length, color);
        color = this.basicSets[index % this.basicSets.length].strokeColor;
        dataSets[index].borderColor = timFillArray(emptyData.length, color);
        dataSets[index].borderWidth = 1;
    }

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:showChartDirective
     * @param question FILL WITH SUITABLE TEXT
     */
    async createChart(question) {

        if (this.divresize) {
            this.canvasw = this.div.width();
            this.canvash = this.div.height();
        }

        this.close();
        const data = question;
        // this.ctx = $(this.canvasId).get(0).getContext("2d");
        this.x = 10;
        this.y = 20;
        if (typeof question.answerFieldType !== "undefined" && question.answerFieldType === "text") {
            this.isText = true;
            this.isText = true;
            this.div.attr("style", "overflow: auto");
            return;
        }
        this.div.attr("style", "overflow: hidden");
        this.isText = false;
        let showLegend = false;
        const labels = [];
        const emptyData = [];
        const backgroundColor = [];
        if (angular.isDefined(data.rows)) {
            const i = 0;
            angular.forEach(data.rows, row => {
                const text = qstShortText(row.text);
                labels.push(text);
                emptyData.push(0);
                // backgroundColor.push(basicSets[i++ % basicSets.length].fillColor);
            });
        }
        backgroundColor.push(this.basicSets[0].fillColor);

        if (angular.isDefined(data.columns)) {
            angular.forEach(data.columns, column => {
                angular.forEach(column.rows, row => {
                    labels.push(qstShortText(row.Value));
                    emptyData.push(0);
                });
            });
        }

        if (!(question.questionType === "matrix" || question.questionType === "true-false")) {
            labels.push("No answer");
            emptyData.push(0);
        }

        const usedDataSets = [];

        if (question.questionType === "true-false" && !question.headers) {
            question.headers[0] = {type: "header", id: 0, text: "True"};
            question.headers[1] = {type: "header", id: 1, text: "False"};
        }

        if (question.questionType === "matrix" || question.questionType === "true-false") {
            for (let i = 0; i < data.rows[0].columns.length; i++) {
                this.fillValues(emptyData, usedDataSets, i);
            }

            let i = usedDataSets.length; // for no answer
            this.fillValues(emptyData, usedDataSets, i);
            usedDataSets[i].label = "No answer";
            for (i = 0; i < data.headers.length; i++) {
                usedDataSets[i].label = qstShortText(data.headers[i].text);
                // if ( i > 0 ) usedDataSets[i].backgroundColor.push(basicSets[i % basicSets.length].fillColor);
                if (i > 0) {
                    showLegend = true;
                }
            }
        } else {
            this.fillValues(emptyData, usedDataSets, 0);
        }

        const bardata: ChartData = {
            labels,
            datasets: usedDataSets,
        };
        $log.info(usedDataSets);
        /*
         var ctx  = this.ctx;
         ctx.canvas.width = 300;
         ctx.canvas.height = 300;
         ctx.canvas.style.width = "300px";
         ctx.canvas.style.height = "300px";
         */
        /*
         this.answerChart = new Chart(this.ctx).Bar(bardata, {animation: false} ,{
         multiTooltipTemplate: "<%= datasetLabel %> - <%= fvalue %>"
         });
         // experiment with: https://jsfiddle.net/4r26box7/85/
         */

        this.chartConfig = {
            type: "horizontalBar",
            //type: 'bar',
            // type: 'pie',
            data: bardata,
            options: {
                responsive: false
                ,
                maintainAspectRatio: true,
                animation: false,
                // multiTooltipTemplate: "<%= datasetLabel %> - <%= fvalue %>",
                legend: {
                    display: showLegend,
                    labels: {
                        // fontColor: 'rgb(255, 99, 132)'
                    },
                },
                scales: {
                    xAxes: [{
                        //stacked: true,
                        ticks: {
                            min: 0,
                            // beginAtZero: true,
                            callback(value) {
                                return this.intScale(value, 0);
                            },
                            // stepSize: 1
                        },
                    }],
                    yAxes: [{
                        //stacked: true
                        ticks: {
                            min: 0,
                            // beginAtZero: true,
                            callback(value) {
                                return this.intScale(value, 1);
                            },
                            // stepSize: 1
                        },
                    }],
                },

            },
        };

        await this.changeType();
        //this.answerChart.options.animation = false;
        $compile(this as any);
        // TODO this seems wrong
    }

    toggle() {
        qstChartIndex = timGetLSIntValue("qstChartIndex", 0);
        if (qstChartIndex != this.chartIndex) {
            qstChartIndex = this.chartIndex;
        }
        qstChartIndex = (qstChartIndex + 1) % this.charts.length;
        this.chartIndex = qstChartIndex;
        window.localStorage.setItem("qstChartIndex", qstChartIndex.toString());
        this.changeType();
    }

    async changeType() {
        if (
            this.isText) {
            return;
        }
        const newType = this.charts[qstChartIndex];
        if (this.answerChart) {
            this.answerChart.destroy();
        }
        if (!this.ctx) {
            this.ctx = $("<canvas id=" + this.canvasId.substring(1) + ' width="' + this.canvasw + '" height="' + this.canvash + '"><canvas>');
            this.div.append(this.ctx);
            // var zoom = $('<a ng-click="zoom()">[+]</a>');
            // this.div.append(zoom);
        }
        const config = jQuery.extend(true, {},
            this.chartConfig);
        config.type = newType;
        const Chart = (await lazyLoad<typeof chartmodule>(
                "chart.js")
        ).Chart;
        this.answerChart = new Chart(this.ctx, config);
        /*
         var legend = this.answerChart.generateLegend();
         var legenddiv = $('<div class="chart-legend">' + legend + '</div>');
         this.div.append(legenddiv);
         */
    }

    resize(w, h) {
        this.canvasw = w;
        if (this.canvasw < 100) {
            this.canvasw = 100;
        }
        this.canvash = h;
        if (this.canvash < 100) {
            this.canvash = 100;
        }
        this.close();
        this.changeType();
    }

    zoom(w, h) {
        this.canvasw += w * 100;
        if (this.canvasw < 100) {
            this.canvasw = 100;
        }
        this.canvash += h * 100;
        if (this.canvash < 100) {
            this.canvash = 100;
        }
        this.close();
        this.changeType();
    }

    resizeDiv() {
        const w = this.div.width();
        const h = this.div.height();
        this.resize(w, h);
    }

    /**
     * FILL WITH SUITABLE TEXT

     * @memberof module:showChartDirective
     * @param answers FILL WITH SUITABLE TEXT
     */
    addAnswer(answers) {
        if (!angular.isDefined(answers)) {
            return;
        }
        // this.ctx.font = "20px Georgia";
        let datasets;
        if (!this.isText) {
            datasets = this.answerChart.data.datasets;
        }
        for (let answerersIndex = 0; answerersIndex < answers.length; answerersIndex++) {
            const answ = answers[answerersIndex].answer;

            const onePersonAnswers = getJsonAnswers(answ);
            for (let a = 0; a < onePersonAnswers.length; a++) {
                const
                    singleAnswers =
                        onePersonAnswers[a];
                for (let sa = 0; sa < singleAnswers.length; sa++) {
                    const singleAnswer = singleAnswers[sa];

                    if (this.isText) {
                        // this.ctx.fillText(singleAnswer, this.x, this.y);
                        // this.y += 20;
                        const t = $("<p>" + singleAnswer + "</p>");
                        this.div.append(t);
                        continue;
                    }
                    if (datasets.length === 1) {
                        let answered = false;
                        for (let b = 0; b < datasets[0].data.length; b++) {
                            if ((b + 1) === parseInt(singleAnswer)) {
                                datasets[0].data[b] += 1;
                                answered = true;
                            }
                        }
                        if (!answered) {
                            datasets[0].data[datasets[0].data.length - 1] += 1;
                        }
                    } else {
                        let answered = false;
                        for (let d = 0; d < datasets.length; d++) {
                            if ((d + 1) === parseInt(singleAnswer)) {
                                datasets[d].data[a] += 1;
                                answered = true;
                                break;
                            }
                        }
                        if (!answered) {
                            datasets[datasets.length - 1].data[a] += 1;
                        }
                    }
                }
            }
        }

        this.chartConfig.data.datasets = datasets;

        if (!this.isText) {
            this.answerChart.update();
        }
    }

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:showChartDirective
     */
    close() {
        /*
         this.ctx.clearRect(0, 0, $(this.canvasId)[0].width, $(this.canvasId)[0].height);
         if (typeof this.answerChart !== "undefined") {
         this.answerChart.destroy();
         }
         $(this.canvasId).remove(); // this is my <canvas> element
         $('#chartDiv').append('<canvas id=' + this.canvasId.substring(1) + ' width="400" height="300"><canvas>');
         */
        if (this.answerChart && typeof this.answerChart !== "undefined") {
            this.answerChart.destroy();
        }
        this.ctx = null;
        this.answerChart = null;
        //$element.empty();
        this.div.empty();
    }
}

timApp.component("showChartDirective", {
    bindings: {
        canvas: "@",
        control: "=",
        divresize: "=",
    },
    controller: ShowChartController,
});
