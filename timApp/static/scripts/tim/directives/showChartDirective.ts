import angular from "angular";
import {ChartData} from "chart.js";
import * as chartmodule from "chart.js";
import $ from "jquery";
import {timApp} from "tim/app";
import {getJsonAnswers} from "tim/directives/dynamicAnswerSheet";
import {lazyLoad} from "../lazyLoad";
import {$compile} from "../ngimport";

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
    return Array.apply(null, new Array(len)).map(function() {return value;});
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

    words.forEach(function(item, index) {
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

timApp.directive("showChartDirective", [function() {
    "use strict";
    if (!qstChartIndex) {
        qstChartIndex = 0;
    }
    return {
        restrict: "E",
        scope: {
            canvas: "@",
            control: "=",
            divresize: "=",
        },
        link($scope: any, $element) {
            $scope.internalControl = $scope.control || {};
            $scope.canvasId = "#" + $scope.canvas || "";
            $scope.isText = false;
            $scope.div = $($element).parent();
            $scope.charts = ["bar", "horizontalBar"];
            $scope.chartIndex = qstChartIndex;
            let canvasw = 400;
            let canvash = 300;

            //TODO: If more than 12 choices this will break. Refactor to better format.
            const basicSets = [
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

            /**
             * FILL WITH SUITABLE TEXT
             * @memberof module:showChartDirective
             * @param question FILL WITH SUITABLE TEXT
             */
            $scope.internalControl.createChart = async function(question) {

                if ($scope.divresize) {
                    canvasw = $scope.div.width();
                    canvash = $scope.div.height();
                }

                $scope.internalControl.close();
                const data = question;
                // $scope.ctx = $($scope.canvasId).get(0).getContext("2d");
                $scope.x = 10;
                $scope.y = 20;
                if (typeof question.answerFieldType !== "undefined" && question.answerFieldType === "text") {
                    $scope.isText = true;
                    $scope.internalControl.isText = true;
                    $scope.div.attr("style", "overflow: auto");
                    return;
                }
                $scope.div.attr("style", "overflow: hidden");
                $scope.isText = false;
                let showLegend = false;
                const labels = [];
                const emptyData = [];
                const backgroundColor = [];
                if (angular.isDefined(data.rows)) {
                    const i = 0;
                    angular.forEach(data.rows, function(row) {
                        const text = qstShortText(row.text);
                        labels.push(text);
                        emptyData.push(0);
                        // backgroundColor.push(basicSets[i++ % basicSets.length].fillColor);
                    });
                }
                backgroundColor.push(basicSets[0].fillColor);

                if (angular.isDefined(data.columns)) {
                    angular.forEach(data.columns, function(column) {
                        angular.forEach(column.rows, function(row) {
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

                function fillValues(dataSets, index) {
                    dataSets.push(basicSets[index % basicSets.length]);
                    dataSets[index].data = emptyData.slice();
                    let color = basicSets[index % basicSets.length].fillColor;
                    dataSets[index].backgroundColor = timFillArray(emptyData.length, color);
                    color = basicSets[index % basicSets.length].strokeColor;
                    dataSets[index].borderColor = timFillArray(emptyData.length, color);
                    dataSets[index].borderWidth = 1;
                }

                if (question.questionType === "true-false" && !question.headers) {
                    question.headers[0] = {type: "header", id: 0, text: "True"};
                    question.headers[1] = {type: "header", id: 1, text: "False"};
                }

                if (question.questionType === "matrix" || question.questionType === "true-false") {
                    for (let i = 0; i < data.rows[0].columns.length; i++) {
                        fillValues(usedDataSets, i);
                    }

                    let i = usedDataSets.length; // for no answer
                    fillValues(usedDataSets, i);
                    usedDataSets[i].label = "No answer";
                    for (i = 0; i < data.headers.length; i++) {
                        usedDataSets[i].label = qstShortText(data.headers[i].text);
                        // if ( i > 0 ) usedDataSets[i].backgroundColor.push(basicSets[i % basicSets.length].fillColor);
                        if (i > 0) {
                            showLegend = true;
                        }
                    }
                } else {
                    fillValues(usedDataSets, 0);
                }

                const bardata: ChartData = {
                    labels,
                    datasets: usedDataSets,
                };
                console.log(usedDataSets);

                /*
                var ctx  = $scope.ctx;
                ctx.canvas.width = 300;
                ctx.canvas.height = 300;
                ctx.canvas.style.width = "300px";
                ctx.canvas.style.height = "300px";
                */
                /*
                                $scope.answerChart = new Chart($scope.ctx).Bar(bardata, {animation: false} ,{
                                    multiTooltipTemplate: "<%= datasetLabel %> - <%= fvalue %>"
                                });
                                // experiment with: https://jsfiddle.net/4r26box7/85/
                */
                $scope.intScale = function(value, axis) {
                    if (axis == $scope.chartIndex) {
                        return value;
                    }
                    if (value % 1 === 0) {
                        return value;
                    }
                };

                $scope.chartConfig = {
                    type: "horizontalBar",
                    //type: 'bar',
                    // type: 'pie',
                    data: bardata,
                    options: {
                        responsive: false,
                        maintainAspectRatio: true,
                        animation: false,
                        multiTooltipTemplate: "<%= datasetLabel %> - <%= fvalue %>",
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
                                    beginAtZero: true,
                                    callback(value) {return $scope.intScale(value, 0);},
                                    // stepSize: 1
                                },
                            }],
                            yAxes: [{
                                //stacked: true
                                ticks: {
                                    min: 0,
                                    beginAtZero: true,
                                    callback(value) {return $scope.intScale(value, 1);},
                                    // stepSize: 1
                                },
                            }],
                        },

                    },
                };

                await $scope.changeType();
                //$scope.answerChart.options.animation = false;
                $compile($scope as any); // TODO this seems wrong
            };

            $scope.internalControl.toggle = function() {
                qstChartIndex = timGetLSIntValue("qstChartIndex", 0);
                if (qstChartIndex != $scope.chartIndex) {
                    qstChartIndex = $scope.chartIndex;
                }
                qstChartIndex = (qstChartIndex + 1) % $scope.charts.length;
                $scope.chartIndex = qstChartIndex;
                window.localStorage.setItem("qstChartIndex", qstChartIndex.toString());
                $scope.changeType();
            };

            $scope.changeType = async function() {
                if ($scope.isText) {
                    return;
                }
                const newType = $scope.charts[qstChartIndex];
                if ($scope.answerChart) {
                    $scope.answerChart.destroy();
                }
                if (!$scope.ctx) {
                    $scope.ctx = $("<canvas id=" + $scope.canvasId.substring(1) + ' width="' + canvasw + '" height="' + canvash + '"><canvas>');
                    $scope.div.append($scope.ctx);
                    // var zoom = $('<a ng-click="zoom()">[+]</a>');
                    // $scope.div.append(zoom);
                }
                const config = jQuery.extend(true, {}, $scope.chartConfig);
                config.type = newType;
                const Chart = (await lazyLoad<typeof chartmodule>("chart.js")).Chart;
                $scope.answerChart = new Chart($scope.ctx, config);
                /*
                var legend = $scope.answerChart.generateLegend();
                var legenddiv = $('<div class="chart-legend">' + legend + '</div>');
                $scope.div.append(legenddiv);
                */
            };

            $scope.internalControl.resize = function(w, h) {
                canvasw = w;
                if (canvasw < 100) {
                    canvasw = 100;
                }
                canvash = h;
                if (canvash < 100) {
                    canvash = 100;
                }
                $scope.internalControl.close();
                $scope.changeType();
            };

            $scope.internalControl.zoom = function(w, h) {
                canvasw += w * 100;
                if (canvasw < 100) {
                    canvasw = 100;
                }
                canvash += h * 100;
                if (canvash < 100) {
                    canvash = 100;
                }
                $scope.internalControl.close();
                $scope.changeType();
            };

            $scope.internalControl.resizeDiv = function() {
                const w = $scope.div.width();
                const h = $scope.div.height();
                $scope.internalControl.resize(w, h);
            };

            $scope.$on("resizeElement", function(event, data) {
                // var w = $scope.div.width();  // data.size.width;
                // var h = $scope.div.height(); //  data.size.height)
                if ($scope.isText) {
                    return;
                }
                $scope.internalControl.resizeDiv();
            });

            /**
             * FILL WITH SUITABLE TEXT
             * @memberof module:showChartDirective
             * @param answers FILL WITH SUITABLE TEXT
             */
            $scope.internalControl.addAnswer = function(answers) {
                if (!angular.isDefined(answers)) {
                    return;
                }
                // $scope.ctx.font = "20px Georgia";
                let datasets;
                if (!$scope.isText) {
                    datasets = $scope.answerChart.data.datasets;
                }
                for (let answerersIndex = 0; answerersIndex < answers.length; answerersIndex++) {
                    const answ = answers[answerersIndex].answer;

                    const onePersonAnswers = getJsonAnswers(answ);
                    for (let a = 0; a < onePersonAnswers.length; a++) {
                        const singleAnswers = onePersonAnswers[a];
                        for (let sa = 0; sa < singleAnswers.length; sa++) {
                            const singleAnswer = singleAnswers[sa];

                            if ($scope.isText) {
                                // $scope.ctx.fillText(singleAnswer, $scope.x, $scope.y);
                                // $scope.y += 20;
                                const t = $("<p>" + singleAnswer + "</p>");
                                $scope.div.append(t);
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

                $scope.chartConfig.data.datasets = datasets;

                if (!$scope.isText) {
                    $scope.answerChart.update();
                }
            };

            /**
             * FILL WITH SUITABLE TEXT
             * @memberof module:showChartDirective
             */
            $scope.internalControl.close = function() {
                /*
                $scope.ctx.clearRect(0, 0, $($scope.canvasId)[0].width, $($scope.canvasId)[0].height);
                if (typeof $scope.answerChart !== "undefined") {
                    $scope.answerChart.destroy();
                }
                $($scope.canvasId).remove(); // this is my <canvas> element
                $('#chartDiv').append('<canvas id=' + $scope.canvasId.substring(1) + ' width="400" height="300"><canvas>');
                */
                if ($scope.answerChart && typeof $scope.answerChart !== "undefined") {
                    $scope.answerChart.destroy();
                }
                $scope.ctx = null;
                $scope.answerChart = null;
                //$element.empty();
                $scope.div.empty();
            };
        },
    };
}]);
