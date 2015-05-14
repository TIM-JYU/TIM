/**
 * Created by hajoviin on 6.5.2015.
 */

timApp.controller('QuestionShowAnswerController', ['$scope', '$http', function ($scope) {

    $scope.dynamicAnswerShowControl = {};

    $scope.close = function () {
        $scope.$emit('closeAnswerShow');
        $scope.dynamicAnswerShowControl.close()
    };

    $scope.$on("putAnswers", function (event, args) {
        $scope.dynamicAnswerShowControl.addAnswer(args.answers);
    });

    $scope.$on("createChart", function (event, args) {
        $scope.dynamicAnswerShowControl.createChart(args);
    });

}]);

timApp.directive('dynamicShowAnswerSheet', ['$interval', '$compile', function ($compile) {
    return {
        restrict: 'E',
        replace: "true",
        scope: {
            control: '='
        },
        transclude: true,
        link: function ($scope, $element) {
            $scope.internalControl = $scope.control || {};
            var basicSets = [
                {
                    label: "Answers",
                    fillColor: "rgba(0,220,0,0.2)",
                    strokeColor: "rgba(0,220,0,1)",
                    pointColor: "rgba(0,220,0,1)",
                    pointStrokeColor: "#fff",
                    pointHighlightFill: "#fff",
                    pointHighlightStroke: "rgba(0,220,0,1)",
                    data: []
                },
                {
                    label: "Answers",
                    fillColor: "rgba(220,0,0,0.2)",
                    strokeColor: "rgba(220,0,0,1)",
                    pointColor: "rgba(220,0,0,1)",
                    pointStrokeColor: "#fff",
                    pointHighlightFill: "#fff",
                    pointHighlightStroke: "rgba(220,0,0,1)",
                    data: []
                },

                {
                    label: "Answers",
                    fillColor: "rgba(0,0,220,0.2)",
                    strokeColor: "rgba(0,0,220,1)",
                    pointColor: "rgba(0,0,220,1)",
                    pointStrokeColor: "#fff",
                    pointHighlightFill: "#fff",
                    pointHighlightStroke: "rgba(0,0,220,1)",
                    data: []
                },
                {
                    label: "Answers",
                    fillColor: "rgba(0,220,220,0.2)",
                    strokeColor: "rgba(0,220,220,1)",
                    pointColor: "rgba(0,220,220,1)",
                    pointStrokeColor: "#fff",
                    pointHighlightFill: "#fff",
                    pointHighlightStroke: "rgba(0,220,220,1)",
                    data: []
                },
                {
                    label: "Answers",
                    fillColor: "rgba(220,0,220,0.2)",
                    strokeColor: "rgba(220,0,220,1)",
                    pointColor: "rgba(220,0,220,1)",
                    pointStrokeColor: "#fff",
                    pointHighlightFill: "#fff",
                    pointHighlightStroke: "rgba(0,0,220,1)",
                    data: []
                },
                {
                    label: "Answers",
                    fillColor: "rgba(220,220,220,0.2)",
                    strokeColor: "rgba(220,220,220,1)",
                    pointColor: "rgba(220,220,220,1)",
                    pointStrokeColor: "#fff",
                    pointHighlightFill: "#fff",
                    pointHighlightStroke: "rgba(220,220,220,1)",
                    data: []
                }
            ];


            $scope.internalControl.createChart = function (question) {
                var labels = [];
                var emptyData = [];
                if (angular.isDefined(question.DATA.ROWS)) {
                    angular.forEach(question.DATA.ROWS, function (row) {
                        labels.push(row.text);
                        emptyData.push(0);
                    })
                }

                if (angular.isDefined(question.DATA.COLUMNS)) {
                    angular.forEach(question.DATA.COLUMNS, function (column) {
                        angular.forEach(column.ROWS, function (row) {
                            labels.push(row.Value);
                            emptyData.push(0);
                        });
                    })
                }

                labels.push("No answer");
                emptyData.push(0);

                $scope.ctx = $("#answerChart").get(0).getContext("2d");

                var usedDataSets = [];

                if (question.TYPE == "matrix") {
                    for (var i = 0; i < question.DATA.ROWS[0].COLUMNS.length; i++) {
                        usedDataSets.push(basicSets[i]);
                        usedDataSets[i].data = emptyData;
                    }

                    for (i = 0; i < question.DATA.HEADERS.length; i++) {
                        usedDataSets[i].label = question.DATA.HEADERS[i].text;
                    }
                } else {
                    usedDataSets.push(basicSets[0]);
                    usedDataSets[0].data = emptyData;
                }


                var data = {
                    labels: labels,
                    datasets: usedDataSets
                };

                $scope.answerChart = new Chart($scope.ctx).Bar(data, {
                    multiTooltipTemplate: "<%= datasetLabel %> - <%= value %>"
                });

                $compile($scope);
            };

            $scope.internalControl.addAnswer = function (answer) {
                var answers = answer[0].answer.split("|");
                var datasets = $scope.answerChart.datasets;
                for (var a = 0; a < answers.length; a++) {
                    var singleAnswer = answers[a];

                    if (datasets.length == 1) {
                        for (var b = 0; b < datasets[0].bars.length; b++) {
                            if (datasets[0].bars[b].label == singleAnswer) {
                                datasets[0].bars[b].value += 1;
                            }
                        }
                    } else {
                        for (var d = 0; d < datasets.length; d++) {
                            if (datasets[d].label == singleAnswer) {
                                datasets[d].bars[a].value += 1;
                            }
                        }
                    }

                    if (singleAnswer == "undefined") {
                        var helperBars = $scope.answerChart.datasets[0].bars;
                        helperBars[helperBars.length - 1].value += 1;
                    }
                }

                $scope.answerChart.update();


            };

            $scope.internalControl.close = function () {
                $scope.answerChart.destroy();
                $element.empty();

            }

        }
    }
}])
;
