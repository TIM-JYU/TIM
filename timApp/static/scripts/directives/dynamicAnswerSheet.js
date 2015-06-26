/**
 * Created by localadmin on 25.5.2015.
 * Directive for dynamic answer sheet. Sheet to answer lecture questions.
 * @module dynamicAnswerSheet
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

var angular;

var timApp = angular.module('timApp');
timApp.directive('dynamicAnswerSheet', ['$interval', '$compile', '$rootScope', function ($interval, $compile, $rootScope) {
    "use strict";
    return {
        restrict: 'E',
        replace: "true",
        scope: {
            control: '='
        },
        transclude: true,
        link: function ($scope, $element) {
            var promise;
            var timeLeft;
            var barFilled;
            $scope.internalControl = $scope.control || {};
            $scope.internalControl.createAnswer = function () {
                $scope.json = $scope.$parent.questionJson;
                var htmlSheet = "<div class = 'answerSheet'>";
                if ($scope.json.TYPE !== "true-false") {
                    htmlSheet += "<h2>" + $scope.json.QUESTION + "<h2>";
                }
                if ($scope.json.TIMELIMIT !== "") {
                    htmlSheet += "<progress value='0' max='" + $scope.json.TIMELIMIT + "' id='progressBar'>";
                    htmlSheet += "</progress>";
                    htmlSheet += "<span id='progressLabel'>" + $scope.json.TIMELIMIT + "</span>";
                }

                htmlSheet += "<table id='answer-sheet-table'>";

                if ($scope.json.TYPE === "true-false") {
                    $scope.json.DATA.HEADERS[0] = {"type": "header", "id": 0, "text": "True"};
                    $scope.json.DATA.HEADERS[1] = {"type": "header", "id": 1, "text": "False"};
                }

                if ($scope.json.DATA.HEADERS.length > 0 && !($scope.json.DATA.HEADERS[0].text === "" && $scope.json.DATA.HEADERS.length === 1)) {
                    htmlSheet += "<tr>";
                    if($scope.json.DATA.HEADERS.length > 1) {
                        htmlSheet += "<th></th>";
                    }
                    angular.forEach($scope.json.DATA.HEADERS, function (header) {
                        htmlSheet += "<th class='answer-button'>" + header.text + "</th>";
                    });
                    htmlSheet += "</tr>";
                }

                angular.forEach($scope.json.DATA.ROWS, function (row) {
                    htmlSheet += "<tr>";
                    if ($scope.json.TYPE === "matrix" || $scope.json.TYPE === "true-false") {
                        if (row.text.length >= 1) {
                            htmlSheet += "<td>" + row.text + "</td>";
                        }
                    }
                    var header = 0;
                    //TODO: Needs correct JSON to be made better way
                    for (var i = 0; i < row.COLUMNS.length; i++) {
                        var group;
                        if ($scope.json.TYPE === "matrix" || $scope.json.TYPE === "true-false") {
                            if (row.COLUMNS[i].answerFieldType === "text") {
                                group = "group" + i;
                                htmlSheet += "<td><label> <textarea id='textarea-answer' name='" + group + "'"
                                if($scope.json.DATA.HEADERS[0].text === "" && $scope.json.DATA.HEADERS.length === 1 && $scope.json.DATA.ROWS.length ===1) {
                                    htmlSheet += "style='height:200px'";
                                }
                                htmlSheet += "></textarea></label></td>";
                                header++;
                            } else {
                                group = "group" + row.text.replace(/[^a-zA-Z0-9]/g, "");
                                htmlSheet += "<td class='answer-button'><label> <input type='" + row.COLUMNS[i].answerFieldType + "' name='" + group + "'" +
                                " value='" + $scope.json.DATA.HEADERS[header].text + "'" +
                                "></label></td>";
                                header++;
                            }
                        } else {
                            group = "group" + row.type.replace(/[^a-zA-Z0-9]/g, "");
                            htmlSheet += "<td class='answer-button'><label> <input type='" + row.COLUMNS[i].answerFieldType + "' name='" + group + "'" +
                            " value='" + row.text + "'" +
                            ">" + row.text + "</label></td>";
                        }
                    }
                    htmlSheet += "</tr>";
                });


                htmlSheet += "</div>";
                $element.append(htmlSheet);
                $compile($scope);

                var fakeTime = $scope.json.TIMELIMIT * 1000;
                timeLeft = $scope.json.TIMELIMIT;
                barFilled = 0;
                var timeBetween = 100;
                var intervalTimes = fakeTime / timeBetween;
                $scope.valChange = fakeTime / 1000 / intervalTimes;
                $scope.progressElem = $("#progressBar");
                $scope.progressText = $("#progressLabel");
                $scope.start = function () {
                    promise = $interval($scope.internalControl.updateBar, timeBetween, intervalTimes);
                };
                $scope.start();

            }
            ;

            /**
             * FILL WITH SUITABLE TEXT
             * @memberof module:dynamicAnswerSheet
             */
            $scope.internalControl.updateBar = function () {
                //TODO: Problem with inactive tab.
                timeLeft -= $scope.valChange;
                barFilled += $scope.valChange;
                $scope.progressElem.attr("value", (barFilled));

                $scope.progressText.text(timeLeft.toFixed(0) + " s");
                if (Math.abs(barFilled - $scope.progressElem.attr("max")) < 0.02) {

                    if (!$scope.$parent.isLecturer) {
                        $scope.internalControl.answerToQuestion();
                    } else {
                        clearInterval(promise);
                        $scope.progressText.text("Time's up");
                    }
                }

            };

            /**
             * FILL WITH SUITABLE TEXT
             * @memberof module:dynamicAnswerSheet
             */
            $scope.internalControl.answerToQuestion = function () {
                var answers = [];
                $interval.cancel(promise);
                if (angular.isDefined($scope.json.DATA.ROWS)) {
                    var groupName = "";
                    if ($scope.json.TYPE === "matrix" || $scope.json.TYPE === "true-false") {
                        for (var i = 0; i < $scope.json.DATA.ROWS.length; i++) {
                            var answer = [];
                            var matrixInputs;
                            groupName = "group" + $scope.json.DATA.ROWS[i].text.replace(/[^a-zA-Z0-9]/g, '');

                            if ($scope.json.DATA.ROWS[0].COLUMNS[0].answerFieldType === "text") {
                                matrixInputs = $('textarea[name=' +"group"+i + ']');
                                for (var c = 0; c < matrixInputs.length; c++) {
                                    answer.push(matrixInputs[c].value);
                                }

                                answers.push(answer);
                                continue;
                            }

                            matrixInputs = $('input[name=' + groupName + ']:checked');

                            for (var k = 0; k < matrixInputs.length; k++) {
                                answer.push(matrixInputs[k].value);
                            }
                            if (matrixInputs.length <= 0) {
                                answer.push("undefined");
                            }
                            answers.push(answer);
                        }
                    }
                    else {
                        groupName = "group" + $scope.json.DATA.ROWS[0].type.replace(/[^a-zA-Z0-9]/g, '');
                        var checkedInputs = $('input[name=' + groupName + ']:checked');
                        for (var j = 0; j < checkedInputs.length; j++) {
                            answers.push(checkedInputs[j].value);
                        }

                        if (checkedInputs.length <= 0) {
                            answers.push("undefined");
                        }
                    }
                }

                if (angular.isDefined($scope.json.DATA.COLUMNS)) {
                    angular.forEach($scope.json.DATA.COLUMNS, function (column) {
                        var groupName = "group" + column.Value.replace(/ /g, '');
                        answers.push($('input[name=' + groupName + ']:checked').val());
                    });
                }

                $element.empty();
                $scope.$emit('answerToQuestion', {answer: answers, questionId: $scope.$parent.questionId});
                clearInterval(promise);
            };

            /**
             * FILL WITH SUITABLE TEXT
             * @memberof module:dynamicAnswerSheet
             */
            $scope.internalControl.closeQuestion = function () {
                clearInterval(promise);
                $element.empty();
                $scope.$emit('closeQuestion');
                $rootScope.$broadcast('closeAnswerSheetForGood');
            };
        }

    };
}
])
;