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
timApp.directive('dynamicAnswerSheet', ['$interval', '$compile', '$rootScope', '$http', function ($interval, $compile, $rootScope, $http) {
    "use strict";
    return {
        restrict: 'E',
        replace: "true",
        scope: {
            control: '=',
            preview: '@'
        },
        transclude: true,
        link: function ($scope, $element) {
            var promise;
            var timeLeft;
            var barFilled;
            var disabled = '';
            if ($scope.preview) disabled = ' disabled ';
            $scope.internalControl = $scope.control || {};

            $scope.internalControl.createAnswer = function () {
                $scope.json = $scope.$parent.questionJson;
                $scope.askedTime = $scope.$parent.askedTime - $scope.$parent.clockOffset;
                $scope.endTime = $scope.$parent.askedTime + $scope.json.TIMELIMIT * 1000 - $scope.$parent.clockOffset;
                var htmlSheet = "<div class = 'answerSheet'>";

                if ($scope.json.TIMELIMIT !== "" && !$scope.preview) {
                    htmlSheet += "<progress value='0' max='" + ($scope.endTime - $scope.askedTime) + "' id='progressBar'>";
                    htmlSheet += "</progress>";
                    htmlSheet += "<span class='progresslabel' id='progressLabel'>" + $scope.json.TIMELIMIT + " s</span>";
                }

                htmlSheet += "<h2>" + $scope.json.QUESTION + "</h2>";
                /*
                 if ($scope.json.TYPE !== "true-false") {
                 htmlSheet += "<h2>" + $scope.json.QUESTION + "</h2>";
                 }*/


                htmlSheet += "<div>";
                htmlSheet += "<table id='answer-sheet-table'>";

                if ($scope.json.TYPE === "true-false") {
                    $scope.json.DATA.HEADERS[0] = {"type": "header", "id": 0, "text": "True"};
                    $scope.json.DATA.HEADERS[1] = {"type": "header", "id": 1, "text": "False"};
                }

                if ($scope.json.DATA.HEADERS.length > 0 && !($scope.json.DATA.HEADERS[0].text === "" && $scope.json.DATA.HEADERS.length === 1)) {
                    htmlSheet += "<tr>";
                    if ($scope.json.DATA.HEADERS.length > 1) {
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
                            if ($scope.json.ANSWERFIELDTYPE === "text") {
                                group = "group" + i;
                                htmlSheet += "<td><label> <textarea id='textarea-answer' name='" + group + "'" + disabled;
                                if ($scope.json.DATA.HEADERS[0].text === "" && $scope.json.DATA.HEADERS.length === 1 && $scope.json.DATA.ROWS.length === 1) {
                                    htmlSheet += "style='height:200px'";
                                }
                                htmlSheet += "></textarea></label></td>";
                                header++;
                            } else {
                                group = "group" + row.text.replace(/[^a-zA-Z0-9]/g, "");
                                htmlSheet += "<td class='answer-button'><label> <input type='" + $scope.json.ANSWERFIELDTYPE + "' name='" + group + "'" +
                                    disabled + " value='" + (parseInt(row.COLUMNS[i].id) + 1) + "'" +
                                    "></label></td>";
                                header++;
                            }
                        } else {
                            group = "group" + row.type.replace(/[^a-zA-Z0-9]/g, "");
                            htmlSheet += "<td class='answer-button2'><label> <input type='" + $scope.json.ANSWERFIELDTYPE + "' name='" + group + "'" +
                                disabled + " value='" + (parseInt(row.id) + 1) + "'" +
                                ">" + row.text + "</label></td>";
                        }
                    }
                    htmlSheet += "</tr>";
                });


                htmlSheet += "</div>";
                $element.append(htmlSheet);
                $compile($scope);

                if (!$scope.preview) {
                    var now = new Date().valueOf();
                    var fakeTime = $scope.endTime - now;
                    timeLeft = $scope.endTime - now;
                    barFilled = 0;
                    var timeBetween = 100;
                    var intervalTimes = fakeTime / timeBetween;
                    $scope.progressElem = $("#progressBar");
                    $scope.progressText = $("#progressLabel");
                    $scope.start = function () {
                        //$scope.getTimeLeft();
                        promise = $interval($scope.internalControl.updateBar, timeBetween);
                    };
                    $scope.start();
                    $scope.internalControl.getExtendTime();
                }
            };


            $scope.internalControl.getExtendTime = function () {
                $http({
                    url: '/getExtendQuestion',
                    type: 'GET',
                    params: {
                        'question_id': $scope.$parent.questionId,
                        'lecture_id': $scope.$parent.lectureId,
                        'buster': new Date().getTime()
                    }
                }).success(function (answer) {
                    $scope.endTime = answer - $scope.$parent.clockOffset;
                    $scope.progressElem.attr("max", $scope.endTime - $scope.askedTime);
                    $scope.internalControl.getExtendTime();
                }).error(function () {
                    console.log("Couldn't get extend time");
                });
            };


            /**
             * FILL WITH SUITABLE TEXT
             * @memberof module:dynamicAnswerSheet
             */
            $scope.internalControl.updateBar = function () {
                //TODO: Problem with inactive tab.
                var now = new Date().valueOf();
                timeLeft = $scope.endTime - now;
                barFilled = ($scope.endTime - $scope.askedTime) - ($scope.endTime - now);
                $scope.progressElem.attr("value", (barFilled));
                $scope.progressText.text(Math.max((timeLeft / 1000), 0).toFixed(0) + " s");
                if (barFilled >= $scope.progressElem.attr("max")) {
                    $scope.$parent.questionEnded = true;
                    clearInterval(promise);
                    $interval.cancel(promise);
                    if (!$scope.$parent.isLecturer) {
                        $scope.internalControl.answerToQuestion();
                    } else {
                        $scope.$parent.questionEnded = true;
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

                            if ($scope.json.ANSWERFIELDTYPE=== "text") {
                                matrixInputs = $('textarea[name=' + "group" + i + ']');
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
                $interval.cancel(promise);
                clearInterval(promise);
                $element.empty();
                $scope.$emit('closeQuestion');
                $rootScope.$broadcast('closeAnswerSheetForGood');
            };

            $scope.internalControl.closePreview = function () {
                $element.empty();
            };
        }

    };
}
])
;