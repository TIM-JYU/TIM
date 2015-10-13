/**
 * Created by localadmin on 25.5.2015.
 * Directive for dynamic answer sheet. Sheet to answer lecture questions.
 * If preview parameter is used, inputs are disable and there is no progressbar or answer button
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
            $scope.internalControl = $scope.control || {};

            /**
             * Creates question answer/preview form.
             */
            $scope.internalControl.createAnswer = function () {
                $scope.result = $scope.$parent.result;
                $scope.previousAnswer = $scope.$parent.previousAnswer;
                var disabled = '';
                // If showing preview or question result, inputs are disabled
                if ($scope.preview || $scope.result) disabled = ' disabled ';
                $element.empty();
                $scope.json = $scope.$parent.json;

                $scope.answerTable = [];
                // If user has answer to question, create table of answers and select inputs according to it
                if ($scope.previousAnswer) {
                    var single_answers = [];
                    var all_answers = $scope.previousAnswer.split('|');

                    for (var i = 0; i < all_answers.length; i++) {
                        single_answers.push(all_answers[i].split(','))
                    }
                    $scope.answerTable = single_answers;
                }

                $scope.expl = $scope.$parent.expl;
                $scope.askedTime = $scope.$parent.askedTime - $scope.$parent.clockOffset;
                $scope.endTime = $scope.$parent.askedTime + $scope.json.TIMELIMIT * 1000 - $scope.$parent.clockOffset;

                var htmlSheet = $('<div>', {class: 'answerSheet'});
                if ($scope.json.TIMELIMIT !== "" && !$scope.preview && !$scope.result) {
                    htmlSheet.append($('<progress>', {
                        max: ($scope.endTime - $scope.askedTime),
                        id: 'progressBar'
                    }));
                    htmlSheet.append($('<span>', {
                        class: 'progresslabel',
                        id: 'progressLabel',
                        text: $scope.json.TIMELIMIT + " s"
                    }));
                }
                htmlSheet.append($('<h2>', {text: $scope.json.QUESTION}));
                var table = $('<table>', {id: 'answer-sheet-table'});

                if ($scope.json.TYPE === "true-false") {
                    $scope.json.DATA.HEADERS[0] = {"type": "header", "id": 0, "text": "True"};
                    $scope.json.DATA.HEADERS[1] = {"type": "header", "id": 1, "text": "False"};
                }

                if ($scope.json.DATA.HEADERS.length > 0 && !($scope.json.DATA.HEADERS[0].text === "" && $scope.json.DATA.HEADERS.length === 1)) {
                    var tr = $('<tr>');
                    if ($scope.json.DATA.HEADERS.length > 1) {
                        tr.append($('<th>'));
                    }
                    angular.forEach($scope.json.DATA.HEADERS, function (header) {
                        tr.append($('<th>', {class: 'answer-button', text: header.text}));
                    });
                    tr.append($('<th>', {class: 'answer-button'}));
                    table.append(tr);
                }

                angular.forEach($scope.json.DATA.ROWS, function (row) {
                    var tr = $('<tr>');
                    if ($scope.json.TYPE === "matrix" || $scope.json.TYPE === "true-false") {
                        tr.append($('<td>', {text: row.text}));
                    }
                    var header = 0;
                    //TODO: Needs correct JSON to be made better way
                    for (var i = 0; i < row.COLUMNS.length; i++) {
                        var group;
                        if ($scope.json.TYPE === "matrix" || $scope.json.TYPE === "true-false") {
                            if ($scope.json.ANSWERFIELDTYPE === "text") {
                                group = "group" + i;
                                var textArea = $('<textarea>', {
                                    id: 'textarea-answer',
                                    name: group
                                });
                                if (disabled !== '') textArea.attr('disabled', true);
                                if ($scope.json.DATA.HEADERS[0].text === "" && $scope.json.DATA.HEADERS.length === 1 && $scope.json.DATA.ROWS.length === 1) {
                                    textArea.attr('style', 'height:200px');
                                }
                                tr.append($('<td>', {class: 'answer-button'}).append($('<label>').append(textArea)));
                                header++;
                            } else {
                                group = "group" + row.text.replace(/[^a-zA-Z0-9]/g, "");
                                var checked = false;
                                if ($scope.answerTable && $scope.answerTable.length > row.id) {
                                    var value = (row.COLUMNS[i].id + 1).toString();
                                    checked = ($scope.answerTable[row.id].indexOf(value) >= 0);
                                }
                                var input = $('<input>', {
                                    type: $scope.json.ANSWERFIELDTYPE,
                                    name: group,
                                    value: parseInt(row.COLUMNS[i].id) + 1,
                                    checked: checked
                                });
                                if (disabled !== '') input.attr('disabled', true);
                                tr.append($('<td>', {class: 'answer-button'}).append($('<label>').append(input)));
                                header++;
                            }
                        } else {
                            group = "group" + row.type.replace(/[^a-zA-Z0-9]/g, "");
                            var checked = false;
                            if ($scope.answerTable && $scope.answerTable.length > 0) {
                                var value = (row.id + 1).toString();
                                checked = ($scope.answerTable[0].indexOf(value) >= 0);
                            }
                            var input = $('<input>', {
                                type: $scope.json.ANSWERFIELDTYPE,
                                name: group,
                                value: parseInt(row.id) + 1,
                                checked: checked
                            });
                            if (disabled !== '') input.attr('disabled', true);
                            var label = $('<label>');
                            label.append(input).append(row.text);
                            tr.append($('<td>', {class: 'answer-button2'}).append(label));
                        }
                    }
                    // If showing question results, add question rows explanation
                    if ($scope.result && row.id.toString() in $scope.expl) {
                        tr.append($('<td>', {class: 'explanation', text: $scope.expl[row.id.toString()]}));
                    }
                    table.append(tr);
                });

                htmlSheet.append($('<div>').append(table));
                $element.append(htmlSheet);
                $compile($scope);

                if (!$scope.preview && !$scope.result) {
                    var $table = $element.find('#answer-sheet-table');
                    window.setTimeout(function () {
                        var $table = $element.find('#answer-sheet-table');
                        var $input = null;
                        if ($scope.json.ANSWERFIELDTYPE !== "text")
                            $input = $table.find('input:first');
                        else
                            $input = $table.find('textarea:first');
                        $input[0].focus();
                    }, 0);
                    //
                    $table.on('keyup.send', $scope.answerWithEnter);
                    var now = new Date().valueOf();
                    timeLeft = $scope.endTime - now;
                    barFilled = 0;
                    var timeBetween = 100;
                    $scope.progressElem = $("#progressBar");
                    $scope.progressText = $("#progressLabel");
                    $scope.start = function () {
                        promise = $interval($scope.internalControl.updateBar, timeBetween);
                    };
                    $scope.start();
                }
            };


            /**
             * Use time parameter to either close question/points window or extend question end time.
             * If time is null, question/points is closed.
             * Else time is set as questions new end time.
             */
            $scope.$on('update_end_time', function (event, time) {
                if (time !== null) {
                    $scope.endTime = time - $scope.$parent.clockOffset;
                    $scope.progressElem.attr("max", $scope.endTime - $scope.askedTime);
                } else {
                    if (!$scope.$parent.isLecturer) {
                        $interval.cancel(promise);
                        clearInterval(promise);
                        $element.empty();
                        $scope.$emit('closeQuestion');
                    }
                }
            });

            /**
             * Updates progressbar and time left text
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
                        $scope.progressText.text("Time's up");
                    }
                }
            };

            $scope.internalControl.endQuestion = function () {
                clearInterval(promise);
                $interval.cancel(promise);
                var max = $scope.progressElem.attr("max");
                $scope.progressElem.attr("value", max);
                $scope.progressText.text("Time's up");
            };

            $scope.answerWithEnter = function (e) {
                if (e.keyCode === 13) {
                    $('#answer-sheet-table').off('keyup.send');
                    $scope.$parent.answer();
                }
            };

            /**
             * Function to create question answer and send it to server.
             * @memberof module:dynamicAnswerSheet
             */
            $scope.internalControl.answerToQuestion = function () {
                var answers = [];
                if (angular.isDefined($scope.json.DATA.ROWS)) {
                    var groupName = "";
                    if ($scope.json.TYPE === "matrix" || $scope.json.TYPE === "true-false") {
                        for (var i = 0; i < $scope.json.DATA.ROWS.length; i++) {
                            var answer = [];
                            var matrixInputs;
                            groupName = "group" + $scope.json.DATA.ROWS[i].text.replace(/[^a-zA-Z0-9]/g, '');

                            if ($scope.json.ANSWERFIELDTYPE === "text") {
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
                        answers.push([]);
                        groupName = "group" + $scope.json.DATA.ROWS[0].type.replace(/[^a-zA-Z0-9]/g, '');
                        var checkedInputs = $('input[name=' + groupName + ']:checked');
                        for (var j = 0; j < checkedInputs.length; j++) {
                            answers[0].push(checkedInputs[j].value);
                        }

                        if (checkedInputs.length <= 0) {
                            answers[0].push("undefined");
                        }
                    }
                }

                if (angular.isDefined($scope.json.DATA.COLUMNS)) {
                    angular.forEach($scope.json.DATA.COLUMNS, function (column) {
                        var groupName = "group" + column.Value.replace(/ /g, '');
                        answers.push($('input[name=' + groupName + ']:checked').val());
                    });
                }

                if (!$scope.$parent.isLecturer) {
                    $element.empty();
                    $interval.cancel(promise);
                    clearInterval(promise);
                }
                $scope.$emit('answerToQuestion', {answer: answers, askedId: $scope.$parent.askedId});
            };

            /**
             * Closes question window and clears updateBar interval.
             * If user is lecturer, also closes answer chart window.
             * @memberof module:dynamicAnswerSheet
             */
            $scope.internalControl.closeQuestion = function () {
                $interval.cancel(promise);
                clearInterval(promise);
                $element.empty();
                $scope.$emit('closeQuestion');
                if ($scope.$parent.isLecturer) $rootScope.$broadcast('closeAnswerSheetForGood');
            };

            $scope.internalControl.closePreview = function () {
                $element.empty();
            };
        }

    };
}
])
;