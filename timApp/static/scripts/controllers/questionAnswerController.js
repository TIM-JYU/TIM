/**
 * Created by hajoviin on 22.4.2015.
 */

timApp.controller('QuestionAnswerController', ['$scope', '$http', function ($scope) {
    
    $scope.questionHeaders = [];
    $scope.answerTypes = [];
    $scope.dynamicAnswerSheetControl = {};

    $scope.$on("setQuestionJson", function (event, args) {
        $scope.questionId = args.questionId;
        $scope.dynamicAnswerSheetControl.createAnswer()
    });

    $scope.answer = function () {
        $scope.dynamicAnswerSheetControl.answerToQuestion()

    };
}]);


timApp.directive('dynamicAnswerSheet', ['$interval', '$compile', function ($interval, $compile) {
    return {
        restrict: 'E',
        replace: "true",
        scope: {
            control: '='
        },
        transclude: true,
        link: function ($scope, $element) {
            var promise;
            $scope.internalControl = $scope.control || {};
            $scope.internalControl.createAnswer = function () {
                $scope.json = $scope.$parent.askedQuestionJson;
                var htmlSheet = "<div class = 'answerSheet'>";
                if ($scope.json.TYPE != "true-false") {
                    htmlSheet += "<h2>" + $scope.json.QUESTION + "<h2>";
                }
                //if ($scope.json.TIME == "undefined") {
                htmlSheet += "<progress value='0' max='10' id='progressBar'>";
                htmlSheet += "</progress>";
                htmlSheet += "<span id='progressLabel'>10</span>";
                //}

                htmlSheet += "<table>";

                if ($scope.json.TYPE == "true-false") {
                    $scope.json.DATA.HEADERS[0] = {"type": "header", "id": 0, "text": "True"};
                    $scope.json.DATA.HEADERS[1] = {"type": "header", "id": 1, "text": "False"};
                }
                var nextBoolean = true;

                if ($scope.json.DATA.HEADERS.length > 0) {
                    htmlSheet += "<tr>";
                    htmlSheet += "<th></th>";
                    angular.forEach($scope.json.DATA.HEADERS, function (header) {
                        htmlSheet += "<th>" + header.text + "</th>";
                    });
                    htmlSheet += "</tr>";


                }

                if (angular.isDefined($scope.json.DATA.ROWS)) {
                    angular.forEach($scope.json.DATA.ROWS, function (row) {
                        htmlSheet += "<tr>";
                        htmlSheet += "<td>" + row.text + "</td>";
                        var header = 0;
                        //TODO: Needs correct JSON to be made better way
                        angular.forEach(row.COLUMNS, function (column) {
                            var group;

                            if ($scope.json.TYPE == "matrix" || $scope.json.TYPE == "true-false") {
                                group = "group" + row.text.replace(/[^a-zA-Z]/g, "");
                                htmlSheet += "<td><label> <input type='" + column.answerFieldType + "' name='" + group + "'" +
                                " value='" + $scope.json.DATA.HEADERS[header].text + "'" +
                                "></label></td>";
                                header++;
                            } else {
                                group = "group" + row.type.replace(/[^a-zA-Z]/g, "");
                                htmlSheet += "<td><label> <input type='" + column.answerFieldType + "' name='" + group + "'" +
                                " value='" + row.text + "'" +
                                "></label></td>";
                            }
                            nextBoolean = !nextBoolean;
                        });
                        htmlSheet += "</tr>";
                    });
                }

                if (angular.isDefined($scope.json.DATA.COLUMNS)) {
                    var faker = 0;
                    angular.forEach($scope.json.DATA.COLUMNS, function (column) {
                        if (column.Value == "") {
                            column.Value = "Fake value";
                        }

                        // htmlSheet += "<th>" + column.Value + "</th>";
                        //TODO: Needs correct JSON to be made better way
                        //TODO: Should be able to answer by pressign the answer tab
                        angular.forEach(column.ROWS, function (row) {
                            row.Value = row.Type + faker; //TODO: Remove with real data
                            htmlSheet += "<tr>";
                            htmlSheet += "<td>" + row.Type + faker + "</td>";
                            htmlSheet += "<td><label> <input type='radio' name='group" +
                            column.Value.replace(/ /g, '') + "'" +
                            " value='" + row.Type + faker + "'" +
                            "></td></label>";
                            faker++;
                            htmlSheet += "</tr>";
                        });
                    });
                }


                htmlSheet += "</div>";
                $element.append(htmlSheet);
                $compile($scope);
                // var maxTime = $scope.json.TIME * 1000; if seconds
                var fakeTime = 10 * 1000;
                var timeBetween = 100;
                var intervalTimes = fakeTime / timeBetween;
                $scope.valChange = fakeTime / 1000 / intervalTimes;
                $scope.progressElem = $("#progressBar");
                $scope.progressText = $("#progressLabel");
                $scope.start = function () {
                    promise = $interval($scope.internalControl.updateBar, timeBetween, intervalTimes);
                };
                $scope.start();
            };

            $scope.internalControl.updateBar = function () {
                //TODO: Problem with inactive tab.
                $scope.progressElem.attr("value", (parseFloat($scope.progressElem.attr("value")) + $scope.valChange));

                $scope.progressText.text((parseFloat($scope.progressText.text()) - $scope.valChange).toFixed(1));
                if (Math.abs($scope.progressElem.attr("value") - $scope.progressElem.attr("max")) < 0.02) {
                    //$scope.internalControl.answerToQuestion();
                }

            };

            $scope.internalControl.answerToQuestion = function () {
                var answers = [];
                $interval.cancel(promise);
                if (angular.isDefined($scope.json.DATA.ROWS)) {
                    if ($scope.json.TYPE == "matrix" || $scope.json.TYPE == "true-false") {
                        for (var i = 0; i < $scope.json.DATA.ROWS.length; i++) {
                            var groupName;
                            groupName = "group" + $scope.json.DATA.ROWS[i].text.replace(/[^a-zA-Z]/g, '');
                            var answer = [];
                            //noinspection JSJQueryEfficiency
                            var matrixInputs = $('input[name=' + groupName + ']:checked');
                            for (var k = 0; k < matrixInputs.length; k++) {
                                answer.push(matrixInputs[k].value);
                            }
                            if (matrixInputs.length <= 0) {
                                answer.push("undefined")
                            }
                            answers.push(answer);
                        }
                    }
                    else {
                        groupName = "group" + $scope.json.DATA.ROWS[0].type.replace(/[^a-zA-Z]/g, '');
                        var checkedInputs = $('input[name=' + groupName + ']:checked');
                        for (var j = 0; j < checkedInputs.length; j++) {
                            answers.push(checkedInputs[j].value);
                        }

                        if (checkedInputs.length <= 0) {
                            answers.push("undefined")
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
            };
        }
    }
}])
;