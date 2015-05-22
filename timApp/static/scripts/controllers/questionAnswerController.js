/**
 * Created by hajoviin on 22.4.2015.
 */

timApp.controller('QuestionAnswerController', ['$scope', '$http', function ($scope) {

    $scope.questionHeaders = [];
    $scope.answerTypes = [];
    $scope.dynamicAnswerSheetControl = {};
    $scope.isLecturer = false;

    $scope.$on("setQuestionJson", function (event, args) {
        $scope.questionId = args.questionId;
        $scope.isLecturer = args.isLecturer;
        $scope.questionJson = args.questionJson;

        $scope.dynamicAnswerSheetControl.createAnswer();
    });

    $scope.answer = function () {
        $scope.dynamicAnswerSheetControl.answerToQuestion()

    };

    $scope.close = function () {
        $scope.dynamicAnswerSheetControl.closeQuestion();
    }
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
            var timeLeft;
            var barFilled;
            $scope.internalControl = $scope.control || {};
            $scope.internalControl.createAnswer = function () {
                $scope.json = $scope.$parent.questionJson;
                var htmlSheet = "<div class = 'answerSheet'>";
                if ($scope.json.TYPE != "true-false") {
                    htmlSheet += "<h2>" + $scope.json.QUESTION + "<h2>";
                }
                if ($scope.json.TIMELIMIT != "") {
                    htmlSheet += "<progress value='0' max='" + $scope.json.TIMELIMIT + "' id='progressBar'>";
                    htmlSheet += "</progress>";
                    htmlSheet += "<span id='progressLabel'>" + $scope.json.TIMELIMIT + "</span>";
                }

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

                angular.forEach($scope.json.DATA.ROWS, function (row) {
                    htmlSheet += "<tr>";
                    if ($scope.json.TYPE == "matrix" || $scope.json.TYPE == "true-false") {
                        htmlSheet += "<td>" + row.text + "</td>";
                    }
                    var header = 0;
                    //TODO: Needs correct JSON to be made better way
                    angular.forEach(row.COLUMNS, function (column) {
                        var group;

                        if ($scope.json.TYPE == "matrix" || $scope.json.TYPE == "true-false") {
                            group = "group" + row.text.replace(/[^a-zA-Z0-9]/g, "");
                            htmlSheet += "<td><label> <input type='" + column.answerFieldType + "' name='" + group + "'" +
                            " value='" + $scope.json.DATA.HEADERS[header].text + "'" +
                            "></label></td>";
                            header++;
                        } else {
                            group = "group" + row.type.replace(/[^a-zA-Z]/g, "");
                            htmlSheet += "<td><label> <input type='" + column.answerFieldType + "' name='" + group + "'" +
                            " value='" + row.text + "'" +
                            ">" + row.text + "</label></td>";
                        }
                        nextBoolean = !nextBoolean;
                    });
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

            $scope.internalControl.closeQuestion = function () {
                $element.empty();
                $scope.$emit('closeQuestion');
            };
        }

    }
}
])
;