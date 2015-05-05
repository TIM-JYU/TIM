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
                htmlSheet += "<h2> Fake Title <h2>";
                //if ($scope.json.TIME == "undefined") {
                htmlSheet += "<progress value='0' max='10' id='progressBar'>";
                htmlSheet += "</progress>";
                htmlSheet += "<span id='progressLabel'>10</span>"
                //}

                htmlSheet += "<table>";
                htmlSheet += "<tr>";
                if ($scope.json.TYPE == "true-false") {
                    htmlSheet += "<th> </th>";
                    htmlSheet += "<th>True</th>";
                    htmlSheet += "<th>False</th>";
                }
                htmlSheet += "</tr>";
                var nextBoolean = true;
                angular.forEach($scope.json.DATA.ROWS, function (row) {
                    htmlSheet += "<tr>";
                    htmlSheet += "<th>" + row.Value + "</th>";
                    //TODO: Needs correct JSON to be made better way
                    angular.forEach(row.Columns, function () {
                        htmlSheet += "<th><label> <input type='radio' name='group" +
                        row.Value.replace(/ /g, '') + "'" +
                        " value='" + nextBoolean + "'" +
                        "></label></th>";
                        nextBoolean = !nextBoolean;
                    });
                    htmlSheet += "</tr>";

                });
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
                $scope.progressElem.attr("value", (parseFloat($scope.progressElem.attr("value")) + $scope.valChange));

                $scope.progressText.text((parseFloat($scope.progressText.text()) - $scope.valChange).toFixed(1));
                if (Math.abs($scope.progressElem.attr("value") - $scope.progressElem.attr("max")) < 0.02) {
                    $scope.internalControl.answerToQuestion();
                }

            };

            $scope.internalControl.answerToQuestion = function () {
                var answers = [];
                $interval.cancel(promise);
                angular.forEach($scope.json.DATA.ROWS, function (row) {
                    var groupName = "group" + row.Value.replace(/ /g, '');
                    answers.push($('input[name=' + groupName + ']:checked').val());
                });
                $element.empty();
                $scope.$emit('closeAnswer', {answer: answers, questionId: $scope.$parent.questionId});
            };
        }
    }
}])
;