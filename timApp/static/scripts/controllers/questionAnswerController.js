/**
 * Created by hajoviin on 22.4.2015.
 */

timApp.controller('QuestionAnswerController', ['$scope', '$http', function ($scope) {

    $scope.questionHeaders = [];
    $scope.answerTypes = [];
    $scope.dynamicAnswerSheetControl = {};

    $scope.$on("setQuestionJson", function () {
        $scope.dynamicAnswerSheetControl.createAnswer()
    });


    $scope.answer = function () {
        $scope.$emit('closeAnswer');
    };
    /*
     $scope.createAnswer = function () {
     var json = $scope.askedQuestionJson;

     console.log(json.TYPE);

     if (json.TYPE == "true-false") {
     console.log("Creating true-false form");
     console.log(json.DATA.ROWS);
     angular.forEach(json.DATA.ROWS, function (row) {
     $scope.questionHeaders.push(row.Value);
     });

     var test = angular.element("<div> test </div>");

     console.log(test);

     console.log($scope.questionHeaders);

     angular.forEach(json.DATA.ROWS, function (row) {

     });
     }

     }
     */
}]);


timApp.directive('dynamicAnswerSheet', function ($compile) {
    return {
        restrict: 'E',
        replace: "true",
        scope: {
            control: '='
        },
        transclude: true,
        link: function ($scope, $element) {
            $scope.internalControl = $scope.control || {};
            $scope.internalControl.createAnswer = function () {
                var json = $scope.$parent.askedQuestionJson;
                console.log(json);
                angular.forEach(json.DATA.ROWS, function (row) {
                    $element.append("<p>" + row.Value + "</p>");
                    angular.forEach(row.Columns, function (column) {
                        $element.append("<p>" + column.Value + "<p>");
                    });
                });
                $compile($scope);
            };
        }
    }
})
;