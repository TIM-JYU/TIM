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

}]);

timApp.directive('dynamicShowAnswerSheet', ['$interval', '$compile', function ($interval, $compile) {
    return {
        restrict: 'E',
        replace: "true",
        scope: {
            control: '='
        },
        transclude: true,
        link: function ($scope, $element) {
            $scope.internalControl = $scope.control || {};

            $scope.internalControl.addAnswer = function(answer){
                angular.forEach(answer, function(onePersonsAnswers){
                    var answers = onePersonsAnswers.answer.split("|");
                    angular.forEach(answers, function(singleAnswer){
                        $element.append("<p>" + singleAnswer + "</p>")
                    })
                })
            };

            $scope.internalControl.close = function() {
                $element.empty();
            }

        }
    }
}])
;
