/**
 * Created by hajoviin on 6.5.2015.
 */

timApp.controller('QuestionShowAnswerController', ['$scope', '$http', function ($scope) {

    $scope.dynamicAnswerShowControl = {};
    $scope.canvas = "";
    $scope.questionTitle = "";

    $scope.close = function () {
        $scope.$emit('closeAnswerShow');
        $scope.dynamicAnswerShowControl.close()
    };

    $scope.$on("putAnswers", function (event, answer) {
        $scope.dynamicAnswerShowControl.addAnswer(answer.answers);
    });

    $scope.$on("createChart", function (event, question) {
        console.log(question);
        $scope.dynamicAnswerShowControl.createChart(question);
        $scope.questionTitle = question.QUESTION;
    });

}]);

