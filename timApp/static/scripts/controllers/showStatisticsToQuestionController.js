/**
 * Created by hajoviin on 6.5.2015.
 */

var angular;

var timApp = angular.module('timApp');
timApp.controller('ShowStatisticsToQuestionController', ['$scope', '$http', function ($scope) {
    "use strict";
    $scope.dynamicAnswerShowControl = {};
    $scope.canvas = "";
    $scope.questionTitle = "";

    $scope.close = function () {
        $scope.$emit('closeAnswerShow');
        $scope.dynamicAnswerShowControl.close();
    };

    $scope.$on("putAnswers", function (event, answer) {
        $scope.dynamicAnswerShowControl.addAnswer(answer.answers);
    });

    $scope.$on("createChart", function (event, question) {
        $scope.dynamicAnswerShowControl.createChart(question);
        $scope.questionTitle = question.QUESTION;
    });

}]);

