/**
 * Created by hajoviin on 22.4.2015.
 */

/*global $:false */

var angular;

var timApp = angular.module('timApp');
timApp.controller('QuestionAnswerController', ['$scope','$window',function ($scope) {
    "use strict";
    $scope.questionHeaders = [];
    $scope.answerTypes = [];
    $scope.dynamicAnswerSheetControl = {};
    $scope.isLecturer = false;
    $scope.questionTitle = "";

    $scope.$on("setQuestionJson", function (event, args) {
        $scope.questionId = args.questionId;
        $scope.isLecturer = args.isLecturer;
        $scope.questionJson = args.questionJson;
        $scope.questionTitle = args.questionJson.TITLE;

        $scope.dynamicAnswerSheetControl.createAnswer();
    });

    $scope.answer = function () {
        $scope.dynamicAnswerSheetControl.answerToQuestion();

    };

    $scope.close = function () {
        $scope.dynamicAnswerSheetControl.closeQuestion();
    };
}]);