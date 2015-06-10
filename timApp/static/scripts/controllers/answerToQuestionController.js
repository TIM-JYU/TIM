/**
 * Created by hajoviin on 22.4.2015
 * FILL WITH SUITABLE TEXT
 * @module answerToQuestionController
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
timApp.controller('AnswerToQuestionController', ['$scope','$window',function ($scope) {
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

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:answerToQuestionController
     */
    $scope.answer = function () {
        $scope.dynamicAnswerSheetControl.answerToQuestion();

    };

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:answerToQuestionController
     */
    $scope.close = function () {
        $scope.dynamicAnswerSheetControl.closeQuestion();
    };
}]);