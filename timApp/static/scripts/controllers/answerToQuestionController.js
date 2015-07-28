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
timApp.controller('AnswerToQuestionController', ['$scope', '$rootScope', '$http', function ($scope, $rootScope, $http) {
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
        $scope.askedTime = args.askedTime;
        $scope.clockOffset = args.clockOffset;
        $scope.questionEnded = false;
        $scope.dynamicAnswerSheetControl.createAnswer();
    });

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:answerToQuestionController
     */
    $scope.answer = function () {
        $scope.dynamicAnswerSheetControl.answerToQuestion();
        if ($scope.isLecturer) {
            $rootScope.$broadcast("lecturerAnswered");
        }

    };

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:answerToQuestionController
     */
    $scope.close = function () {
        $scope.dynamicAnswerSheetControl.closeQuestion();
    };

    /**
     *
     */
    $scope.showAnswers = function () {
        $scope.$emit('showAnswers', true);
    };

    $scope.askAgain = function () {
        $http({
            url: '/askQuestion',
            method: 'POST',
            params: {
                lecture_id: $scope.lectureId,
                question_id: $scope.questionId,
                doc_id: $scope.docId,
                buster: new Date().getTime()
            }
        }).success(function () {
            $scope.close();
            $rootScope.$broadcast('askQuestion', {"json": $scope.questionJson, "questionId": $scope.questionId});
        }).error(function (error) {
            $scope.close();
            $window.console.log(error);
        });
    };
}]);