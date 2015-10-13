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
        $scope.result = args.result;
        $scope.previousAnswer = args.answer;
        $scope.askedId = args.askedId;
        $scope.questionId = args.questionId;
        $scope.isLecturer = args.isLecturer;
        $scope.json = args.questionJson;
        $scope.questionTitle = args.questionJson.TITLE;
        $scope.askedTime = args.askedTime;
        $scope.clockOffset = args.clockOffset;
        $scope.expl = args.expl;
        $scope.questionEnded = false;
        $scope.answered = false;
        $scope.dynamicAnswerSheetControl.createAnswer();
    });

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:answerToQuestionController
     */
    $scope.answer = function () {
        $scope.dynamicAnswerSheetControl.answerToQuestion();
        $scope.answered = true;
        if ($scope.isLecturer) {
            $rootScope.$broadcast("lecturerAnswered");
        }
    };

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:answerToQuestionController
     */
    $scope.close = function (callback) {
        if ($scope.isLecturer) $scope.stopQuestion(callback);
        else if ($scope.result) $scope.$emit('pointsClosed', $scope.askedId);
        $scope.dynamicAnswerSheetControl.closeQuestion();
    };

    $scope.stopQuestion = function (callback) {
        $http({
            url: '/stopQuestion',
            method: 'POST',
            params: {
                'asked_id': $scope.askedId,
                'lecture_id': $scope.lectureId,
                'buster': new Date().getTime()
            }
        })
            .success(function () {
                $scope.questionEnded = true;
                $scope.dynamicAnswerSheetControl.endQuestion();
                console.log("Question ", $scope.askedId, " stopped");
                if (callback) callback();
            })
            .error(function () {
                console.log("Failed to stop question");
            });
    };

    /**
     *
     */
    $scope.showAnswers = function () {
        $scope.$emit('showAnswers', true);
    };

    $scope.reAsk = function () {
        $scope.close($scope.reAskEmit);
    };

    $scope.askAsNew = function () {
        $scope.close($scope.askAsNewEmit);
    };

    $scope.edit = function () {
        $http({
            url: '/getAskedQuestionById',
            method: 'GET',
            params: {'asked_id': $scope.askedId, 'buster': new Date().getTime()}
        })
            .success(function (data) {
                var json = JSON.parse(data.json);
                $rootScope.$broadcast('changeQuestionTitle', {'title': json.TITLE});
                $rootScope.$broadcast("editQuestion", {
                    "asked_id": $scope.askedId,
                    "json": json,
                    "points": data.points
                });
            })
            .error(function () {
                console.log("There was some error getting question.");
            });
    };

    $scope.showPoints = function () {
        $http({
            url: '/showAnswerPoints',
            method: 'POST',
            params: {
                'asked_id': $scope.askedId,
                'lecture_id': $scope.lectureId,
                'buster': new Date().getTime()
            }
        })
            .success(function () {
                $scope.result = true;
                $scope.dynamicAnswerSheetControl.createAnswer();
            })
            .error(function () {
                console.log("Could not show points to students.");
            });
    };

    $scope.reAskEmit = function () {
        $scope.$emit('askQuestion', {
            "lecture_id": $scope.lectureId,
            "asked_id": $scope.askedId,
            "question_id": $scope.questionId,
            "doc_id": $scope.docId,
            "json": $scope.json,
            "expl": $scope.expl
        });
    };

    $scope.askAsNewEmit = function () {
        $scope.$emit('askQuestion', {
            "lecture_id": $scope.lectureId,
            "question_id": $scope.questionId,
            "doc_id": $scope.docId,
            "json": $scope.json,
            "expl": $scope.expl
        });
    };
}]);