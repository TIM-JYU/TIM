import {timApp} from "tim/app";
import * as answerSheet from "tim/directives/dynamicAnswerSheet";
import {markAsUsed} from "tim/utils";

markAsUsed(answerSheet);

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

timApp.controller("AnswerToQuestionController", ["$scope", "$rootScope", "$http", function($scope, $rootScope, $http) {
    "use strict";
    $scope.questionHeaders = [];
    $scope.answerTypes = [];
    $scope.dynamicAnswerSheetControl = {};
    $scope.isLecturer = false;
    $scope.questionTitle = "";

    $scope.$on("setQuestionJson", function(event, args) {
        $scope.result = args.result;
        $scope.previousAnswer = args.answer;
        $scope.askedId = args.askedId;
        $scope.questionId = args.questionId;
        $scope.questionParId = args.questionParId;
        $scope.isLecturer = args.isLecturer;
        $scope.markup = args.markup;
        $scope.questionTitle = args.markup.json.questionTitle;
        $scope.askedTime = args.askedTime;
        $scope.clockOffset = args.clockOffset;
        $scope.questionEnded = false;
        $scope.answered = false;
        $scope.buttonText = $scope.markup.button || $scope.markup.buttonText || "Answer";
        $scope.dynamicAnswerSheetControl.createAnswer($scope);
        if ( args.isAsking ) $scope.isAsking = true;
    });

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:answerToQuestionController
     */
    $scope.answer = function() {
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
    $scope.close = function(callback) {
        if ($scope.isLecturer) $scope.stopQuestion(callback);
        if ($scope.result) $scope.$emit("pointsClosed", $scope.askedId);
        $scope.dynamicAnswerSheetControl.closeQuestion();
    };

    $scope.stopQuestion = function(callback) {
        $http({
            url: "/stopQuestion",
            method: "POST",
            params: {
                asked_id: $scope.askedId,
                lecture_id: $scope.lectureId,
            },
        })
            .success(function() {
                $scope.$emit("questionStopped");
                $scope.questionEnded = true;
                $scope.dynamicAnswerSheetControl.endQuestion();
                console.log("Question ", $scope.askedId, " stopped");
                if (callback) callback();
            })
            .error(function() {
                console.log("Failed to stop question");
            });
    };

    /**
     *
     */
    $scope.showAnswers = function() {
        $scope.$emit("showAnswers", true);
    };

    $scope.reAsk = function() {
        $scope.close($scope.reAskEmit);
    };

    $scope.askAsNew = function() {
        $scope.close($scope.askAsNewEmit);
    };

    $scope.edit = function() {
        $http({
            url: "/getAskedQuestionById",
            method: "GET",
            params: {asked_id: $scope.askedId},
        })
            .success(function(data) {
                let markup = JSON.parse(data.json);
                if (!markup.json) markup = {json: markup}; // compability for old
                markup.points = data.points;
                $rootScope.$broadcast("changeQuestionTitle", {questionTitle: markup.questionTitle});
                $rootScope.$broadcast("editQuestion", {
                    asked_id: $scope.askedId,
                    markup,
                });
            })
            .error(function() {
                console.log("There was some error getting question.");
            });
    };

    $scope.showPoints = function() {
        $http({
            url: "/showAnswerPoints",
            method: "POST",
            params: {
                asked_id: $scope.askedId,
                lecture_id: $scope.lectureId,
                current_question_id: $scope.askedId,
            },
        })
            .success(function(answer) {
                $scope.current_points_id = $scope.askedId;
                $scope.result = true;
                $scope.points = answer.points;
                if (answer.expl) $scope.expl = JSON.parse(answer.expl);
                $scope.dynamicAnswerSheetControl.createAnswer($scope);
            })
            .error(function() {
                console.log("Could not show points to students.");
            });
    };

    $scope.reAskEmit = function() {
        $scope.$emit("askQuestion", {
            lecture_id: $scope.lectureId,
            asked_id: $scope.askedId,
            par_id: $scope.questionParId,
            question_id: $scope.questionId,
            doc_id: $scope.docId,
            markup: $scope.markup,
        });
    };

    $scope.askAsNewEmit = function() {
        $scope.$emit("askQuestion", {
            lecture_id: $scope.lectureId,
            question_id: $scope.questionId,
            par_id: $scope.questionParId,
            doc_id: $scope.docId,
            markup: $scope.markup,
        });
    };
}]);
