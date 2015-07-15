/**
 * FILL WITH SUITABLE TEXT
 * @module questionPreviewController
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

var angular, docId, lectureId, lectureCode, lectureStartTime, lectureEndTime;

var timApp = angular.module('timApp');
timApp.controller('QuestionPreviewController', ['$scope', '$window', '$http', '$rootScope',
    function ($scope, $window, http, $rootScope) {
        //TODO parse json and set values from rows and columns to scope variables
        //TODO edit questionPreview.html to repeat rows and columns
        "use strict";

        $scope.questionHeaders = [];
        $scope.answerTypes = [];
        $scope.dynamicAnswerSheetControl = {};
        $scope.isLecturer = false;
        $scope.questionTitle = "";

        $scope.$on("setPreviewJson", function (event, args) {
            $scope.questionId = args.questionId;
            $scope.isLecturer = args.isLecturer;
            $scope.questionJson = args.questionJson;
            $scope.questionTitle = args.questionJson.TITLE;

            $scope.dynamicAnswerSheetControl.createAnswer();
        });

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:questionPreviewController
         */
        $scope.editQuestion = function () {
            $scope.close();
            $rootScope.$broadcast("editQuestion", {"question_id": $scope.qId, "json": $scope.json});
        };

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:questionPreviewController
         */
        $scope.ask = function () {
            http({
                url: '/askQuestion',
                method: 'POST',
                params: {
                    lecture_id: $scope.lectureId,
                    question_id: $scope.qId,
                    doc_id: $scope.docId,
                    buster: new Date().getTime()
                }
            })
                .success(function () {
                    $rootScope.$broadcast('askQuestion', {"json": $scope.json, "questionId": $scope.qId});
                    $scope.close();
                })
                .error(function (error) {
                    $scope.close();
                    $window.console.log(error);
                });
        };

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:questionPreviewController
         */
        $scope.close = function () {
            $scope.dynamicAnswerSheetControl.closePreview();
            $scope.$emit('closeQuestionPreview');
        };

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:questionPreviewController
         */
        $scope.deleteQuestion = function () {
            var confirmDi = $window.confirm("Are you sure you want to delete this question?");
            if (confirmDi) {
                http({
                    url: '/deleteQuestion',
                    method: 'POST',
                    params: {question_id: $scope.qId, doc_id: $scope.docId}
                })
                    .success(function () {
                        $scope.$emit('closeQuestionPreview');
                        $window.console.log("Deleted question");
                        location.reload();
                    })
                    .error(function (error) {

                        $scope.$emit('closeQuestionPreview');
                        $window.console.log(error);
                    });

            }
        };
    }
]);