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

var angular, docId, lectureCode, lectureStartTime, lectureEndTime;

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
            $scope.questionParId = args.questionParId;
            $scope.questionParIdNext = args.questionParIdNext;
            $scope.isLecturer = args.isLecturer;
            $scope.json = args.questionjson;
            $scope.questionTitle = args.questionjson.title;
            $scope.points = args.points;
            $scope.expl = args.expl;
            $scope.dynamicAnswerSheetControl.createAnswer();

        });

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:questionPreviewController
         */
        $scope.editQuestion = function () {
            $scope.close();
            $rootScope.$broadcast("editQuestion", {
                "question_id": $scope.questionId,
                "par_id": $scope.questionParId,
                "par_id_next": $scope.questionParIdNext,
                "json": $scope.json,
                "points": $scope.points,
                "expl": $scope.expl
            });
        };

        /**
         * FILL WITH SUITABLE TEXT
         * @memberof module:questionPreviewController
         */
        $scope.ask = function () {
            $scope.$emit('askQuestion', {
                "lecture_id": $scope.lectureId,
                "question_id": $scope.questionId,
                "par_id": $scope.questionParId,
                "doc_id": $scope.docId,
                "json": $scope.json,
                "expl": $scope.expl
            });
            $scope.close();
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
                http.post('/deleteParagraph/' + $scope.docId, {par: $scope.questionParId})
                    .success(function (data) {
                        $scope.handleDelete(data, {par: $scope.questionParId, area_start: null, area_end: null});
                        $scope.$emit('closeQuestionPreview');
                        $window.console.log("Deleted question");
                    })
                    .error(function (error) {
                        $scope.$emit('closeQuestionPreview');
                        $window.console.log(error);
                    });

            }
        };
    }
]);