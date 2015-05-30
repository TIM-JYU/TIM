/*global $:false */
var angular, docId, lectureId, lectureCode, lectureStartTime, lectureEndTime;

var timApp = angular.module('timApp');
timApp.controller('QuestionPreviewController', ['$scope', '$window', '$http', '$rootScope',
    function ($scope, $window, http, $rootScope) {
        //TODO parse json and set values from rows and columns to scope variables
        //TODO edit questionPreview.html to repeat rows and columns
        "use strict";

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
                    $scope.$emit('closeQuestionPreview');
                })
                .error(function (error) {
                    $scope.$emit('closeQuestionPreview');
                    $window.console.log(error);
                });
        };

        $scope.close = function () {
            $scope.$emit('closeQuestionPreview');
        };

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