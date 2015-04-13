var angular, Waypoint;
var timApp = angular.module('timApp');

timApp.directive("answerbrowser", ['$upload', '$http', '$sce', '$compile', '$window',
    function ($upload, $http, $sce, $compile, $window) {
        "use strict";
        return {
            templateUrl: "/static/templates/answerBrowser.html",
            restrict: 'E',
            scope: {
                taskId: '@'
            },
            controller: function ($scope) {

            },
            link: function ($scope, $element, $attrs) {
                $scope.loading = 0;
                $scope.changeAnswer = function () {
                    var $par = $element.parents('.par');
                    var par_id = $scope.$parent.getParIndex($par);
                    $scope.loading++;
                    $http.get('/getState', {
                        params: {
                            doc_id: $scope.$parent.docId,
                            par_id: par_id,
                            user: $scope.user.name,
                            state: $scope.selectedAnswer.content
                        }
                    }).success(function (data, status, headers, config) {
                        $element.parents('.par').find('.parContent').html($compile(data.html)($scope));
                    }).error(function (data, status, headers, config) {
                        $window.alert('Error getting answers: ' + data.error);
                    }).finally(function () {
                        $scope.loading--;
                    });
                };

                $scope.next = function () {
                    var newIndex = $scope.answers.indexOf($scope.selectedAnswer) - 1;
                    if (newIndex < 0) {
                        newIndex = $scope.answers.length - 1;
                    }
                    $scope.selectedAnswer = $scope.answers[newIndex];
                    $scope.changeAnswer();
                };

                $scope.previous = function () {
                    var newIndex = $scope.answers.indexOf($scope.selectedAnswer) + 1;
                    if (newIndex >= $scope.answers.length) {
                        newIndex = 0;
                    }
                    $scope.selectedAnswer = $scope.answers[newIndex];
                    $scope.changeAnswer();
                };

                $scope.setNewest = function () {
                    $scope.selectedAnswer = $scope.answers[0];
                    $scope.changeAnswer();
                };

                $scope.getAvailableAnswers = function (updateHtml) {
                    updateHtml = (typeof updateHtml === "undefined") ? true : updateHtml;
                    if (!$scope.$parent.rights.browse_own_answers) {
                        return;
                    }
                    $scope.loading++;
                    $http.get('/answers/' + $scope.taskId + '/' + $scope.user.name)
                        .success(function (data, status, headers, config) {
                            $scope.answers = data;
                            if ($scope.answers.length > 0) {
                                $scope.selectedAnswer = $scope.answers[0];

                                if (updateHtml) {
                                    $scope.changeAnswer();
                                }
                            }
                        }).error(function (data, status, headers, config) {
                            $window.alert('Error getting answers: ' + data.error);
                        }).finally(function () {
                            $scope.loading--;
                        });
                };

                $scope.$on('answerSaved', function (event, args) {
                    if (!$scope.$parent.teacherMode && args.taskId === $scope.taskId) {
                        $scope.getAvailableAnswers(false);
                    }
                });

                $scope.$on('userChanged', function (event, args) {
                    $scope.user = args.user;
                    $scope.changed = true;
                    $scope.shouldUpdateHtml = true;
                });

                $scope.loadIfChanged = function () {
                    if ($scope.changed) {
                        $scope.getAvailableAnswers($scope.shouldUpdateHtml);
                        $scope.changed = false;
                        $scope.shouldUpdateHtml = false;
                    }
                };

                $scope.user = $scope.$parent.users[0];
                $element.parent().on('mouseenter touchstart', function () {
                    $scope.loadIfChanged();
                });
                $scope.changed = true;
                $scope.shouldUpdateHtml = false;
            }
        };
    }]);
