var timApp = angular.module('timApp');

timApp.directive("answerbrowser", ['$upload', '$http', '$sce', '$compile', '$window',
    function ($upload, $http, $sce, $compile, $window) {
        return {
            templateUrl: "/static/templates/answerBrowser.html",
            restrict: 'E',
            scope: {
                taskId: '@',
                userId: '@'
            },
            controller: function ($scope) {

            },
            link: function ($scope, $element, $attrs) {
                $scope.loading = false;
                $scope.changeAnswer = function () {
                    var $par = $element.parents('.par');
                    var par_id = $scope.$parent.getParIndex($par);
                    $scope.loading = true;
                    $http.get('/getState', {
                        params: {
                            doc_id: $scope.$parent.docId,
                            par_id: par_id,
                            user: $scope.userId,
                            state: $scope.selectedAnswer.content
                        }
                    }).success(function (data, status, headers, config) {
                        $element.parents('.par').find('.parContent').html($compile(data.html)($scope));
                    }).error(function (data, status, headers, config) {
                        $window.alert('Error getting answers: ' + data.error);
                    }).finally(function () {
                        $scope.loading = false;
                    });
                };

                $scope.previous = function () {
                    var newIndex = $scope.answers.indexOf($scope.selectedAnswer) - 1;
                    if (newIndex < 0) {
                        newIndex = $scope.answers.length - 1;
                    }
                    $scope.selectedAnswer = $scope.answers[newIndex];
                    $scope.changeAnswer();
                };

                $scope.next = function () {
                    var newIndex = $scope.answers.indexOf($scope.selectedAnswer) + 1;
                    if (newIndex >= $scope.answers.length) {
                        newIndex = 0;
                    }
                    $scope.selectedAnswer = $scope.answers[newIndex];
                    $scope.changeAnswer();
                };

                $scope.getAvailableAnswers = function () {
                    $http.get('/answers/' + $scope.taskId + '/' + $scope.userId)
                        .success(function (data, status, headers, config) {
                            $scope.answers = data;
                            if ($scope.answers.length > 0 && !$scope.selectedAnswer) {
                                $scope.selectedAnswer = $scope.answers[0];
                                $scope.changeAnswer();
                            }
                        }).error(function (data, status, headers, config) {
                            $window.alert('Error getting answers: ' + data.error);
                        });
                };
                $scope.getAvailableAnswers();
            }
        };
    }]);
