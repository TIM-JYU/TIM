var angular;
var timApp = angular.module('timApp');

timApp.directive('loginMenu', ['Users', '$http', '$httpParamSerializer', function (Users, $http, $httpParamSerializer) {
    "use strict";
    return {
        restrict: 'E',
        scope: {},
        templateUrl: '/static/templates/loginMenu.html',
        controller: function ($scope, $element) {
            $scope.getCurrentUser = Users.getCurrent;
            $scope.getSessionUsers = Users.getSessionUsers;
            $scope.form = {};
            $scope.form.email = "";
            $scope.form.password = "";
            $scope.addingToSession = false;
            $scope.addUser = function ($event) {
                $event.stopPropagation();
                $scope.addingToSession = !$scope.addingToSession;
            };
            $scope.logout = Users.logout;
            $scope.isLoggedIn = Users.isLoggedIn;
            $scope.korppiLogin = function(addingToSession){
                $scope.korppiLoading = true;
                Users.korppiLogin(addingToSession);
            };
            $scope.stopClick = function ($event) {
                $event.stopPropagation();
            };
            $scope.toggled = function (open) {
                if (!open) {
                    $scope.addingToSession = false;
                }
            };
            $scope.addTestUser = function ($event) {
                Users.addUser();
            };
            $scope.loginWithEmail = function () {
                Users.loginWithEmail($scope.form.email, $scope.form.password, $scope.addingToSession,
                    function () {
                        $scope.addingToSession = false;
                    });
            };
        }
    };
}]);
