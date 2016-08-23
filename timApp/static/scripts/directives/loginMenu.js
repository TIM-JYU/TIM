var angular;
var timApp = angular.module('timApp');

timApp.directive('loginMenu', ['Users', '$http', function (Users, $http) {
    "use strict";
    return {
        restrict: 'E',
        scope: {},
        templateUrl: '/static/templates/loginMenu.html',
        controller: function ($scope, $element) {
            $scope.getCurrentUser = Users.getCurrent;
            $scope.getSessionUsers = Users.getSessionUsers;
            $scope.addUser = function ($event) {
                $event.stopPropagation();
                $scope.isOpen = !$scope.isOpen;
            };
            $scope.logout = Users.logout;
            $scope.isLoggedIn = Users.isLoggedIn;
            $scope.korppiLogin = Users.korppiLogin;
            $scope.stopClick = function ($event) {
                $event.stopPropagation();
            };
            $scope.toggled = function (open) {
                if (!open) {
                    $scope.isOpen = false;
                }
            };
            $scope.addTestUser = function ($event) {
                Users.addUser();
            };
        }
    };
}]);
