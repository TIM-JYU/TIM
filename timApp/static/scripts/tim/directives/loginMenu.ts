import {timApp} from "tim/app";
import {Users} from "../services/userService";

timApp.directive("loginMenu", [function() {
    "use strict";
    return {
        restrict: "E",
        scope: {},
        templateUrl: "/static/templates/loginMenu.html",
        controller: ["$scope", function($scope) {
            $scope.getCurrentUser = () => Users.getCurrent();
            $scope.getSessionUsers = () => Users.getSessionUsers();
            $scope.form = {};
            $scope.loggingout = false;
            $scope.form.email = "";
            $scope.form.password = "";
            $scope.addingToSession = false;
            $scope.addUser = function($event) {
                $event.stopPropagation();
                $scope.addingToSession = !$scope.addingToSession;
            };
            $scope.logout = (user, logoutFromKorppi = false) => Users.logout(user, logoutFromKorppi);
            $scope.isLoggedIn = () => Users.isLoggedIn();
            $scope.korppiLogin = function(addingToSession) {
                $scope.korppiLoading = true;
                Users.korppiLogin(addingToSession);
            };
            $scope.isKorppi = () => Users.isKorppi();
            $scope.stopClick = function($event) {
                $event.stopPropagation();
            };
            $scope.toggled = function(open) {
                if (!open) {
                    $scope.addingToSession = false;
                }
            };
            $scope.addTestUser = function($event) {
                Users.addUser();
            };
            $scope.loginWithEmail = function() {
                Users.loginWithEmail($scope.form.email, $scope.form.password, $scope.addingToSession,
                    function() {
                        $scope.addingToSession = false;
                    });
            };
            $scope.beginLogout = function($event) {
                if (Users.isKorppi()) {
                    $scope.loggingout = true;
                    $event.stopPropagation();
                } else {
                    $scope.logout($scope.getCurrentUser());
                }
            };
        }],
    };
}]);
