/**
 * Created by hajoviin on 24.2.2015.
 */

var WallController = angular.module('controller', []);

WallController.controller("WallController", ['$scope', '$controller', "$http",

    function ($scope, controller, http) {

        $scope.test = "Hey!";
        $scope.count = 0;
        $scope.msg = "Welcome to wall 0.0.1";
        $scope.newMsg = "";

        $scope.toggle = function () {
            alert("Test1");
        };

        $scope.sendMessage = function (message) {
            var today = new Date();
            if(message != "") {
                $scope.msg = $scope.msg + "\n" + today.getHours() + ":" + today.getMinutes() + " > " + message;
                $scope.newMsg = "";
            }
        };

        $scope.getMessages = function () {
            var message = "Stupid test message";
            return message;
        };

    }]);