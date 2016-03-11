/**
 * Created by sevitarv on 8.3.2016.
 */

var angular;
var timApp = angular.module('timApp');

console.log("reviewController.js added");

timApp.controller("ReviewController", ['$scope', '$http', function ($scope, $http) {
    "use strict";
    alert("working");

    $scope.selectText = function () {
        var sel = $window.getSelection();
        if (sel.toString().length > 0) {
            var range = sel.getRangeAt(0);
            $scope.selectedArea = range;
        } else {
            $scope.selectedArea = undefined;
        }
    };

}]);