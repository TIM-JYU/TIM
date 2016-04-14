/**
 * Created by localadmin on 13.4.2016.
 */

var angular;
var timApp = angular.module('timApp');

/**
 * Angular directive for phrase selection
 */
timApp.directive('velpSummary', function () {
    return{
        templateUrl: "/static/templates/velpSummary.html",
        controller: 'VelpSummaryController'
    }
});

timApp.controller('VelpSummaryController', ['$scope', '$http', function ($scope, $http) {
    "use strict";
    $scope.testi = "Heippa";
}]);