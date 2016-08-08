/**
 * Handles velp summary. Requires reviewController.
 *
 * @module velpSummary
 * @author Joonas Lattu
 * @author Petteri Palojärvi
 * @author Seppo Tarvainen
 * @licence MIT
 * @copyright 2016 Timber project authors
 */

var angular;
var timApp = angular.module('timApp');
/**
 * Angular directive for phrase selection
 */
timApp.directive('velpSummary', function () {
    "use strict";
    return {
        templateUrl: "/static/templates/velpSummary.html",
        controller: 'VelpSummaryController',
        scope: {annotations: "="}
    };
});

timApp.controller('VelpSummaryController', ['$scope', '$http', '$window', function ($scope, $http, $window) {
    "use strict";
    var console = $window.console;
    $scope.settings = {selectedAll: false};

    $scope.toggleAnnotation = function (annotation, user) {
        console.log("Annotation");
        $scope.$parent.toggleAnnotation(annotation, user);
    };

    /**
     * Get total number of points
     * @returns {number}
     */
    $scope.getTotalPoints = function () {
        var p = 0;
        if ($scope.annotations === undefined)
            return p;

        for (var i = 0; i < $scope.annotations.length; i++) {
            p += $scope.annotations[i].points;
        }
        //cast back to a number, the string has trailing zeros.
        return Number(p.toPrecision(4));
    };

    /**
     * Check all annotations in velp summary.
     */
    $scope.checkAll = function () {
        angular.forEach($scope.annotations, function (a) {
            a.selected = $scope.settings.selectedAll;
        });
    };


}]);