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
 * @lends module:velpSummary
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

    /**
     * Toggles annotation in document
     * @method toggleAnnotation
     * @param annotation - annotation to toggle
     */
    $scope.toggleAnnotation = function (annotation) {
        console.log("Annotation");
        $scope.$parent.toggleAnnotation(annotation);
    };

    /**
     * Get total number of points
     * @method getTotalPoints
     * @returns {number}
     */
    $scope.getTotalPoints = function (user) {
        var p = 0;
        if ($scope.annotations === undefined)
            return p;

        for (var i = 0; i < $scope.annotations.length; i++) {
            if ($scope.annotatios[i].user_id === user) {
                p += $scope.annotations[i].points;
            }
        }
        //cast back to a number, the string has trailing zeros.
        return Number(p.toPrecision(4));
    };

    /**
     * Check all annotations in velp summary.
     * @method checkAll
     */
    $scope.checkAll = function () {
        angular.forEach($scope.annotations, function (a) {
            a.selected = $scope.settings.selectedAll;
        });
    };


}]);