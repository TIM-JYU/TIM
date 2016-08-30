/**
 * The directive handles velp summary. Requires reviewController.
 *
 * @module velpSummary
 * @author Joonas Lattu
 * @author Petteri Palojärvi
 * @author Seppo Tarvainen
 * @licence MIT
 * @copyright 2016 Timber project members
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
     * Toggles annotation in the document
     * @method toggleAnnotation
     * @param annotation - Annotation to toggle
     */
    $scope.toggleAnnotation = function (annotation) {
        console.log("Annotation");
        $scope.$parent.toggleAnnotation(annotation);
    };

    /**
     * Gets total number of points.
     * @method getTotalPoints
     * @returns {number} Total number of points
     */
    $scope.getTotalPoints = function (annotations) {
        var p = 0;
        if (annotations === undefined)
            return p;

        for (var i = 0; i < annotations.length; i++) {

                p += $scope.annotations[i].points;

        }
        //cast back to a number, the string has trailing zeros.
        return Number(p.toPrecision(4));
    };

    /**
     * Checks all checkboxes linked to the annotations in the velp summary.
     * @method checkAll
     */
    $scope.checkAll = function () {
        angular.forEach($scope.annotations, function (a) {
            a.selected = $scope.settings.selectedAll;
        });
    };


}]);