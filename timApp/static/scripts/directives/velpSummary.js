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
        controller: 'VelpSummaryController',
        scope: {annotations: "="}
    }
});

timApp.controller('VelpSummaryController', ['$scope', '$http', function ($scope) {
    "use strict";
    $scope.settings = {selectedAll: false};

    $scope.toggleAnnotation = function(annotation){
        console.log("Annotation");
        $scope.$parent.toggleAnnotation(annotation);
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
        return p;
    };

    $scope.checkAll = function () {
        angular.forEach($scope.annotations, function (a) {
            a.selected = $scope.settings.selectedAll;
        });
    };


}]);