/**
 * Created by localadmin on 4.4.2016.
 */

'use strict';

/* Directives */
timApp.directive("marking", function() {
    return{
        templateUrl: "/static/templates/marking.html",
        transclude: true,
        scope: {
            // locked, selected ?
            show: '@',
            velp: '@',
            points: '@',
            comments: '@',
            evalAsync: '@'
        },
        controller: 'MarkingController'
    }
});

timApp.controller('MarkingController', ['$scope', '$timeout', function ($scope, $timeout){
    $scope.comments = [
        {
            "content": "Ensimmäinen kommentti tähän ketjuun",
            "author": "sevitarv"
        }
    ];

    $scope.toggleMarking = function() {
        $scope.show = !$scope.show;
    };

    $timeout(function () {$scope.toggleMarking();}, 100);

}]);