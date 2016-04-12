/**
 * Created by localadmin on 4.4.2016.
 */

'use strict';

/* Directive for marking */
timApp.directive("marking", function() {
    return{
        templateUrl: "/static/templates/marking.html",
        transclude: true,
        scope: {
            // locked, selected ?
            show: '=',
            velp: '@',
            points: '@',
            evalAsync: '@',
            user: '@',
            comments: '=',
            aid: '@'
        },
        controller: 'MarkingController'
    }
});

timApp.controller('MarkingController', ['$scope', '$timeout', function ($scope, $timeout){
    $scope.newComment = "";

    /**
     * Toggle marking visibility
     */
    $scope.toggleAnnotation = function() {
        $scope.show = !$scope.show;
    };

    $scope.deleteAnnotation = function(){
        //console.log($scope.aid);
        $scope.$parent.deleteAnnotation($scope.aid);
        $scope.toggleAnnotation();
    };

    /**
     * Add comment to annotation
     */
    $scope.addComment = function() {
        if ($scope.newComment.length > 0) {
            $scope.comments.push({author: $scope.user, content: $scope.newComment});
            $scope.newComment = "";
        }
    };

    $timeout(function () {
        $scope.toggleAnnotation();
    }, 1);
    $scope.toggleAnnotation();

}]);