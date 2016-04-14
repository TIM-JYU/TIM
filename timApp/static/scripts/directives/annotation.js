/**
 * Created by localadmin on 4.4.2016.
 */

'use strict';

/* Directive for marking */
timApp.directive("annotation", function() {
    return{
        templateUrl: "/static/templates/annotation.html",
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
        controller: 'AnnotationController'
    }
});

timApp.controller('AnnotationController', ['$scope', '$timeout', function ($scope, $timeout){
    $scope.newComment = "";

    /**
     * Toggle annotation visibility
     */
    $scope.toggleAnnotation = function() {
        $scope.show = !$scope.show;
    };

    /**
     * Delete selected annotation. Queries parent scope.
     */
    $scope.deleteAnnotation = function(){
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