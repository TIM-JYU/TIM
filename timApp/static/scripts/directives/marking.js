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
            id: '@',
        },
        controller: 'MarkingController',
        link: function(scope){
            console.log("Elementti");
            console.log(scope);
        }
    }
});

timApp.controller('MarkingController', ['$scope', '$timeout', function ($scope, $timeout){
    $scope.newComment = "";

    /**
     * Toggle marking visibility
     */
    $scope.toggleMarking = function() {
        $scope.show = !$scope.show;
    };

    $scope.deleteAnnotation = function(){
        $scope.$parent.deleteAnnotation($scope.id);
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
        $scope.toggleMarking();
    }, 1);
    $scope.toggleMarking();

}]);