/**
 * Created by localadmin on 4.4.2016.
 *
 * Annotations uses attribute as a directive declaration, because IE does not support
 * custom elements reliably.
 */

'use strict';

/* Directive for marking */
timApp.directive("annotation", function() {
    return{
        templateUrl: "/static/templates/annotation.html",
        transclude: true,
        scope: {
            show: '=',
            points: '=',
            visibleto: '=',
            comments: '=',
            aid: '=',
            user: '@',
            velp: '@'
        },
        controller: 'AnnotationController'
    }
});

timApp.controller('AnnotationController', ['$scope', '$http', function ($scope, $http){
    $scope.newComment = "";
    console.log($scope);
    $scope.visible_options = {
        "type": "select",
        "value": $scope.visibleto,
        "values": [1, 2, 3, 4] ,
        "names": ["Just me", "Document owner", "Teachers", "Everyone"]
    };

    // Original visibility, or visibility in session

    // TODO: origin visibility
    $scope.original = {
        points: $scope.points,
        velp: $scope.velp,
        visible_to: $scope.visibleto,
        comment: $scope.newComment,
        annotation_id: $scope.aid
    };

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
        if ($scope.comments.length < 2) {
            $scope.$parent.deleteAnnotation($scope.aid);
            $scope.toggleAnnotation();
        }
    };

    /**
     * Changes points of selected annotation. Queries parent scope.
     */
    $scope.changePoints = function(){
        console.log($scope.points);
        $scope.$parent.changeAnnotationPoints($scope.aid, $scope.points);
    };

    /**
     * Save changes to annotation
     */
    $scope.saveChanges = function() {
        var id = $scope.$parent.getRealAnnotationId($scope.aid);

        // Add comment
        if ($scope.newComment.length > 0) {
            $scope.comments.push({author: $scope.user, content: $scope.newComment});
            var data = {annotation_id: id, content: $scope.newComment};
            $scope.$parent.makePostRequest("/add_annotation_comment", data, function(json){console.log(json);});
        }
        $scope.newComment = "";

        $scope.original = {
            points: $scope.points,
            annotation_id: id,
            visible_to: $scope.visible_options.value,
            velp: $scope.velp,
            comment: $scope.newComment
        };

        $scope.$parent.makePostRequest("/update_annotation", $scope.original, function(json){console.log(json);});


    };

    $scope.checkIfChanged = function(){
        if ($scope.original.points !== $scope.points)
            return true;
        if ($scope.original.comment !== $scope.newComment)
            return true;
        if ($scope.original.visible_to != $scope.visible_options.value)
            return true;
        if ($scope.original.velp !== $scope.velp)
            return true;
        return false;
    };
    /*
    $scope.toggleAnnotation();
    $scope.toggleAnnotation();
    */

}]);