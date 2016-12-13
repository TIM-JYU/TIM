/**
 * Created by Seppo Tarvainen on 25.11.2016.
 *
 * @module velpWindow
 * @author Seppo Tarvainen
 * @licence MIT
 */


// var UNDEFINED = "undefined";

/**
 * Angular directive for velp selection
 */
timApp.directive('velpWindow', function () {
    "use strict";

    return {
        templateUrl: "/static/templates/velpWindow.html",
        scope: {
            velp: "=",
            velpGroups: "=", // all velpgroups, not just selected ones
            advancedOn: "=",
            index: "@"
        },
        controller: 'VelpWindowController'

    };
});

/**
 * Controller for velp Window
 * @lends module:velpWindow
 */
timApp.controller('VelpWindowController', ['$scope', function ($scope) {
    "use strict";
    console.log($scope.velp);
    var original = JSON.parse(JSON.stringify($scope.velp)); // clone object

    $scope.submitted = false;
    var doc_id = $scope.$parent.docId;

    /**
     * Toggles velp for editing. If another velp is currently open,
     * this method closes it.
     */
    $scope.toggleVelpToEdit = function () {
        var lastEdited = $scope.$parent.getVelpUnderEdit();

        if (lastEdited.edit && lastEdited.id !== $scope.velp.id){
            lastEdited.edit = false;
        }

        $scope.velp.edit = !$scope.velp.edit;
        console.log($scope.velp.edit);
        $scope.$parent.setVelpToEdit($scope.velp);
    };


    /**
     * Saves velp to database
     * @param form
     */
    $scope.saveVelp = function (form) {
        $scope.submitted = true;
        if (!form.$valid) return;

        form.$setPristine();

        $scope.$parent.makePostRequest("/{0}/update_velp".replace('{0}', doc_id), $scope.velp, function (json) {
            original = JSON.parse(JSON.stringify($scope.velp)); // clone object
            $scope.toggleVelpToEdit();
        });


    };

    $scope.useVelp = function () {
        console.log($scope.$parent.getVelpUnderEdit());
        console.log($scope.velp.edit);
        //if (!$scope.velp.edit) {
        $scope.$parent.useVelp($scope.velp);
        //}
    };

    $scope.isVelpValid = function () {
        if (typeof $scope.velp.content === UNDEFINED)
            return false;
        // TODO: check rights for velp groups
        return true;
    };

    /**
     * Return true if user has teacher rights.
     * @returns {boolean}
     */
    $scope.allowChangePoints = function () {
        return $scope.$parent.$parent.item.rights.teacher;
    };
}]);