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
            velpdata: "@",
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
    var original = JSON.parse($scope.velpdata);
    $scope.velp = Object.create(original);

    $scope.toggleVelpToEdit = function () {
        $scope.velp.edit = !$scope.velp.edit;
    };

    $scope.saveVelp = function () {
        original = Object.create($scope.velp);
    };

    $scope.useVelp = function () {
        $parent.useVelp($scope.velp);
    };

    /**
     * Return true if user has teacher rights.
     * @returns {boolean}
     */
    $scope.allowChangePoints = function () {
        return $scope.$parent.$parent.item.rights.teacher;
    };


    // get data from parent?

}]);