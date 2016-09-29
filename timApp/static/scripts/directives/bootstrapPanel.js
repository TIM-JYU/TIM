var angular;
var timApp = angular.module('timApp');

timApp.directive("bootstrapPanel", ['$window', '$log', '$http', function ($window, $log, $http) {
    "use strict";
    return {
        restrict: 'E',
        transclude: true,
        scope: {
            title: '@?',
            closeFn: '&onClose',
            showClose: '=?'
        },
        templateUrl: "/static/templates/bootstrapPanel.html",
        link: function ($scope, $element) {

        },

        controller: function ($scope, $element, $attrs) {
            var sc = $scope;

            sc.close = function () {
                $element.hide();
                sc.closeFn();
            };
        }
    };
}]);
