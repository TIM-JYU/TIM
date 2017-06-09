
import {timApp} from "tim/app";
timApp.directive("bootstrapPanel", ["$window", "$log", "$http", function($window, $log, $http) {
    "use strict";
    return {
        restrict: "E",
        transclude: true,
        scope: {
            title: "@?",
            closeFn: "&",
            showClose: "=?",
        },
        templateUrl: "/static/templates/bootstrapPanel.html",
        link($scope, $element) {

        },

        controller: ["$scope", "$element", "$attrs", function($scope, $element, $attrs) {
            const sc = $scope;

            sc.close = function() {
                $element.addClass("ng-hide");
                sc.closeFn();
            };
        }],
    };
}]);
