
import {timApp} from "tim/app";
timApp.directive("bootstrapPanel", [function() {
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

        controller: ["$scope", "$element", function($scope, $element) {
            const sc = $scope;

            sc.close = function() {
                $element.addClass("ng-hide");
                sc.closeFn();
            };
        }],
    };
}]);
