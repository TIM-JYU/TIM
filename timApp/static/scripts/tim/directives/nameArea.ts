
import $ from "jquery";
import {timApp} from "tim/app";

timApp.directive('noPeriod', function() {
    return function(scope, element, attrs) {

        var keyCode = [190, 188, 110]; // . keycode
        element.bind("keydown", function(event) {
            //console.log($.inArray(event.which,keyCode));
            if ($.inArray(event.which, keyCode) !== -1) {
                scope.$apply(function() {
                    scope.$eval(attrs.noPeriod);
                    event.preventDefault();
                });
                event.preventDefault();
            }

        });
    };
});

/**
 * A popup window directive that is used in the document view
 * when creating a new area.
 */
timApp.directive("nameArea", [function() {
    return {
        restrict: "E",
        scope: true, // we need functions from parent scope
        templateUrl: "/static/templates/nameArea.html",
        replace: true,

        link($scope: any, $element, $attrs) {
            $scope.$area = $element.parents(".area").first();
            if ($attrs.onclose) {
                $scope.onClose = eval("$scope." + $attrs.onclose);
            }
            if ($attrs.onok) {
                $scope.onOk = eval("$scope." + $attrs.onok);
            }
            if ($attrs.oncancel) {
                $scope.onCancel = eval("$scope." + $attrs.oncancel);
            }

            $("#areaname").keypress(function(e) {
                if (e.which == 13) {
                    $scope.addArea();
                }
            });
        },

        controller: ["$scope", "$element", function($scope, $element) {
            $scope.closePopup = function() {
                $scope.$destroy();
                $element.remove();

                if ($scope.onClose) {
                    $scope.onClose($scope.$area);
                }
            };

            $scope.addArea = function() {
                $scope.closePopup();

                if ($scope.onOk) {
                    $scope.onOk($scope.$area, $scope.areaName, $scope.options);
                }
            };

            $scope.cancelAdd = function() {
                $scope.closePopup();

                if ($scope.onCancel) {
                    $scope.onCancel($scope.$area);
                }
            };

            $scope.areaName = "";
            $scope.options = {collapse: true, hlevel: 0};
            $element.css("position", "absolute"); // IE needs this
            $scope.datePickerOptions = {
                format: "D.M.YYYY HH:mm:ss",
                showTodayButton: true,
            };

            $("#areaname").focus();
        }],
    };
}]);
