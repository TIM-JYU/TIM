/* globals angular */
var timApp = angular.module('timApp');

/**
 * Displays an error message for the given form element when it is invalid.
 */
timApp.directive("timErrorMessage", [function () {
    "use strict";
    return {
        restrict: 'E',
        scope: {
            for: '='
        },
        templateUrl: "/static/templates/formErrorMessage.html"
    };
}]);

/**
 * Adds 'has-error' class to the element if the given form element is invalid and dirty; otherwise removes it.
 */
timApp.directive("timErrorState", [function () {
    "use strict";
    return {
        restrict: 'A',
        scope: {
            for: '='
        },
        link: function ($scope, $element, $attrs) {
            $scope.$watch('for.$invalid', function (newVal, oldVal) {
                if (newVal && $scope.for.$dirty) {
                    $element.addClass('has-error');
                } else {
                    $element.removeClass('has-error');
                }
            });
        }
    };
}]);
