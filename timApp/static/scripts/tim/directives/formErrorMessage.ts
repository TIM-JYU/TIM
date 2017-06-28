import {IAttributes, IRootElementService, IScope} from "angular";
import {timApp} from "tim/app";

/**
 * Displays an error message for the given form element when it is invalid.
 */
timApp.component("timErrorMessage", {
    bindings: {
        for: "=",
    },
    templateUrl: "/static/templates/formErrorMessage.html",
});

/**
 * Adds 'has-error' class to the element if the given form element is invalid and dirty; otherwise removes it.
 */
timApp.directive("timErrorState", [function() {
    return {
        restrict: "A",
        scope: {
            for: "=",
        },
        link($scope: IScope, $element: IRootElementService, $attrs: IAttributes) {
            $scope.$watch("for.$invalid", (newVal, oldVal) => {
                if (newVal && $scope.for.$dirty) {
                    $element.addClass("has-error");
                } else {
                    $element.removeClass("has-error");
                }
            });
        },
    };
}]);
