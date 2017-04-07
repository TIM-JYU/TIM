// from https://stackoverflow.com/a/14837021
import {timApp} from "tim/app";
timApp.directive('focusMe', ['$timeout', '$parse', function ($timeout, $parse) {
    "use strict";
    return {
        link: function (scope, element, attrs) {
            var model = $parse(attrs.focusMe);
            scope.$watch(model, function (value) {
                if (value === true) {
                    $timeout(function () {
                        element[0].focus();
                    });
                }
            });
            element.bind('blur', function () {
                // Catch "model.assign is not a function"
                try {
                    scope.$apply(model.assign(scope, false));
                } catch (e) {
                }
            });
        }
    };
}]);
