// from https://stackoverflow.com/a/14837021
import {timApp} from "tim/app";
import {$parse, $timeout} from "../ngimport";

timApp.directive("focusMe", [function() {
    "use strict";
    return {
        link(scope, element, attrs) {
            const model = $parse(attrs.focusMe);
            scope.$watch(model, function(value) {
                if (value === true) {
                    $timeout(function() {
                        element[0].focus();
                    });
                }
            });
            element.bind("blur", function() {
                // Catch "model.assign is not a function"
                try {
                    scope.$apply(model.assign(scope, false));
                } catch (e) {
                }
            });
        },
    };
}]);
