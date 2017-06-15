import {timApp} from "tim/app";
import {slugify} from "../services/slugify";

timApp.directive("timShortName", [function() {
    "use strict";
    return {
        require: "?ngModel",
        link(scope, elm, attrs, ctrl: any) {
            if (ctrl) {
                ctrl.$validators.timShortName = function(modelValue, viewValue) {
                    if (ctrl.$isEmpty(modelValue)) {
                        return true;
                    }
                    return viewValue.toLowerCase() === slugify(viewValue);
                };
            }
        },
    };
}]);

timApp.directive("timLocation", [function() {
    "use strict";
    return {
        require: "?ngModel",
        link(scope, elm, attrs, ctrl: any) {
            if (ctrl) {
                ctrl.$validators.timLocation = function(modelValue, viewValue) {
                    if (ctrl.$isEmpty(modelValue)) {
                        return true;
                    }
                    viewValue = viewValue.replace(/\//g, "");
                    return viewValue.toLowerCase() === slugify(viewValue);
                };
            }
        },
    };
}]);
