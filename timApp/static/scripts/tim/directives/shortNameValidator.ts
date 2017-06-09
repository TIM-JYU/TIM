import {timApp} from "tim/app";

timApp.directive("timShortName", ["Slugify", function(Slugify) {
    "use strict";
    return {
        require: "?ngModel",
        link(scope, elm, attrs, ctrl: any) {
            if (ctrl) {
                ctrl.$validators.timShortName = function(modelValue, viewValue) {
                    if (ctrl.$isEmpty(modelValue)) {
                        return true;
                    }
                    return viewValue.toLowerCase() === Slugify.slugify(viewValue);
                };
            }
        },
    };
}]);

timApp.directive("timLocation", ["Slugify", function(Slugify) {
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
                    return viewValue.toLowerCase() === Slugify.slugify(viewValue);
                };
            }
        },
    };
}]);
