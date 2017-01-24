/* globals angular */
var app = angular.module('timApp');

app.directive('timShortName', ['Slugify', function (Slugify) {
    "use strict";
    return {
        require: '?ngModel',
        link: function (scope, elm, attrs, ctrl) {
            if (ctrl) {
                ctrl.$validators.timShortName = function (modelValue, viewValue) {
                    if (ctrl.$isEmpty(modelValue)) {
                        return true;
                    }
                    return viewValue.toLowerCase() === Slugify.slugify(viewValue);
                };
            }
        }
    };
}]);

app.directive('timLocation', ['Slugify', function (Slugify) {
    "use strict";
    return {
        require: '?ngModel',
        link: function (scope, elm, attrs, ctrl) {
            if (ctrl) {
                ctrl.$validators.timLocation = function (modelValue, viewValue) {
                    if (ctrl.$isEmpty(modelValue)) {
                        return true;
                    }
                    viewValue = viewValue.replace(/\//g, '');
                    return viewValue.toLowerCase() === Slugify.slugify(viewValue);
                };
            }
        }
    };
}]);
