/* globals angular */
var app = angular.module('timApp');

app.directive('shortName', ['Slugify', function (Slugify) {
    "use strict";
    return {
        require: '?ngModel',
        scope: {
            allowSlash: '=?'
        },
        link: function (scope, elm, attrs, ctrl) {
            if (ctrl) {
                ctrl.$validators.shortName = function (modelValue, viewValue) {
                    if (ctrl.$isEmpty(modelValue)) {
                        return true;
                    }
                    if (scope.allowSlash) {
                        viewValue = viewValue.replace(/\//g, '');
                    }
                    return viewValue === Slugify.slugify(viewValue);
                };
            }
        }
    };
}]);
