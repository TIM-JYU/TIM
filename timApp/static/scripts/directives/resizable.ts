var angular;
var timApp = angular.module('timApp');

timApp.directive('resizable', ['$window', function ($window) {
    var resizableConfig = {};

    return {
        restrict: 'A',
        scope: {
            callback: '&onResize'
        },
        link: function (scope, elem) {
            $window.setTimeout(function () {
                elem.resizable();
                elem.on('resizestop', function () {
                    if (scope.callback) scope.callback();
                });
            }, 200);
        }
    };
}]);
