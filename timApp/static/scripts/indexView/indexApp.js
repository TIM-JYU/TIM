var indexApp = angular.module('indexApp', ['ngSanitize', 'controller', 'angularFileUpload']);
indexApp.directive('bindOnce', function () {
    return {
        scope: true,
        link: function ($scope) {
            setTimeout(function () {
                $scope.$destroy();
            }, 0);
        }
    }
});
indexApp.filter('escape', function () {
    return function (str) {
        return encodeURIComponent(str).replace('%2F', '/');
    };
});
