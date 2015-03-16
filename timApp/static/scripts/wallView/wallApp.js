/**
 * Created by hajoviin on 24.2.2015.
 */
var wallApp = angular.module('wallApp', ['ngSanitize', 'controller' ]);
wallApp.directive('bindOnce', function () {
    return {
        scope: true,
        link: function ($scope) {
            setTimeout(function () {
                $scope.$destroy();
            }, 0);
        }
    }
});
wallApp.filter('escape', function () {
    return function (str) {
        return encodeURIComponent(str).replace('%2F', '/');
    };
});