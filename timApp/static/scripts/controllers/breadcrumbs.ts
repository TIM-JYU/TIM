/* globals angular */
var timApp = angular.module('timApp');

timApp.controller('Breadcrumbs', ['$scope', '$window', function (sc, $window) {
    "use strict";
    sc.crumbs = $window.breadcrumbs;
    sc.item = $window.item;
}]);
