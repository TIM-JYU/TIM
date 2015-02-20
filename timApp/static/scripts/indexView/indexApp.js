var angular;
var indexApp = angular.module('indexApp', ['ngSanitize', 'controller', 'angularFileUpload']);

indexApp.filter('escape', function () {
    "use strict";
    return function (str) {
        return encodeURIComponent(str).replace(/%2F/g, '/');
    };
});
