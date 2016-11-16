var angular, item;

var timApp = angular.module('timApp');

timApp.controller('Breadcrumbs', ['$scope', function (sc) {
    "use strict";
    sc.splitPath = function (path) {
        var result = [];
        var relstart = 0;
        for (var i = 0; i < path.length; i++) {
            if (path[i] == '/') {
                result.push({name: path.substr(relstart, i - relstart), path: path.substr(0, i)});
                relstart = i + 1;
            }
        }
        if (relstart < i)
            result.push({name: path.substr(relstart, i), path: path.substr(0, i)});
        return result;
    };

    sc.getBreadcrumbs = function (folder) {
        var crumbs = sc.splitPath(folder);
        crumbs.unshift({name: 'All documents', path: ''});
        return crumbs;
    };

    sc.crumbs = sc.getBreadcrumbs(item.path);
}]);
