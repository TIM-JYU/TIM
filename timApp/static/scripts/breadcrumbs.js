var angular;

var timApp = angular.module('timApp');

timApp.controller('Breadcrumbs', ['$scope', function (sc) {
    sc.splitPath = function (path) {
        var result = [];
        var relstart = 0;
        for (var i = 0; i < path.length; i++) {
            if (path[i] == '/') {
                result.push({name: path.substr(relstart, i - relstart), fullname: path.substr(0, i)});
                relstart = i + 1;
            }
        }
        if (relstart < i)
            result.push({name: path.substr(relstart, i), fullname: path.substr(0, i)});
        return result;
    };

    sc.getBreadcrumbs = function (folder) {
        var crumbs = sc.splitPath(folder);
        crumbs.unshift({name: '[root]', fullname: ''});
        return crumbs;
    };

    sc.crumbs = sc.getBreadcrumbs(docName);
}]);
