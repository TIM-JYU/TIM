var angular;
var timApp = angular.module('timApp');

timApp.directive("bookmarks", ['$window', '$log', '$http', function ($window, $log, $http) {
    "use strict";
    return {
        restrict: 'E',
        scope: {
            userId: '=?',
            data: '=?'
        },
        templateUrl: "/static/templates/bookmarks.html",
        link: function ($scope, $element) {

        },

        controller: function ($scope, $element, $attrs) {
            var sc = $scope;
            sc.data = [];

            sc.getFromServer = function (response) {
                sc.data = response.data;
            };

            sc.isSaveablePage = function () {
                return $window.location.pathname.indexOf('/view/') === 0;
            };

            sc.addCurrentPageToGroup = function (group) {
                var path = $window.location.pathname.slice('/view/'.length);
                var itemName = $window.prompt('Bookmark name:',
                    window.location.pathname.slice(window.location.pathname.lastIndexOf('/') + 1));
                if (!itemName) {
                    return;
                }
                $http.post('/bookmarks/add/' + group.name + '/' + itemName + '/' + path, {})
                    .then(sc.getFromServer, function (data, status, headers, config) {
                        $window.alert("Could not add bookmark.");
                    });
            };

            sc.deleteItem = function (group, item, e) {
                e.stopPropagation();
                e.preventDefault();
                $http.post('/bookmarks/delete/' + group.name + '/' + item.name, {})
                    .then(function (response) {
                        sc.getFromServer(response);
                        for (var i = 0; i < sc.data.length; ++i) {
                            if (sc.data[i].name === group.name) {
                                //console.log('opening');
                                sc.data[i].isOpen = true;
                                return;
                            }
                        }
                    }, function (data, status, headers, config) {
                        $window.alert("Could not delete bookmark.");
                    });
            };

            sc.newGroup = function () {
                var name = $window.prompt('Bookmark group name:');
                if (!name) {
                    return;
                }
                $http.post('/bookmarks/createGroup/' + name, {})
                    .then(sc.getFromServer, function (data, status, headers, config) {
                        $window.alert("Could not create bookmark group.");
                    });
            };

            sc.deleteGroup = function (group) {
                $http.post('/bookmarks/deleteGroup/' + group.name, {})
                    .then(sc.getFromServer, function (data, status, headers, config) {
                        $window.alert("Could not delete bookmark group.");
                    });
            };

            sc.toggleDelete = function (e) {
                e.stopPropagation();
                sc.deleting = !sc.deleting;
            };

            sc.deleting = false;

            if (sc.userId) {
                $http.get('/bookmarks/get/' + sc.userId).then(sc.getFromServer, function (data, status, headers, config) {
                    $window.alert("Could not fetch permissions.");
                });
            }
        }
    };
}]);
