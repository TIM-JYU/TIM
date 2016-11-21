var angular;
var timApp = angular.module('timApp');

timApp.directive("createItem", ['$window', '$log', '$http', function ($window, $log, $http) {
    "use strict";
    return {
        restrict: 'E',
        scope: {
            itemType: '@', // folder or document
            itemName: '@?',
            itemLocation: '@?',
            fullPath: '@?'
        },
        templateUrl: "/static/templates/createItem.html",
        link: function ($scope, $element) {

        },

        controller: function ($scope, $element, $attrs) {
            var sc = $scope;

            if (sc.fullPath) {
                var str = sc.fullPath;
                sc.itemLocation = str.substring(0, str.lastIndexOf("/"));
                sc.itemName = str.substring(str.lastIndexOf("/") + 1, str.length);
            }

            sc.alerts = [];

            sc.createItem = function (name) {
                $http.post('/createItem', {
                    "item_path": sc.itemLocation + '/' + sc.itemName,
                    "item_type": sc.itemType
                }).then(function (response) {
                    $window.location.href = "/view/" + response.data.name;
                }, function (response) {
                    sc.alerts = [];
                    sc.alerts.push({msg: response.data.error, type: 'danger'});
                });
            };

            sc.closeAlert = function (index) {
                sc.alerts.splice(index, 1);
            };
        }
    };
}]);
