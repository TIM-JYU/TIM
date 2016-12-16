var angular;
var timApp = angular.module('timApp');

timApp.directive("createItem", ['$window', '$log', '$http', function ($window, $log, $http) {
    "use strict";
    return {
        restrict: 'E',
        // TODO add itemTitle
        scope: {
            itemType: '@', // folder or document
            itemName: '@?',
            itemLocation: '@?',
            fullPath: '@?',
            params: '=?' // any additional parameters to be sent to server
        },
        templateUrl: "/static/templates/createItem.html",
        link: function ($scope, $element) {

        },

        controller: function ($scope, $element, $attrs) {
            var sc = $scope;

            // from https://stackoverflow.com/a/5782563
            sc.slugify = function (str) {
                str = str.replace(/^\s+|\s+$/g, ''); // trim
                str = str.toLowerCase();

                // remove accents, swap ñ for n, etc.
                var f = "ãàáäâåẽèéëêìíïîõòóöôùúüûñç·/,:;";
                var t = "aaaaaaeeeeeiiiiooooouuuunc-----";
                for (var i = 0, l = f.length; i < l; i++) {
                    str = str.replace(new RegExp(f.charAt(i), 'g'), t.charAt(i));
                }

                str = str.replace(/[^a-z0-9 _-]/g, '') // remove invalid chars
                    .replace(/\s+/g, '-') // collapse whitespace and replace by -
                    .replace(/-+/g, '-'); // collapse dashes

                return str;
            };

            sc.automaticShortName = true;

            if (sc.fullPath) {
                var str = sc.fullPath;
                sc.itemLocation = str.substring(0, str.lastIndexOf("/"));
                sc.itemTitle = str.substring(str.lastIndexOf("/") + 1, str.length);
                sc.itemName = sc.slugify(sc.itemTitle);
            }

            sc.alerts = [];

            sc.createItem = function () {
                $http.post('/createItem', angular.extend({
                    "item_path": sc.itemLocation + '/' + sc.itemName,
                    "item_type": sc.itemType,
                    "item_title": sc.itemTitle
                }, sc.params)).then(function (response) {
                    $window.location.href = "/view/" + response.data.path;
                }, function (response) {
                    sc.alerts = [];
                    sc.alerts.push({msg: response.data.error, type: 'danger'});
                });
            };

            sc.closeAlert = function (index) {
                sc.alerts.splice(index, 1);
            };

            sc.titleChanged = function () {
                if (!sc.automaticShortName) {
                    return;
                }
                sc.itemName = sc.slugify(sc.itemTitle);
            };

            sc.nameChanged = function () {
                sc.automaticShortName = sc.itemName.length === 0;
            };
        }
    };
}]);
