var angular;
var timApp = angular.module('timApp');

timApp.directive("rightsEditor", ['$window', '$log', '$http', function ($window, $log, $http) {
    "use strict";
    return {
        restrict: 'E',
        scope: {
            itemId: '=?',
            urlRoot: '@?',
            accessTypes: '=?',
            control: '=?'
        },
        templateUrl: "/static/templates/rightsEditor.html",
        link: function ($scope, $element) {

        },

        controller: function ($scope, $element, $attrs) {
            var sc = $scope;
            sc.internalControl = sc.control || {};
            sc.grouprights = [];
            if (sc.accessTypes) {
                sc.accessType = sc.accessTypes[0];
            }

            sc.showAddRightFn = function (type) {
                sc.accessType = type;
                sc.showAddRight = true;
                sc.focusEditor = true;
            };

            sc.removeConfirm = function (group, type) {
                if ($window.confirm("Remove " + type + " right from " + group.name + "?")) {
                    sc.removePermission(group, type);
                }
            };

            sc.getPermissions = function () {
                if (!sc.urlRoot || !sc.itemId) {
                    return;
                }
                $http.get('/' + sc.urlRoot + '/get/' + sc.itemId).success(function (data, status, headers, config) {
                    sc.grouprights = data.grouprights;
                    if (data.accesstypes) {
                        sc.accessTypes = data.accesstypes;
                        if (!sc.accessType) {
                            sc.accessType = sc.accessTypes[0];
                        }
                    }
                }).error(function (data, status, headers, config) {
                    $window.alert("Could not fetch permissions.");
                });
            };

            sc.removePermission = function (right, type) {
                $http.put('/' + sc.urlRoot + '/remove/' + sc.itemId + '/' + right.gid + '/' + type, {}).success(
                    function (data, status, headers, config) {
                        sc.getPermissions();
                    }).error(function (data, status, headers, config) {
                    $window.alert(data.error);
                });
            };

            sc.addPermission = function (groupname, type) {
                $http.put('/' + sc.urlRoot + '/add/' + sc.itemId + '/' + groupname.split('\n').join(';') + '/' + type.name, {}).success(
                    function (data, status, headers, config) {
                        sc.getPermissions();
                        sc.showAddRight = false;
                    }).error(function (data, status, headers, config) {
                    $window.alert(data.error);
                });
            };

            sc.getPlaceholder = function () {
                return 'enter username(s)/group name(s) separated by semicolons' + (sc.listMode ? ' or newlines' : '');
            };

            sc.getPermissions();
        }
    };
}]);
