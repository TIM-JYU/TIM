define(['require', 'exports', 'tim/app', 'moment', 'tim/directives/focusMe'], function (require, exports, app, moment) {

app.timApp.directive("rightsEditor", ['$window', '$log', '$http', function ($window, $log, $http) {
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
            sc.timeOpt = {};
            sc.selectedRight = null;
            sc.showActiveOnly = true;
            sc.timeOpt.type = 'always';
            sc.timeOpt.durationType = 'hours';
            sc.timeOpt.durationAmount = 4;
            sc.durationTypes = ['seconds', 'minutes', 'hours', 'days', 'weeks', 'months', 'years'];
            sc.datePickerOptionsFrom = {
                format: 'D.M.YYYY HH:mm:ss',
                defaultDate: moment(),
                showTodayButton: true
            };
            sc.datePickerOptionsTo = {
                format: 'D.M.YYYY HH:mm:ss',
                defaultDate: moment(),
                showTodayButton: true
            };
            sc.datePickerOptionsDurationFrom = {
                format: 'D.M.YYYY HH:mm:ss',
                showTodayButton: true
            };
            sc.datePickerOptionsDurationTo = {
                format: 'D.M.YYYY HH:mm:ss',
                showTodayButton: true
            };
            if (sc.accessTypes) {
                sc.accessType = sc.accessTypes[0];
            }

            sc.showAddRightFn = function (type) {
                sc.accessType = type;
                sc.selectedRight = null;
                sc.addingRight = true;
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

            sc.cancel = function () {
                sc.addingRight = false;
                sc.selectedRight = null;
            };

            sc.editingRight = function () {
                return sc.selectedRight !== null;
            };

            sc.addOrEditPermission = function (groupname, type) {
                $http.put('/' + sc.urlRoot + '/add/' + sc.itemId + '/' + groupname.split('\n').join(';') + '/' + type.name,
                    sc.timeOpt).success(
                    function (data, status, headers, config) {
                        sc.getPermissions();
                        sc.cancel();
                    }).error(function (data, status, headers, config) {
                    $window.alert(data.error);
                });
            };

            sc.getPlaceholder = function () {
                return 'enter username(s)/group name(s) separated by semicolons' + (sc.listMode ? ' or newlines' : '');
            };

            sc.getGroupDesc = function (group) {
                return group.fullname ? group.fullname + ' (' + group.name + ')' : group.name;
            };

            sc.shouldShowBeginTime = function (group) {
                // having -1 here (instead of 0) avoids "begins in a few seconds" right after adding a right
                return moment().diff(group.accessible_from, 'seconds') < -1;
            };

            sc.shouldShowEndTime = function (group) {
                return group.accessible_to !== null && moment().diff(group.accessible_to) <= 0;
            };

            sc.shouldShowEndedTime = function (group) {
                return group.accessible_to !== null && moment().diff(group.accessible_to) > 0;
            };

            sc.shouldShowDuration = function (group) {
                return group.duration !== null && group.accessible_from === null;
            };

            sc.shouldShowUnlockable = function (group) {
                return group.duration !== null &&
                    group.duration_from !== null &&
                    group.accessible_from === null &&
                    moment().diff(group.duration_from) < 0;
            };

            sc.shouldShowNotUnlockable = function (group) {
                return group.duration !== null &&
                    group.duration_to !== null &&
                    group.accessible_from === null &&
                    moment().diff(group.duration_to) <= 0;
            };

            sc.shouldShowNotUnlockableAnymore = function (group) {
                return group.duration !== null &&
                    group.duration_to !== null &&
                    group.accessible_from === null &&
                    moment().diff(group.duration_to) > 0;
            };

            sc.isObsolete = function (group) {
                return sc.shouldShowEndedTime(group) || sc.shouldShowNotUnlockableAnymore(group);
            };

            sc.obsoleteFilterFn = function (group) {
                return !sc.showActiveOnly || !sc.isObsolete(group);
            };

            sc.showClock = function (group) {
                return group.duration !== null || group.accessible_to !== null;
            };

            // TODO make duration editor its own component
            sc.$watchGroup(['timeOpt.durationAmount', 'timeOpt.durationType'], function (newValues, oldValues, scope) {
                sc.timeOpt.duration = moment.duration(sc.timeOpt.durationAmount, sc.timeOpt.durationType);
            });

            sc.expireRight = function (group) {
                sc.editRight(group);
                sc.timeOpt.to = moment();
                sc.timeOpt.type = 'range';
                sc.addOrEditPermission(group.name, sc.accessType);
            };

            sc.editRight = function (group) {
                sc.groupName = group.name;
                sc.accessType = {id: group.access_type, name: group.access_name};
                sc.addingRight = false;
                sc.selectedRight = group;

                if (group.duration_from) {
                    sc.timeOpt.durationFrom = moment(group.duration_from);
                } else {
                    sc.timeOpt.durationFrom = null;
                }
                if (group.duration_to) {
                    sc.timeOpt.durationTo = moment(group.duration_to);
                } else {
                    sc.timeOpt.durationTo = null;
                }

                if (group.accessible_from) {
                    sc.timeOpt.from = moment(group.accessible_from);
                } else {
                    sc.timeOpt.from = null;
                }
                if (group.accessible_to) {
                    sc.timeOpt.to = moment(group.accessible_to);
                } else {
                    sc.timeOpt.to = null;
                }

                if (group.duration && group.accessible_from === null) {
                    var d = moment.duration(group.duration);
                    sc.timeOpt.type = 'duration';
                    for (var i = sc.durationTypes.length - 1; i >= 0; --i) {
                        var amount = d.as(sc.durationTypes[i]);
                        if (parseInt(amount) === amount || i === 0) {
                            // preserve last duration type choice if the amount is zero
                            if (amount !== 0) {
                                sc.timeOpt.durationType = sc.durationTypes[i];
                            }
                            sc.timeOpt.durationAmount = amount;
                            break;
                        }
                    }
                } else {
                    sc.timeOpt.type = 'range';
                }
            };

            sc.getPermissions();
        }
    };
}]);
});
