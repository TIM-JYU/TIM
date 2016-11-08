var angular, moment;
var timApp = angular.module('timApp');

timApp.controller('AllAnswersCtrl', ['$uibModalInstance', '$window', '$httpParamSerializer', '$localStorage', '$http', '$log', 'options',
    function ($uibModalInstance, $window, $httpParamSerializer, $localStorage, $http, $log, options) {
        "use strict";
        moment.locale('en', {
            week: {dow: 1} // set Monday as the first day of the week
        });
        var $ctrl = this;

        $ctrl.datePickerOptionsFrom = {
            format: 'D.M.YYYY HH:mm:ss',
            showTodayButton: true
        };
        $ctrl.datePickerOptionsTo = {
            format: 'D.M.YYYY HH:mm:ss',
            defaultDate: moment().add({minutes: 10}),
            showTodayButton: true
        };
        $ctrl.showSort = options.allTasks;
        $ctrl.options = {};
        $ctrl.options.age = 'max';
        $ctrl.options.valid = '1';
        $ctrl.options.name = 'both';
        $ctrl.options.sort = 'username';
        $ctrl.$storage = $localStorage.$default({
            allAnswersOptions: $ctrl.options
        });

        $ctrl.options = $ctrl.$storage.allAnswersOptions;

        $ctrl.ok = function () {
            if ($ctrl.options.periodFrom) {
                $ctrl.options.periodFrom = $ctrl.options.periodFrom.toDate();
            }
            if ($ctrl.options.periodTo) {
                $ctrl.options.periodTo = $ctrl.options.periodTo.toDate();
            }
            $window.open(options.url + '?' + $httpParamSerializer($ctrl.options), '_blank');
            $uibModalInstance.close('close');
        };

        $ctrl.cancel = function () {
            $uibModalInstance.dismiss('cancel');
        };

        $ctrl.lastFetch = null;
        $http.get('/settings/get/last_answer_fetch').then(function (response) {
            if (response.data.last_answer_fetch) {
                $ctrl.lastFetch = response.data.last_answer_fetch[options.identifier];
                if ($ctrl.lastFetch) {
                    $ctrl.options.periodFrom = moment($ctrl.lastFetch);
                } else {
                    $ctrl.lastFetch = 'no fetches yet';
                }
            }
        }, function (response) {
            $log.error('Failed to fetch last answer time');
        });
    }]);
