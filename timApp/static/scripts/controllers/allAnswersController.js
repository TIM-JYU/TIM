var angular, moment;
var timApp = angular.module('timApp');

timApp.controller('AllAnswersCtrl', ['$uibModalInstance', '$window', '$httpParamSerializer', '$localStorage', 'options', function ($uibModalInstance, $window, $httpParamSerializer, $localStorage, options) {
    "use strict";
    var $ctrl = this;
    $ctrl.datePickerOptionsFrom = {
        format: 'D.M.YYYY HH:mm:ss',
        defaultDate: moment().subtract({weeks: 1})
    };
    $ctrl.datePickerOptionsTo = {
        format: 'D.M.YYYY HH:mm:ss',
        defaultDate: moment().add({minutes: 10})
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
        $window.open(options.url + '?' + $httpParamSerializer($ctrl.options), '_blank');
        $uibModalInstance.close('close');
    };

    $ctrl.cancel = function () {
        $uibModalInstance.dismiss('cancel');
    };
}]);
