var angular;
var timApp = angular.module('timApp');

timApp.controller('AllAnswersCtrl', ['$uibModalInstance', '$window', '$httpParamSerializer', '$localStorage', 'options', function ($uibModalInstance, $window, $httpParamSerializer, $localStorage, options) {
    "use strict";
    var $ctrl = this;
    $ctrl.options = {};
    $ctrl.options.age = 'max';
    $ctrl.options.valid = '1';
    $ctrl.options.name = 'both';
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
