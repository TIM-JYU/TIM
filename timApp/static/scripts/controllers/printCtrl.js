/**
 * Created by miimatku on 27.3.2017.
 */

var angular, $;
var timApp = angular.module('timApp');

timApp.controller("PrintCtrl", ['$scope', "$http", "$window", 'Users', '$log', '$uibModal', 'document', '$uibModalInstance', '$location', 'testaa',

    function ($scope, $http, $window, $uibModal, Users, $log, document, $uibModalInstance, $location, testaa) {
        var $ctrl = this;
        $ctrl.document = document;
        $ctrl.testaa = testaa;
        /**
         $scope.printDocument = function () {
            alert("testi");
        }
         */
        $ctrl.ok = function () {
            var locationProtocol = $location.protocol();
            var locationHost = $location.host();
            //console.log(proto);
            $window.open(locationProtocol + '://' + locationHost +  '/print/' + $ctrl.document.path, "_blank");
            //$window.open('http://www.google.com', "_blank");
        }


        $ctrl.cancel = function () {
            $uibModalInstance.dismiss('cancel');
        };

    }
])
;
