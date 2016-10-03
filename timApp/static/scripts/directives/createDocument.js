var angular;
var timApp = angular.module('timApp');

timApp.directive("createDocument", ['$window', '$log', '$http', function ($window, $log, $http) {
    "use strict";
    return {
        restrict: 'E',
        scope: {
            docName: '@?',
            docLocation: '@?'
        },
        templateUrl: "/static/templates/createDocument.html",
        link: function ($scope, $element) {

        },

        controller: function ($scope, $element, $attrs) {
            var sc = $scope;

            sc.alerts = [];

            sc.createDocument = function (name) {
                $http.post('/createDocument', {
                    "doc_name": sc.docLocation + '/' + sc.docName
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
