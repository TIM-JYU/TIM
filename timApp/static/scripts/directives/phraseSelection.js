/**
 * Created by sevitarv on 8.3.2016.
 * @module phraseSelection
 * @licence MIT
 * @copyright 2016 Timber project authors
 */

var angular;
var timApp = angular.module('timApp');

/**
 * Angular directive for phrase selection
 */
timApp.directive('phraseSelection', function () {
    return{
        templateUrl: "/static/templates/phraseSelection.html",
        controller: 'PhraseSelectionController'
    }
});

timApp.controller('PhraseSelectionController', ['$scope', '$http', function ($scope, $http) {
    "use strict";
    $scope.testi = "Oikea testi";

    $http.get('http://192.168.99.100/static/test_data/phrases.json').success(function (data) {
        $scope.phrases = data;
        $scope.selectedPhrase = $scope.phrases[0];
    });
    $http.get('http://192.168.99.100/static/test_data/tags.json').success(function (data) {
        $scope.tags = data;
    });

}]);
