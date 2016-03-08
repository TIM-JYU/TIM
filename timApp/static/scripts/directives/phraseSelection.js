/**
 * Created by sevitarv on 8.3.2016.
 * @module phraseSelection
 * @licence MIT
 * @copyright 2016 Timber project authors
 */

var angular;
var timApp = angular.module('timApp');

var colorPalette = ["blueviolet", "darkcyan", "orange", "darkgray", "lightblue", "coral", "goldenrod", "blue"];

/**
 * Angular directive for phrase selection
 */
timApp.directive('phraseSelection', function () {
    return{
        templateUrl: "/static/templates/phraseSelection.html",
        controller: 'PhraseSelectionController'
    }
});

/**
 * Controller for phrase selection
 */
timApp.controller('PhraseSelectionController', ['$scope', '$http', function ($scope, $http) {
    "use strict";

    // Data
    $scope.orderPhrase = 'tag';
    $scope.labelsOn = true;

    $http.get('http://192.168.99.100/static/test_data/phrases.json').success(function (data) {
        $scope.phrases = data;
        $scope.selectedPhrase = $scope.phrases[0];
    });
    $http.get('http://192.168.99.100/static/test_data/tags.json').success(function (data) {
        $scope.tags = data;
    });

    // Methods
    $scope.getColor = function (index) {
        return colorPalette[index];
    };

    $scope.toggleTag = function (tag) {
        tag.selected = !tag.selected;
    };

    $scope.toggleLabelShow = function () {
        $scope.labelsOn = !$scope.labelsOn;
    };

    $scope.showLabels = function(){
        if (!$scope.labelsOn)
            return "hide";
        return "";
    }

}]);


timApp.filter('selectedTags', function () {
    return function (phrases, tags) {
        var selectedPhrases = new Set();
        var selectedTags = [];
        if (typeof tags != "undefined" && typeof phrases != "undefined") {

            for (var i = 0; i < tags.length; i++) {
                if (tags[i].selected)
                    selectedTags.push(tags[i].id);
            }
            // return all phrases if no tags selected
            if (selectedTags.length == 0)
                return phrases;

            for (var i = 0; i < phrases.length; i++) {
                for (var j = 0; j < selectedTags.length; j++) {
                    if (phrases[i].tags.indexOf(selectedTags[j]) != -1)
                        selectedPhrases.add(phrases[i]);
                }
            }
        }
        return Array.from(selectedPhrases);
    };
});