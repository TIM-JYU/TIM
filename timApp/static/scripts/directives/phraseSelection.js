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
    $scope.advancedOn = false;
    $scope.newPhrase = {"content": "", "points": 0, "tags": []};


    $http.get('/doc_id/par_id/velps').success(function (data) {
        $scope.phrases = data;
        $scope.selectedPhrase = $scope.phrases[0];
    });
    $http.get('/static/test_data/tags.json').success(function (data) {
        $scope.tags = data;
    });

    // Methods
    $scope.getColor = function (index) {
        return colorPalette[index];
    };

    $scope.toggleTag = function (tag) {
        tag.selected = !tag.selected;
    };

    $scope.toggleAdvancedShow = function () {
        $scope.advancedOn = !$scope.advancedOn;
    };

    $scope.getAdvancedStyle = function(){
        if (!$scope.advancedOn)
            return "hide";
        return "";
    };

    /**
     * Adds new phrase
     */
    $scope.addPhrase = function(){

        var phraseLabels = [];
        for (var i = 0; i < $scope.tags.length; i++) {
            if ($scope.tags[i].selected)
                phraseLabels.push($scope.tags[i].id);
        }

        console.log(phraseLabels);
        var phraseToAdd = {
            "tags": phraseLabels,
            "used": 0,
            "id": $scope.phrases.length,
            "points": $scope.newPhrase["points"],
            "content": $scope.newPhrase["content"]
        };
        $scope.resetNewPhrase();
        $scope.phrases.push(phraseToAdd);
    };

    $scope.resetNewPhrase = function(){
        $scope.newPhrase = {"content": "", "points": 0, "tags": []};
    }

}]);

/**
 * Filter for ordering phrases
 */
timApp.filter('selectedTags', function () {
    return function (phrases, tags) {
        var selectedPhrases = {};
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
                        if (!(i in selectedPhrases))
                            selectedPhrases[i] = [phrases[i],1];
                        else
                            selectedPhrases[i][1] += 1;
                }
            }
        }

        var selectedArray = [];
        var returnPhrases = [];
        for (var p in selectedPhrases){
            selectedArray.push(selectedPhrases[p]);
        }

        selectedArray.sort(function(a, b) {return b[1] - a[1]});

        for (var s in selectedArray)
            returnPhrases.push(selectedArray[s][0]);

       // console.log(returnPhrases);


        return returnPhrases;
    };
});