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
    $scope.newPhrase = {"content": "", "points": 0, "labels": []};
    $scope.newLabel = {"content": "", "selected": true};

    //
    console.log($scope.extraData["docId"]);
    $http.get('/{0}/asd/velps'.replace('{0}',1)).success(function (data) {
        console.log(data);
        $scope.velps = data;
        $scope.selectedPhrase = $scope.velps[0];
    });

    $http.get('/static/test_data/tags.json').success(function (data) {
        $scope.labels = data;
    });

    // Methods

    /**
     * Get color for object.
     * @param index index of the color in color palette. (modulo by lenght of color palette)
     * @returns {string}
     */
    $scope.getColor = function (index) {
        return colorPalette[index % colorPalette.length];
    };

    /**
     * Toggles label selected attribute
     * @param tag label to toggle
     */
    $scope.toggleTag = function (tag) {
        tag.selected = !tag.selected;
    };

    /**
     * Toggles advanced view on and off
     */
    $scope.toggleAdvancedShow = function () {
        $scope.advancedOn = !$scope.advancedOn;
    };

    /**
     * Get correct CSS-style for advanced view
     * @returns {*}
     */
    $scope.getAdvancedStyle = function(){
        if (!$scope.advancedOn)
            return "hide";
        return "";
    };

    /**
     * Add new label on form submit
     */
    $scope.addLabel = function(form){
        var valid = form.$valid;
        $scope.labelAdded = true;
        if (!valid) return;

        form.$setPristine();
        var labelToAdd = {
            "id": $scope.labels.length,
            "content": $scope.newLabel["content"],
            "selected": $scope.newLabel["selected"]
        };

        $scope.resetNewLabel();
        $scope.labels.push(labelToAdd);
        $scope.labelAdded = false;
    };


    /**
     * Adds new phrase on form submit
     */
    $scope.addVelp = function(form) {
        var valid = form.$valid;
        $scope.submitted = true;
        if (!valid) return;

        // Form is valid:
        form.$setPristine();
        var phraseLabels = [];
        for (var i = 0; i < $scope.labels.length; i++) {
            if ($scope.labels[i].selected)
                phraseLabels.push($scope.labels[i].id);
        }

        var phraseToAdd = {
            "labels": phraseLabels,
            "used": 0,
            "id": $scope.velps.length,
            "default_points": $scope.newPhrase["default_points"],
            "content": $scope.newPhrase["content"]
        };

        $scope.resetNewPhrase();
        $scope.velps.push(phraseToAdd);
        $scope.submitted = false;
    };

    /**
     * Reset velp information
     */
    $scope.resetNewPhrase = function(){
        $scope.newPhrase = {"content": "", "default_points": 0, "labels": []};
    };

    /**
     * Reset label information
     */
    $scope.resetNewLabel = function(){
        $scope.newLabel = {"content": "", "selected": true};
    };
}]);

/**
 * Filter for ordering phrases
 */
timApp.filter('selectedTags', function () {
    return function (velps, labels) {
        var selectedVelps = {};
        var selectedLabels = [];
        if (typeof labels != "undefined" && typeof velps != "undefined") {

            for (var i = 0; i < labels.length; i++) {
                if (labels[i].selected)
                    selectedTags.push(labels[i].id);
            }

            // return all phrases if no tags selected
            if (selectedLabels.length == 0)
                return velps;

            for (var i = 0; i < velps.length; i++) {
                for (var j = 0; j < selectedTags.length; j++) {
                    if (velps[i].tags.indexOf(selectedTags[j]) != -1)
                        if (!(i in selectedVelps))
                            selectedVelps[i] = [velps[i],1];
                        else
                            selectedVelps[i][1] += 1;
                }
            }
        }

        var selectedArray = [];
        var returnPhrases = [];
        for (var p in selectedVelps){
            selectedArray.push(selectedVelps[p]);
        }

        selectedArray.sort(function(a, b) {return b[1] - a[1]});

        for (var s in selectedArray)
            returnPhrases.push(selectedArray[s][0]);

       // console.log(returngit Phrases);

        return returnPhrases;
    };
});