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

    var doc_id = $scope.extraData["docId"];
    var par = $scope.extraData["par"];
    //
    console.log($scope.extraData);
    $http.get('/{0}/{1}/velps'.replace('{0}',doc_id).replace('{1}', par)).success(function (data) {
        $scope.velps = data;
        $scope.selectedPhrase = $scope.velps[0];
    });

    $http.get('/{0}/labels'.replace('{0}',doc_id)).success(function (data) {
        $scope.labels = data;
        $scope.labels.forEach(function (l) {
            l.selected = false;
        });
    });

    // Methods

    var makePostRequest = function(velp){
        $http({
        method: 'POST',
        url: url,
        headers: {'Content-Type': 'application/x-www-form-urlencoded'},
        transformRequest: function(obj) {
            var str = [];
            for(var p in obj)
                str.push(encodeURIComponent(p) + "=" + encodeURIComponent(obj[p]));
            return str.join("&");
        },
        data: {username: $scope.userName, password: $scope.password}
        }).success(function () {});
    };

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
     * @param label label to toggle
     */
    $scope.toggleTag = function (label) {
        label.selected = !label.selected;
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
     * Adds new label
     * @param form form information
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
            "points": $scope.newPhrase["points"],
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
        $scope.newPhrase = {"content": "", "points": 0, "labels": []};
    };

    /**
     * Reset label information
     */
    $scope.resetNewLabel = function(){
        $scope.newLabel = {"content": "", "selected": true};
    };
}]);

/**
 * Filter for ordering velps
 */
timApp.filter('selectedLabels', function () {
    return function (velps, labels) {
        var selectedVelps = {};
        var selectedLabels = [];
        if (typeof labels != "undefined" && typeof velps != "undefined") {

            for (var i = 0; i < labels.length; i++) {
                if (labels[i].selected)
                    selectedLabels.push(labels[i].id);
            }

            // return all phrases if no tags selected
            if (selectedLabels.length == 0)
                return velps;

            for (var i = 0; i < velps.length; i++) {
                for (var j = 0; j < selectedLabels.length; j++) {

                    if (typeof velps[i].labels != "undefined" && velps[i].labels.indexOf(selectedLabels[j]) != -1)
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