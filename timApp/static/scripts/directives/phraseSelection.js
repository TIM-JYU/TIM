/**
 * Created by sevitarv on 8.3.2016.
 * @module phraseSelection
 * @licence MIT
 * @copyright 2016 Timber project authors
 */

var angular;
var timApp = angular.module('timApp');

var colorPalette = ["blueviolet", "darkcyan", "orange", "darkgray", "cornflowerblue", "coral", "goldenrod", "blue"];

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

    /*
    var doc_id = $scope.extraData["docId"];
    var par = $scope.extraData["par"];
    */
    var doc_id = $scope.docId;
    var par = 0;

    var new_velp_id = 0; // get latest velp id
    var new_label_id = 0; // get latest label id

    $scope.filteredVelpCount = 0;

    console.log($scope.extraData);

    // Get velp data
    $http.get('/{0}/{1}/velps'.replace('{0}',doc_id).replace('{1}', par)).success(function (data) {
        $scope.velps = data;
        $scope.velps.forEach(function (v) {
            v.used = 0;
            if (v.id > new_velp_id)
                new_velp_id = v.id
        });
        new_velp_id++;
        console.log($scope.velps);
    });

    // Get label data
    $http.get('/{0}/labels'.replace('{0}',doc_id)).success(function (data) {
        $scope.labels = data;
        $scope.labels.forEach(function (l) {
            l.selected = false;
            if (l.id > new_label_id)
                new_label_id = l.id
        });
        new_label_id++;
    });

    // Methods

    $scope.makePostRequest = function(url, params){
        console.log("testi2");
        $http({
        method: 'POST',
        url: url,
        params: params}).success(function () {
            console.log("sent post to: " + url);
        });
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
            "id": new_label_id,
            "content": $scope.newLabel["content"],
            "selected": $scope.newLabel["selected"]
        };
        $scope.makePostRequest("/addLabel", labelToAdd);

        new_label_id++;
        $scope.resetNewLabel();
        $scope.labels.push(labelToAdd);
        $scope.labelAdded = false;
    };


    /**
     * Adds new phrase on form submit
     * @param form form information
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

        var velpToAdd = {
            "labels": phraseLabels,
            "used": 0,
            "id": ++new_velp_id,
            "points": $scope.newPhrase["points"],
            "content": $scope.newPhrase["content"],
            "language_id": "FI"
        };

        $scope.makePostRequest("/addvelp", velpToAdd);

        $scope.resetNewVelp();
        $scope.velps.push(velpToAdd);

        $scope.submitted = false;

        $scope.resetLabels();
    };

    /**
     * Reset velp information
     */
    $scope.resetNewVelp = function(){
        $scope.newPhrase = {"content": "", "points": 0, "labels": []};
    };

    /**
     * Reset label information
     */
    $scope.resetNewLabel = function(){
        $scope.newLabel = {"content": "", "selected": true};
    };

    /**
     * Set all labels not selected
     */
    $scope.resetLabels = function() {
        for (var i=0;i<$scope.labels.length; i++)
            $scope.labels[i].selected = false;
    }
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