/**
 * Created by sevitarv on 8.3.2016.
 * @module velpSelection
 * @licence MIT
 * @copyright 2016 Timber project authors
 */

var angular;
var timApp = angular.module('timApp');

var colorPalette = ["blueviolet", "darkcyan", "orange", "darkgray", "cornflowerblue", "coral", "goldenrod", "blue"];

/**
 * Angular directive for velp selection
 */
timApp.directive('velpSelection', function () {
    return{
        templateUrl: "/static/templates/velpSelection.html",
        controller: 'VelpSelectionController'
    }
});

/**
 * Controller for velp selection
 */
timApp.controller('VelpSelectionController', ['$scope', '$http', function ($scope, $http) {
    "use strict";

    // Data
    $scope.orderVelp = 'label';
    $scope.advancedOn = false;
    $scope.newVelp = {content: "", points: 0, labels: []};
    $scope.newLabel = {content: "", selected: true};

    var doc_id = $scope.docId;
    var par = 0;

    var new_velp_id = 0; // get latest velp id
    var new_label_id = 0; // get latest label id

    $scope.filteredVelpCount = 0;

    // Get velp and annotation data
    $http.get('/{0}/{1}/velps'.replace('{0}',doc_id).replace('{1}', par)).success(function (data) {
        $scope.velps = data;
        $scope.velps.forEach(function (v) {
            v.used = 0;
        });

        $http.get('/static/test_data/markings.json').success(function (data) {
            $scope.annotations = data;
            $scope.loadAnnotations();
        });
    });

    // Get label data
    $http.get('/{0}/labels'.replace('{0}',doc_id)).success(function (data) {
        $scope.labels = data;
        var i = 0;
        $scope.labels.forEach(function (l) {
            l.selected = false;
        });
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
     * @param label label to toggle
     */
    $scope.toggleLabel = function (label) {
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
            content: $scope.newLabel["content"],
            language_id: "FI", // TODO: Change to user lang
            selected: false
        };
        $scope.makePostRequest("/addlabel", labelToAdd, function (json) {
            labelToAdd.id = parseInt(json.data);
            console.log(labelToAdd);
            $scope.resetNewLabel();
            $scope.labels.push(labelToAdd);
            $scope.labelAdded = false;
        });
    };


    /**
     * Adds new velp on form submit
     * @param form form information
     */
    $scope.addVelp = function(form) {
        var valid = form.$valid;
        $scope.submitted = true;
        if (!valid) return;

        // Form is valid:
        form.$setPristine();
        var velpLabels = [];
        for (var i = 0; i < $scope.labels.length; i++) {
            if ($scope.labels[i].selected)
                velpLabels.push($scope.labels[i].id);
        }

        var velpToAdd = {
            labels: velpLabels,
            used: 0,
            points: $scope.newVelp["points"],
            content: $scope.newVelp["content"],
            language_id: "FI"
        };

        $scope.makePostRequest("/addvelp", velpToAdd, function (data) {
            velpToAdd.id = data;
            $scope.resetNewVelp();
            $scope.velps.push(velpToAdd);
            $scope.submitted = false;
            $scope.resetLabels();
        });
    };

    /**
     * Reset velp information
     */
    $scope.resetNewVelp = function(){
        $scope.newVelp = {"content": "", "points": 0, "labels": []};
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

            // return all velps if no labels selected
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
        var returnVelps = [];

        for (var sv in selectedVelps){
            if (selectedVelps.hasOwnProperty(sv))
                selectedArray.push(selectedVelps[sv]);
        }

        selectedArray.sort(function(a, b) {return b[1] - a[1]});

        for (var i=0; i<selectedArray.length; i++)
            returnVelps.push(selectedArray[i][0]);

        return returnVelps;
    };
});