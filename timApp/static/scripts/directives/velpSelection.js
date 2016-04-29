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
    $scope.newVelp = {content: "", points: "", labels: [], edit: false, id:-2};
    $scope.velpToEdit = {content: "", points: "", labels: [], edit: false, id:-1};
    $scope.newLabel = {content: "", selected: true, edit: false};
    $scope.labelToEdit = {content: "", selected: false, edit: false};
    $scope.selectedLabels = [];
    $scope.previouslySelectedLabels = [];
    $scope.lockedVelps = [];


    // Dictionaries for easier searching: Velp ids? Label ids? Annotation ids?

    var doc_id = $scope.docId;
    var par = 0;
    var default_velp_group = 0; // TODO Use route to add this information

    var new_velp_id = 0; // get latest velp id
    var new_label_id = 0; // get latest label id

    // $scope.filteredVelpCount = 0;

    // Get velp and annotation data
    $http.get('/{0}/{1}/velps'.replace('{0}',doc_id).replace('{1}', par)).success(function (data) {
        $scope.velps = data;
        $scope.velps.forEach(function (v) {
            v.used = 0;
            v.edit = false;
            if (v.labels === undefined)
                v.labels = [];
        });

        $http.get('/{0}/annotations'.replace('{0}', doc_id)).success(function (data) {
            $scope.annotations = data;
            //$scope.loadAnnotations();
        });

        $http.get('/{0}/defaultvelpgroup'.replace('{0}', doc_id)).success(function (data){
            console.log(data[0].id);
            default_velp_group = data[0].id;
        });
    });

    // Get label data
    $http.get('/{0}/labels'.replace('{0}',doc_id)).success(function (data) {
        $scope.labels = data;
        $scope.labels.forEach(function (l) {
            l.edit = false;
            l.selected = false;
        });
    });


    // Methods

    /**
     * Get total number of points
     * @returns {number}
     */
    $scope.getTotalPoints = function () {
        var p = 0;
        if ($scope.annotations === undefined)
            return p;

        for (var i = 0; i < $scope.annotations.length; i++) {
            p += $scope.annotations[i].points;
        }
        return p;
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
    $scope.toggleLabel = function (label) {
        label.selected = !label.selected;
        var labelIndex = $scope.selectedLabels.indexOf(label.id);
        if (labelIndex < 0){
            $scope.selectedLabels.push(label.id);
        } else {
            $scope.selectedLabels.splice(labelIndex, 1);
        }
        if ($scope.velpToEdit.edit){
            $scope.velpToEdit.labels = $scope.selectedLabels;
        }
    };

    /**
     * lock velps in list
     * @param velps velps to lock
     */
    $scope.lockVelps = function(velps) {
        if (!$scope.velpToEdit.edit){
            $scope.lockedVelps = velps;
        }
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

        var velpToAdd = {
            labels: $scope.selectedLabels.splice(0),
            used: 0,
            points: $scope.newVelp["points"],
            content: $scope.newVelp["content"],
            language_id: "FI",
            icon_id: null,
            valid_until: null,
            velp_groups: [default_velp_group]  // TODO: Change to default group, add choices where to add
        };

        $scope.makePostRequest("/addvelp", velpToAdd, function (json) {
            velpToAdd.id = parseInt(json.data);
            $scope.resetNewVelp();
            $scope.velps.push(velpToAdd);
            $scope.submitted = false;
            $scope.resetLabels();
        });
    };


    $scope.deselectLabels = function(){
        for (var i = 0; i < $scope.labels.length; i++) {
            if ($scope.labels[i].selected) {
                $scope.toggleLabel($scope.labels[i]);
            }
        }
    };

    $scope.selectLabelToEdit = function(label){
        if (label.id == $scope.labelToEdit.id){
            label.edit = false;
            $scope.labelToEdit = {content: "", selected: false, edit: false};
            return;
        }

        if ($scope.labelToEdit.edit) {
            $scope.labelTo.edit = false;
            for (var i=0; i<$scope.labels.length; i++){
               $scope.labels[i].edit = false;
            }
        }

        label.edit = true;
        $scope.labelToEdit = Object.create(label);
    };

    $scope.selectVelpToEdit = function(velp){


        if (velp.id == $scope.velpToEdit.id){
            velp.edit = false;
            $scope.velpToEdit = {content: "", points: "", labels: [], edit: false};
            return
        }

        if ($scope.velpToEdit.edit) {
            $scope.velpToEdit.edit = false;
            for (var i=0; i<$scope.velps.length; i++){
               $scope.velps[i].edit = false;
            }
            $scope.newVelp.edit = false;
        }

        velp.edit = true;
        $scope.velpToEdit = Object.create(velp);

        /*
        if (velp.labels !== undefined) {
            for (var i = 0; i < velp.labels.length; i++) {
                $scope.toggleLabel($scope.getLabelById(velp.labels[i]));
            }
        }
        */
    };

    $scope.editVelp = function(form){
        var valid = form.$valid;
        $scope.submitted = true;
        if (!valid) return;

        // Form is valid
        form.$setPristine();

        for (var i=0; i<$scope.velps.length; i++){
            if ($scope.velps[i].id == $scope.velpToEdit.id){
                $scope.velps[i] = $scope.velpToEdit;
                $scope.velps[i].edit = false;
                $scope.velps[i].labels = $scope.selectedLabels.slice(0);
                break;
            }
        }
        /*
        $scope.makePostRequest("/editvelp", form, function () {
        });
        */
        $scope.deselectLabels();
    };

    /**
     * Get velp by its id
     * @param id velp to find
     * @returns velp or undefined
     */
    $scope.getLabelById = function(id){
        for (var i=0; i<$scope.labels.length; i++)
            if ($scope.labels[i].id == id)
                return $scope.labels[i];

        return null;
    };

    /**
     * Reset new velp information
     */
    $scope.resetNewVelp = function(){
        $scope.newVelp = {content: "", points: "", labels: [], edit: false, id:-2};
    };

    /**
     * Reset new label information
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
timApp.filter('filterByLabels', function () {
    return function (velps, labels, lockedVelps, advancedOn) {
        var selectedVelps = {};
        var selectedLabels = [];

        if (!advancedOn)
            return velps;

        if (labels !== undefined) {
            for (var i = 0; i < labels.length; i++) {
                if (labels[i].selected)
                    selectedLabels.push(labels[i].id);
            }
        }

        if (velps !== undefined) {
            for (var i = 0; i < velps.length; i++) {
                // return all velps if velp is under edit
                /*if (velps[i].edit){
                 return lockedVelps;
                 }*/

                for (var j = 0; j < selectedLabels.length; j++) {
                    if (typeof velps[i].labels != "undefined" && velps[i].labels.indexOf(selectedLabels[j]) != -1)
                        if (!(i in selectedVelps))
                            selectedVelps[i] = [velps[i], 1];
                        else
                            selectedVelps[i][1] += 1;
                }
            }
        }

        // return all velps if no labels selected
        if (selectedLabels.length == 0)
            return velps;

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