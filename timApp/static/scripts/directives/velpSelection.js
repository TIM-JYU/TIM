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
    return {
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
    $scope.newVelp = {content: "", points: "", labels: [], edit: false, id: -2, velp_groups:[]};
    $scope.velpToEdit = {content: "", points: "", labels: [], edit: false, id: -1};
    $scope.newLabel = {content: "", selected: false, edit: false, valid: true};
    $scope.labelToEdit = {content: "", selected: false, edit: false, id: -3};
    $scope.selectedLabels = [];
    $scope.settings = {selectedAllGroups: false};


    // Dictionaries for easier searching: Velp ids? Label ids? Annotation ids?
    var doc_id = $scope.docId;
    var par = 0;
    var default_velp_group = 1; // TODO Use route to add this information

    var new_velp_id = 0; // get latest velp id
    var new_label_id = 0; // get latest label id
    $scope.annotations = [];
    // $scope.filteredVelpCount = 0;

    // Get default velpgroup data
    $http.get('/{0}/get_default_velp_group'.replace('{0}', doc_id)).success(function (data) {
        console.log("Test");
        default_velp_group = data;
        console.log(default_velp_group);

        // Get velp and annotation data
        $http.get('/{0}/get_velps'.replace('{0}', doc_id)).success(function (data) {
            $scope.velps = data;
            $scope.velps.forEach(function (v) {
                v.used = 0;
                v.edit = false;
                if (v.labels === undefined)
                    v.labels = [];
            });
            console.log($scope.velps);

            $http.get('/{0}/get_annotations'.replace('{0}', doc_id)).success(function (data) {
                $scope.annotations = data;
                $scope.loadDocumentAnnotations();
            });

            // Get velpgroup data
            $http.get('/{0}/get_velp_groups'.replace('{0}', doc_id)).success(function (data) {
                $scope.velpGroups = data;
                console.log($scope.velpGroups);
                $scope.velpGroups.forEach(function(g) {
                    g.default = false;
                    if (g.id == default_velp_group.id)
                        g.default = true;
                })
            });

            // Get label data
            $http.get('/{0}/get_velp_labels'.replace('{0}', doc_id)).success(function (data) {
                $scope.labels = data;
                $scope.labels.forEach(function (l) {
                    l.edit = false;
                    l.selected = false;
                });
            });

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
        var labelIndex = $scope.selectedLabels.indexOf(label.id);
        if (labelIndex < 0) {
            $scope.selectedLabels.push(label.id);
        } else {
            $scope.selectedLabels.splice(labelIndex, 1);
        }

    };

    $scope.toggleLabelToEdit = function(label){
        label.edit = !label.edit;
    };

    /**
     * Toggles advanced view on and off
     */
    $scope.toggleAdvancedShow = function () {
        $scope.advancedOn = !$scope.advancedOn;
    };

    /**
     * Adds new label
     * @param velp velp where label is added
     */
    $scope.addLabel = function (velp) {

        if ($scope.newLabel.content.length < 1){
            $scope.newLabel.valid = false;
            return;
        }

        var labelToAdd = {
            content: $scope.newLabel.content,
            language_id: "FI", // TODO: Change to user lang
            selected: false
        };

        $scope.makePostRequest("/add_velp_label", labelToAdd, function (json) {
            labelToAdd.id = parseInt(json.data);
            $scope.resetNewLabel();
            $scope.labels.push(labelToAdd);
            $scope.labelAdded = false;
            velp.labels.push(labelToAdd.id);
        });

        // TODO: add label to edited or new velp
    };

    $scope.isLabelInVelp = function(velp, label){
        return velp.labels.indexOf(label.id) >= 0;
    };

    /**
     * Adds new velp on form submit
     * @param form form information
     */
    $scope.addVelp = function (form) {
        var valid = form.$valid;
        $scope.submitted = true;
        if (!valid) return;

        // Form is valid:
        form.$setPristine();

        if ($scope.newVelp.velp_groups.length === 0)
            $scope.newVelp.velp_groups = [default_velp_group.id];

        var velpToAdd = {
            labels: $scope.newVelp["labels"],
            used: 0,
            points: $scope.newVelp["points"],
            content: $scope.newVelp["content"],
            language_id: "FI",
            icon_id: null,
            valid_until: null,
            velp_groups: $scope.newVelp['velp_groups'] // TODO: Change to default group, add choices where to add
        };

        $scope.makePostRequest("/add_velp", velpToAdd, function (json) {
            velpToAdd.id = parseInt(json.data);
            console.log(velpToAdd);
            $scope.resetNewVelp();
            $scope.velpToEdit = {content: "", points: "", labels: [], edit: false, id: -1};
            $scope.velps.push(velpToAdd);
            $scope.submitted = false;
            //$scope.resetLabels();
        });
    };


    $scope.deselectLabels = function () {
        for (var i = 0; i < $scope.labels.length; i++) {
            if ($scope.labels[i].selected) {
                $scope.toggleLabel($scope.labels[i]);
            }
        }
    };

    $scope.selectLabelToEdit = function (label) {
        if (label.id == $scope.labelToEdit.id && label.edit) {
            label.edit = false;
            $scope.labelToEdit = {content: "", selected: false, edit: false};
            return;
        }

        if ($scope.labelToEdit.edit) {
            $scope.labelToEdit.edit = false;
            for (var i = 0; i < $scope.labels.length; i++) {
                $scope.labels[i].edit = false;
            }
        }

        label.edit = true;
        $scope.labelToEdit = Object.create(label);
    };


    $scope.updateVelpLabels = function(velp, label){

        var index = velp.labels.indexOf(label.id);
        if (index < 0){
            velp.labels.push(label.id);
        }
        else if (index >= 0){
            velp.labels.splice(index, 1);
        }
    };

    $scope.selectVelpToEdit = function (velp) {

        if (velp.id == $scope.velpToEdit.id && velp.edit) {
            velp.edit = false;
            $scope.velpToEdit = {content: "", points: "", labels: [], edit: false};
            return;
        }

        if ($scope.velpToEdit.edit) {
            $scope.velpToEdit.edit = false;
            for (var i = 0; i < $scope.velps.length; i++) {
                $scope.velps[i].edit = false;
            }
            $scope.newVelp.edit = false;
        }

        velp.edit = true;

        $scope.velpToEdit = (JSON.parse(JSON.stringify(velp)));

        /*
         if (velp.labels !== undefined) {
            for (var i = 0; i < velp.labels.length; i++) {
            $scope.toggleLabel($scope.getLabelById(velp.labels[i]));
            }
         }
         */
    };

    /**
     * Edit velp
     * @param form velp form
     */
    $scope.editVelp = function (form) {
        var valid = form.$valid;
        $scope.submitted = true;
        if (!valid) return;

        form.$setPristine();

        // TODO: Make velpGroups to [{'id':1, 'selected':'True'}]
        console.log($scope.velpToEdit);
        $scope.makePostRequest("/{0}/update_velp".replace('{0}', doc_id), $scope.velpToEdit, function (json) {
            console.log(json);
        });

        for (var i = 0; i < $scope.velps.length; i++) {
            if ($scope.velps[i].id == $scope.velpToEdit.id) {
                $scope.velpToEdit.edit = false;
                $scope.velps[i] = $scope.velpToEdit;
                break;
            }
        }
    };

    /**
     * Edit label
     */
    $scope.editLabel = function (){
        if ($scope.labelToEdit.content.length < 1){
            $scope.labelToEdit.edit = false;
            return;
        }

        for (var i=0; i<$scope.labels.length; i++){
            if ($scope.labels[i].id == $scope.labelToEdit.id){
                // TODO: This works yet not
                $scope.labelToEdit.edit = false;
                $scope.labels[i].content = $scope.labelToEdit.content;
                $scope.labels[i].edit = false;
                var updatedLabel = $scope.labels[i];
                break;
            }
        }

        $scope.makePostRequest("/update_velp_label", updatedLabel, function (json) {
            console.log(json);
        });

    };

    /**
     * Get velp by its id
     * @param id velp to find
     * @returns velp or undefined
     */
    $scope.getLabelById = function (id) {
        for (var i = 0; i < $scope.labels.length; i++)
            if ($scope.labels[i].id == id)
                return $scope.labels[i];

        return null;
    };

    /**
     * Reset new velp information
     */
    $scope.resetNewVelp = function () {
        $scope.newVelp = {content: "", points: "", labels: [], edit: false, id: -2, velp_groups:[]};
    };

    /**
     * Reset new label information
     */
    $scope.resetNewLabel = function () {
        $scope.newLabel = {content: "", selected: true, valid: true};
    };

    /** Velpgroup methods **/

    $scope.changeVelpGroupSelection = function(group){
        console.log(group);

        $scope.makePostRequest("/{0}/change_selection".replace('{0}', doc_id), group, function (json) {
            console.log(json);
        });
    };

    $scope.getVelpGroupsAsDictionary = function(velp){
        var groups = [];

        for (var i=0; i<velp.velp_groups.length; i++){
            for (var j=0; j<$scope.velpGroups.length; j++){
                groups.push($scope.velpGroups[j]);
                if (velp.velp_groups.indexOf($scope.velpGroups[j].id) >= 0){
                    groups[i].selected = true;
                } else {
                    groups[i].selected = false;
                }
            }
        }
        return groups;
    };

    $scope.isGroupInVelp = function(velp, group){
        if (typeof velp.velp_groups == "undefined" || typeof group.id == "undefined")
            return false;
        return velp.velp_groups.indexOf(group.id) >= 0;
    };

    $scope.updateVelpGroups = function(velp, group){
        var index = velp.velp_groups.indexOf(group.id);
        if (index < 0){
            velp.velp_groups.push(group.id);
        }
        else if (index >= 0){
            velp.velp_groups.splice(index, 1);
        }
    };

}]);

timApp.filter('filterByVelpGroups', function() {
   return function (velps, groups) {

       var selectedVelps = [];
       var checkedGroups = [];

       if (typeof groups == "undefined" || typeof velps == "undefined")
           return velps;

       for (var j=0; j<groups.length; j++)
            if (groups[j].selected) checkedGroups.push(groups[j].id);

       for (var i=0; i<velps.length; i++){
           for (var j=0; j<checkedGroups.length; j++){
               if (velps[i].velp_groups.indexOf(checkedGroups[j]) >= 0)
                    selectedVelps.push(velps[i]);
           }
       }

       return selectedVelps;
   }
});

/**
 * Filter for ordering velps
 */
timApp.filter('filterByLabels', function () {
    return function (velps, labels, advancedOn) {
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

        for (var sv in selectedVelps) {
            if (selectedVelps.hasOwnProperty(sv))
                selectedArray.push(selectedVelps[sv]);
        }

        selectedArray.sort(function (a, b) {
            return b[1] - a[1]
        });

        for (var i = 0; i < selectedArray.length; i++)
            returnVelps.push(selectedArray[i][0]);

        return returnVelps;
    };
});