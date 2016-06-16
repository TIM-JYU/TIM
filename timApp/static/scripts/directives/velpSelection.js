/**
 * Created by sevitarv on 8.3.2016.
 * @module velpSelection
 * @licence MIT
 * @copyright 2016 Timber project authors
 */

var angular;
var timApp = angular.module('timApp');

var colorPalette = ["blueviolet", "darkcyan", "orange", "darkgray", "cornflowerblue", "coral", "goldenrod", "blue"];

var UNDEFINED = "undefined";
var console = window.console;

/**
 * Angular directive for velp selection
 */
timApp.directive('velpSelection', function () {
    "use strict";
    return {
        templateUrl: "/static/templates/velpSelection.html",
        controller: 'VelpSelectionController'
    };
});

/**
 * Controller for velp selection
 */
timApp.controller('VelpSelectionController', ['$scope', '$http', function ($scope, $http) {
    "use strict";

    // Data
    $scope.velps = [];
    $scope.annotations = [];
    $scope.labels = [];
    $scope.velpGroups = [];

    $scope.orderVelp = 'label';
    $scope.advancedOn = false;
    $scope.newVelp = {content: "", points: "", labels: [], edit: false, id: -2, velp_groups: []};
    $scope.velpToEdit = {content: "", points: "", labels: [], edit: false, id: -1, velp_groups: []};
    $scope.newLabel = {content: "", selected: false, edit: false, valid: true};
    $scope.labelToEdit = {content: "", selected: false, edit: false, id: -3};
    $scope.newVelpGroup = {name: "", target_type: 0};
    $scope.selectedLabels = [];
    $scope.settings = {selectedAllShows: false, selectedAllDefault: false};
    $scope.submitted = {velp: false, velpGroup: false};

    $scope.groupAttachment = {target_type: 0, id: null};

    $scope.groupSelections = {};
    $scope.groupDefaults = {};

    // Dictionaries for easier searching: Velp ids? Label ids? Annotation ids?
    var doc_id = $scope.docId;
    var default_velp_group = {id: -1, name: "No access to default group", edit_access: false}; // TODO Use route to add this information
    var default_personal_velp_group = {id: -2, name: "No personal default"}

    $scope.annotations = [];



    // Get default velpgroup data
    $http.get('/{0}/get_default_velp_group'.replace('{0}', doc_id)).success(function (data) {
        console.log("Get default velp group");
        default_velp_group = data;
        if (default_velp_group.edit_access)
            default_velp_group.selected = true;
        console.log(data);
    });

    // Get velpgroup data
    $http.get('/{0}/get_velp_groups'.replace('{0}', doc_id)).success(function (data) {
        $scope.velpGroups = data;
        if (default_velp_group.id < 0){
            $scope.velpGroups.push(default_velp_group);
        } else {
            $scope.velpGroups.forEach(function(g) {
               if (g.id === default_velp_group.id)
                   g.selected = true;
            });
        }

        if (default_velp_group.edit_access)
            $scope.newVelp.velp_groups = [default_velp_group.id];
        console.log("VELP GROUPS")
        console.log($scope.velpGroups);

        // Get velp and annotation data
        $http.get('/{0}/get_velps'.replace('{0}', doc_id)).success(function (data) {
            $scope.velps = data;
            $scope.velps.forEach(function (v) {
                v.used = 0;
                v.edit = false;
                if (typeof v.labels === UNDEFINED)
                    v.labels = [];
            });
        });

        $http.get('/get_default_personal_velp_group').success(function (data) {
            default_personal_velp_group = {id: data.id, name: data.name};
            if (data.created_new_group) {
                $scope.velpGroups.push(data);
            }
            else $scope.velpGroups.some(function(g){
                if (g.id === default_personal_velp_group.id)
                    return g.selected = true;
            });
            if (!default_velp_group.edit_access)
                $scope.newVelp.velp_groups = [default_personal_velp_group.id];
        });

        $http.get('/{0}/get_annotations'.replace('{0}', doc_id)).success(function (data) {
            $scope.annotations = data;
            $scope.loadDocumentAnnotations();
            console.log(data);
        });

        // Get label data
        $http.get('/{0}/get_velp_labels'.replace('{0}', doc_id)).success(function (data) {
            $scope.labels = data;
            $scope.labels.forEach(function (l) {
                l.edit = false;
                l.selected = false;
            });
            console.log("labels!");
            console.log($scope.labels);
        });

        $http.get('/{0}/get_velp_group_personal_selections'.replace('{0}', doc_id)).success(function (data) {
            $scope.groupSelections = data;
            if (!$scope.groupSelections.hasOwnProperty("0"))
                $scope.groupSelections["0"] = [];
            var docSelections = $scope.groupSelections["0"];
            console.log("SELECTIONS: ");
            console.log($scope.groupSelections);
            $scope.velpGroups.forEach(function(g){
                g.show = docSelections.indexOf(g.id) >= 0;
            });
        });

        $http.get('/{0}/get_velp_group_default_selections'.replace('{0}', doc_id)).success(function (data) {
            $scope.groupDefaults = data;

            var docDefaults = $scope.groupDefaults["0"];
            console.log("DEFAULTS: ");
            console.log($scope.groupDefaults);
            $scope.velpGroups.forEach(function(g){
                g.default = docDefaults.indexOf(g.id) >= 0;
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

    $scope.toggleLabelToEdit = function (label) {
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

        if ($scope.newLabel.content.length < 1) {
            $scope.newLabel.valid = false;
            return;
        }

        var labelToAdd = {
            content: $scope.newLabel.content,
            language_id: "FI", // TODO: Change to user language
            selected: false
        };

        $scope.makePostRequest("/add_velp_label", labelToAdd, function (json) {
            labelToAdd.id = parseInt(json.data);
            $scope.resetNewLabel();
            $scope.labels.push(labelToAdd);
            $scope.labelAdded = false;
            velp.labels.push(labelToAdd.id);
        });

    };

    $scope.isLabelInVelp = function (velp, label) {
        return velp.labels.indexOf(label.id) >= 0;
    };

    /**
     * Adds new velp on form submit
     * @param form form information
     */
    $scope.addVelp = function (form) {
        var valid = form.$valid;
        $scope.submitted.velp = true;
        if (!valid) return;

        // Form is valid:
        form.$setPristine();

        if ($scope.isGroupInVelp($scope.newVelp, default_velp_group) && default_velp_group.id === -1) {
            // $scope.isGroupInVelp($scope.newVelp, -1);
            //$scope.newVelp.velp_groups = [default_velp_group];

            var old_default_group = default_velp_group;
            $scope.generateDefaultVelpGroup(function () {

                var oldGroupIndex = $scope.newVelp.velp_groups.indexOf(old_default_group.id); // -1 = old
                if (oldGroupIndex >= 0)
                    $scope.newVelp.velp_groups.splice(oldGroupIndex, 1);
                $scope.newVelp.velp_groups.push(default_velp_group.id);

                addNewVelpToDatabase();
            });

        } else if ($scope.newVelp.velp_groups.length > 0) {
            addNewVelpToDatabase();
        }
    };

    var addNewVelpToDatabase = function () {
        var velpToAdd = {
            labels: $scope.newVelp.labels,
            used: 0,
            points: $scope.newVelp.points,
            content: $scope.newVelp.content,
            language_id: "FI",
            icon_id: null,
            valid_until: null,
            velp_groups: $scope.newVelp.velp_groups

        };
        console.log(velpToAdd);
        $scope.makePostRequest("/add_velp", velpToAdd, function (json) {
            velpToAdd.id = parseInt(json.data);
            console.log(velpToAdd);
            $scope.resetNewVelp();
            $scope.velpToEdit = {content: "", points: "", labels: [], edit: false, id: -1};
            $scope.velps.push(velpToAdd);
            $scope.submitted.velp = false;
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
        if (label.id === $scope.labelToEdit.id && label.edit) {
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


    $scope.updateVelpLabels = function (velp, label) {

        var index = velp.labels.indexOf(label.id);
        if (index < 0) {
            velp.labels.push(label.id);
        }
        else if (index >= 0) {
            velp.labels.splice(index, 1);
        }
    };

    $scope.selectVelpToEdit = function (velp) {

        if (velp.id === $scope.velpToEdit.id && velp.edit) {
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
    };

    /**
     * Edit velp
     * @param form velp form
     */
    $scope.editVelp = function (form) {
        var valid = form.$valid;
        $scope.submitted.velp = true;
        if (!valid) return;

        form.$setPristine();

        // TODO: Make velpGroups to [{'id':1, 'selected':'True'}]
        console.log($scope.velpToEdit);

        if ($scope.isGroupInVelp($scope.velpToEdit, default_velp_group) && default_velp_group.id === -1){

            var old_default_group = default_velp_group;
            $scope.generateDefaultVelpGroup(function () {

                var oldGroupIndex = $scope.velpToEdit.velp_groups.indexOf(old_default_group.id); // -1 = old
                if (oldGroupIndex >= 0)
                    $scope.velpToEdit.velp_groups.splice(oldGroupIndex, 1);
                $scope.velpToEdit.velp_groups.push(default_velp_group.id);

                $scope.makePostRequest("/{0}/update_velp".replace('{0}', doc_id), $scope.velpToEdit, function (json) {
                    console.log(json);
                });
            });
        } else if ($scope.velpToEdit.velp_groups.length > 0) {
            $scope.makePostRequest("/{0}/update_velp".replace('{0}', doc_id), $scope.velpToEdit, function (json) {
                console.log(json);
            });
        }

        for (var i = 0; i < $scope.velps.length; i++) {
            if ($scope.velps[i].id === $scope.velpToEdit.id) {
                $scope.velpToEdit.edit = false;
                $scope.velps[i] = $scope.velpToEdit;
                break;
            }
        }
    };

    $scope.generateDefaultVelpGroup = function (method) {
        if (default_velp_group.edit_access) {
            $scope.makePostRequest('/{0}/create_default_velp_group'.replace('{0}', doc_id), null, function (json) {
                var new_default_velp_group = json.data;

                var index = $scope.velpGroups.indexOf(default_velp_group);
                $scope.velpGroups.splice(index, 1);

                if ($scope.velpGroups.indexOf(new_default_velp_group) < 0)
                    $scope.velpGroups.push(new_default_velp_group);

                default_velp_group = new_default_velp_group;
                method();
            });
        }
        else console.log("No edit access to default velp group")
    };

    /**
     * Edit label
     */
    $scope.editLabel = function () {
        if ($scope.labelToEdit.content.length < 1) {
            $scope.labelToEdit.edit = false;
            return;
        }

        var updatedLabel = null;
        for (var i = 0; i < $scope.labels.length; i++) {
            if ($scope.labels[i].id === $scope.labelToEdit.id) {
                $scope.labelToEdit.edit = false;
                $scope.labels[i].content = $scope.labelToEdit.content;
                $scope.labels[i].edit = false;
                updatedLabel = $scope.labels[i];
                break;
            }
        }

        $scope.makePostRequest("/update_velp_label", updatedLabel, function (json) {
            console.log(json);
        });
    };

    /**
     * Reset new velp information
     */
    $scope.resetNewVelp = function () {
        $scope.newVelp = {
            content: "",
            points: "",
            labels: [],
            edit: false,
            id: -2,
            velp_groups: $scope.newVelp.velp_groups
        };
    };

    /**
     * Reset new label information
     */
    $scope.resetNewLabel = function () {
        $scope.newLabel = {content: "", selected: true, valid: true};
    };

    /** Velpgroup methods **/

    $scope.updateVelpList = function(){
        var showGroupsInPar = [];
        var defaultGroupsInPar = [];

        //console.log($scope.selectedElement.id);

        if ($scope.selectedElement !== null && $scope.groupAttachment.target_type === 1){
            console.log("Täällä");
            if ($scope.groupSelections.hasOwnProperty($scope.selectedElement.id)){
                console.log( $scope.groupSelections[$scope.selectedElement.id]);
                showGroupsInPar = $scope.groupSelections[$scope.selectedElement.id];
            }
            if ($scope.groupDefaults.hasOwnProperty($scope.selectedElement.id)){
                console.log( $scope.groupDefaults[$scope.selectedElement.id]);
                defaultGroupsInPar = $scope.groupDefaults[$scope.selectedElement.id];
            }
        }
        else if ($scope.groupAttachment.target_type === 0){
            showGroupsInPar = $scope.groupSelections["0"];
            defaultGroupsInPar = $scope.groupDefaults["0"];
        }


            console.log(showGroupsInPar);
            console.log(defaultGroupsInPar);
            $scope.velpGroups.forEach(function (g) {
                g.show = false;
                g.default = false;

                g.show = showGroupsInPar.indexOf(g.id) >= 0;
                g.default =  defaultGroupsInPar.indexOf(g.id) >= 0;
            });

    };

    $scope.addVelpGroup = function (form) {
        var valid = form.$valid;
        $scope.submitted.velpGroup = true;
        if (!valid) return;

        form.$setPristine();

        $scope.newVelpGroup.target_type = parseInt($scope.newVelpGroup.target_type);
        console.log($scope.newVelpGroup);
        $scope.makePostRequest("/{0}/create_velp_group".replace('{0}', doc_id), $scope.newVelpGroup, function (json) {
            var group = json.data;
            group.selected = false;
            group.show = true;
            $scope.velpGroups.push(json.data);
        });
    };

    $scope.changeVelpGroupSelection = function (group, type) {

        $scope.groupAttachment.target_type = parseInt($scope.groupAttachment.target_type);

        if ($scope.groupAttachment.target_type === 1){
            group.target_id = $scope.selectedElement.id;
            group.target_type = 1;
        } else {
            group.target_id = "0";
            group.target_type = 0;
        }

        if (type === "show"){
            $scope.makePostRequest("/{0}/change_selection".replace('{0}', doc_id), group, function (json) {console.log(json);});

            if (!$scope.groupSelections.hasOwnProperty(group.target_id))
                $scope.groupSelections[group.target_id] = [];

            var groups = $scope.groupSelections[group.target_id];

            if (groups.indexOf(group.id) < 0)
                $scope.groupSelections[group.target_id].push(group.id);
            else
                $scope.groupSelections[group.target_id].splice(groups.indexOf(group.id), 1);

        }
        else if (type === "default"){
            $scope.makePostRequest("/{0}/change_default_selection".replace('{0}', doc_id), group, function (json) {console.log(json);});

            if (!$scope.groupDefaults.hasOwnProperty(group.target_id))
                $scope.groupDefaults[group.target_id] = [];

            var defGroups = $scope.groupDefaults[group.target_id];

            if (defGroups.indexOf(group.id) < 0)
                $scope.groupDefaults[group.target_id].push(group.id);
            else
                $scope.groupDefaults[group.target_id].splice(defGroups.indexOf(group.id), 1);
        }
    };

    $scope.changeAllVelpGroupSelections = function (type) {

        $scope.groupAttachment.target_type = parseInt($scope.groupAttachment.target_type);

        var targetID, targetType;

        if ($scope.groupAttachment.target_type === 1){
            targetID = $scope.selectedElement.id;
            targetType = 1;
        } else {
            targetID = "0";
            targetType = 0;
        }

        console.log(targetID);
        console.log(targetType);
        console.log($scope.settings.selectedAllShows);

        if (type === "show"){
            if (!$scope.settings.selectedAllShows) {
                $scope.groupSelections[targetID] = [];
            } else {
                $scope.velpGroups.forEach(function (g) {
                    $scope.groupSelections[targetID].push(g.id);
                });
            }

            $scope.makePostRequest("/{0}/change_all_selections".replace('{0}', doc_id), {
                'target_id': targetID, 'target_type': targetType, 'selection': $scope.settings.selectedAllShows
            }, function (json) {
                console.log(json);
            });



        }
        else if (type === "default") {
            console.log("DEFAULT " + $scope.settings.selectedAllDefault);
            if (!$scope.settings.selectedAllDefault) {
                $scope.groupDefaults[targetID] = [];
            } else {
                for (var i=0;i<$scope.velpGroups.length; i++)
                    $scope.groupDefaults[targetID].push($scope.velpGroups[i].id);
            }
        }

        $scope.updateVelpList();

    };

    $scope.resetCurrentShowsToDefaults = function (){

        var targetID;
        $scope.groupAttachment.target_type = parseInt($scope.groupAttachment.target_type);

        if ($scope.groupAttachment.target_type === 1){
            targetID = $scope.selectedElement.id;
        } else {
            targetID = "0";
        }

        $scope.groupSelections[targetID] = JSON.parse(JSON.stringify($scope.groupDefaults[targetID]));

        $scope.makePostRequest("/{0}/reset_target_area_selections_to_defaults".replace('{0}', doc_id), {'target_id': targetID}, function (json) {
            console.log(json);
            $scope.updateVelpList();
        });
    };

    $scope.resetAllShowsToDefaults = function (){
        $scope.groupSelections = JSON.parse(JSON.stringify($scope.groupDefaults));

        $scope.makePostRequest("/{0}/reset_all_selections_to_defaults".replace('{0}', doc_id), null, function (json) {
            console.log(json);
            $scope.updateVelpList();
        });
    };

    $scope.checkCheckBoxes = function(type){
        var targetID = null;

        if ($scope.groupAttachment.target_type === 1){
            targetID = $scope.selectedElement.id;
        } else {
            targetID = "0";
        }

        if (type === "show" && typeof $scope.groupSelections[targetID] !== UNDEFINED){
            return $scope.groupSelections[targetID].length === $scope.velpGroups.length;
        } else if (type === "default" && typeof $scope.groupDefaults[targetID] !== UNDEFINED){
            return $scope.groupDefaults[targetID].length === $scope.velpGroups.length;
        }
    };

    $scope.getVelpsVelpGroups = function (velp) {
        var groups = [];

        for (var i = 0; i < velp.velp_groups.length; i++) {
            for (var j = 0; j < $scope.velpGroups.length; j++) {
                groups.push($scope.velpGroups[j]);
                groups[i].selected = velp.velp_groups.indexOf($scope.velpGroups[j].id) >= 0;
            }
        }
        return groups;
    };

    $scope.isSomeVelpGroupSelected = function(velp){
        if (typeof velp.velp_groups === UNDEFINED)
            return false;
        return velp.velp_groups.length > 0;
    };

    $scope.isVelpValid = function(velp){
        if (typeof velp.content === UNDEFINED)
            return false;
        return $scope.isSomeVelpGroupSelected(velp) && velp.content.length > 0;
    };

    $scope.isGroupInVelp = function (velp, group) {
        if (typeof velp.velp_groups === UNDEFINED || typeof group.id === UNDEFINED)
            return false;
        return velp.velp_groups.indexOf(group.id) >= 0;
    };


    $scope.updateVelpGroups = function (velp, group) {
        var index = velp.velp_groups.indexOf(group.id);
        if (index < 0) {
            velp.velp_groups.push(group.id);
        }
        else if (index >= 0) {
            velp.velp_groups.splice(index, 1);
        }
    };
}]);

timApp.filter('filterByVelpGroups', function () {
    "use strict";
    return function (velps, groups) {

        var selected = [];
        var checkedGroups = [];

        if (typeof groups === UNDEFINED || typeof velps === UNDEFINED)
            return velps;

        for (var j = 0; j < groups.length; j++)
            if (groups[j].show) checkedGroups.push(groups[j].id);

        for (var i = 0; i < velps.length; i++) {
            for (var k = 0; k < checkedGroups.length; k++) {
                if (velps[i].velp_groups.indexOf(checkedGroups[k]) >= 0 && selected.indexOf(velps[i]) < 0)
                    selected.push(velps[i]);
            }
        }

        return selected;
    };
});

/**
 * Filter for ordering velps
 */
timApp.filter('filterByLabels', function () {
    "use strict";
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
            for (var j = 0; j < velps.length; j++) {

                for (var k = 0; k < selectedLabels.length; k++) {
                    if (typeof velps[j].labels !== UNDEFINED && velps[j].labels.indexOf(selectedLabels[k]) !== -1)
                        if (!(j in selectedVelps))
                            selectedVelps[j] = [velps[j], 1];
                        else
                            selectedVelps[j][1] += 1;
                }
            }
        }

        // return all velps if no labels selected
        if (selectedLabels.length === 0)
            return velps;

        var selectedArray = [];
        var returnVelps = [];

        for (var sv in selectedVelps) {
            if (selectedVelps.hasOwnProperty(sv))
                selectedArray.push(selectedVelps[sv]);
        }

        selectedArray.sort(function (a, b) {
            return b[1] - a[1];
        });

        for (var l = 0; l < selectedArray.length; l++)
            returnVelps.push(selectedArray[l][0]);

        return returnVelps;
    };
});