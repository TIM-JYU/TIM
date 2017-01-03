/**
 * Created by Seppo Tarvainen on 25.11.2016.
 *
 * @module velpWindow
 * @author Seppo Tarvainen
 * @licence MIT
 */


// var UNDEFINED = "undefined";
var colorPalette = ["blueviolet", "darkcyan", "orange", "darkgray", "cornflowerblue", "coral", "goldenrod", "blue"];

/**
 * Angular directive for velp selection
 */
timApp.directive('velpWindow', function () {
    "use strict";

    return {
        templateUrl: "/static/templates/velpWindow.html",
        scope: {
            velp: "=",
            velpGroups: "=", // all velpgroups, not just selected ones
            advancedOn: "=",
            labels: "=",
            new: "@",
            index: "@"
        },
        controller: 'VelpWindowController'

    };
});

/**
 * Controller for velp Window
 * @lends module:velpWindow
 */
timApp.controller('VelpWindowController', ['$scope', function ($scope) {
    "use strict";
    $scope.velpLocal = JSON.parse(JSON.stringify($scope.velp)); // clone object

    $scope.newLabel = {content: "", selected: true, valid: true};
    $scope.labelToEdit = {content: "", selected: false, edit: false, valid: true};

    $scope.visible_options = {
                "type": "select",
                "values": [1, 2, 3, 4],
                "names": ["Just me", "Document owner", "Teachers", "Everyone"]
    };

    if (typeof $scope.velp.visible_to === UNDEFINED){
        $scope.velp.visible_to = 4; // Everyone by default
    }



    $scope.settings = {
        saveButtonText: function () {
            if ($scope.new === "true") {
                return "Add velp";
            }
            return "Save"
        },
        teacherRightsError: "You need to have teacher rights change points in this document.",
        labelContentError: "Label content too short",
        velpGroupError: "Select at least one velp group.",
        velpGroupWarning: "All selected velp groups are hidden in the current area.",
        velpContentError: "Velp content too short"
    };

    $scope.submitted = false;

    $scope.hasEditAccess = false;

    var doc_id = $scope.$parent.docId;

    /**
     * Toggles velp for editing. If another velp is currently open,
     * this method closes it.
     */
    $scope.toggleVelpToEdit = function () {
        var lastEdited = $scope.$parent.getVelpUnderEdit();

        if (lastEdited.edit && lastEdited.id !== $scope.velp.id){
            //if ($scope.new === "true") $scope.$parent.resetNewVelp();
            $scope.$parent.resetEditVelp();
        }

        $scope.velp.edit = !$scope.velp.edit;
        if (!$scope.velp.edit){
            $scope.cancelEdit();
        } else {
            if ($scope.new){
                $scope.velpLocal = JSON.parse(JSON.stringify($scope.velp));
                // TODO: focus velp content textarea
            }
            $scope.$parent.setVelpToEdit($scope.velp, $scope.cancelEdit);
        }
    };


    /**
     * Saves velp to database
     * @param form
     */
    $scope.saveVelp = function (form) {
        if (!form.$valid) return;
        form.$setPristine();
        //$scope.submitted = true;

        if ($scope.new === "true"){ // add new velp
            $scope.addVelp()
        } else { // edit velp
            editVelp();

        }
    };

    /**
     * Cancel edit and restore velp back to its original version
     * TODO: new velp reset does not work
     */
    $scope.cancelEdit = function () {
        $scope.velp = JSON.parse(JSON.stringify($scope.velpLocal));
        $scope.velp.edit = false;
    };

    $scope.useVelp = function () {
        if (!$scope.velp.edit && !$scope.notAnnotationRights($scope.velp.points)) {
            $scope.$parent.useVelp($scope.velp);
        }

    };

    /**
     * Detect user right to annotation to document.
     * @param points - Points given in velp or annotation
     * @returns {boolean} - Right to make annotations
     */
    $scope.notAnnotationRights = function (points) {
        if ($scope.$parent.item.rights.teacher) {
            return false;
        } else {
            if (points === null) {
                return false;
            } else {
                return true;
            }
        }
    };

    $scope.isVelpValid = function () {
        if (typeof $scope.velp.content === UNDEFINED)
            return false;
        if (JSON.stringify($scope.velpLocal) === JSON.stringify($scope.velp)) // check if still original
            return false;
        return $scope.isSomeVelpGroupSelected() && $scope.velp.content.length > 0 ;
    };


    $scope.setLabelValid = function (label) {
        label.valid = label.content.length > 0;
    };



    /**
     * Returns whether the velp contains the label or not.
     * @method isLabelInVelp
     * @param label - Label to check
     * @returns {boolean} Whether the velp contains the label or not.
     */
    $scope.isLabelInVelp = function (label) {
        return $scope.velp.labels.indexOf(label.id) >= 0;
    };


    /**
     * Checks whether the velp contains the velp group.
     * @method isGroupInVelp
     * @param group - Velp group to check
     * @returns {boolean} Whether the velp contains the velp group or not
     */
    $scope.isGroupInVelp = function (group) {
        if (typeof $scope.velp.velp_groups === UNDEFINED || typeof group.id === UNDEFINED)
            return false;
        return $scope.velp.velp_groups.indexOf(group.id) >= 0;
    };



    /**
     * Updates the labels of the velp.
     * @method updateVelpLabels
     * @param label - Label to be added or removed from the velp
     */
    $scope.updateVelpLabels = function (label) {

        var index = $scope.velp.labels.indexOf(label.id);
        if (index < 0) {
            $scope.velp.labels.push(label.id);
        }
        else if (index >= 0) {
            $scope.velp.labels.splice(index, 1);
        }
    };

    /**
     * Updates velp groups of this velp.
     * @method updateVelpGroups
     * @param group - Group to be added or removed from the velp
     */
    $scope.updateVelpGroups = function (group) {
        var index = $scope.velp.velp_groups.indexOf(group.id);
        if (index < 0) {
            $scope.velp.velp_groups.push(group.id);
        }
        else if (index >= 0) {
            $scope.velp.velp_groups.splice(index, 1);
        }
    };


    /**
     * Checks if the velp has any velp groups selected.
     * @method isSomeVelpGroupSelected
     * @returns {boolean} Whether velp has any groups selected or not
     */
    $scope.isSomeVelpGroupSelected = function () {
        if (typeof $scope.velp.velp_groups === UNDEFINED)
            return false;
        return $scope.velp.velp_groups.length > 0;
    };

    $scope.isSomeVelpGroupShown = function(){
        if (typeof $scope.velp.velp_groups === UNDEFINED || $scope.velp.velp_groups.length === 0)
            return true;

        for (var i=0; i<$scope.velp.velp_groups.length; i++){
            for (var j=0; j<$scope.velpGroups.length; j++){
                if ($scope.velpGroups[j].id === $scope.velp.velp_groups[i] && $scope.velpGroups[j].show)
                    return true;
            }
        }
        return false;
    };

    /**
     * Adds new label to this velp.
     * @method addLabel
     */
    $scope.addLabel = function () {

        if ($scope.newLabel.content.length < 1) {
            $scope.newLabel.valid = false;
            return;
        }

        var labelToAdd = {
            content: $scope.newLabel.content,
            language_id: "FI", // TODO: Change to user language
            selected: false
        };

        $scope.$parent.makePostRequest("/add_velp_label", labelToAdd, function (json) {
            labelToAdd.id = parseInt(json.data.id);
            $scope.resetNewLabel();
            $scope.labels.push(labelToAdd);
            //$scope.labelAdded = false;
            $scope.velp.labels.push(labelToAdd.id);
        });
    };

    /**
     * Selects the label for editing.
     * @method toggleLabelToEdit
     * @param label - Label to edit
     */
    $scope.toggleLabelToEdit = function (label) {

        if ($scope.labelToEdit.edit && label.id === $scope.labelToEdit.id){
            $scope.cancelLabelEdit(label);
            return;
        }

        if ($scope.labelToEdit.edit) {
            $scope.labelToEdit.edit = false;
            for (var i = 0; i < $scope.labels.length; i++) {
                $scope.labels[i].edit = false;
            }
        }

        label.edit = true;
        copyLabelToEditLabel(label);
        $scope.setLabelValid($scope.labelToEdit);
    };

    $scope.cancelLabelEdit = function (label) {
        label.edit = false;
        $scope.labelToEdit = {content: "", selected: false, edit: false, valid: true};
    };

    $scope.clearVelpColor = function () {
        $scope.velp.color = "";
    };

    $scope.isVelpCustomColor = function () {
        return $scope.velp.color.length === 7; // hex colors are 7 characters long
    }

    var copyLabelToEditLabel = function (label) {
        for (var key in label){
            if(!label.hasOwnProperty(key)) continue;

            $scope.labelToEdit[key] = label[key];
        }
    };



    /**
     * Edits the label according to the $scope.labelToedit variable.
     * All required data exists in the $scope.labelToedit variable,
     * including the ID of the label.
     * TODO: This can be simplified
     * @method editLabel
     */
    $scope.editLabel = function () {
        if ($scope.labelToEdit.content.length < 1) {
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

        $scope.$parent.makePostRequest("/update_velp_label", updatedLabel, function (json) {
        });
    };


    /**
     * Reset new label information to the initial (empty) state.
     * @method resetNewLabel
     */
    $scope.resetNewLabel = function () {
        $scope.newLabel = {content: "", selected: true, valid: true};
    };

    /**
     * Return true if user has teacher rights.
     * @returns {boolean}
     */
    $scope.allowChangePoints = function () {
        return $scope.$parent.item.rights.teacher;
    };

    var editVelp = function () {
        var default_velp_group = $scope.$parent.getDefaultVelpGroup();

        if ($scope.isGroupInVelp(default_velp_group) && default_velp_group.id === -1) {
            handleDefaultVelpGroupIssue(updateVelpInDatabase);
        } else if ($scope.velp.velp_groups.length > 0) {
            updateVelpInDatabase();
        }
    };

    var updateVelpInDatabase = function () {
        $scope.$parent.makePostRequest("/{0}/update_velp".replace('{0}', doc_id), $scope.velp, function (json) {
                $scope.velpLocal = JSON.parse(JSON.stringify($scope.velp));
                $scope.toggleVelpToEdit();
        });
    };

    /**
     * Adds a new velp on form submit event.
     * @method addVelp
     */
    $scope.addVelp = function () {

        var default_velp_group = $scope.$parent.getDefaultVelpGroup();

        if ($scope.isGroupInVelp(default_velp_group) && default_velp_group.id === -1) {
            handleDefaultVelpGroupIssue(addNewVelpToDatabase);
        } else if ($scope.velp.velp_groups.length > 0) {
            addNewVelpToDatabase();
        }

        $scope.$parent.updateVelpList();

    };

    /**
     * Adds a new velp to the database. Requires values in `$scope.newVelp` variable.
     * @method addNewVelpToDatabase
     */
    var addNewVelpToDatabase = function () {
        var velpToAdd = {
            labels: $scope.velp.labels,
            used: 0,
            points: $scope.velp.points,
            content: $scope.velp.content,
            default_comment: $scope.velp.default_comment,
            language_id: "FI",
            icon_id: null,
            valid_until: null,
            color: $scope.velp.color,
            visible_to: $scope.velp.visible_to,
            velp_groups: JSON.parse(JSON.stringify($scope.velp.velp_groups))
        };

        //$scope.velp.edit = false;

        $scope.$parent.makePostRequest("/add_velp", velpToAdd, function (json) {
            velpToAdd.id = json.data;
            $scope.$parent.velps.push(velpToAdd);

            $scope.velpLocal.velp_groups = velpToAdd.velp_groups;
            $scope.velpLocal.labels = velpToAdd.labels;

            $scope.toggleVelpToEdit();
            $scope.$parent.updateVelpList();

            //$scope.velp =  JSON.parse(JSON.stringify($scope.velpLocal));
            //$scope.velpLocal = JSON.parse(JSON.stringify($scope.velp));
            /*
            velpToAdd.id = parseInt(json.data);

            $scope.resetNewVelp();
            $scope.velpToEdit = {content: "", points: "", labels: [], edit: false, id: -1};

            $scope.velps.push(velpToAdd);
            $scope.submitted.velp = false;
            //$scope.resetLabels();
            */

        });

    };

    /**
     *
     * @param method - Method to execute after default velp group is created
     */
    var handleDefaultVelpGroupIssue = function (method) {

        var old_default_group = $scope.$parent.getDefaultVelpGroup();

        $scope.$parent.generateDefaultVelpGroup(function (new_default_group) {
            var oldGroupIndex = $scope.velp.velp_groups.indexOf(old_default_group.id);
            if (oldGroupIndex >= 0)
                $scope.velp.velp_groups.splice(oldGroupIndex, 1);

            $scope.velp.velp_groups.push(new_default_group.id);
            $scope.$parent.setDefaultVelpGroup(new_default_group);
            method();
        });


    };

    /**
     * Get color for the object from colorPalette variable.
     * @method getColor
     * @param index - Index of the color in the colorPalette variable (modulo by lenght of color palette)
     * @returns {string} String representation of the color
     */
    $scope.getColor = function (index) {
        return colorPalette[index % colorPalette.length];
    };

    $scope.getCustomColor = function () {
        if (typeof $scope.velp.color !== UNDEFINED || $scope.velp.color !== null)
            return $scope.velp.color;
    };

    // declare edit rights
    if ($scope.new === "true") {
        $scope.hasEditAccess = true;
    } else {
        $scope.velpGroups.some(function (g) {
            if (g.edit_access && $scope.isGroupInVelp(g)){
                $scope.hasEditAccess = true;
                return;
            }
        });
    }

}]);