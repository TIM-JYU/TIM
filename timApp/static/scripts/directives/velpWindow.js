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

    $scope.settings = {
        saveButtonText: function () {
            console.log(typeof $scope.new, $scope.new);
            if ($scope.new === "true") {
                return "Add velp";
            }
            return "Save"
        },
        teacherRightsError: "You need to have teacher rights change points in this document.",
        labelContentError: "Label content too short",
        velpGroupError: "Select at least one velp group.",
        velpContentError: "Velp content too short"
    };


    var doc_id = $scope.$parent.docId;

    /**
     * Toggles velp for editing. If another velp is currently open,
     * this method closes it.
     */
    $scope.toggleVelpToEdit = function () {
        var lastEdited = $scope.$parent.getVelpUnderEdit();
        console.log($scope.$parent.getVelpUnderEdit());
        if (lastEdited.edit && lastEdited.id !== $scope.velpLocal.id){
            $scope.$parent.resetEditVelp();
        }

        $scope.velpLocal.edit = !$scope.velpLocal.edit;
        if (!$scope.velpLocal.edit){
            $scope.cancelEdit();
        }

        $scope.$parent.setVelpToEdit($scope.velpLocal, $scope.cancelEdit);
    };


    /**
     * Saves velp to database
     * @param form
     */
    $scope.saveVelp = function (form) {
        if (!form.$valid) return;
        form.$setPristine();

        if ($scope.new === "true"){ // add new velp
            $scope.addVelp()
        } else { // edit velp
            $scope.$parent.makePostRequest("/{0}/update_velp".replace('{0}', doc_id), $scope.velpLocal, function (json) {
                $scope.toggleVelpToEdit();
                $scope.velp = $scope.velpLocal;
            });
        }
    };

    /**
     * Cancel edit and restore velp back to its original version
     */
    $scope.cancelEdit = function () {
        $scope.velpLocal = JSON.parse(JSON.stringify($scope.velp));
        $scope.velpLocal.edit = false;
    };

    $scope.useVelp = function () {
        if (!$scope.velpLocal.edit && !$scope.notAnnotationRights($scope.velp.points)) {
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
        // TODO: check rights for velp groups
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
        return $scope.velpLocal.labels.indexOf(label.id) >= 0;
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

        var index = $scope.velpLocal.labels.indexOf(label.id);
        if (index < 0) {
            $scope.velpLocal.labels.push(label.id);
        }
        else if (index >= 0) {
            $scope.velpLocal.labels.splice(index, 1);
        }
    };

    /**
     * Updates velp groups of this velp.
     * @method updateVelpGroups
     * @param group - Group to be added or removed from the velp
     */
    $scope.updateVelpGroups = function (group) {
        var index = $scope.velpLocal.velp_groups.indexOf(group.id);
        if (index < 0) {
            $scope.velpLocal.velp_groups.push(group.id);
        }
        else if (index >= 0) {
            $scope.velpLocal.velp_groups.splice(index, 1);
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
            $scope.velpLocal.labels.push(labelToAdd.id);
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

    /**
     * Adds a new velp on form submit event.
     * @method addVelp
     * @param form - Form information
     */
    $scope.addVelp = function () {
        addNewVelpToDatabase();
        /*
        if ($scope.isGroupInVelp($scope.velp, default_velp_group) && default_velp_group.id === -1) {

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

        $scope.updateVelpList();
        */
    };

    /**
     * Adds a new velp to the database. Requires values in `$scope.newVelp` variable.
     * @method addNewVelpToDatabase
     */
    var addNewVelpToDatabase = function () {
        var velpToAdd = {
            labels: $scope.velpLocal.labels,
            used: 0,
            points: $scope.velpLocal.points,
            content: $scope.velpLocal.content,
            language_id: "FI",
            icon_id: null,
            valid_until: null,
            visible_to: 4, // TODO: make this work $scope.visible_options.value
            velp_groups: JSON.parse(JSON.stringify($scope.velp.velp_groups))
        };

        $scope.$parent.velps.push(velpToAdd);
        $scope.cancelEdit();

        //$scope.velp.edit = false;

        /*
        $scope.$parent.makePostRequest("/add_velp", velpToAdd, function (json) {
            velpToAdd.id = parseInt(json.data);

            $scope.resetNewVelp();
            $scope.velpToEdit = {content: "", points: "", labels: [], edit: false, id: -1};

            $scope.velps.push(velpToAdd);
            $scope.submitted.velp = false;
            //$scope.resetLabels();

            $scope.cancelEdit();
        });
        */
    };

    /**
     * Generates the default velp group and runs the custom method.
     * @method generateDefaultVelpGroup
     * @param method - Method to be run after this mehtod.

    $scope.generateDefaultVelpGroup = function (method) {
        if (default_velp_group.edit_access) {
            $scope.$parent.makePostRequest('/{0}/create_default_velp_group'.replace('{0}', doc_id), "{}", function (json) {
                var new_default_velp_group = json.data;
                new_default_velp_group.default = true;

                var index = $scope.velpGroups.indexOf(default_velp_group);
                $scope.velpGroups.splice(index, 1);

                if ($scope.velpGroups.indexOf(new_default_velp_group) < 0)
                    $scope.velpGroups.push(new_default_velp_group);

                default_velp_group = new_default_velp_group;
                console.log(new_default_velp_group);
                method();
            });
        }
        else {
            // No edit access to default velp group
        }
    };
    */

    /**
     * Get color for the object from colorPalette variable.
     * @method getColor
     * @param index - Index of the color in the colorPalette variable (modulo by lenght of color palette)
     * @returns {string} String representation of the color
     */
    $scope.getColor = function (index) {
        return colorPalette[index % colorPalette.length];
    };

}]);