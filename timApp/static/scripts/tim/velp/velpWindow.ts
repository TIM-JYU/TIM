import {IController, IFormController} from "angular";
import {timApp} from "tim/app";
import {$http} from "../util/ngimport";
import {Binding, clone, Require} from "../util/utils";
import {VelpSelectionController} from "./velpSelection";
import {ILabel, ILabelUI, INewLabel, IVelp, IVelpGroup, IVelpGroupUI, IVelpUI} from "./velptypes";

/**
 * Created by Seppo Tarvainen on 25.11.2016.
 *
 * @module velpWindow
 * @author Seppo Tarvainen
 * @licence MIT
 */

const UNDEFINED = "undefined";
export let colorPalette = ["blueviolet", "darkcyan", "orange", "darkgray", "cornflowerblue", "coral", "goldenrod", "blue"];

// TODO: add keyboard shortcuts to velps
// TODO: add min and max values for points
// TODO: user should be able to delete velp without any annotations

/**
 * Controller for velp Window
 * @lends module:velpWindow
 */
export class VelpWindowController implements IController {
    private onVelpSelect!: Binding<(params: {$VELP: IVelp}) => void, "&">;
    private velpLocal!: IVelp;
    public velp!: Binding<IVelpUI, "<">;
    private newLabel: INewLabel;
    private labelToEdit: INewLabel;
    private visible_options: {type: string; title: string; values: [number, number, number, number]; names: [string, string, string, string]};
    private settings: {teacherRightsError: string; labelContentError: string; velpGroupError: string; velpGroupWarning: string; velpContentError: string};
    private submitted: boolean;
    private hasEditAccess: boolean;
    private new!: Binding<boolean, "<">;
    private velpGroups!: Binding<IVelpGroupUI[], "<">;
    private velpSelection!: Require<VelpSelectionController>;
    private labels!: Binding<ILabelUI[], "<">;
    private docId!: Binding<number, "<">;
    private teacherRight!: Binding<boolean, "<">;

    $onInit() {
        this.velpLocal = JSON.parse(JSON.stringify(this.velp)); // clone object

        if (typeof this.velp.visible_to === UNDEFINED) {
            this.velp.visible_to = 4; // Everyone by default
        }

        // declare edit rights
        if (this.new) {
            this.hasEditAccess = true;
        } else {
            this.hasEditAccess = this.velpGroups.some((g) => g.edit_access && this.isGroupInVelp(g) || false);
        }
        this.velpSelection.rctrl.vctrl.registerVelpWindow(this);
    }

    constructor() {
        this.newLabel = {content: "", selected: true, valid: true, id: null};
        this.labelToEdit = {content: "", selected: false, edit: false, valid: true, id: null};
        this.visible_options = {
            type: "select",
            title: "Visible to",
            values: [1, 2, 3, 4],
            names: ["Just me", "Document owner", "Teachers", "Everyone"],
        };
        this.settings = {
            teacherRightsError: "You need to have teacher rights to change points in this document.",
            labelContentError: "Label content too short",
            velpGroupError: "Select at least one velp group.",
            velpGroupWarning: "All selected velp groups are hidden in the current area.",
            velpContentError: "Velp content too short",
        };
        this.submitted = false;
        this.hasEditAccess = false;
    }

    saveButtonText() {
        if (this.new) {
            return "Add velp";
        }
        return "Save";
    }

    /**
     * Toggles velp for editing. If another velp is currently open,
     * this method closes it.
     */
    toggleVelpToEdit() {
        const lastEdited = this.velpSelection.getVelpUnderEdit();

        if (lastEdited.edit && lastEdited.id !== this.velp.id) {
            // if (this.new === "true") this.$parent.resetNewVelp();
            this.velpSelection.resetEditVelp();
        }

        this.velp.edit = !this.velp.edit;
        if (!this.velp.edit) {
            this.cancelEdit();
        } else {
            if (this.new) {
                this.velpLocal = JSON.parse(JSON.stringify(this.velp));
                // TODO: focus velp content textarea
            }
            this.velpSelection.setVelpToEdit(this.velp, this.cancelEdit);
        }
    }

    /**
     * Saves velp to database
     * @param form
     */
    saveVelp(form: IFormController) {
        if (!form.$valid) {
            return;
        }
        form.$setPristine();
        // this.submitted = true;

        if (this.new) { // add new velp
            this.addVelp();
        } else { // edit velp
            this.editVelp();

        }
    }

    /**
     * Cancel edit and restore velp back to its original version
     * TODO: new velp reset does not work
     */
    cancelEdit() {
        this.velp = JSON.parse(JSON.stringify(this.velpLocal));
        this.velp.edit = false;
    }

    useVelp() {
        if (!this.velp.edit && !this.notAnnotationRights(this.velp.points)) {
            this.onVelpSelect({$VELP: this.velp});
        }

    }

    /**
     * Detect user right to annotation to document.
     * @param points - Points given in velp or annotation
     * @returns {boolean} - Right to make annotations
     */
    notAnnotationRights(points: number | null) {
        if (this.teacherRight) {
            return false;
        } else {
            if (points == null) {
                return false;
            } else {
                return true;
            }
        }
    }

    isVelpValid() {
        if (typeof this.velp.content === UNDEFINED) {
            return false;
        }
        // check if still original
        if (JSON.stringify(this.velpLocal) === JSON.stringify(this.velp)) {
            return false;
        }
        return this.isSomeVelpGroupSelected() && this.velp.content.length > 0;
    }

    setLabelValid(label: INewLabel) {
        label.valid = label.content.length > 0;
    }

    /**
     * Returns whether the velp contains the label or not.
     * @method isLabelInVelp
     * @param label - Label to check
     * @returns {boolean} Whether the velp contains the label or not.
     */
    isLabelInVelp(label: ILabel): boolean {
        if (label.id == null) {
            return false;
        }
        return this.velp.labels.indexOf(label.id) >= 0;
    }

    /**
     * Checks whether the velp contains the velp group.
     * @method isGroupInVelp
     * @param group - Velp group to check
     * @returns {boolean} Whether the velp contains the velp group or not
     */
    isGroupInVelp(group: IVelpGroup) {
        if (this.velp.velp_groups == null || group.id == null) {
            return false;
        }
        return this.velp.velp_groups.indexOf(group.id) >= 0;
    }

    /**
     * Updates the labels of the velp.
     * @method updateVelpLabels
     * @param label - Label to be added or removed from the velp
     */
    updateVelpLabels(label: ILabel) {
        if (label.id == null) {
            return;
        }
        const index = this.velp.labels.indexOf(label.id);
        if (index < 0) {
            this.velp.labels.push(label.id);
        } else if (index >= 0) {
            this.velp.labels.splice(index, 1);
        }
    }

    /**
     * Updates velp groups of this velp.
     * @method updateVelpGroups
     * @param group - Group to be added or removed from the velp
     */
    updateVelpGroups(group: IVelpGroup) {
        if (group.id == null) {
            return;
        }
        const index = this.velp.velp_groups.indexOf(group.id);
        if (index < 0) {
            this.velp.velp_groups.push(group.id);
        } else if (index >= 0) {
            this.velp.velp_groups.splice(index, 1);
        }
    }

    /**
     * Checks if the velp has any velp groups selected.
     * @method isSomeVelpGroupSelected
     * @returns {boolean} Whether velp has any groups selected or not
     */
    isSomeVelpGroupSelected() {
        if (typeof this.velp.velp_groups === UNDEFINED) {
            return false;
        }
        return this.velp.velp_groups.length > 0;
    }

    isSomeVelpGroupShown() {
        if (typeof this.velp.velp_groups === UNDEFINED || this.velp.velp_groups.length === 0) {
            return true;
        }

        for (let i = 0; i < this.velp.velp_groups.length; i++) {
            for (let j = 0; j < this.velpGroups.length; j++) {
                if (this.velpGroups[j].id === this.velp.velp_groups[i] && this.velpGroups[j].show) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Adds new label to this velp.
     * @method addLabel
     */
    async addLabel() {

        if (this.newLabel.content.length < 1) {
            this.newLabel.valid = false;
            return;
        }

        const data = {
            content: this.newLabel.content,
            language_id: "FI", // TODO: Change to user language
        };

        const json = await $http.post<{id: number}>("/add_velp_label", data);
        const labelToAdd = {
            ...data,
            selected: false,
            id: json.data.id,
        };
        this.resetNewLabel();
        this.labels.push(labelToAdd);
        // this.labelAdded = false;
        this.velp.labels.push(labelToAdd.id);
    }

    /**
     * Selects the label for editing.
     * @method toggleLabelToEdit
     * @param label - Label to edit
     */
    toggleLabelToEdit(label: INewLabel) {

        if (this.labelToEdit.edit && label.id === this.labelToEdit.id) {
            this.cancelLabelEdit(label);
            return;
        }

        if (this.labelToEdit.edit) {
            this.labelToEdit.edit = false;
            for (let i = 0; i < this.labels.length; i++) {
                this.labels[i].edit = false;
            }
        }

        label.edit = true;
        this.copyLabelToEditLabel(label);
        this.setLabelValid(this.labelToEdit);
    }

    cancelLabelEdit(label: INewLabel) {
        label.edit = false;
        this.labelToEdit = {content: "", selected: false, edit: false, valid: true, id: null};
    }

    clearVelpColor() {
        this.velp.color = "";
    }

    isVelpCustomColor() {
        if (this.velp.color) {
            return this.velp.color.length === 7; // hex colors are 7 characters long
        }
        return false;
    }

    copyLabelToEditLabel(label: INewLabel) {
        this.labelToEdit = clone(label);
    }

    /**
     * Edits the label according to the this.labelToedit variable.
     * All required data exists in the this.labelToedit variable,
     * including the ID of the label.
     * TODO: This can be simplified
     * @method editLabel
     */
    editLabel() {
        if (this.labelToEdit.content.length < 1) {
            return;
        }

        let updatedLabel = null;
        for (let i = 0; i < this.labels.length; i++) {
            if (this.labels[i].id === this.labelToEdit.id) {
                this.labelToEdit.edit = false;
                this.labels[i].content = this.labelToEdit.content;
                this.labels[i].edit = false;
                updatedLabel = this.labels[i];
                break;
            }
        }

        $http.post("/update_velp_label", updatedLabel);
    }

    /**
     * Reset new label information to the initial (empty) state.
     * @method resetNewLabel
     */
    resetNewLabel() {
        this.newLabel = {content: "", selected: true, valid: true, id: null};
    }

    /**
     * Return true if user has teacher rights.
     * @returns {boolean}
     */
    allowChangePoints() {
        return this.teacherRight;
    }

    async editVelp() {
        const default_velp_group = this.velpSelection.getDefaultVelpGroup();

        if (this.isGroupInVelp(default_velp_group) && default_velp_group.id === -1) {
            await this.handleDefaultVelpGroupIssue();
            await this.updateVelpInDatabase();
        } else if (this.velp.velp_groups.length > 0) {
            await this.updateVelpInDatabase();
        }
    }

    async updateVelpInDatabase() {
        await $http.post("/{0}/update_velp".replace("{0}", this.docId.toString()), this.velp);
        this.velpLocal = JSON.parse(JSON.stringify(this.velp));
        this.toggleVelpToEdit();
    }

    /**
     * Adds a new velp on form submit event.
     * @method addVelp
     */
    async addVelp() {
        const defaultVelpGroup = this.velpSelection.getDefaultVelpGroup();
        if (this.isGroupInVelp(defaultVelpGroup) && defaultVelpGroup.id === -1) {
            await this.handleDefaultVelpGroupIssue();
            await this.addNewVelpToDatabase();
        } else if (this.velp.velp_groups.length > 0) {
            await this.addNewVelpToDatabase();
        }
        this.velpSelection.updateVelpList();
    }

    /**
     * Adds a new velp to the database. Requires values in `this.newVelp` variable.
     * @method addNewVelpToDatabase
     */
    async addNewVelpToDatabase() {

        // this.velp.edit = false;
        const data = {
            labels: this.velp.labels,
            used: 0,
            points: this.velp.points,
            content: this.velp.content,
            default_comment: this.velp.default_comment,
            language_id: "FI",
            icon_id: null,
            valid_until: null,
            color: this.velp.color,
            visible_to: this.velp.visible_to,
            velp_groups: JSON.parse(JSON.stringify(this.velp.velp_groups)),
        };
        const json = await $http.post<number>("/add_velp", data);
        const velpToAdd: IVelp = {
            id: json.data,
            ...data,
        };
        velpToAdd.id = json.data;
        this.velpSelection.velps.push(velpToAdd);

        this.velpLocal.velp_groups = velpToAdd.velp_groups;
        this.velpLocal.labels = velpToAdd.labels;

        this.toggleVelpToEdit();
        this.velpSelection.updateVelpList();

        // this.velp =  JSON.parse(JSON.stringify(this.velpLocal));
        // this.velpLocal = JSON.parse(JSON.stringify(this.velp));
        /*
         velpToAdd.id = parseInt(json.data);

         this.resetNewVelp();
         this.velpToEdit = {content: "", points: "", labels: [], edit: false, id: -1};

         this.velps.push(velpToAdd);
         this.submitted.velp = false;
         //this.resetLabels();
         */

    }

    /**
     *
     */
    async handleDefaultVelpGroupIssue() {

        const oldDefaultGroup = this.velpSelection.getDefaultVelpGroup();

        const newDefaultGroup = await this.velpSelection.generateDefaultVelpGroup();
        if (newDefaultGroup == null) {
            return;
        }
        if (oldDefaultGroup.id == null || newDefaultGroup.id == null) {
            return;
        }
        const oldGroupIndex = this.velp.velp_groups.indexOf(oldDefaultGroup.id);
        if (oldGroupIndex >= 0) {
            this.velp.velp_groups.splice(oldGroupIndex, 1);
        }

        this.velp.velp_groups.push(newDefaultGroup.id);
        this.velpSelection.setDefaultVelpGroup(newDefaultGroup);
    }

    /**
     * Get color for the object from colorPalette variable.
     * @method getColor
     * @param index - Index of the color in the colorPalette variable (modulo by length of color palette)
     * @returns {string} String representation of the color
     */
    getColor(index: number) {
        return colorPalette[index % colorPalette.length];
    }

    getCustomColor() {
        if (typeof this.velp.color !== UNDEFINED || this.velp.color != null) {
            return this.velp.color;
        }
    }
}

/**
 * Angular directive for velp selection
 */
timApp.component("velpWindow", {
    bindings: {
        advancedOn: "=",
        docId: "<",
        index: "<",
        labels: "=",
        new: "<",
        onVelpSelect: "&",
        teacherRight: "<",
        velp: "=",
        velpGroups: "=", // all velpgroups, not just selected ones
    },
    require: {
        velpSelection: "^velpSelection",
    },
    controller: VelpWindowController,
    templateUrl: "/static/templates/velpWindow.html",
});
