import angular from "angular";
import $ from "jquery";
import {timApp} from "tim/app";
import * as velpSummary from "tim/directives/velpSummary";
import {colorPalette} from "tim/directives/velpWindow";
import {markAsUsed} from "tim/utils";
import {$http, $q, $window} from "../ngimport";
import {IAnnotation, ILabel, IUIFields, IVelp, IVelpGroup, VelpGroupSelectionType} from "./velptypes";

markAsUsed(velpSummary);

/**
 * The directive retrieves all the data from the server including velps, labels, velp groups and annotations.
 * The directive also handles majority of the functionality that is relevant in handling velps, labels and velp groups.
 *
 * @module velpSelection
 * @author Joonas Lattu
 * @author Petteri Palojärvi
 * @author Seppo Tarvainen
 * @licence MIT
 * @copyright 2016 Timber project members
 */

const UNDEFINED = "undefined";

// TODO: show velps with same name side by side. Make changes to the template.

/**
 * Controller for velp selection
 * @lends module:velpSelection
 */
export class VelpSelectionController {
    public velps: (IVelp & IUIFields)[];
    private annotations: IAnnotation[];
    private labels: (ILabel & IUIFields)[];
    private velpGroups: (IVelpGroup & IUIFields)[];
    private newVelp: IVelp & IUIFields;
    private velpToEdit: IVelp & IUIFields;
    private newLabel: ILabel & IUIFields;
    private labelToEdit: ILabel & IUIFields;
    private newVelpGroup: IVelpGroup & IUIFields;
    private settings: {selectedAllShows: boolean; selectedAllDefault: boolean};
    private submitted: {velp: boolean; velpGroup: boolean};
    private groupAttachment: {target_type: number; id: any};
    private groupSelections: {};
    private groupDefaults: {};
    private docId: number;
    private order: string;
    private selectedLabels: number[];
    private advancedOn: boolean;
    private velpOrderingKey: string;
    private velpLabelsKey: string;
    private advancedOnKey: string;
    private default_velp_group: IVelpGroup;
    private labelAdded: boolean;
    private selectedElement: HTMLElement;
    private default_personal_velp_group: IVelpGroup;
    private previewReleased: boolean;
    private $parent: any; // TODO use bindings

    // Data
    constructor() {
        this.velps = [];
        this.annotations = [];
        this.labels = [];
        this.velpGroups = [];

        this.newVelp = {
            content: "",
            default_comment: "",
            points: "",
            labels: [],
            edit: false,
            id: -2,
            velp_groups: [],
            visible_to: 4,
            icon_id: null,
            language_id: "FI",
            color: null,
            valid_until: null,
        };
        this.velpToEdit = {
            content: "",
            default_comment: "",
            points: "",
            labels: [],
            edit: false,
            id: -1,
            velp_groups: [],
            visible_to: 4,
            icon_id: null,
            language_id: "FI",
            color: null,
            valid_until: null,
        };
        this.newLabel = {content: "", selected: false, edit: false, valid: true, id: null};
        this.labelToEdit = {content: "", selected: false, edit: false, id: -3};
        this.newVelpGroup = {name: "", target_type: 0, selected: false, id: null, show: false, default: false};

        this.settings = {selectedAllShows: false, selectedAllDefault: false};
        this.submitted = {velp: false, velpGroup: false};

        this.groupAttachment = {target_type: 1, id: null};

        this.groupSelections = {};
        this.groupDefaults = {};

        // Dictionaries for easier searching: Velp ids? Label ids? Annotation ids?
        const doc_id = this.docId;
        let default_velp_group = {
            id: -1,
            name: "No access to default group",
            edit_access: false,
            show: true,
            default: true,
            selected: false,
            target_type: null,
        }; // TODO Use route to add this information
        this.default_velp_group = default_velp_group;
        this.default_personal_velp_group = {id: -2, name: "Personal-default", target_type: null, default: true};

        this.velpOrderingKey = "velpOrdering_" + doc_id;
        this.velpLabelsKey = "velpLabels_" + doc_id;
        this.advancedOnKey = "advancedOn"; // TODO: should this be document specific?

        // Values to store in localstorage:
        this.order = this.getValuesFromLocalStorage(this.velpOrderingKey, "content");
        this.selectedLabels = JSON.parse(this.getValuesFromLocalStorage(this.velpLabelsKey, "[]"));
        this.advancedOn = JSON.parse(this.getValuesFromLocalStorage(this.advancedOnKey, "false"));

        // Get velpgroup data
        const promises = [];
        promises.push();
        const p1 = $http.get<IVelpGroup[]>("/{0}/get_velp_groups".replace("{0}", doc_id.toString()));
        promises.push(p1);
        p1.then((response) => {
            this.velpGroups = response.data;

            // Get default velp group data

            const p2 = $http.get<typeof default_velp_group>("/{0}/get_default_velp_group".replace("{0}", doc_id.toString()));
            promises.push(p2);
            p2.then((response2) => {
                default_velp_group = response2.data;

                // If doc_default exists already for some reason but isn't a velp group yet, remove it from fetched velp groups
                for (const g of this.velpGroups) {
                    if (g.name === default_velp_group.name && default_velp_group.id < 0) {
                        const extraDefaultIndex = this.velpGroups.indexOf(g);
                        this.velpGroups.push(default_velp_group);
                        this.velpGroups.splice(extraDefaultIndex, 1);
                        break;
                    }
                }

                if (default_velp_group.edit_access) {
                    this.velpGroups.forEach((g) => {
                        if (g.id === default_velp_group.id) {
                            g.selected = true;
                        }
                    });
                    default_velp_group.selected = true;
                    this.newVelp.velp_groups.push(default_velp_group.id);
                }

                // this.groupDefaults["0"] = [default_velp_group];

                // Get personal velp group data
                const p3 = $http.get<IVelpGroup & {created_new_group: boolean}>("/get_default_personal_velp_group");
                promises.push(p3);
                p3.then((response3) => {
                    const data = response3.data;
                    this.default_personal_velp_group = {id: data.id, name: data.name, target_type: null, default: true};

                    if (data.created_new_group) {
                        this.velpGroups.push(data);
                    }

                    if (!default_velp_group.edit_access) {
                        this.newVelp.velp_groups.push(this.default_personal_velp_group.id);
                        /*this.velpGroups.some(function (g) {
                         if (g.id === default_personal_velp_group.id)
                         g.selected = true;
                         });*/
                    }

                    if (this.default_personal_velp_group.id < 0) {
                        this.velpGroups.push(default_velp_group);
                    }

                    /*
                     this.velpGroups.forEach(function(g) {
                     if (g.id === default_personal_velp_group.id){
                     if (typeof g.default === UNDEFINED){
                     g.default = true;
                     }

                     } else if (g.id === default_velp_group.id){

                     if (typeof g.default === UNDEFINED){
                     g.default = true;
                     }

                     //g.show = true;
                     //g.default = true;
                     }
                     });
                     */
                    this.updateVelpList();
                });

                if (default_velp_group.id < 0) {
                    this.velpGroups.push(default_velp_group);
                }

            });

            // Get velp and annotation data
            const p4 = $http.get<IVelp[]>("/{0}/get_velps".replace("{0}", doc_id.toString()));
            promises.push(p4);
            p4.then((response2) => {
                this.velps = response2.data;
                this.velps.forEach((v) => {
                    v.used = 0;
                    v.edit = false;
                    if (typeof v.labels === UNDEFINED) {
                        v.labels = [];
                    }
                });
            });

            /*
             $http.get('/get_default_personal_velp_group').success(function (data) {
             default_personal_velp_group = {id: data.id, name: data.name};
             if (data.created_new_group) {
             this.velpGroups.push(data);
             }
             if (!default_velp_group.edit_access) {
             this.newVelp.velp_groups.push(default_personal_velp_group.id);
             this.velpGroups.some(function (g) {
             if (g.id === default_personal_velp_group.id)
             return g.selected = true;
             });
             }
             });
             */
            /*
             p = $http.get('/{0}/get_annotations'.replace('{0}', doc_id));
             promises.push(p);
             p.success(function (data) {
             this.annotations = data;
             this.loadDocumentAnnotations();
             });
             */
            // Get label data
            const p5 = $http.get<ILabel[]>("/{0}/get_velp_labels".replace("{0}", doc_id.toString()));
            promises.push(p5);
            p5.then((response2) => {
                this.labels = response2.data;
                this.labels.forEach((l) => {
                    l.edit = false;
                    l.selected = false;
                    for (let i = 0; i < this.selectedLabels.length; i++) {
                        if (l.id === this.selectedLabels[i]) {
                            l.selected = true;
                        }
                    }
                });
            });

            const p6 = $http.get("/{0}/get_velp_group_personal_selections".replace("{0}", doc_id.toString()));
            promises.push(p6);
            p6.then((response2) => {
                this.groupSelections = response2.data;
                if (!this.groupSelections.hasOwnProperty("0")) {
                    this.groupSelections["0"] = [];
                }

                const docSelections = this.groupSelections["0"];

                this.velpGroups.forEach((g) => {
                    g.show = false;
                    for (let i = 0; i < docSelections.length; i++) {
                        if (docSelections[i].id === g.id && docSelections[i].selected) {
                            g.show = true;
                            break;
                        }
                    }
                });
                //this.updateVelpList();

            });

            const p7 = $http.get("/{0}/get_velp_group_default_selections".replace("{0}", doc_id.toString()));
            promises.push(p7);
            p7.then((response2) => {
                this.groupDefaults = response2.data;

                const docDefaults = this.groupDefaults["0"];

                this.velpGroups.forEach((g) => {

                    for (let i = 0; i < docDefaults.length; i++) {
                        if (docDefaults[i].id === g.id && docDefaults[i].selected) {
                            g.default = true;
                            break;
                        }
                    }
                });
                //this.updateVelpList();

            });

            $q.all(promises).then(() => {
                this.updateVelpList();
            });

        });
    }

    // Methods

    /**
     * Gets values from local storage.
     * If values are not found, returns given default value.
     * @param key - Key in localstorage
     * @param defaultValue - default value to return if key is not found
     * @returns {*}
     */
    getValuesFromLocalStorage(key: string, defaultValue: string): string {
        if ($window.localStorage.getItem(key) === null) {
            return defaultValue;
        }
        return $window.localStorage.getItem(key);
    }

    changeOrdering(order) {
        $window.localStorage.setItem(this.velpOrderingKey, order);
    }

    changeSelectedLabels() {
        $window.localStorage.setItem(this.velpLabelsKey, JSON.stringify(this.selectedLabels));
    }

    /**
     * Return true if user has teacher rights.
     * @returns {boolean}
     */

    allowChangePoints() {
        return this.$parent.vctrl.item.rights.teacher;
    }

    /**
     * Get color for the object from colorPalette variable.
     * @method getColor
     * @param index - Index of the color in the colorPalette variable (modulo by lenght of color palette)
     * @returns {string} String representation of the color
     */
    getColor(index) {
        return colorPalette[index % colorPalette.length];
    }

    /**
     * Toggles the label's selected attribute.
     * @method toggleLabel
     * @param label - Label to toggle
     */
    toggleLabel(label) {
        label.selected = !label.selected;
        const labelIndex = this.selectedLabels.indexOf(label.id);
        if (labelIndex < 0) {
            this.selectedLabels.push(label.id);
        } else {
            this.selectedLabels.splice(labelIndex, 1);
        }
        this.changeSelectedLabels(); // Update localstorage

    }

    /**
     * Toggles the label's edit attribute.
     * @method toggleLabelToEdit
     * @param label - Label to edit

     this.toggleLabelToEdit = function (label) {
        label.edit = !label.edit;
    };
     */

    setAdvancedOnlocalStorage(value) {
        $window.localStorage.setItem(this.advancedOnKey, value.toString());
    }

    /**
     * Adds new label tp the specified velp.
     * @method addLabel
     * @param velp - Velp where the label is to be added.
     */
    async addLabel(velp) {

        if (this.newLabel.content.length < 1) {
            this.newLabel.valid = false;
            return;
        }

        const labelToAdd = {
            id: null,
            content: this.newLabel.content,
            language_id: "FI", // TODO: Change to user language
            selected: false,
        };

        const response = await $http.post<{id: number}>("/add_velp_label", labelToAdd);
        labelToAdd.id = response.data.id;
        this.resetNewLabel();
        this.labels.push(labelToAdd);
        this.labelAdded = false;
        velp.labels.push(labelToAdd.id);
    }

    /**
     * Adds a new velp on form submit event.
     * @method addVelp
     * @param form - Form information

     this.addVelp = function (form) {
        var valid = form.$valid;
        this.submitted.velp = true;
        if (!valid) return;

        // Form is valid:
        form.$setPristine();

        if (this.isGroupInVelp(this.newVelp, default_velp_group) && default_velp_group.id === -1) {
            // this.isGroupInVelp(this.newVelp, -1);
            //this.newVelp.velp_groups = [default_velp_group];

            var old_default_group = default_velp_group;
            this.generateDefaultVelpGroup(function () {

                var oldGroupIndex = this.newVelp.velp_groups.indexOf(old_default_group.id); // -1 = old
                if (oldGroupIndex >= 0)
                    this.newVelp.velp_groups.splice(oldGroupIndex, 1);

                this.newVelp.velp_groups.push(default_velp_group.id);

                addNewVelpToDatabase();
            });

        } else if (this.newVelp.velp_groups.length > 0) {
            addNewVelpToDatabase();
        }

        this.updateVelpList();
    };
     */
    /**
     * Adds a new velp to the database. Requires values in `this.newVelp` variable.
     * @method addNewVelpToDatabase
     */
    async addNewVelpToDatabase() {
        const velpToAdd = {
            id: null,
            labels: this.newVelp.labels,
            used: 0,
            points: this.newVelp.points,
            content: this.newVelp.content,
            language_id: "FI",
            icon_id: null,
            valid_until: null,
            visible_to: this.visible_options.value,
            velp_groups: JSON.parse(JSON.stringify(this.newVelp.velp_groups)),
            default_comment: "",
            color: null,
        };
        this.velpToEdit.edit = false;
        this.newVelp.edit = false;

        const json = await $http.post<number>("/add_velp", velpToAdd);
        velpToAdd.id = json.data;
        this.resetNewVelp();
        this.velpToEdit = {
            content: "",
            points: "",
            labels: [],
            edit: false,
            id: -1,
            default_comment: "",
            velp_groups: [],
            visible_to: null,
            icon_id: null,
            language_id: "FI",
            color: null,
            valid_until: null,
        };
        this.velps.push(velpToAdd);
        this.submitted.velp = false;
        //this.resetLabels();
    }

    /**
     * Deselects all the labels.
     * @method deselectLabels
     */
    deselectLabels() {
        for (let i = 0; i < this.labels.length; i++) {
            if (this.labels[i].selected) {
                this.toggleLabel(this.labels[i]);
            }
        }
    }

    /**
     * Selects the label for editing.
     * @method toggleLabelToEdit
     * @param label - Label to edit

     this.toggleLabelToEdit = function (label) {
        if (label.id === this.labelToEdit.id && label.edit) {
            label.edit = false;
            this.labelToEdit = {content: "", selected: false, edit: false};
            return;
        }

        if (this.labelToEdit.edit) {
            this.labelToEdit.edit = false;
            for (var i = 0; i < this.labels.length; i++) {
                this.labels[i].edit = false;
            }
        }

        label.edit = true;
        this.labelToEdit = Object.create(label);
    };
     */

    /**
     * Selects velp to edit
     * @method selectVelpToEdit
     */
    openCreateNewVelpWindow() {
        const velp = angular.element(
            document.getElementById("newVelp"),
        ).isolateScope();
        velp.toggleVelpToEdit();

        //if (this.getVelpUnderEdit().id !== this.newVelp.id)
        //    this.resetEditVelp();

        //this.resetEditVelp = this.resetNewVelp;

        /*
         if (velp.id === this.velpToEdit.id && velp.edit) {
         velp.edit = false;
         this.velpToEdit = {content: "", points: "", labels: [], edit: false};
         return;
         }

         if (this.velpToEdit.edit) {
         this.velpToEdit.edit = false;
         for (var i = 0; i < this.velps.length; i++) {
         this.velps[i].edit = false;
         }
         this.newVelp.edit = false;
         }

         velp.edit = true;

         this.velpToEdit = (JSON.parse(JSON.stringify(velp)));
         */
    }

    /**
     * Edits velp according to the this.velpToEdit variable.
     * All required data exists in the this.velpToedit variable,
     * including the ID of the velp.
     * @method editVelp
     * @param form - Velp form

     this.editVelp = function (form) {
        var valid = form.$valid;
        this.submitted.velp = true;
        if (!valid) return;

        form.$setPristine();

        // TODO: Make velpGroups to [{'id':1, 'selected':'True'}]

        if (this.isGroupInVelp(this.velpToEdit, default_velp_group) && default_velp_group.id === -1) {

            var old_default_group = default_velp_group;
            this.generateDefaultVelpGroup(function () {

                var oldGroupIndex = this.velpToEdit.velp_groups.indexOf(old_default_group.id); // -1 = old
                if (oldGroupIndex >= 0)
                    this.velpToEdit.velp_groups.splice(oldGroupIndex, 1);
                this.velpToEdit.velp_groups.push(default_velp_group.id);

                this.makePostRequest("/{0}/update_velp".replace('{0}', doc_id), this.velpToEdit, function (json) {
                });
            });

        } else if (this.velpToEdit.velp_groups.length > 0) {
            this.makePostRequest("/{0}/update_velp".replace('{0}', doc_id), this.velpToEdit, function (json) {
            });
        }

        for (var i = 0; i < this.velps.length; i++) {
            if (this.velps[i].id === this.velpToEdit.id) {
                this.velpToEdit.edit = false;
                this.velps[i] = this.velpToEdit;
                break;
            }
        }

        this.resetEditVelp();
    };
     */

    /**
     * Generates the default velp group and runs the custom method.
     * @method generateDefaultVelpGroup
     */
    async generateDefaultVelpGroup(): Promise<IVelpGroup> {
        if (this.default_velp_group.edit_access) {
            const json = await $http.post<IVelpGroup>("/{0}/create_default_velp_group".replace("{0}", this.docId.toString()), "{}");
            const new_default_velp_group = json.data;
            new_default_velp_group.default = true;

            const index = this.velpGroups.indexOf(this.default_velp_group);
            this.velpGroups.splice(index, 1);

            if (this.velpGroups.indexOf(new_default_velp_group) < 0) {
                this.velpGroups.push(new_default_velp_group);
            }

            this.default_velp_group = new_default_velp_group;
            return new_default_velp_group;
        } else {
            // No edit access to default velp group
            return null;
        }
    }

    /**
     * Selects or deselects velp for being edited.
     * @param velp - Velp information, contains all edited info
     * @param resetFunction - Function to execute in cancel edit
     */
    setVelpToEdit(velp, resetFunction) {
        this.velpToEdit = velp;
        this.resetEditVelp = resetFunction;
    }

    /**
     * Initially resets this.velpToEdit variable to the original (empty) state.
     * NOTE! this function is replaced in 'setVelpToEdit'. When replaced
     * this mehtod resets the velp that is being edited to its original state.
     * @method resetEditVelp
     */
    resetEditVelp() {
        this.velpToEdit = {
            content: "",
            points: "",
            labels: [],
            edit: false,
            id: -1,
            velp_groups: [],
            default_comment: "",
            visible_to: null,
            icon_id: null,
            language_id: "FI",
            color: null,
            valid_until: null,
        };
    }

    /**
     * Returns whether velp is being edited or not.
     * @returns Boolean
     */
    getVelpUnderEdit() {
        return this.velpToEdit;
    }

    /**
     * Edits the label according to the this.labelToedit variable.
     * All required data exists in the this.labelToedit variable,
     * including the ID of the label.
     * @method editLabel
     */
    editLabel() {
        if (this.labelToEdit.content.length < 1) {
            this.labelToEdit.edit = false;
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
     * Reset new velp information to the initial (empty) state.
     * @method resetNewVelp
     */
    resetNewVelp() {
        this.newVelp = {
            content: "",
            points: "",
            labels: [],
            edit: false,
            id: -2,
            velp_groups: this.newVelp.velp_groups,
            default_comment: "",
            visible_to: null,
            icon_id: null,
            language_id: "FI",
            color: null,
            valid_until: null,
        };
    }

    /**
     * Reset new label information to the initial (empty) state.
     * @method resetNewLabel
     */
    resetNewLabel() {
        this.newLabel = {content: "", selected: true, valid: true, id: null};
    }

    /** Velpgroup methods **/

    getDefaultVelpGroup() {
        return this.default_velp_group;
    }

    setDefaultVelpGroup(group: IVelpGroup) {
        this.default_velp_group = group;
    }

    /**
     * Updates the velp list according to how the velp groups are selected in the area.
     * @method updateVelpList
     */
    updateVelpList() {
        this.velpGroups.forEach((g) => {
            if (this.selectedElement !== null && this.groupAttachment.target_type === 1) {
                g.show = this.isVelpGroupShownHere(g.id, this.selectedElement.id);
                g.default = this.isVelpGroupDefaultHere(g.id, this.selectedElement.id);
            } else {
                g.show = this.isVelpGroupShownHere(g.id, 0);
                g.default = this.isVelpGroupDefaultHere(g.id, 0);
            }
        });
    }

    /**
     * Return whether the group is shown based on the various selected and default values.
     * @method isVelpGroupShownHere
     * @param groupId - VelpGroup ID
     * @param paragraphId - Paragraph ID or "0" for the document
     * @returns {boolean} Whether the velp group is shown here or not
     */
    isVelpGroupShownHere(groupId, paragraphId) {
        let returnValue;
        // Are we checking for the whole document? This "if" might be unnecessary.
        if (paragraphId === "0") {
            returnValue = this.lazyIsVelpGroupSelectedInParagraph(groupId, paragraphId);
            if (returnValue !== null) {
                return returnValue;
            }
            // Not set for the document, we'll try the defaults instead.
            returnValue = this.lazyIsVelpGroupDefaultInParagraph(groupId, paragraphId);
            if (returnValue !== null) {
                return returnValue;
            }
        } else {
            // First check "selected" attributes for paragraph.
            returnValue = this.lazyIsVelpGroupSelectedInParagraph(groupId, paragraphId);
            if (returnValue !== null) {
                return returnValue;
            }
            // Found nothing, we try the defaults instead.
            returnValue = this.isVelpGroupDefaultHere(groupId, paragraphId);
            if (returnValue !== null) {
                return returnValue;
            }
        }
        // Ok, hard coded ones left:
        return this.isVelpGroupDefaultFallBack(groupId);
    }

    /**
     * Returns whether the velp group is default in specified paragraph (or document) or not.
     * @method isVelpGroupDefaultHere
     * @param groupId - Velp group ID
     * @param paragraphId - Paragraph ID or "0" for the document
     * @returns {boolean} Whether the velp group is default here or not.
     */
    isVelpGroupDefaultHere(groupId, paragraphId) {
        let returnValue;
        // First check defaults here
        returnValue = this.lazyIsVelpGroupDefaultInParagraph(groupId, paragraphId);
        if (returnValue !== null) {
            return returnValue;
        }
        // and then try document instead. If we started with a document, this is wasted work.
        returnValue = this.lazyIsVelpGroupDefaultInParagraph(groupId, "0");
        if (returnValue !== null) {
            return returnValue;
        }
        return this.isVelpGroupDefaultFallBack(groupId);
    }

    /**
     * Checks whether the given velp group is either personal default or document default group.
     * Personal default group and the document default group have always default, unless the user has
     * specified otherwise.
     * @method isVelpGroupDefaultFallBack
     * @param groupId - Velp group ID
     * @returns {boolean} Whether the group is personal default or document default group or not.
     */
    isVelpGroupDefaultFallBack(groupId) {
        return (groupId === this.default_personal_velp_group.id || groupId === this.default_velp_group.id);
    }

    /**
     * Helper function for checking if the velp group is shown in the paragraph or not.
     * Despite the name, can check document selections as well.
     * @method lazyIsVelpGroupSelectedInParagraph
     * @param groupId - Velp group ID
     * @param paragraphId - Paragraph ID or "0" for the document
     * @returns true/false/null
     */
    lazyIsVelpGroupSelectedInParagraph(groupId, paragraphId) {
        return this.checkCollectionForSelected(groupId, paragraphId, this.groupSelections);
    }

    /**
     * Helper function for checking if the velp group is default in the paragraph or not.
     * Despite the name, can check document selections as well.
     * @method lazyIsVelpGroupDefaultInParagraph
     * @param groupId - Velp group ID
     * @param paragraphId - Paragraph ID or "0" for the document
     * @returns true/false/null
     */
    lazyIsVelpGroupDefaultInParagraph(groupId, paragraphId) {
        return this.checkCollectionForSelected(groupId, paragraphId, this.groupDefaults);
    }

    /**
     * Checks whether the collection is selected or not.
     * @method checkCollectionForSelected
     * @param groupId - Velp group ID
     * @param paragraphId - Paragraph ID or document "0".
     * @param collection - Shows or defaults
     * @returns {boolean|null} Whether the collection is selected or not. Null if paragraph is not found.
     */
    checkCollectionForSelected(groupId, paragraphId, collection) {
        if (collection.hasOwnProperty(paragraphId)) {
            const selectionsHere = collection[paragraphId];
            for (let i = 0; i < selectionsHere.length; ++i) {
                if (selectionsHere[i].id === groupId) {
                    return selectionsHere[i].selected;
                }
            }
        }
        return null;
    }

    /**
     * Adds a velp group on form submit event.
     * @method addVelpGroup
     * @param form - Velp group form
     */
    async addVelpGroup(form) {
        const valid = form.$valid;
        this.submitted.velpGroup = true;
        if (!valid) {
            return;
        }

        form.$setPristine();

        const json = await $http.post<IVelpGroup>("/{0}/create_velp_group".replace("{0}", this.docId.toString()),
            this.newVelpGroup);
        const group: IVelpGroup & IUIFields = json.data;
        group.selected = false;
        group.show = true;
        this.velpGroups.push(json.data);

        // TODO: show in selected area
    }

    /**
     * Changes velp group (default or show) selection in the current element or in the document.
     * @method changeVelpGroupSelection
     * @param group - Velp group
     * @param type - "show" or "default"
     */
    changeVelpGroupSelection(group: IVelpGroup, type: VelpGroupSelectionType) {

        let target_id: string;
        let target_type: number;
        if (this.groupAttachment.target_type === 1 && this.selectedElement !== null) {
            target_id = this.selectedElement.id;
            target_type = 1;
        } else {
            target_id = "0";
            target_type = 0;
        }

        const data = Object.assign({target_id, target_type, selection_type: type}, group);

        if (type === "show") {
            $http.post("/{0}/change_selection".replace("{0}", this.docId.toString()), data);

            this.groupSelections[target_id] = [];

            this.velpGroups.forEach((g) => {
                this.groupSelections[target_id].push({id: g.id, selected: g.show});
            });

            /*
             if (!this.groupSelections.hasOwnProperty(group.target_id))
             this.groupSelections[group.target_id] = [];

             var groups = this.groupSelections[group.target_id];
             for (var i = 0; i < groups.length; i++) {
             if (groups[i].id === group.id) {
             groups[i].selected = group.show;
             found = true;
             break;
             }
             }
             if (!found) {
             this.groupSelections[group.target_id].push({id: group.id, selected: group.show});
             }
             */
        } else if (type === "default") {
            $http.post("/{0}/change_selection".replace("{0}", this.docId.toString()), data);

            this.groupDefaults[target_id] = [];

            this.velpGroups.forEach((g) => {
                this.groupDefaults[target_id].push({id: g.id, selected: g.default});
            });

            /*if (!this.groupDefaults.hasOwnProperty(group.target_id))
             this.groupDefaults[group.target_id] = [];

             var defGroups = this.groupDefaults[group.target_id];
             found = false;
             for (var j = 0; j < defGroups.length; j++) {
             if (defGroups[j].id === group.id) {
             defGroups[j].selected = group.show;
             found = true;
             break;
             }
             }
             if (!found) {
             this.groupDefaults[group.target_id].push({id: group.id, selected: group.default});
             }
             */
        }

        this.updateVelpList();
    }

    /**
     * Changes all velp group selections (defaults and shows).
     * @method changeAllVelpGroupSelections
     * @param type - "show" or "default"
     */
    changeAllVelpGroupSelections(type: VelpGroupSelectionType) {

        let targetID;
        let targetType;

        if (this.groupAttachment.target_type === 1 && this.selectedElement !== null) {
            targetID = this.selectedElement.id;
            targetType = 1;
        } else {
            targetID = "0";
            targetType = 0;
        }

        if (type === "show") {
            this.groupSelections[targetID] = [];
            if (!this.settings.selectedAllShows) {

                this.velpGroups.forEach((g) => {
                    this.groupSelections[targetID].push({id: g.id, selected: false});
                });
            } else {
                this.velpGroups.forEach((g) => {
                    this.groupSelections[targetID].push({id: g.id, selected: true});
                });
            }

            $http.post("/{0}/change_all_selections".replace("{0}", this.docId.toString()), {
                target_id: targetID,
                target_type: targetType,
                selection: this.settings.selectedAllShows,
                selection_type: type,
            });

        } else if (type === "default") {
            this.groupDefaults[targetID] = [];

            if (!this.settings.selectedAllDefault) {
                this.velpGroups.forEach((g) => {
                    this.groupDefaults[targetID].push({id: g.id, selected: false});
                });
            } else {

                this.velpGroups.forEach((g) => {
                    this.groupDefaults[targetID].push({id: g.id, selected: true});
                });
            }

            $http.post("/{0}/change_all_selections".replace("{0}", this.docId.toString()), {
                target_id: targetID,
                target_type: targetType,
                selection: this.settings.selectedAllDefault,
                selection_type: type,
            });

        }

        this.updateVelpList();

    }

    /**
     * Sets all velp group show selections to defaults in the current element or in the document.
     * @method resetCurrentShowsToDefaults
     */
    async resetCurrentShowsToDefaults() {

        let targetID;
        if (this.groupAttachment.target_type === 1 && this.selectedElement !== null) {
            targetID = this.selectedElement.id;
        } else {
            targetID = "0";
        }

        this.groupSelections[targetID] = JSON.parse(JSON.stringify(this.groupDefaults[targetID]));
        await $http.post("/{0}/reset_target_area_selections_to_defaults".replace("{0}", this.docId.toString()), {target_id: targetID});
        this.updateVelpList();
    }

    /**
     * Sets all the show-checkbox values according to the default-checkboxes.
     * @method resetAllShowsToDefaults
     */
    async resetAllShowsToDefaults() {
        this.groupSelections = JSON.parse(JSON.stringify(this.groupDefaults));

        await $http.post("/{0}/reset_all_selections_to_defaults".replace("{0}", this.docId.toString()), null);
        this.updateVelpList();
    }

    /**
     * Changes default and show checkboxes according to selected element or document.
     * @method checkCheckBoxes
     * @param type - Paragraph ID or "0" for the document
     * @returns {boolean} Whether all velp groups are used in the selected element or document
     */
    checkCheckBoxes(type) {
        let targetID = null;

        if (this.groupAttachment.target_type === 1) {
            targetID = this.selectedElement.id;
        } else {
            targetID = "0";
        }

        if (type === "show" && typeof this.groupSelections[targetID] !== UNDEFINED) {
            return this.groupSelections[targetID].length === this.velpGroups.length;
        } else if (type === "default" && typeof this.groupDefaults[targetID] !== UNDEFINED) {
            return this.groupDefaults[targetID].length === this.velpGroups.length;
        }
    }

    /**
     * Gets all the velp groups of the specific velp.
     * @method getVelpsVelpGroups
     * @param velp - Velp whose velp groups are retrieved
     * @returns {Array} - Array of the velp's velp groups
     */
    getVelpsVelpGroups(velp: IVelp) {
        const groups = [];

        for (let i = 0; i < velp.velp_groups.length; i++) {
            for (let j = 0; j < this.velpGroups.length; j++) {
                groups.push(this.velpGroups[j]);
                groups[i].selected = velp.velp_groups.indexOf(this.velpGroups[j].id) >= 0;
            }
        }
        return groups;
    }

    /**
     * Checks if the velp has any velp groups selected.
     * @method isSomeVelpGroupSelected
     * @param velp - Velp whose velp groups are checked
     * @returns {boolean} Whether velp has any groups selected or not
     */
    isSomeVelpGroupSelected(velp: IVelp) {
        if (typeof velp.velp_groups === UNDEFINED) {
            return false;
        }
        return velp.velp_groups.length > 0;
    }

    /**
     * Checks if the velp can be added or modified. The velp has to have a name and
     * it has to be included in at least one velp group.
     * @method isVelpValid
     * @param velp - Velp to check
     * @returns {boolean} Whether the added or modified velp is valid or not.
     */
    isVelpValid(velp: IVelp) {
        if (typeof velp.content === UNDEFINED) {
            return false;
        }
        return this.isSomeVelpGroupSelected(velp) && velp.content.length > 0;
    }

    /**
     * Checks whether the velp contains the velp group.
     * @method isGroupInVelp
     * @param velp - Velp to check
     * @param group - Velp group to check
     * @returns {boolean} Whether the velp contains the velp group or not
     */
    isGroupInVelp(velp: IVelp, group: IVelpGroup) {
        if (typeof velp.velp_groups === UNDEFINED || typeof group.id === UNDEFINED) {
            return false;
        }
        return velp.velp_groups.indexOf(group.id) >= 0;
    }

    /**
     * Updates velp groups of the specified velp.
     * @method updateVelpGroups
     * @param velp - Velp to update
     * @param group - Group to be added or removed from the velp
     */
    updateVelpGroups(velp: IVelp, group: IVelpGroup) {
        const index = velp.velp_groups.indexOf(group.id);
        if (index < 0) {
            velp.velp_groups.push(group.id);
        } else if (index >= 0) {
            velp.velp_groups.splice(index, 1);
        }
    }

    /**
     * Releases select tab.
     * @method releaseClicked
     */

    releaseClicked() {
        const div = $("#selectVelpsDiv");
        this.previewReleased = !(this.previewReleased);
        const top = div.offset().top;
        const left = div.offset().left - 270;
        const element = div.detach();
        if (div.css("position") === "fixed") {
            $("#selectVelpsStack").append(element);
            // If preview has been clicked back in, save the preview position before making it static again
            div.css("position", "static");
            div.find(".draghandle").css("visibility", "hidden");
            div.find(".closedraggable").css("visibility", "hidden");
            div.css("display", "default");
            div.css("padding", 0);

            document.getElementById("releaseSelectVelpsButton").innerHTML = "&#8592;";

        } else {
            // If preview has just been released or it was released last time editor was open
            $("#velpMenu").append(element);
            div.css("position", "fixed");
            div.find(".draghandle").css("visibility", "visible");
            div.find(".closedraggable").css("visibility", "visible");

            div.css("display", "table");
            div.css("width", "19em");
            div.css("padding", 5);
            div.css("z-index", 9999);
            document.getElementById("releaseSelectVelpsButton").innerHTML = "&#8594;";

            div.offset({left, top});

        }

    }
}

/**
 * Filter for ordering velps
 */
timApp.filter("filterByLabels", () => {
    "use strict";
    return (velps, labels, advancedOn) => {

        const selectedVelps = {};
        const selectedLabels = [];

        if (!advancedOn) {
            return velps;
        }

        if (labels !== undefined) {
            for (let i = 0; i < labels.length; i++) {
                if (labels[i].selected) {
                    selectedLabels.push(labels[i].id);
                }
            }
        }

        if (velps !== undefined) {
            for (let j = 0; j < velps.length; j++) {

                for (let k = 0; k < selectedLabels.length; k++) {
                    if (typeof velps[j].labels !== UNDEFINED && velps[j].labels.indexOf(selectedLabels[k]) !== -1) {
                        if (!(j in selectedVelps)) {
                            selectedVelps[j] = [velps[j], 1];
                        } else {
                            selectedVelps[j][1] += 1;
                        }
                    }
                }
            }
        }

        // return all velps if no labels selected
        if (selectedLabels.length === 0) {
            return velps;
        }

        const selectedArray = [];
        const returnVelps = [];

        for (const sv in selectedVelps) {
            if (selectedVelps.hasOwnProperty(sv)) {
                selectedArray.push(selectedVelps[sv]);
            }
        }

        selectedArray.sort((a, b) => b[1] - a[1]);

        for (let l = 0; l < selectedArray.length; l++) {
            returnVelps.push(selectedArray[l][0]);
        }

        return returnVelps;
    };

});

timApp.filter("filterByVelpGroups", () => {
    "use strict";
    return (velps, groups) => {

        const selected = [];
        const checkedGroups = [];

        if (typeof groups === UNDEFINED || typeof velps === UNDEFINED) {
            return velps;
        }

        for (let j = 0; j < groups.length; j++) {
            if (groups[j].show) {
                checkedGroups.push(groups[j].id);
            }
        }

        for (let i = 0; i < velps.length; i++) {
            // always include velp that is being edited
            if (velps[i].edit) {
                selected.push(velps[i]);
                continue;
            }

            for (let k = 0; k < checkedGroups.length; k++) {
                if (velps[i].velp_groups.indexOf(checkedGroups[k]) >= 0 && selected.indexOf(velps[i]) < 0) {
                    selected.push(velps[i]);
                }
            }
        }

        return selected;
    };
});

timApp.filter("orderByWhenNotEditing", () => {
    "use strict";

    return (velps, order, filteredVelps) => {
        for (let i = 0; i < velps.length; i++) {
            if (velps[i].edit) {
                return filteredVelps;
            }
        }

        let list;
        let reverse = false;

        if (order[0] === "-") {
            reverse = true;
            order = order.substring(1);
        }

        if (order === "labels") {
            list = velps;
        } else if (order === "content") {
            list = velps.sort((v1, v2) => v1[order].localeCompare(v2[order]));
        } else {
            list = velps.sort((v1, v2) => {
                if (v1[order] == null && v2[order] != null) {
                    return -1;
                } else if (v1[order] != null && v2[order] == null) {
                    return 1;
                } else if (v1[order] == null && v2[order] == null) {
                    return 0;
                }

                if (v1[order] < v2[order]) {
                    return -1;
                } else if (v1[order] > v2[order]) {
                    return 1;
                }
                return 0;
            });
        }

        if (reverse) {
            list = list.reverse();
        }

        return list;
    };
});

/**
 * Angular directive for velp selection
 */
timApp.component("velpSelection", {
    bindings: {
        docId: "<",
    },
    controller: VelpSelectionController,
    templateUrl: "/static/templates/velpSelection.html",
});
