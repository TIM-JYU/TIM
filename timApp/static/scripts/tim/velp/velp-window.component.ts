import {IScope} from "angular";
import type {IFormController} from "angular";
import {Component} from "@angular/core";
import {ParCompiler} from "tim/editor/parCompiler";
import type {ViewCtrl} from "tim/document/viewctrl";
import {$http, $timeout} from "tim/util/ngimport";
import type {Binding, Require} from "tim/util/utils";
import {clone, to} from "tim/util/utils";
import type {VelpMenuComponent} from "tim/velp/velp-menu-dialog.component";
import type {
    ILabel,
    ILabelUI,
    INewLabel,
    IVelp,
    IVelpGroup,
    IVelpGroupUI,
    IVelpUI,
} from "tim/velp/velptypes";

/**
 * Created by Seppo Tarvainen on 25.11.2016.
 *
 * @author Seppo Tarvainen
 * @licence MIT
 */

export const colorPalette = [
    "blueviolet",
    "darkcyan",
    "orange",
    "darkgray",
    "cornflowerblue",
    "coral",
    "goldenrod",
    "blue",
];

// TODO: add keyboard shortcuts to velps
// TODO: add min and max values for points
// TODO: user should be able to delete velp without any annotations

interface IVelpOptionSetting {
    type: string;
    title: string;
    values: number[];
    names: string[];
}

/**
 * Controller for velp Window
 */
@Component({
    selector: "tim-velp-window",
    template: `
        <div class="velp" [ngStyle]="{top: 0.5+index*2 + 'em'}" (click)="notAnnotationRights(velp.points) || useVelp()">
            <div [ngClass]="['velp-data', 'emphasise', 'default',
                    {neutral: velp.points == 0,
                    positive: velp.points > 0,
                    negative: velp.points < 0,
                    new: new,
                    inactive: notAnnotationRights(velp.points),
                    edit: velp.edit}]"
                 [ngStyle]="{ backgroundColor: getCustomColor()}"
            >
                
                <!-- Show this when velp is NOT being edited -->
                <div *ngIf="!velp.edit" class="content velpContent">
                    <div>
                        <span class="header math">{{ velp.content }}</span>

                        <span (click)="toggleVelpToEdit(); $event.stopPropagation();"
                              *ngIf="hasEditAccess"
                              class="pull-right glyphicon glyphicon-pencil clickable-icon">
                        </span>
                        
                        <span *ngIf="notAnnotationRights(velp.points)"
                              class="annmark glyphicon glyphicon-exclamation-sign clickable-icon pull-right"
                              ng-attr-title="{{ settings.teacherRightsError }}">
                        </span>
                        
                        <span class="margin-5-right header pull-right">{{ velp.points }}</span>
                        
                        <p class="velpInfoText truncate math">{{ velp.default_comment }}</p>
                        
                        <div class="tags">
                            <span *ngIf="velpSelection.advancedOn">
                                <span class="pull-right"
                                      [ngClass]="['glyphicon', 'glyphicon-tag']"
                                      *ngFor="let label of velp.labels" title="label.content"
                                      [ngStyle]="{ color: getColor(label) }">
                                </span>
                            </span>
                        </div>
                    </div>
                    <div class="bottom-part"></div>
                </div>
                
                <!-- Show this when velp IS being edited -->
                <div *ngIf="velp.edit" class="content">
                    <tim-close-button (click)="toggleVelpToEdit(); $event.stopPropagation();"
                                      class="clickable-icon"></tim-close-button>

                    <form #saveVelpForm (ngSubmit)="saveVelp(saveVelpForm)">
                        
                        <!-- Basic velp info -->
                        <div class="add-velp-info">
                            <p class="header"><input name="velpName" type="text" [(ngModel)]="velp.content"
                                                     required placeholder="Velp content"
                                                     title="Annotation visible text">
                            </p>
                            <p *ngIf="(saveVelpForm.velpName.$invalid && !saveVelpForm.velpName.$pristine)"
                               class="error velpInfoText">
                                {{ settings.velpContentError }}
                            </p>
                            
                            <p *ngIf="allowChangePoints()" class="header">
                                <input id="addVelpPoints" type="number"
                                     style="width: 50%;"
                                     title="Default points from this velp"
                                     [(ngModel)]="velp.points"
                                     step="0.01"
                                     placeholder="Points" value="0">
                                <input type="color" [(ngModel)]="velp.color" title="change Velp default color"
                                       class="velp-color-selector">
                                <input *ngIf="isVelpCustomColor()" class="btn-restore-velp-color" type="button"
                                       (click)="clearVelpColor()" title="Reset color to original value" value="R">
                            </p>
                            <p ng-hide="allowChangePoints()" class="velpInfoText">{{ settings.teacherRightsError }}</p>

                            <p><textarea placeholder="Add default comment" [(ngModel)]="velp.default_comment"
                                         title="Default comment for annotation"></textarea></p>
                        </div>
                        
                        <!-- Velp visibility options -->
                        <div class="add-velp-visibility-options">
                            <p class="velpEdit-remove-margin" style="margin-top: 4px;">
                                <label for="edit-velp-visibility-options">
                                    <span title="Visible to" class="glyphicon glyphicon-eye-open visible-icon"></span>
                                </label>
                                <select id="edit-velp-visibility-options"
                                        [(ngModel)]="velp.visible_to" title="Visible to"
                                        ng-options="v as visibleOptions.names[v-1] for v in visibleOptions.values">
                                </select>
                                <span [ngClass]="['glyphicon', 'glyphicon-question-sign', 'clickable-icon']"
                                      uib-popover="Who can see the velp as a default?
                                         'Just me' means that the annotation is visible only to yourself.
                                         'Document owner' refers to the person or group who has been named as the document owner.
                                         'Teachers' refers to the users that have teacher access to this document.
                                         'Everyone' means that the annotation is visible to everyone who can view the assessed content."
                                      popover-placement="top">
                                </span>
                            </p>
                            <p class="velpEdit-remove-margin">
                                <label for="edit-velp-style-options">
                                    <span>Style</span>
                                </label>
                                <select id="edit-velp-style-options"
                                        [(ngModel)]="velp.style" title="Style"
                                        ng-options="v as styleOptions.names[v-1] for v in styleOptions.values">
                                </select>
                            </p>
                        </div>
                        
                        <!-- Advanced velp options -->
                        <div class="collapsible-menu">
                            <details>
                                <summary>Advanced options</summary>
                                <div class="collapsible-menu-open-content">
                                    <fieldset>
                                        <legend>Labels
                                            <span class="pull-right"
                                                  [ngClass]="['glyphicon', 'glyphicon-tag']"
                                                  *ngFor="let l of velp.labels"
                                                  [ngStyle]="{ color: getColor(l) }">
                                            </span>
                                        </legend>
                                        <div class="editVelp-scrollarea">
                                            <span *ngFor="let l of labels">
                                                
                                               <!-- Show this when label IS NOT being edited -->
                                                <span *ngIf="!l.edit">
                                                    <label>
                                                        <input type="checkbox"
                                                               (click)="updateVelpLabels(l)"
                                                               [checked]="isLabelInVelp(l)">
                                                        {{ l.content }}
                                                    </label>
                                                   <span class="pull-right" (click)="toggleLabelToEdit(l)"
                                                         [ngClass]="['glyphicon', 'glyphicon-pencil', 'clickable-icon']">
                                                    </span>
                                                    <br>
                                                </span>
                                                
                                                <!-- Show this when label IS being edited -->
                                                <span *ngIf="l.edit">
                                                    <input type="text"
                                                           placeholder="Add label"
                                                           (ngModelChange)="setLabelValid(labelToEdit)"
                                                           [(ngModel)]="labelToEdit">
                                                    <span class="label-edit-glyphicons"
                                                          (click)="editLabel()"
                                                          [ngClass]="['glyphicon', 'glyphicon-ok', 'clickable-icon']"
                                                          [disabled]="!isVelpValid()"
                                                          [ngStyle]="{color: 'green'}" title="Save">
                                                    </span>
                                                    <span class="label-edit-glyphicons"
                                                          (click)="toggleLabelToEdit(l)"
                                                          [ngClass]="['glyphicon', 'glyphicon-ban-circle', 'clickable-icon']"
                                                          [ngStyle]="{color: 'red'}" title="Cancel">
                                                    </span>
                                                   <span *ngIf="!labelToEdit.valid">
                                                        <span class="error velpInfoText">{{ settings.labelContentError }}</span>
                                                        <br>
                                                    </span>
                                                </span>
                                            </span>
                                        </div>
                                        <!-- Add new label -->
                                        <div class="velpEdit-addLabel-controls">
                                            <input class="addLabelField" placeholder="Add label"
                                                   (ngModelChange)="setLabelValid(newLabel)"
                                                   [(ngModel)]="newLabel">
                                            <input class="addLabelBtn" type="button" value="Add"
                                                   (click)="addLabel()">
                                            <span *ngIf="!newLabel.valid"
                                                  class="error velpInfoText">{{ settings.labelContentError }}</span>
                                        </div>
                                    </fieldset>
                                    <!-- Velp groups -->
                                    <fieldset>
                                        <legend>Velp groups</legend>
                                        <div class="editVelp-scrollarea">
                                            <span *ngFor="let g of velpGroups">
                                                <label title="{{ !g.show ? 'This group is not shown in the selected area. Change setting in manage tab.' : ''}}"
                                                       [ngClass]="{disabled: !g.edit_access, gray: !g.show}">
                                                    <input type="checkbox"
                                                           (click)="updateVelpGroups(g)"
                                                           [checked]="isGroupInVelp(g)"
                                                           [disabled]="!g.edit_access">
                                                    {{ g.name }}
                                                </label>
                                                <br>
                                            </span>
                                        </div>
                                    </fieldset>
                                    <p class="error velpInfoText" *ngIf="!isSomeVelpGroupSelected()">
                                        {{ settings.velpGroupError }}
                                    </p>
                                    <p class="warning velpInfoText" *ngIf="!isSomeVelpGroupShown()">
                                        {{ settings.velpGroupWarning }}
                                    </p>
                                </div>
                            </details>
                        </div>

                        <!-- submit executes 'saveVelp'-function -->
                        <p>
                            <input type="submit" class="timButton" [value]="saveButtonText()"
                                   [disabled]="!isVelpValid()"/>
                            <input type="button" class="timButton" value="Cancel"
                                   (click)="toggleVelpToEdit(); $event.stopPropagation();">
                        </p>
                    </form>
                </div>
            </div>

        </div>
    `,
})
export class VelpWindowComponent {
    private onVelpSelect!: Binding<(params: {$VELP: IVelp}) => void, "&">;
    velpLocal!: IVelp;
    velp!: Binding<IVelpUI, "<">;
    newLabel: INewLabel;
    labelToEdit: INewLabel;
    visibleOptions: IVelpOptionSetting;
    styleOptions: IVelpOptionSetting;
    settings: {
        teacherRightsError: string;
        labelContentError: string;
        velpGroupError: string;
        velpGroupWarning: string;
        velpContentError: string;
    };
    private submitted: boolean;
    hasEditAccess: boolean;
    new!: Binding<boolean, "<">;
    velpGroups!: Binding<IVelpGroupUI[], "<">;
    velpSelection!: Require<VelpMenuComponent>;
    labels!: Binding<ILabelUI[], "<">;
    private docId!: Binding<number, "<">;
    private teacherRight!: Binding<boolean, "<">;
    private vctrl!: ViewCtrl;

    ngOnInit() {
        this.velpLocal = clone(this.velp);

        if (this.velp.visible_to == null) {
            this.velp.visible_to = 4; // Everyone by default
        }
        if (this.velp.style == null) {
            this.velp.style = 1;
        }

        // declare edit rights
        if (this.new) {
            this.hasEditAccess = true;
            this.velpSelection.registerNewVelp(this);
        } else {
            this.hasEditAccess = this.velpGroups.some(
                (g) => (g.edit_access && this.isGroupInVelp(g)) || false
            );
        }
        $timeout(() => {
            this.scope.$evalAsync(() => {
                ParCompiler.processAllMath(this.element.find(".velpContent"));
            });
        });
    }

    constructor(private scope: IScope, private element: JQLite) {
        this.newLabel = {content: "", selected: true, valid: true, id: null};
        this.labelToEdit = {
            content: "",
            selected: false,
            edit: false,
            valid: true,
            id: null,
        };
        this.visibleOptions = {
            type: "select",
            title: "Visible to",
            values: [1, 2, 3, 4],
            names: ["Just me", "Document owner", "Teachers", "Everyone"],
        };
        this.styleOptions = {
            type: "select",
            title: "Style",
            values: [1, 2, 3],
            names: ["Default", "Text", "Text (always visible)"],
        };
        this.settings = {
            teacherRightsError:
                "You need to have teacher rights to change points in this document.",
            labelContentError: "Label content too short",
            velpGroupError: "Select at least one velp group.",
            velpGroupWarning:
                "All selected velp groups are hidden in the current area.",
            velpContentError: "Velp content too short",
        };
        this.submitted = false;
        this.hasEditAccess = false;
        scope.$watch(
            () => this.velp.edit,
            (newValue, oldValue) => {
                if (!newValue && oldValue) {
                    scope.$evalAsync(() => {
                        ParCompiler.processAllMath(
                            this.element.find(".velpContent")
                        );
                    });
                }
            }
        );
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
            if (!this.new) {
                this.element.addClass("velp-edit-available");
            }

            if (this.new) {
                this.velpLocal = clone(this.velp);
                // TODO: focus velp content textarea
            }
            this.velpSelection.setVelpToEdit(this.velp, () =>
                this.cancelEdit()
            );
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

        if (this.new) {
            // add new velp
            this.addVelp();
        } else {
            // edit velp
            this.editVelp();
        }
    }

    /**
     * Cancel edit and restore velp back to its original version
     * TODO: new velp reset does not work
     */
    cancelEdit() {
        this.velp = clone(this.velpLocal);
        this.velp.edit = false;
        this.element.removeClass("velp-edit-available");
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
            if (points == null || this.vctrl.peerReviewInProcess()) {
                return false;
            } else {
                return true;
            }
        }
    }

    isVelpValid() {
        if (this.velp.content == null) {
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
     * @param label - Label to check
     * @returns {boolean} Whether the velp contains the label or not.
     */
    isLabelInVelp(label: ILabel): boolean {
        if (label.id == null) {
            return false;
        }
        return this.velp.labels.includes(label.id);
    }

    /**
     * Checks whether the velp contains the velp group.
     * @param group - Velp group to check
     * @returns {boolean} Whether the velp contains the velp group or not
     */
    isGroupInVelp(group: IVelpGroup) {
        if (this.velp.velp_groups == null || group.id == null) {
            return false;
        }
        return this.velp.velp_groups.includes(group.id);
    }

    /**
     * Updates the labels of the velp.
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
     * @returns {boolean} Whether velp has any groups selected or not
     */
    isSomeVelpGroupSelected() {
        if (this.velp.velp_groups == null) {
            return false;
        }
        return this.velp.velp_groups.length > 0;
    }

    isSomeVelpGroupShown() {
        if (
            this.velp.velp_groups == null ||
            this.velp.velp_groups.length === 0
        ) {
            return true;
        }

        for (const vg of this.velp.velp_groups) {
            for (const g of this.velpGroups) {
                if (g.id === vg && g.show) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Adds new label to this velp.
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

        const json = await to(
            $http.post<{id: number}>("/add_velp_label", data)
        );
        if (!json.ok) {
            return;
        }
        const labelToAdd = {
            ...data,
            selected: false,
            id: json.result.data.id,
        };
        this.resetNewLabel();
        this.labels.push(labelToAdd);
        // this.labelAdded = false;
        this.velp.labels.push(labelToAdd.id);
    }

    /**
     * Selects the label for editing.
     * @param label - Label to edit
     */
    toggleLabelToEdit(label: INewLabel) {
        if (this.labelToEdit.edit && label.id === this.labelToEdit.id) {
            this.cancelLabelEdit(label);
            return;
        }

        if (this.labelToEdit.edit) {
            this.labelToEdit.edit = false;
            for (const l of this.labels) {
                l.edit = false;
            }
        }

        label.edit = true;
        this.copyLabelToEditLabel(label);
        this.setLabelValid(this.labelToEdit);
    }

    cancelLabelEdit(label: INewLabel) {
        label.edit = false;
        this.labelToEdit = {
            content: "",
            selected: false,
            edit: false,
            valid: true,
            id: null,
        };
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
     */
    editLabel() {
        if (this.labelToEdit.content.length < 1) {
            return;
        }

        let updatedLabel = null;
        for (const l of this.labels) {
            if (l.id === this.labelToEdit.id) {
                this.labelToEdit.edit = false;
                l.content = this.labelToEdit.content;
                l.edit = false;
                updatedLabel = l;
                break;
            }
        }

        $http.post("/update_velp_label", updatedLabel);
    }

    /**
     * Reset new label information to the initial (empty) state.
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
        const defaultVelpGroup = this.velpSelection.getDefaultVelpGroup();

        if (
            this.isGroupInVelp(defaultVelpGroup) &&
            defaultVelpGroup.id === -1
        ) {
            await this.handleDefaultVelpGroupIssue();
            await this.updateVelpInDatabase();
        } else if (this.velp.velp_groups.length > 0) {
            await this.updateVelpInDatabase();
        }
    }

    async updateVelpInDatabase() {
        await to(
            $http.post(
                "/{0}/update_velp".replace("{0}", this.docId.toString()),
                this.velp
            )
        );
        this.velpLocal = clone(this.velp);
        this.toggleVelpToEdit();
    }

    /**
     * Adds a new velp on form submit event.
     */
    async addVelp() {
        const defaultVelpGroup = this.velpSelection.getDefaultVelpGroup();
        if (
            this.isGroupInVelp(defaultVelpGroup) &&
            defaultVelpGroup.id === -1
        ) {
            await this.handleDefaultVelpGroupIssue();
            await this.addNewVelpToDatabase();
        } else if (this.velp.velp_groups.length > 0) {
            await this.addNewVelpToDatabase();
        }
        this.velpSelection.updateVelpList();
    }

    /**
     * Adds a new velp to the database. Requires values in `this.newVelp` variable.
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
            valid_until: null,
            color: this.velp.color,
            visible_to: this.velp.visible_to,
            velp_groups: clone(this.velp.velp_groups),
            style: this.velp.style,
        };
        const json = await to($http.post<number>("/add_velp", data));
        if (!json.ok) {
            return;
        }
        const velpToAdd: IVelp = {
            id: json.result.data,
            ...data,
        };
        velpToAdd.id = json.result.data;
        if (this.velpSelection.rctrl.velps != null) {
            this.velpSelection.rctrl.velps.push(velpToAdd);
        }

        this.velpLocal.velp_groups = velpToAdd.velp_groups;
        this.velpLocal.labels = velpToAdd.labels;

        this.toggleVelpToEdit();
        this.velpSelection.updateVelpList();

        // this.velp =  clone(this.velpLocal);
        // this.velpLocal = clone(this.velp);
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

        const newDefaultGroup =
            await this.velpSelection.generateDefaultVelpGroup();
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
     * @param index - Index of the color in the colorPalette variable (modulo by length of color palette)
     * @returns {string} String representation of the color
     */
    getColor(index: number) {
        return colorPalette[index % colorPalette.length];
    }

    getCustomColor() {
        return this.velp.color;
    }
}

/**
 * Angular directive for velp selection
 */
