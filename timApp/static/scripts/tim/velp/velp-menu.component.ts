import * as t from "io-ts";
import {Component, Input} from "@angular/core";
import type {OnInit} from "@angular/core";
import type {NgForm} from "@angular/forms";
import type {Require} from "tim/util/utils";
import {clone, TimStorage, toPromise} from "tim/util/utils";
import type {VelpTemplateComponent} from "tim/velp/velp-template.component";
import {colorPalette} from "tim/velp/velp-template.component";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {showConfirm} from "tim/ui/showConfirmDialog";
import type {ViewCtrl} from "tim/document/viewctrl";
import {HttpClient} from "@angular/common/http";
import type {
    IVelpData,
    ILabel,
    ILabelUI,
    INewLabel,
    INewVelp,
    // INewVelpGroup,
    IVelp,
    IVelpGroup,
    IVelpGroupCollection,
    IVelpGroupUI,
    VelpGroupSelectionType,
} from "tim/velp/velptypes";
import {Users} from "tim/user/userService";

/**
 * The directive retrieves all the data from the server including velps, labels, velp groups and annotations.
 * The directive also handles the majority of the functionality that is relevant in handling velps, labels and velp groups.
 *
 * @author Joonas Lattu
 * @author Petteri Palojärvi
 * @author Seppo Tarvainen
 * @licence MIT
 * @copyright 2016 Timber project members
 */

// TODO: show velps with same name side by side. Make changes to the template.
// TODO Fix (refactor) velp group forms
// TODO Fix tooltips (uib-tooltip) for velp group deletion

// const sortLang: string = "fi";

/**
 * Component for managing and using Velp templates."
 */
@Component({
    selector: "tim-velp-menu",
    template: `
        <div tim-draggable-fixed="" class="velpFixed hidden-print" save="%%PAGEID%%velpMenu" click="true"
             caption="Velp menu" id="velpSelection">
            <div class="velpMenu draggable-content">
                <div class="velp-minimal-controls">
                    <div class="btn-create-new-velp">
                        <button class="timButton add-velp" style=""
                                [disabled]="!initialized" (click)="openCreateNewVelpWindow()" value=""><span
                                class="glyphicon glyphicon-plus icon-plus-add-velp" style="font-size: x-small"></span>Add
                            velp
                        </button>
                    </div>
                    <div class="velp-advanced-control">
                        <input type="checkbox" [(ngModel)]="advancedOn"
                               (ngModelChange)="setAdvancedOnlocalStorage(advancedOn)" id="velp-advanced-checkbox">
                        <label for="velp-advanced-checkbox"> Advanced view</label>
                    </div>
                </div>
                <div class="velpArea velpArea-available-velps">
                    <div save="%%PAGEID%%selectVelpsDiv" class="velpList">

                        <!-- Edit window for velps -->
                        <tim-velp-template class="velp" [ngStyle]="{top: '0.2em'}"
                                           *ngIf="newVelp.edit"
                                           velp="newVelp"
                                           index="-1"
                                           velp-groups="velpGroups"
                                           [teacherRight]="hasTeacherRights()"
                                           labels="labels"
                                           new="true"
                                           vctrl="vctrl"
                                           advanced-on="advancedOn"></tim-velp-template>

                        <!-- Actual velps -->
                        <div class="velp-stack draggable-content available-velps autoscroll">
                            <tim-velp-template
                                    *ngFor="let velp of filteredVelps 
                                                        | filterByContent:this.filterVelp:this.advancedOn 
                                                        | orderByWhenNotEditing:this.order:this.advancedOn 
                                                        | filterByVelpGroups:this.velpGroups:this.advancedOn 
                                                        | filterByLabels:this.labels:this.advancedOn; 
                                                        trackBy: trackByVelpIDFn"
                                    advanced-on="advancedOn"
                                    doc-id="docId"
                                    index="$index"
                                    labels="labels"
                                    new="false"
                                    [teacherRight]="hasTeacherRights()"
                                    velp-groups="velpGroups"
                                    velp="velp"></tim-velp-template>
                        </div>
                    </div>
                </div>
                <div class="velp-menu-selected-area">
                    <label for="selection">Selected area:</label>
                    <p id="selection">
                        <span *ngIf="!rctrl.selectedElement">Nothing selected</span>
                        <span *ngIf="rctrl.selectedElement">
                    <span *ngIf="rctrl.isSomeAreaSelected()">
                        '{{ rctrl.getSelectedAreaBeginning() }}' of
                    </span>
                    <span *ngIf="rctrl.isSelectionDrawing()">
                        new drawing in review image of
                    </span>
                </span>
                        <span *ngIf="rctrl.selectedElement">
                    <span *ngIf="!rctrl.getSelectedAnswerTaskName()">paragraph '{{rctrl.getSelectedParagraphBeginning()}}
                        '</span>
                    <span *ngIf="rctrl.getSelectedAnswerTaskName()">answer in task '{{rctrl.getSelectedAnswerTaskName()}}
                        '</span>
                </span>
                    </p>
                </div>
                <div *ngIf="advancedOn" class="velp-menu-advanced-controls">
                    <tabset active="active" class="velpFill">

                        <!-- Velp filtering options -->
                        <tab index="0" heading="Filter velps">
                            <div class="velp-filter-tab">
                                <!-- LABELS -->
                                <div id="labels">
                                    <form class="form-inline adjustForm">
                                        <div class="form-group velp-filters">
                                            <label class="formLabel" for="searchLabels">Search labels:</label>
                                            <input class="formInput" id="searchLabels" [(ngModel)]="filterLabel"
                                                   placeholder="Filter labels">
                                        </div>
                                    </form>
                                    <div>
                                        <div class="labels-scrollarea">
                                            <p *ngFor="let label of filteredLabels | filterLabelByContent:this.filterLabel:this.advancedOn"
                                               class="label tag-false"
                                               [ngStyle]="{backgroundColor: getColor(label.id)}"
                                               (click)="toggleLabel(label)">
                                                {{ label.content }} <span class="glyphicon glyphicon-ok"
                                                                          *ngIf="label.selected"></span>
                                            </p>
                                        </div>
                                    </div>
                                </div>
                                <!-- VELP CONTENT/GROUPS-->
                                <div>
                                    <form class="form-inline adjustForm">
                                        <div class="form-group velp-filters">
                                            <label class="formLabel" for="searchVelps">Search velps:</label>
                                            <input class="formInput" id="searchVelps" [(ngModel)]="filterVelp"
                                                   placeholder="Filter velps">
                                        </div>
                                    </form>
                                </div>
                                <div>
                                    <form class="form-inline adjustForm">
                                        <div class="form-group velp-filters">
                                            <label class="formLabel" for="orderVelps">Order velps: </label>
                                            <select id="orderVelps" class="formInput" [(ngModel)]="order"
                                                    (ngModelChange)="changeOrdering(order)">
                                                <option value="content">Alphabetical</option>
                                                <option value="-used">Most used</option>
                                                <option value="label">Labels</option>
                                                <option value="-points">Highest point</option>
                                                <option value="points">Lowest point</option>
                                            </select>
                                        </div>
                                    </form>
                                </div>
                                <div>
                                    <form class="form-inline adjustForm">
                                        <div class="form-group velp-filters">
                                            <label class="formLabel" for="displayVelps">Displayed velps: </label>
                                            <select class="formInput" id="displayVelps"
                                                    [(ngModel)]="displayedVelpGroupsScope"
                                                    (ngModelChange)="changeDisplayedVelpGroupsScope(displayedVelpGroupsScope)">
                                                <option [value]="0">Whole document</option>
                                                <option [value]="1">Selected paragraph</option>
                                            </select>
                                        </div>
                                    </form>
                                </div>
                            </div>
                        </tab>

                        <!-- Velp groups -->
                        <tab index="1" heading="Velp groups">
                            <div class="velp-groups-tab">
                                <div class="velpSummary autoscroll">
                                    <form style="border-bottom: 1px solid gainsboro">
                                        <label for="velpGroupArea">Velp group area:</label>
                                        <fieldset>
                                            <select class="formInput" id="velpGroupArea"
                                                    [(ngModel)]="groupAttachment.target_type"
                                                    (ngModelChange)="updateVelpList()">
                                                <option [value]="0" name="selArea" id="selDoc">Whole document</option>
                                                <option [value]="1" name="selArea" id="selPar"
                                                        [ngClass]="{disabled: rctrl.selectedElement == null}"
                                                        [disabled]="rctrl.selectedElement == null">
                                                    Selected paragraph
                                                </option>
                                            </select>
                                        </fieldset>
                                    </form>
                                    <div class="velp-groups-table velp-groups-resize">
                                        <table class="fulldiv">
                                            <tr>
                                                <th>
                                                    <input id="velp-group-table-cb-show" type="checkbox"
                                                           [(ngModel)]="settings.selectedAllShows"
                                                           (ngModelChange)="changeAllVelpGroupSelections('show')">
                                                    <label class="comment-info small glyphicon glyphicon-eye-open"
                                                           for="velp-group-table-cb-show" title="Show">
                                                        <!--                                                <span class="glyphicon glyphicon-eye-open"></span>-->
                                                    </label>
                                                </th>
                                                <th>
                                                    <input id="velp-group-table-cb-default"
                                                           type="checkbox"
                                                           [(ngModel)]="settings.selectedAllDefault"
                                                           [disabled]="!hasManageRights()"
                                                           (ngModelChange)="changeAllVelpGroupSelections('default')">
                                                    <label for="velp-group-table-cb-default" title="Default"
                                                           [ngClass]="['comment-info', 'small', {disabled: !hasManageRights()}]">
                                                        <span class="glyphicon glyphicon-check"></span>
                                                    </label>
                                                </th>
                                                <th><span class="small">Velp group</span></th>
                                                <th><span class="small glyphicon glyphicon-trash"
                                                          title="Delete velp group"></span></th>
                                            </tr>
                                            <ng-container *ngFor="let group of velpGroups">
                                                <tr *ngIf="group.id < 0">
                                                    <td>
                                                        <input type="checkbox" [(ngModel)]="group.show"
                                                               (ngModelChange)="changeVelpGroupSelection(group, 'show')">
                                                    </td>
                                                    <td>
                                                        <input type="checkbox" [(ngModel)]="group.default"
                                                               [disabled]="!hasManageRights()"
                                                               (ngModelChange)="changeVelpGroupSelection(group, 'default')">
                                                    </td>
                                                    <td>
                                                        <a href="/manage/{{ group.location }}">{{ group.name }}</a>
                                                    </td>
                                                    <td>
                                                        <span (click)="deleteVelpGroup(group);"
                                                              *ngIf="group.edit_access && !isDefaultLockedGroup(group)"
                                                              class="glyphicon glyphicon-trash clickable-icon"
                                                              title="Delete velp group '{{ group.name }}'"></span>
                                                                <span *ngIf="!group.edit_access && !isDefaultLockedGroup(group)"
                                                                      class="glyphicon glyphicon-trash lightgray"
                                                                      uib-tooltip-html="toolTipMessages.deleteVelpGroupInsufficientRights"
                                                                      tooltip-placement="auto left"></span>
                                                                <span *ngIf="isDefaultLockedGroup(group)"
                                                                      class="glyphicon glyphicon-lock"
                                                                      uib-tooltip-html="toolTipMessages.deleteVelpGroupLockedGroup"
                                                                      tooltip-placement="auto left"></span>
                                                    </td>
                                                </tr>
                                            </ng-container>
                                        </table>
                                    </div>
                                </div>
                                <div class="collapsible-menu">
                                    <details>
                                        <summary>New velp group</summary>
                                        <div class="collapsible-menu-open-content">
                                            <form #addVelpGroupForm="ngForm" (ngSubmit)="addVelpGroup(addVelpGroupForm)" name="addVelpGroupForm">
                                                <div class="velp-groups-new-group-control">
                                                    <label for="new-velp-group-name">Group name: </label>
                                                    <input #newVelpGroup.name id="new-velp-group-name" type="text"
                                                           [(ngModel)]="newVelpGroup.name" placeholder="Velp group name"
                                                           required>
                                                </div>
                                                <div class="velp-groups-new-group-control">
                                                    <label for="velpGroupSaveToSelect">Save to: </label>
                                                    <fieldset>
                                                        <select name="velpGroupSaveToSelect" id="velpGroupSaveToSelect"
                                                                [(ngModel)]="newVelpGroup.target_type"
                                                                class="formInput"
                                                                #newVelpGroup.target_type>
                                                            <option [value]="0" selected>Personal collection</option>
                                                            <option [value]="1"
                                                                    [disabled]="!hasEditRights()">Document
                                                            </option>
                                                            <option [value]="2"
                                                                    [disabled]="!hasEditRights()">Folder
                                                            </option>
                                                        </select>
                                                    </fieldset>
                                                </div>
                                                <div class="velp-groups-new-group-control">
                                                    <input type="submit" class="timButton" value="Create velp group">
                                                    <p *ngIf="(addVelpGroupForm.newVelpGroup.name.invalid && !addVelpGroupForm.newVelpGroup.name.pristine) || (submitted.velpGroup && addVelpGroupForm.newVelpGroup.name.invalid)"
                                                       class="error">
                                                        Velp group name is required!
                                                    </p>
                                                </div>
                                            </form>
                                        </div>
                                    </details>
                                </div>
                            </div>
                        </tab>

                        <!-- SUMMARY -->
                        <tab index="2" heading="Summary">
                            <div class="velp-summary-tab">
                                <tim-velp-summary
                                        [annotations]="rctrl.getAllAnnotations()"
                                        (annotationSelected)="rctrl.toggleAnnotation($event, true)"
                                        [selectedUser]="vctrl.selectedUser"
                                >
                                </tim-velp-summary>
                            </div>
                        </tab>
                    </tabset>
                </div>


            </div>
        </div>
    `,
    styleUrls: ["velp-menu.component.scss"],
})
export class VelpMenuComponent implements OnInit {
    initialized = false;
    labels: ILabelUI[];
    velpGroups: IVelpGroupUI[];
    newVelp: INewVelp;
    private velpToEdit: INewVelp;
    private newLabel: INewLabel;
    private labelToEdit: INewLabel;
    newVelpGroup: IVelpGroupUI;
    settings: {selectedAllShows: boolean; selectedAllDefault: boolean};
    submitted: {velp: boolean; velpGroup: boolean};
    groupAttachment: {target_type: number; id: number | null};
    private groupSelections: IVelpGroupCollection;
    private groupDefaults: IVelpGroupCollection;
    order!: string; // ngOnInit
    private selectedLabels: number[] = [];
    advancedOn: boolean = false;
    displayedVelpGroupsScope: number = 0;
    private defaultVelpGroup: IVelpGroupUI;
    private labelAdded: boolean = false;
    public vctrl!: Require<ViewCtrl>;
    private defaultPersonalVelpGroup: IVelpGroup;

    newVelpCtrl?: VelpTemplateComponent;

    private storage!: {
        velpOrdering: TimStorage<string>;
        velpGroupsDisplayed: TimStorage<number>;
        velpLabels: TimStorage<number[]>;
        advancedOn: TimStorage<boolean>;
    };

    public toolTipMessages: {
        deleteVelpGroupInsufficientRights: string;
        deleteVelpGroupLockedGroup: string;
    };

    filteredVelps: IVelp[];
    filteredLabels: ILabel[];

    @Input() filterVelp?: string;
    @Input() filterLabel?: string;

    constructor(private http: HttpClient) {
        this.labels = [];
        this.velpGroups = [];

        this.newVelp = {
            content: "",
            default_comment: "",
            points: null,
            labels: [],
            edit: false,
            id: -2,
            velp_groups: [],
            visible_to: 4,
            // TODO use User language setting from TIM settings
            language_id: "FI",
            color: null,
            valid_until: null,
            style: undefined,
        };
        this.velpToEdit = {
            content: "",
            default_comment: "",
            points: null,
            labels: [],
            edit: false,
            id: -1,
            velp_groups: [],
            visible_to: 4,
            // TODO use User language setting from TIM settings
            language_id: "FI",
            color: null,
            valid_until: null,
            style: undefined,
        };
        this.newLabel = {
            content: "",
            selected: false,
            edit: false,
            valid: true,
            id: null,
        };
        this.labelToEdit = {content: "", selected: false, edit: false, id: -3};
        this.newVelpGroup = {
            name: "",
            target_type: 0,
            selected: false,
            location: "",
            id: -9, // instead of null
            show: false,
            default: false,
        };

        this.settings = {selectedAllShows: false, selectedAllDefault: false};
        this.submitted = {velp: false, velpGroup: false};

        this.groupAttachment = {target_type: 0, id: null};

        this.groupSelections = {};
        this.groupDefaults = {};
        // TODO initialize document default velp group path properly
        this.defaultVelpGroup = {
            id: -1,
            name: "No access to default group",
            location: "",
            edit_access: false,
            show: true,
            default: true,
            selected: false,
            target_type: null,
        }; // TODO Use route to add this information
        this.defaultPersonalVelpGroup = {
            id: -2,
            name: "Personal-default",
            location: this.getPersonalVelpGroupPath(),
            target_type: null,
            default: true,
        };
        this.toolTipMessages = {
            deleteVelpGroupInsufficientRights: $localize`Insufficient permissions to delete group`,
            deleteVelpGroupLockedGroup: $localize`Permanent default group cannot be deleted`,
        };

        this.filteredVelps = [];
        this.filteredLabels = [];
    }

    public get rctrl() {
        return this.vctrl.reviewCtrl;
    }

    async ngOnInit() {
        this.rctrl.initVelpSelection(this);
        this.rctrl.velps = [];
        // Dictionaries for easier searching: Velp ids? Label ids? Annotation ids?
        const docId = this.docId;
        this.storage = {
            velpOrdering: new TimStorage("velpOrdering_" + docId, t.string),
            velpGroupsDisplayed: new TimStorage(
                "velpGroupsDisplayed_" + docId,
                t.number
            ),
            velpLabels: new TimStorage(
                "velpLabels_" + docId,
                t.array(t.number)
            ),
            advancedOn: new TimStorage("advancedOn" + docId, t.boolean), // TODO: should this be document specific?
        };

        // Values to store in localstorage:
        this.order = this.storage.velpOrdering.get() ?? "content";
        this.displayedVelpGroupsScope =
            this.storage.velpGroupsDisplayed.get() ?? 0;
        this.selectedLabels = this.storage.velpLabels.get() ?? [];
        this.advancedOn = this.storage.advancedOn.get() ?? false;

        const r = await toPromise(
            this.http.get<IVelpData>(`/${docId}/get_velp_initialization_data`)
        );
        if (!r.ok) {
            return; // TODO proper error handling
        }

        this.velpGroups = r.result.velp_groups;
        this.defaultVelpGroup = r.result.default_velp_group;
        this.defaultPersonalVelpGroup = r.result.personal_velp_group;
        this.rctrl.velps = r.result.velps;
        this.labels = r.result.velp_labels;
        this.groupSelections = r.result.personal_vg_selections;
        this.groupDefaults = r.result.default_vg_selections;

        // If doc_default exists already for some reason but isn't a velp group yet, remove it from fetched velp groups
        for (const g of this.velpGroups) {
            if (
                g.name === this.defaultVelpGroup.name &&
                this.defaultVelpGroup.id < 0
            ) {
                const extraDefaultIndex = this.velpGroups.indexOf(g);
                this.velpGroups.push(this.defaultVelpGroup);
                this.velpGroups.splice(extraDefaultIndex, 1);
                break;
            }
        }

        if (this.defaultVelpGroup.edit_access) {
            this.velpGroups.forEach((g) => {
                if (g.id === this.defaultVelpGroup.id) {
                    g.selected = true;
                }
            });
            this.defaultVelpGroup.selected = true;
            this.newVelp.velp_groups.push(this.defaultVelpGroup.id);
        }

        // Add new default personal velp group if it didn't exist before
        if (r.result.personal_velp_group.created_new_group) {
            this.velpGroups.push(this.defaultPersonalVelpGroup);
        }

        if (!this.defaultVelpGroup.edit_access) {
            this.newVelp.velp_groups.push(this.defaultPersonalVelpGroup.id);
        }

        if (
            this.defaultPersonalVelpGroup.id < 0 &&
            !this.velpGroupsContain(this.defaultVelpGroup)
        ) {
            this.velpGroups.push(this.defaultVelpGroup);
        }

        if (
            this.defaultVelpGroup.id < 0 &&
            !this.velpGroupsContain(this.defaultVelpGroup)
        ) {
            this.velpGroups.push(this.defaultVelpGroup);
        }

        // Get velp and annotation data
        this.rctrl.velps.forEach((v) => {
            v.edit = false;
            if (v.labels == null) {
                v.labels = [];
            }
        });

        // Set label data
        this.labels.forEach((l) => {
            l.edit = false;
            l.selected = false;
            for (const s of this.selectedLabels) {
                if (l.id === s) {
                    l.selected = true;
                }
            }
        });

        if (!this.groupSelections.hasOwnProperty("0")) {
            this.groupSelections["0"] = [];
        }

        const docSelections = this.groupSelections["0"];

        this.velpGroups.forEach((g) => {
            g.show = false;
            for (const s of docSelections) {
                if (s.id === g.id && s.selected) {
                    g.show = true;
                    break;
                }
            }
        });

        const docDefaults = this.groupDefaults["0"];

        this.velpGroups.forEach((g) => {
            for (const d of docDefaults) {
                if (d.id === g.id && d.selected) {
                    g.default = true;
                    break;
                }
            }
        });

        this.filteredVelps = this.rctrl.velps;
        this.filteredLabels = this.labels;
        this.updateVelpList();
        this.initialized = true;
    }

    // Methods

    changeOrdering(order: string) {
        this.storage.velpOrdering.set(order);
    }

    changeDisplayedVelpGroupsScope(scope: number) {
        this.storage.velpGroupsDisplayed.set(scope);
        this.groupAttachment.target_type = scope;
        this.updateVelpList();
    }

    changeSelectedLabels() {
        this.storage.velpLabels.set(this.selectedLabels);
    }

    /**
     * Return true if user has teacher rights.
     * @returns {boolean}
     */
    allowChangePoints() {
        return this.vctrl.item.rights.teacher;
    }

    /**
     * Get color for the object from colorPalette variable.
     * @param index - Index of the color in the colorPalette variable (modulo by length of color palette)
     * @returns {string} String representation of the color
     */
    getColor(index: number) {
        return colorPalette[index % colorPalette.length];
    }

    /**
     * Toggles the label's selected attribute.
     * @param label - Label to toggle
     */
    toggleLabel(label: ILabelUI) {
        label.selected = !label.selected;
        const labelIndex = this.selectedLabels.indexOf(label.id);
        if (labelIndex < 0) {
            this.selectedLabels.push(label.id);
        } else {
            this.selectedLabels.splice(labelIndex, 1);
        }
        this.changeSelectedLabels(); // Update localstorage
    }

    setAdvancedOnlocalStorage(value: boolean) {
        this.storage.advancedOn.set(value);
    }

    /**
     * Adds new label tp the specified velp.
     * @param velp - Velp where the label is to be added.
     */
    async addLabel(velp: IVelp) {
        if (this.newLabel.content.length < 1) {
            this.newLabel.valid = false;
            return;
        }

        const data = {
            content: this.newLabel.content,
            language_id: "FI", // TODO: Change to user language
        };
        const response = await toPromise(
            this.http.post<ILabel>("/add_velp_label", data)
        );
        if (!response.ok) {
            return;
        }

        const labelToAdd = {
            ...data,
            id: response.result.id,
            selected: false,
        };
        this.resetNewLabel();
        this.labels.push(labelToAdd);
        this.labelAdded = false;
        velp.labels.push(labelToAdd.id);
    }

    /**
     * Deselects all the labels.
     */
    deselectLabels() {
        for (const l of this.labels) {
            if (l.selected) {
                this.toggleLabel(l);
            }
        }
    }

    registerNewVelp(v: VelpTemplateComponent) {
        this.newVelpCtrl = v;
    }

    /**
     * Selects velp to edit
     */
    openCreateNewVelpWindow() {
        if (!this.newVelpCtrl) {
            showMessageDialog("New velp component is not registered");
            return;
        }
        this.newVelpCtrl.toggleVelpToEdit();
    }

    get docId() {
        return this.rctrl.item.id;
    }

    /**
     * Generates the default velp group.
     */
    async generateDefaultVelpGroup(): Promise<IVelpGroup | null> {
        if (this.defaultVelpGroup.edit_access) {
            const response = await toPromise(
                this.http.post<IVelpGroup>(
                    "/{0}/create_default_velp_group".replace(
                        "{0}",
                        this.docId.toString()
                    ),
                    "{}"
                )
            );
            if (!response.ok) {
                await showMessageDialog(response.result.error.error);
                return null;
            }
            const newDefaultVelpGroup = response.result;
            newDefaultVelpGroup.default = true;

            const index = this.velpGroups.indexOf(this.defaultVelpGroup);
            this.velpGroups.splice(index, 1);

            if (!this.velpGroups.includes(newDefaultVelpGroup)) {
                this.velpGroups.push(newDefaultVelpGroup);
            }

            this.defaultVelpGroup = newDefaultVelpGroup;
            return newDefaultVelpGroup;
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
    setVelpToEdit(velp: IVelp, resetFunction: () => void) {
        this.velpToEdit = velp;
        this.resetEditVelp = resetFunction;
    }

    /**
     * Initially resets this.velpToEdit variable to the original (empty) state.
     * NOTE! this function is replaced in 'setVelpToEdit'. When replaced
     * this method resets the velp that is being edited to its original state.
     */
    resetEditVelp() {
        this.velpToEdit = {
            content: "",
            points: null,
            labels: [],
            edit: false,
            id: -1,
            velp_groups: [],
            default_comment: "",
            visible_to: null,
            language_id: "FI",
            color: null,
            valid_until: null,
            style: undefined,
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
     * TODO: error handling in case of update failure
     */
    editLabel() {
        if (this.labelToEdit.content.length < 1) {
            this.labelToEdit.edit = false;
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

        this.http.post("/update_velp_label", updatedLabel);
    }

    /**
     * Reset new velp information to the initial (empty) state.
     */
    resetNewVelp() {
        this.newVelp = {
            content: "",
            points: null,
            labels: [],
            edit: false,
            id: -2,
            velp_groups: this.newVelp.velp_groups,
            default_comment: "",
            visible_to: null,
            language_id: "FI",
            color: null,
            valid_until: null,
            style: undefined,
        };
    }

    /**
     * Reset new label information to the initial (empty) state.
     */
    resetNewLabel() {
        this.newLabel = {content: "", selected: true, valid: true, id: null};
    }

    /* Velpgroup methods */

    getDefaultVelpGroup() {
        return this.defaultVelpGroup;
    }

    setDefaultVelpGroup(group: IVelpGroup) {
        this.defaultVelpGroup = group;
    }

    /* Whether the group is a non-removable default group for the document or the user */
    isDefaultLockedGroup(group: IVelpGroup) {
        return this.isVelpGroupDefaultFallBack(group.id);
    }

    /**
     * Updates the velp list according to how the velp groups are selected in the area.
     */
    updateVelpList() {
        this.velpGroups.forEach((g) => {
            if (
                this.isAttachedToParagraph() &&
                this.rctrl.selectedElement != null
            ) {
                g.show = this.isVelpGroupShownHere(
                    g.id,
                    this.rctrl.selectedElement.par.id
                );
                g.default = this.isVelpGroupDefaultHere(
                    g.id,
                    this.rctrl.selectedElement.par.id
                );
            } else {
                g.show = this.isVelpGroupShownHere(g.id, "0");
                g.default = this.isVelpGroupDefaultHere(g.id, "0");
            }
        });
    }

    /**
     * Return whether the group is shown based on the various selected and default values.
     * @param groupId - VelpGroup ID
     * @param paragraphId - Paragraph ID or "0" for the document
     * @returns {boolean} Whether the velp group is shown here or not
     */
    isVelpGroupShownHere(groupId: number, paragraphId: string) {
        let returnValue;
        // Are we checking for the whole document? This "if" might be unnecessary.
        if (paragraphId === "0") {
            returnValue = this.lazyIsVelpGroupSelectedInParagraph(
                groupId,
                paragraphId
            );
            if (returnValue != null) {
                return returnValue;
            }
            // Not set for the document, we'll try the defaults instead.
            returnValue = this.lazyIsVelpGroupDefaultInParagraph(
                groupId,
                paragraphId
            );
            if (returnValue != null) {
                return returnValue;
            }
        } else {
            // First check "selected" attributes for paragraph.
            returnValue = this.lazyIsVelpGroupSelectedInParagraph(
                groupId,
                paragraphId
            );
            if (returnValue != null) {
                return returnValue;
            }
            // Found nothing, we try the defaults instead.
            returnValue = this.isVelpGroupDefaultHere(groupId, paragraphId);
            if (returnValue != null) {
                return returnValue;
            }
        }
        // Ok, hard coded ones left:
        return this.isVelpGroupDefaultFallBack(groupId);
    }

    /**
     * Returns whether the velp group is default in specified paragraph (or document) or not.
     * @param groupId - Velp group ID
     * @param paragraphId - Paragraph ID or "0" for the document
     * @returns {boolean} Whether the velp group is default here or not.
     */
    isVelpGroupDefaultHere(groupId: number, paragraphId: string) {
        let returnValue;
        // First check defaults here
        returnValue = this.lazyIsVelpGroupDefaultInParagraph(
            groupId,
            paragraphId
        );
        if (returnValue != null) {
            return returnValue;
        }
        // and then try document instead. If we started with a document, this is wasted work.
        returnValue = this.lazyIsVelpGroupDefaultInParagraph(groupId, "0");
        if (returnValue != null) {
            return returnValue;
        }
        return this.isVelpGroupDefaultFallBack(groupId);
    }

    /**
     * Checks whether the given velp group is either personal default or document default group.
     * Personal default group and the document default group have always default, unless the user has
     * specified otherwise.
     * @param groupId - Velp group ID
     * @returns {boolean} Whether the group is personal default or document default group or not.
     */
    isVelpGroupDefaultFallBack(groupId: number) {
        return (
            groupId === this.defaultPersonalVelpGroup.id ||
            groupId === this.defaultVelpGroup.id
        );
    }

    /**
     * Helper function for checking if the velp group is shown in the paragraph or not.
     * Despite the name, can check document selections as well.
     * @param groupId - Velp group ID
     * @param paragraphId - Paragraph ID or "0" for the document
     * @returns true/false/null
     */
    lazyIsVelpGroupSelectedInParagraph(groupId: number, paragraphId: string) {
        return this.checkCollectionForSelected(
            groupId,
            paragraphId,
            this.groupSelections
        );
    }

    /**
     * Helper function for checking if the velp group is default in the paragraph or not.
     * Despite the name, can check document selections as well.
     * @param groupId - Velp group ID
     * @param paragraphId - Paragraph ID or "0" for the document
     * @returns true/false/null
     */
    lazyIsVelpGroupDefaultInParagraph(groupId: number, paragraphId: string) {
        return this.checkCollectionForSelected(
            groupId,
            paragraphId,
            this.groupDefaults
        );
    }

    /**
     * Checks whether the collection is selected or not.
     * @param groupId - Velp group ID
     * @param paragraphId - Paragraph ID or document "0".
     * @param collection - Shows or defaults
     * @returns {boolean|null} Whether the collection is selected or not. Null if paragraph is not found.
     */
    checkCollectionForSelected(
        groupId: number,
        paragraphId: string,
        collection: IVelpGroupCollection
    ) {
        if (collection.hasOwnProperty(paragraphId)) {
            const selectionsHere = collection[paragraphId];
            for (const s of selectionsHere) {
                if (s.id === groupId) {
                    return s.selected;
                }
            }
        }
        return null;
    }

    /**
     * Adds a velp group on form submit event.
     * @param form - Velp group form
     */
    async addVelpGroup(form: NgForm) {
        const valid = form.valid;
        this.submitted.velpGroup = true;
        if (!valid) {
            return;
        }

        // TODO should we also set 'dirty' to false and remove ng-dirty here,
        //  as per https://docs.angularjs.org/api/ng/type/form.FormController#$setPristine
        form.setValue({pristine: true});

        const response = await toPromise(
            this.http.post<IVelpGroup>(
                "/{0}/create_velp_group".replace("{0}", this.docId.toString()),
                this.newVelpGroup
            )
        );
        if (!response.ok) {
            await showMessageDialog(response.result.error.error);
            return;
        }
        const group: IVelpGroupUI = response.result;
        group.selected = false;
        group.show = true;
        this.velpGroups.push(response.result);

        // TODO: show in selected area
    }

    /**
     * Removes the velp group.
     * @param group the velp group to be deleted
     * TODO: Move localized messages outside the function
     */
    async deleteVelpGroup(group: IVelpGroupUI) {
        /* Make a list of velps that belong to this velp group */
        let velpsInGroupList = "";
        this.rctrl.velps?.forEach((v) => {
            if (this.isGroupInVelp(v, group)) {
                velpsInGroupList += "- " + v.content + "</br>";
            }
        });
        let warningGroupHasVelps = "";
        if (velpsInGroupList.length > 0) {
            warningGroupHasVelps = $localize`<b>Warning!</b> The velp group <b>${group.name}</b> still contains velps:</br>
                                             </br>
                                             ${velpsInGroupList}
                                             </br>
                                             If the listed velps do not belong to any other velp groups, they will not be shown in the velp templates list after this operation completes.</br>`;
        }

        const confirmMessage = $localize`Are you sure you want to delete this velp group: <b>${group.name}</b>?</br>
                      </br>
                      ${warningGroupHasVelps}
                      Velps attached to documents will not be removed.`;

        if (
            !(await showConfirm($localize`Delete velp group?`, confirmMessage))
        ) {
        } else {
            const deleteResponse = await toPromise(
                this.http.delete("/velp/group/" + group.id)
            );

            if (deleteResponse.ok) {
                /*
                Database tables to check/modify:
                  - velpgroup (primary key: id -> velp_group_id)
                  - velpgroupdefaults (velp_group_id)
                  - velpgrouplabel (velp_group_id) NOT USED
                  - velpgroupselection (velp_group_id)
                  - velpgroupsindocument (velp_group_id)
                  - velpingroup (velp_group_id) Velp link
                  */

                /* Update velp group list in UI */
                const g_index = this.velpGroups.indexOf(group);
                if (g_index >= 0) {
                    this.velpGroups.splice(g_index, 1);
                }
            } else {
                let errMessage = "";
                if (deleteResponse.result.status == 403) {
                    errMessage = $localize`Insufficient permissions to delete velp group: <b>${group.name}</b>.`;
                }
                await showMessageDialog(errMessage);
            }
        }
    }

    /**
     * Changes velp group (default or show) selection in the current element or in the document.
     * @param group - Velp group
     * @param type - "show" or "default"
     */
    changeVelpGroupSelection(group: IVelpGroup, type: VelpGroupSelectionType) {
        let targetId: string;
        let targetType: number;
        if (
            this.isAttachedToParagraph() &&
            this.rctrl.selectedElement != null
        ) {
            targetId = this.rctrl.selectedElement.par.id;
            targetType = 1;
        } else {
            targetId = "0";
            targetType = 0;
        }

        const data = Object.assign(
            {
                target_id: targetId,
                target_type: targetType,
                selection_type: type,
            },
            group
        );

        if (type === "show") {
            this.http.post(
                "/{0}/change_selection".replace("{0}", this.docId.toString()),
                data
            );

            this.groupSelections[targetId] = [];

            this.velpGroups.forEach((g) => {
                this.groupSelections[targetId].push({
                    id: g.id,
                    selected: g.show,
                });
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
            this.http.post(
                "/{0}/change_selection".replace("{0}", this.docId.toString()),
                data
            );

            this.groupDefaults[targetId] = [];

            this.velpGroups.forEach((g) => {
                this.groupDefaults[targetId].push({
                    id: g.id,
                    selected: g.default,
                });
            });

            /* if (!this.groupDefaults.hasOwnProperty(group.target_id))
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
     * @param type - "show" or "default"
     */
    changeAllVelpGroupSelections(type: VelpGroupSelectionType) {
        let targetID: string;
        let targetType;

        if (
            this.isAttachedToParagraph() &&
            this.rctrl.selectedElement != null
        ) {
            targetID = this.rctrl.selectedElement.par.id;
            targetType = 1;
        } else {
            targetID = "0";
            targetType = 0;
        }

        if (type === "show") {
            this.groupSelections[targetID] = [];
            if (!this.settings.selectedAllShows) {
                this.velpGroups.forEach((g) => {
                    this.groupSelections[targetID].push({
                        id: g.id,
                        selected: false,
                    });
                });
            } else {
                this.velpGroups.forEach((g) => {
                    this.groupSelections[targetID].push({
                        id: g.id,
                        selected: true,
                    });
                });
            }

            this.http.post(
                "/{0}/change_all_selections".replace(
                    "{0}",
                    this.docId.toString()
                ),
                {
                    target_id: targetID,
                    target_type: targetType,
                    selection: this.settings.selectedAllShows,
                    selection_type: type,
                }
            );
        } else if (type === "default") {
            this.groupDefaults[targetID] = [];

            if (!this.settings.selectedAllDefault) {
                this.velpGroups.forEach((g) => {
                    this.groupDefaults[targetID].push({
                        id: g.id,
                        selected: false,
                    });
                });
            } else {
                this.velpGroups.forEach((g) => {
                    this.groupDefaults[targetID].push({
                        id: g.id,
                        selected: true,
                    });
                });
            }

            this.http.post(
                "/{0}/change_all_selections".replace(
                    "{0}",
                    this.docId.toString()
                ),
                {
                    target_id: targetID,
                    target_type: targetType,
                    selection: this.settings.selectedAllDefault,
                    selection_type: type,
                }
            );
        }

        this.updateVelpList();
    }

    private isAttachedToParagraph() {
        return this.groupAttachment.target_type === 1;
    }

    /**
     * Sets all velp group show selections to defaults in the current element or in the document.
     */
    async resetCurrentShowsToDefaults() {
        let targetID;
        if (
            this.isAttachedToParagraph() &&
            this.rctrl.selectedElement != null
        ) {
            targetID = this.rctrl.selectedElement.par.id;
        } else {
            targetID = "0";
        }

        this.groupSelections[targetID] = clone(this.groupDefaults[targetID]);
        await toPromise(
            this.http.post(
                "/{0}/reset_target_area_selections_to_defaults".replace(
                    "{0}",
                    this.docId.toString()
                ),
                {target_id: targetID}
            )
        );
        this.updateVelpList();
    }

    /**
     * Sets all the show-checkbox values according to the default-checkboxes.
     */
    async resetAllShowsToDefaults() {
        this.groupSelections = clone(this.groupDefaults);

        await toPromise(
            this.http.post(
                "/{0}/reset_all_selections_to_defaults".replace(
                    "{0}",
                    this.docId.toString()
                ),
                null
            )
        );
        this.updateVelpList();
    }

    /**
     * Changes default and show checkboxes according to selected element or document.
     * @param type - Paragraph ID or "0" for the document
     * @returns {boolean} Whether all velp groups are used in the selected element or document
     */
    checkCheckBoxes(type: VelpGroupSelectionType) {
        let targetID = null;

        if (
            this.isAttachedToParagraph() &&
            this.rctrl.selectedElement != null
        ) {
            targetID = this.rctrl.selectedElement.par.id;
        } else {
            targetID = "0";
        }

        if (type === "show" && typeof this.groupSelections[targetID] != null) {
            return (
                this.groupSelections[targetID].length === this.velpGroups.length
            );
        } else if (
            type === "default" &&
            typeof this.groupDefaults[targetID] != null
        ) {
            return (
                this.groupDefaults[targetID].length === this.velpGroups.length
            );
        }
    }

    /**
     * Gets all the velp groups of the specific velp.
     * @param velp - Velp whose velp groups are retrieved
     * @returns {Array} - Array of the velp's velp groups
     */
    getVelpsVelpGroups(velp: IVelp) {
        const groups = [];

        for (let i = 0; i < velp.velp_groups.length; i++) {
            for (const v of this.velpGroups) {
                groups.push(v);
                groups[i].selected = velp.velp_groups.includes(v.id);
            }
        }
        return groups;
    }

    /**
     * Checks if the velp has any velp groups selected.
     * @param velp - Velp whose velp groups are checked
     * @returns {boolean} Whether velp has any groups selected or not
     */
    isSomeVelpGroupSelected(velp: IVelp) {
        if (velp.velp_groups == null) {
            return false;
        }
        return velp.velp_groups.length > 0;
    }

    /**
     * Checks if the velp can be added or modified. The velp has to have a name and
     * it has to be included in at least one velp group.
     * @param velp - Velp to check
     * @returns {boolean} Whether the added or modified velp is valid or not.
     */
    isVelpValid(velp: IVelp) {
        if (velp.content == null) {
            return false;
        }
        return this.isSomeVelpGroupSelected(velp) && velp.content.length > 0;
    }

    /**
     * Checks whether the velp contains the velp group.
     * @param velp - Velp to check
     * @param group - Velp group to check
     * @returns {boolean} Whether the velp contains the velp group or not
     */
    isGroupInVelp(velp: IVelp, group: IVelpGroup) {
        if (velp.velp_groups == null || group.id === null) {
            return false;
        }
        return velp.velp_groups.includes(group.id);
    }

    /**
     * Updates velp groups of the specified velp.
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

    velpGroupsContain(g: IVelpGroup) {
        for (const vg of this.velpGroups) {
            if (vg.id === g.id) {
                return true;
            }
        }
        return false;
    }

    /**
     * Tracking function for velp templates
     * @param index
     * @param velp
     */
    trackByVelpIDFn(index: number, velp: IVelp): number {
        return velp.id;
    }

    hasManageRights(): boolean {
        return this.vctrl.item.rights.manage;
    }
    hasTeacherRights(): boolean {
        return this.vctrl.item.rights.teacher;
    }
    hasEditRights(): boolean {
        return this.vctrl.item.rights.editable;
    }

    /**
     * Return path to current user's Personal-default velp group
     * TODO should return empty string or undefined if the Personal-default group
     *      or velp-groups folder do not exist
     * @private
     */
    private getPersonalVelpGroupPath(): string {
        return (
            "users/" + Users.getCurrent().name + "/velp-groups/Personal-default"
        );
    }
}
