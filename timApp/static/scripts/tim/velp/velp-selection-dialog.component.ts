/**
 * The directive retrieves all the data from the server including velps, labels, velp groups and annotations.
 * The directive also handles majority of the functionality that is relevant in handling velps, labels and velp groups.
 *
 * @author Joonas Lattu
 * @author Petteri Palojärvi
 * @author Seppo Tarvainen
 * @licence MIT
 * @copyright 2016 Timber project members
 */
import {Component, Pipe, PipeTransform} from "@angular/core";
import * as t from "io-ts";
import {clone, TimStorage, to} from "tim/util/utils";
import {colorPalette, VelpWindowController} from "tim/velp/velpWindow";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {
    Alignment,
    AngularDialogComponent,
} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {NgForm} from "@angular/forms";
import {ViewCtrl} from "../document/viewctrl";
import {$http} from "../util/ngimport";
import {
    ILabel,
    ILabelUI,
    INewLabel,
    INewVelp,
    INewVelpGroup,
    IVelp,
    IVelpGroup,
    IVelpGroupCollection,
    IVelpGroupUI,
    VelpGroupSelectionType,
} from "./velptypes";

// TODO: show velps with same name side by side. Make changes to the template.

const sortLang: string = "fi";

/**
 * Controller for velp selection
 */
@Component({
    selector: "tim-velp-selection",
    template: `
        <tim-dialog-frame [mini]="true"
                          [closeable]="false"
                          [autoHeight]="false"
                          [initialAutoHeight]="true"
                          [anchor]="'fixed'" 
                          [modalStyle]="{ maxWidth: '22.5em', minWidth: '15em' }">
            <ng-container header>Velp menu</ng-container>
            <ng-container body>
                <div class="velpMenu">
                    <tabset>
                        <tab heading="Select">
                            <p id="selection"
                               [tooltip]="rctrl.selectedArea?.toString()"
                               tooltipPlacement="bottom">
                                <span *ngIf="!rctrl.selectedElement">
                                    Nothing selected
                                </span>
                                <span *ngIf="rctrl.selectedElement">
                                Selected:
                                    <span *ngIf="rctrl.selectedArea">
                                        '{{ rctrl.getSelectedAreaBeginning() }}' of
                                    </span>
                                    <span *ngIf="rctrl.selectionIsDrawing">
                                        new drawing in review image of
                                    </span>
                                </span>
                                <span *ngIf="rctrl.selectedElement">
                                    <span *ngIf="!rctrl.getSelectedAnswerTaskName()">
                                        paragraph '{{rctrl.getSelectedParagraphBeginning()}}'
                                    </span>
                                    <span *ngIf="rctrl.getSelectedAnswerTaskName()">
                                        answer in task '{{rctrl.getSelectedAnswerTaskName()}}'
                                    </span>
                                </span>
                            </p>

                            <label><input type="checkbox" name="velpAdvancedOn" [(ngModel)]="advancedOn"
                                          (ngModelChange)="setAdvancedOnlocalStorage(advancedOn)"> Advanced view</label>
                            <div class="velpArea">

                                <!-- LABELS -->
                                <div id="labels" *ngIf="advancedOn">
                                    <form class="form-inline adjustForm">
                                        <div class="form-group">
                                            <label class="formLabel" for="searchLabels">Search labels:</label>
                                            <input class="formInput" id="searchLabels" name="searchLabels" [(ngModel)]="filterLabel"
                                                   placeholder="Filter labels">
                                        </div>
                                    </form>
                                    <div>
                                        <div class="fulldiv">
                                            <p *ngFor="let label of (labels | filterLabelsByContent: filterLabel)"
                                               class="label tag-false"
                                               [style.background-color]="getColor(label.id)"
                                               (click)="toggleLabel(label)">
                                                {{ label.content }} <span class="glyphicon glyphicon-ok"
                                                                          *ngIf="label.selected"></span>
                                            </p>
                                            <hr/>
                                        </div>
                                    </div>
                                </div>
                                <div>
                                    <form class="form-inline adjustForm">
                                        <div class="form-group">
                                            <label class="formLabel" for="searchVelps">Search velps:</label>
                                            <input class="formInput" id="searchVelps" name="searchVelps" [(ngModel)]="filterVelp"
                                                   placeholder="Filter velps">
                                        </div>
                                    </form>
                                </div>
                                <div *ngIf="advancedOn">
                                    <form class="form-inline adjustForm">
                                        <div class="form-group">
                                            <label class="formLabel" for="orderVelps">Order velps:</label>
                                            <select id="orderVelps" name="orderVelps" [(ngModel)]="order"
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

                                <button class="timButton margin-4"
                                        [disabled]="!initialized"
                                        (click)="openCreateNewVelpWindow()"
                                        value="">Create new velp
                                </button>
                                <tim-dialog-frame [detachable]="true">
                                    <ng-container header>Available velps</ng-container>
                                    <ng-container content>
                                        <div class="velp-stack">
                                            <tim-velp-window class="velp"
                                                             *ngIf="newVelp.edit"
                                                             [velp]="newVelp" 
                                                             [velpGroups]="velpGroups"
                                                             [teacherRight]="vctrl.item.rights.teacher"
                                                             [labels]="labels"
                                                             [new]="true" 
                                                             [advancedOn]="advancedOn"></tim-velp-window>
                                            <tim-velp-window *ngFor="let velp of ((rctrl.velps | filterLabelsByContent:filterVelp) | orderByWhenNotEditing:order)"></tim-velp-window>
                                        </div>
                                    </ng-container>
                                </tim-dialog-frame>

                                <!--                                <div save="%%PAGEID%%selectVelpsDiv"-->
                                <!--                                     tim-draggable-fixed-->
                                <!--                                     detachable="true"-->
                                <!--                                     click="true"-->
                                <!--                                     resize="false"-->
                                <!--                                     caption="Available velps">-->
                                <!--                                    <div class="velp-stack draggable-content">-->
                                <!--                &lt;!&ndash;                        <velp-window class="velp" [ngStyle]="{top: 0}"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                     *ngIf="newVelp.edit"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                     velp="newVelp"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                     index="-1"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                     velp-groups="velpGroups"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                     teacher-right="vctrl.item.rights.teacher"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                     labels="labels"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                     new="true"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                     advanced-on="advancedOn"></velp-window>&ndash;&gt;-->
                                <!--                &lt;!&ndash;                        <velp-window&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                *ngFor="let velp of filteredVelps = (rctrl.velps | filter:{content:filterVelp} | orderByWhenNotEditing:order:filteredVelps | filterByVelpGroups:velpGroups | filterByLabels:labels:advancedOn&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                                 ) track by $index"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                advanced-on="advancedOn"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                doc-id="docId"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                index="$index"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                labels="labels"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                new="false"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                on-velp-select="rctrl.useVelp($VELP)"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                teacher-right="vctrl.item.rights.teacher"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                velp-groups="velpGroups"&ndash;&gt;-->
                                <!--                &lt;!&ndash;                                velp="velp"></velp-window>&ndash;&gt;-->
                                <!--                                    </div>-->
                                <!--                                </div>-->
                            </div>
                        </tab>
                        <tab heading="Manage">
                            <div class="velpArea">
                                <div class="velpSummary">
                                    <form>
                                        <h5>Choose area for velp group:</h5>
                                        <fieldset>
                                            <input type="radio" (ngModelChange)="updateVelpList()"
                                                   [(ngModel)]="groupAttachment.target_type"
                                                   [value]="0" name="selArea" id="selDoc">
                                            <label for="selDoc">Whole document</label><br/>
                                            <input title="A paragraph must be clicked to choose this option"
                                                   type="radio"
                                                   (ngModelChange)="updateVelpList()"
                                                   [(ngModel)]="groupAttachment.target_type" [value]="1"
                                                   name="selArea" id="selPar"
                                                   [class.disabled]="rctrl.selectedElement == null"
                                                   [disabled]="rctrl.selectedElement == null">
                                            <label for="selPar" [class.disabled]="rctrl.selectedElement == null">Selected paragraph</label>
                                        </fieldset>
                                        <table class="fulldiv tim-table">
                                            <tbody>
                                            <tr>
                                                <th><label class="comment-info small"><input type="checkbox"
                                                                                             name="selectedAllShows"
                                                                                             [(ngModel)]="settings.selectedAllShows"
                                                                                             (ngModelChange)="changeAllVelpGroupSelections('show')">Show</label>
                                                </th>
                                                <th><label
                                                        class="comment-info small"
                                                        [class.disabled]="!vctrl.item.rights.manage"><input
                                                        type="checkbox"
                                                        name="selectedAllDefault"
                                                        [(ngModel)]="settings.selectedAllDefault"
                                                        [disabled]="!vctrl.item.rights.manage"
                                                        (ngModelChange)="changeAllVelpGroupSelections('default')">Default</label>
                                                </th>
                                                <th><span class="small">Velp group</span></th>
                                            </tr>

                                            <tr *ngFor="let group of velpGroups" [hidden]="group.id<0">
                                                <td>
                                                    <input type="checkbox" name="groupShow" [(ngModel)]="group.show"
                                                           (ngModelChange)="changeVelpGroupSelection(group, 'show')">
                                                </td>
                                                <td>
                                                    <input type="checkbox" name="groupDefault" [(ngModel)]="group.default"
                                                           [disabled]="!vctrl.item.rights.manage"
                                                           (ngModelChange)="changeVelpGroupSelection(group, 'default')">
                                                </td>
                                                <td>
                                                    <a href="/manage/{{ group.location }}">{{ group.name }}</a>
                                                </td>
                                            </tr>
                                            </tbody>
                                        </table>
                                        <input type="button" class="timButton"
                                               title="Shows only the default velp groups in current area (document or paragraph)"
                                               value="Set area default groups visible"
                                               (click)="resetCurrentShowsToDefaults()"/>
                                        <input id="resetAllSelBtn" type="button" class="timButton"
                                               title="Shows only the default velp groups in all areas"
                                               value="Set all default groups visible"
                                               (click)="resetAllShowsToDefaults()"/>
                                    </form>
                                    <hr>
                                    <form (submit)="addVelpGroup(addVelpGroupForm)" #addVelpGroupForm="ngForm">
                                        <p><label>Create new velp group:</label><input type="text"
                                                                                       name="newVelpGroupName"
                                                                                       [(ngModel)]="newVelpGroup.name"
                                                                                       placeholder="Velp group name"
                                                                                       required></p>
                                        <fieldset>
                                            <p><input type="radio" name="group1" [(ngModel)]="newVelpGroup.target_type"
                                                      id="radioPar"
                                                      [value]="0" checked="checked"> <label for="radioPar">Personal
                                                collection</label><br>
                                                <input type="radio" name="group1" [(ngModel)]="newVelpGroup.target_type"
                                                       id="radioDoc"
                                                       [value]="1" [disabled]="!vctrl.item.rights.editable"> <label
                                                        for="radioDoc"
                                                        [class.disabled]="!vctrl.item.rights.editable">Document</label><br>
                                                <input type="radio" name="group1" [(ngModel)]="newVelpGroup.target_type"
                                                       id="radioFolder"
                                                       [value]="2" [disabled]="!vctrl.item.rights.editable"> <label
                                                        for="radioFolder"
                                                        [class.disabled]="!vctrl.item.rights.editable">Folder</label>
                                            </p>
                                        </fieldset>

                                        <p><input type="submit" class="timButton" value="Create velp group"></p>
                                        <p *ngIf="(addVelpGroupForm.form.invalid && !addVelpGroupForm.form.pristine) || (submitted.velpGroup && addVelpGroupForm.form.invalid)"
                                           class="error">
                                            Velp group name is required!
                                        </p>
                                    </form>
                                </div>
                            </div>
                        </tab>
                        <tab heading="Summary">
                            <velp-summary [annotations]="rctrl.annotations"
                                          (annotationselected)="rctrl.toggleAnnotation($event, true)"
                                          [selectedUser]="vctrl.selectedUser"></velp-summary>
                        </tab>
                    </tabset>
                </div>
            </ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["velp-selection-dialog.component.scss"],
})
export class VelpSelectionDialog extends AngularDialogComponent<
    undefined,
    undefined
> {
    protected dialogName = "velpSelection";
    protected initialHorizontalAlign = Alignment.Right;
    protected initialPosition = {x: 0, y: 50};

    initialized = false;
    labels: ILabelUI[];
    velpGroups: IVelpGroupUI[];
    newVelp: INewVelp;
    private velpToEdit: INewVelp;
    private newLabel: INewLabel;
    private labelToEdit: INewLabel;
    newVelpGroup: INewVelpGroup;
    settings: {selectedAllShows: boolean; selectedAllDefault: boolean};
    submitted: {velp: boolean; velpGroup: boolean};
    groupAttachment: {target_type: number; id: number | null};
    private groupSelections: IVelpGroupCollection;
    private groupDefaults: IVelpGroupCollection;
    order!: string; // ngOnInit
    private selectedLabels: number[] = [];
    advancedOn: boolean = false;
    private defaultVelpGroup: IVelpGroupUI;
    private labelAdded: boolean = false;
    vctrl!: ViewCtrl;
    private defaultPersonalVelpGroup: IVelpGroup;
    newVelpCtrl?: VelpWindowController;
    filterLabel: string = "";
    filterVelp: string = "";

    private storage!: {
        velpOrdering: TimStorage<string>;
        velpLabels: TimStorage<number[]>;
        advancedOn: TimStorage<boolean>;
    };

    constructor() {
        super();
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
            id: null,
            show: false,
            default: false,
        };

        this.settings = {selectedAllShows: false, selectedAllDefault: false};
        this.submitted = {velp: false, velpGroup: false};

        this.groupAttachment = {target_type: 1, id: null};

        this.groupSelections = {};
        this.groupDefaults = {};
        this.defaultVelpGroup = {
            id: -1,
            name: "No access to default group",
            edit_access: false,
            show: true,
            default: true,
            selected: false,
            target_type: null,
        }; // TODO Use route to add this information
        this.defaultPersonalVelpGroup = {
            id: -2,
            name: "Personal-default",
            target_type: null,
            default: true,
        };
    }

    get rctrl() {
        return this.vctrl.reviewCtrl;
    }

    async ngOnInit() {
        this.vctrl = vctrlInstance!;
        this.rctrl.initVelpSelection(this);
        this.rctrl.velps = [];
        // Dictionaries for easier searching: Velp ids? Label ids? Annotation ids?
        const docId = this.docId;
        this.storage = {
            velpOrdering: new TimStorage("velpOrdering_" + docId, t.string),
            velpLabels: new TimStorage(
                "velpLabels_" + docId,
                t.array(t.number)
            ),
            advancedOn: new TimStorage("advancedOn" + docId, t.boolean), // TODO: should this be document specific?
        };

        // Values to store in localstorage:
        this.order = this.storage.velpOrdering.get() ?? "content";
        this.selectedLabels = this.storage.velpLabels.get() ?? [];
        this.advancedOn = this.storage.advancedOn.get() ?? false;

        // TODO check if these routes can all be called simultaneously
        // TODO fetch all data using just one route
        const p1 = $http.get<IVelpGroup[]>(`/${docId}/get_velp_groups`);
        // Get velpgroup data
        const response = await p1;
        const p2 = $http.get<IVelpGroup>(`/${docId}/get_default_velp_group`);
        const p4 = $http.get<IVelp[]>(`/${docId}/get_velps`);
        const p5 = $http.get<ILabel[]>(`/${docId}/get_velp_labels`);
        const p6 = $http.get<IVelpGroupCollection>(
            `/${docId}/get_velp_group_personal_selections`
        );
        const p7 = $http.get<IVelpGroupCollection>(
            `/${docId}/get_velp_group_default_selections`
        );
        // Get default velp group data
        const response2 = await p2;
        const p3 = $http.get<IVelpGroup & {created_new_group: boolean}>(
            "/get_default_personal_velp_group"
        );

        this.velpGroups = response.data;

        this.defaultVelpGroup = response2.data;

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

        // Get personal velp group data
        const response3 = await p3;
        const data = response3.data;
        this.defaultPersonalVelpGroup = {
            id: data.id,
            name: data.name,
            target_type: null,
            default: true,
        };

        if (data.created_new_group) {
            this.velpGroups.push(data);
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
        const response4 = await p4;
        this.rctrl.velps = response4.data;
        this.rctrl.velps.forEach((v) => {
            v.edit = false;
            if (v.labels == null) {
                v.labels = [];
            }
        });

        // Get label data
        const response5 = await p5;
        this.labels = response5.data;
        this.labels.forEach((l) => {
            l.edit = false;
            l.selected = false;
            for (const s of this.selectedLabels) {
                if (l.id === s) {
                    l.selected = true;
                }
            }
        });

        const response6 = await p6;
        this.groupSelections = response6.data;
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

        const response7 = await p7;
        this.groupDefaults = response7.data;

        const docDefaults = this.groupDefaults["0"];

        this.velpGroups.forEach((g) => {
            for (const d of docDefaults) {
                if (d.id === g.id && d.selected) {
                    g.default = true;
                    break;
                }
            }
        });

        this.updateVelpList();
        this.initialized = true;
    }

    // Methods

    changeOrdering(order: string) {
        this.storage.velpOrdering.set(order);
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
        const response = await to(
            $http.post<{id: number}>("/add_velp_label", data)
        );
        if (!response.ok) {
            return;
        }
        const labelToAdd = {
            ...data,
            id: response.result.data.id,
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

    registerNewVelp(v: VelpWindowController) {
        this.newVelpCtrl = v;
    }

    /**
     * Selects velp to edit
     */
    openCreateNewVelpWindow() {
        if (!this.newVelpCtrl) {
            showMessageDialog("New velp controller is not registered");
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
            const json = await to(
                $http.post<IVelpGroup>(
                    "/{0}/create_default_velp_group".replace(
                        "{0}",
                        this.docId.toString()
                    ),
                    "{}"
                )
            );
            if (!json.ok) {
                return null;
            }
            const newDefaultVelpGroup = json.result.data;
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
     * this mehtod resets the velp that is being edited to its original state.
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

        $http.post("/update_velp_label", updatedLabel);
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
        const valid = form.form.valid;
        this.submitted.velpGroup = true;
        if (!valid) {
            return;
        }

        form.form.markAsPristine();

        const json = await to(
            $http.post<IVelpGroup>(
                "/{0}/create_velp_group".replace("{0}", this.docId.toString()),
                this.newVelpGroup
            )
        );
        if (!json.ok) {
            return;
        }
        const group: IVelpGroupUI = json.result.data;
        group.selected = false;
        group.show = true;
        this.velpGroups.push(json.result.data);

        // TODO: show in selected area
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
            $http.post(
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
            $http.post(
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

            $http.post(
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

            $http.post(
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
        await to(
            $http.post(
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

        await to(
            $http.post(
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
}

@Pipe({name: "filterByLabels"})
export class FilterByLabelsPipe implements PipeTransform {
    transform(
        velps?: IVelp[],
        labels?: ILabelUI[],
        advancedOn?: boolean
    ): IVelp[] | undefined {
        const selectedVelps: Record<number, [IVelp, number]> = {};
        const selectedLabels = [];

        if (!advancedOn) {
            return velps;
        }

        if (labels != null) {
            for (const l of labels) {
                if (l.selected) {
                    selectedLabels.push(l.id);
                }
            }
        }

        if (velps != null) {
            for (let j = 0; j < velps.length; j++) {
                for (const s of selectedLabels) {
                    if (velps[j].labels?.includes(s)) {
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

        for (const s of selectedArray) {
            returnVelps.push(s[0]);
        }

        return returnVelps;
    }
}

@Pipe({name: "filterByVelpGroups"})
export class FilterByVelpGroupsPipe implements PipeTransform {
    transform(
        velps?: INewVelp[],
        groups?: IVelpGroupUI[]
    ): INewVelp[] | undefined {
        const selected: INewVelp[] = [];
        const checkedGroups = [];

        if (groups == null || velps == null) {
            return velps;
        }

        for (const g of groups) {
            if (g.show) {
                checkedGroups.push(g.id);
            }
        }

        for (const v of velps) {
            // always include velp that is being edited
            if (v.edit) {
                selected.push(v);
                continue;
            }

            for (const c of checkedGroups) {
                if (v.velp_groups.includes(c) && !selected.includes(v)) {
                    selected.push(v);
                }
            }
        }

        return selected;
    }
}

@Pipe({name: "orderByWhenNotEditing"})
export class OrderByWhenNotEditingPipe implements PipeTransform {
    transform(
        velps: INewVelp[],
        orderStr: string,
        filteredVelps: IVelp[]
    ): INewVelp[] | undefined {
        for (const v of velps) {
            if (v.edit) {
                return filteredVelps;
            }
        }

        let list;
        let reverse = false;
        let order: keyof IVelp = orderStr as keyof IVelp;
        if (orderStr.startsWith("-")) {
            reverse = true;
            order = order.substring(1) as keyof IVelp;
        }

        if (order === "labels") {
            list = velps;
        } else if (order === "content") {
            list = velps.sort((v1, v2) =>
                v1.content.localeCompare(v2.content, sortLang)
            );
        } else {
            list = velps.sort((v1, v2) => {
                const v1o = v1[order];
                const v2o = v2[order];
                if (v1o == null) {
                    return v2o != null ? -1 : 0;
                } else if (v2o == null) {
                    return 1;
                } else if (v1o < v2o) {
                    return -1;
                } else if (v1o > v2o) {
                    return 1;
                }
                return 0;
            });
        }

        if (reverse) {
            list = list.reverse();
        }

        return list;
    }
}

@Pipe({name: "filterLabelsByContent"})
export class FilterLabelsByContentPipe implements PipeTransform {
    transform(velps?: ILabelUI[], content?: string): ILabelUI[] | undefined {
        return velps?.filter((v) => v.content == content);
    }
}
