import type {OnInit} from "@angular/core";
import {Input} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {ReactiveFormsModule, Validators} from "@angular/forms";
import {FormGroup, FormControl} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import {HttpClientModule} from "@angular/common/http";
import {BadgeViewerModule} from "tim/gamification/badge/badge-viewer.component";
import {BadgeGiverModule} from "tim/gamification/badge/badge-giver.component";
import {cons} from "fp-ts/ReadonlyNonEmptyArray";
import {getFormBehavior} from "tim/plugin/util";
import {BadgeService} from "tim/gamification/badge/badge.service";
import type {IBadge} from "tim/gamification/badge/badge.interface";
import {Users} from "tim/user/userService";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {toPromise} from "tim/util/utils";
import {
    BadgeComponent,
    BadgeModule,
} from "tim/gamification/badge/badge.component";
import {BadgeWithdrawModule} from "tim/gamification/badge/badge-withdraw.component";
import {ViewCtrl} from "tim/document/viewctrl";
import {documentglobals} from "tim/util/globals";
import {TimUtilityModule} from "tim/ui/tim-utility.module";

@Component({
    selector: "tim-badge-creator",
    template: `
        <ng-container *ngIf="this.hasPermission; else noPermissionView">
        <div class="badge-creator" [formGroup]="badgeForm">
          <fieldset class="form-fieldset">
            <div class="all_badges">
                <fieldset>
                  <h2>{{ selectedContextGroup ? "All Badges (" + selectedContextGroup + ")" : "All Badges" }}</h2>
                  <div class="badge_view">
                      <ng-container *ngIf="all_badges.length == 0">
                          <p>No badges</p>
                      </ng-container>
                      <ng-container *ngIf="all_badges.length > 0">
                          <div class="badge-card" *ngFor="let badge of all_badges">
                            <tim-badge
                               [ngClass]="{'selected-badge': clickedBadge === badge}"
                                       title="{{badge.title}}"
                                       color="{{badge.color}}"
                                       shape="{{badge.shape}}"
                                       [image]="badge.image"
                                       description="{{badge.description}}"
                                       (click)="selectBadge(badge);">
                            </tim-badge>
                          </div>
                  </ng-container>
                  </div>
                    
                </fieldset>
                <div class="button-group">
                    <div class="left-buttons">
                        <button id="showBadgeForm" type="button" (click)="clickCreate()">Create</button>
                        <button id="editButton" type="button" (click)="editBadge(clickedBadge)" 
                                [disabled]="!clickedBadge" 
                                [ngClass]="{'disabled-btn': !clickedBadge}">Edit</button>
                        <button id="giveBadgeButton" type="button" (click)="showBadgeGiver(clickedBadge)" 
                                [disabled]="!clickedBadge" 
                                [ngClass]="{'disabled-btn': !clickedBadge}">Give badge</button>
                    </div>
                    <div class="right-buttons">
                        <button id="withdrawButton" type="button"
                                (click)="showBadgeWithdraw()"
                                class="right-button withdrawButton">With- draw</button>
                        <button id="deleteButton" type="button"
                                [disabled]="!editingBadge" 
                                (click)="deleteBadge()"
                                class="right-button">Delete</button>
                    </div>
                </div>            
            </div>
              
              <ng-container *ngIf="showGiver">
                  <timBadgeGiver (cancelEvent)="handleCancel()" [badgegroupContext]="badgegroupContext" [selectedBadge]="clickedBadge"></timBadgeGiver>                        
              </ng-container>
              
              <ng-container *ngIf="showWithdraw">
                  <tim-badge-withdraw [badgegroupContext]="badgegroupContext"></tim-badge-withdraw>                        
              </ng-container>
              
            <div class="upper-form-group" *ngIf="this.badgeFormShowing">
                <h2>{{ editingBadge ? 'Edit ' + editingBadge.title + ' Badge' : 'Create a Badge' }}</h2>
                <form (ngSubmit)="onSubmit()" id="badgeForm">
                  <div class="form-group">
                    <label for="title">Badge Title</label>
                    <input type="text" id="title" name="title" formControlName="title">
                  </div>
    
                  <div class="form-group">
                    <label for="description">Description</label>
                    <textarea rows="3" cols="" id="description" formControlName="description"> </textarea>
                  </div>
    
                  <div class="icon-color-group">
                    <div class="form-group">
                      <label for="image">Icon</label>
                      <select id="image" formControlName="image">
                        <option *ngFor="let image of availableImages" [value]="image.id">{{ image.name }}</option>
                      </select>
                    </div>
    
                    <div class="form-group">
                      <label for="color">Color</label>
                      <select id="color" formControlName="color">
                        <option *ngFor="let color of availableColors" [value]="color">{{ color }}</option>
                      </select>
                    </div>
    
                  </div>
                    <div class="shape-preview-group">
                        <div class="form-group">
                            <label>Shape</label>
                            <div class="shape">
                              <label *ngFor="let shape of shapes">
                                <input type="radio" [id]="shape.value" formControlName="shape" [value]="shape.value" [checked]="shape.value === 'hexagon'"> {{ shape.label }}
                              </label>
                            </div>
                          </div>
                        <div class="form-group">
                            <label>Preview of the badge</label>
                            <div class="preview">
                                <fieldset>
                                  <tim-badge
                                    [title]="badgeForm.value.title || ''"
                                    [color]="badgeForm.value.color || 'gray'"
                                    [image]="badgeForm.value.image || 0 "
                                    [description]="badgeForm.value.description || ''"
                                    [shape]="badgeForm.value.shape || 'hexagon'">
                                  </tim-badge>
                                </fieldset>
                            </div>
                        </div>
                    </div>
                    
                    <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                        <div [innerHTML]="alert.msg"></div>
                    </tim-alert>
                    
                    <div class="button-container">
                      <button id="createButton"
                              type="submit"
                              [attr.title]="!badgeForm.valid ? 'Fill all the required fields' : null"
                              [disabled]="!badgeForm.valid"
                              (click)="editingBadge ? saveBadgeChanges() : onSubmit()">
                          {{ editingBadge ? 'Save Changes' : 'Create Badge' }}
                      </button>
                      <div class="button-group">
                          <button id="cancelButton" type="button" [disabled]="!isFormChanged" (click)="onCancel()">Cancel</button>
                      </div>
                    </div>
                </form>
              </div>
          </fieldset>
        </div>
    </ng-container>

    <ng-template #noPermissionView>
      <p>Access denied for students.</p>
    </ng-template>
    `,
    styleUrls: ["./badge-creator.component.scss"],
})
export class BadgeCreatorComponent implements OnInit {
    private userName?: string;
    private userID?: number;
    constructor(
        private http: HttpClient,
        protected badgeService: BadgeService
    ) {}

    currentDocumentID = documentglobals().curr_item.id;
    isFormChanged = false; // Flag to track form changes
    all_badges: IBadge[] = [];
    selectedContextGroup: string = "";
    hasPermission = true;
    badgeFormShowing = false;

    clickedBadge: any = null;
    editingBadge: any = null;

    showGiver = false;
    showWithdraw = false;
    @Input() badgegroupContext?: string;
    alerts: Array<{
        msg: string;
        type: "warning" | "danger";
        id?: string;
    }> = [];

    // Method called when a badge is clicked
    selectBadge(badge: IBadge) {
        if (this.clickedBadge === badge) {
            this.clickedBadge = null;
            this.emptyForm();
            this.badgeFormShowing = false;
            this.showGiver = false;
            this.showWithdraw = false;
            return;
        }
        this.clickedBadge = badge;

        this.showEditingForm(badge);
    }

    // Initializes the component by loading badges and subscribing to form value changes.
    // It tracks changes to the context_group field and triggers a handler when the value changes.
    ngOnInit() {
        this.selectedContextGroup = this.badgegroupContext || "";
        if (Users.isLoggedIn()) {
            this.userName = Users.getCurrent().name;
            this.userID = Users.getCurrent().id;

            if (Users.belongsToGroup("Administrators")) {
                this.hasPermission = true;
            }
        }

        this.getBadges();
        this.badgeForm.valueChanges.subscribe(() => {
            this.isFormChanged = true;
        });
    }

    handleCancel() {
        this.resetForm();
        this.showGiver = false;
    }

    // If user has pressed the create badge button, toggles the visibility of the badge creating form
    clickCreate() {
        this.clickedBadge = null;
        this.showGiver = false;
        this.showWithdraw = false;
        if (this.badgeFormShowing) {
            this.resetForm();
            return;
        }

        this.showForm();
        this.emptyForm();
    }

    // Edit an existing badge, show attributes in input fields
    editBadge(badge: IBadge) {
        this.badgeFormShowing = !this.badgeFormShowing;
        this.showGiver = false;
        this.showWithdraw = false;
        if (this.badgeFormShowing) {
            this.showEditingForm(badge);
        }
    }

    showBadgeGiver(badge: IBadge) {
        this.badgeFormShowing = false;
        this.showWithdraw = false;
        this.showGiver = !this.showGiver;

        this.clickedBadge = badge;
    }

    showBadgeWithdraw() {
        this.clickedBadge = null;
        this.badgeFormShowing = false;
        this.showGiver = false;
        this.showWithdraw = !this.showWithdraw;
    }

    // when create button is pressed, shows empty form
    showForm() {
        this.badgeFormShowing = true;
        this.isFormChanged = true;
    }

    // When existing badge is pressed, shows form with filled information of the badge
    showEditingForm(badge: IBadge) {
        this.editingBadge = badge;
        this.clickedBadge = badge;
        this.isFormChanged = true;
        this.badgeForm.patchValue({
            id: badge.id,
            title: badge.title,
            description: badge.description,
            image: badge.image,
            color: badge.color,
            shape: badge.shape,
            context_group: badge.context_group,
        });
    }

    // toggles between showing and not showing form, happens when create or cancel button is pressed.
    resetForm() {
        this.clickedBadge = null;
        this.editingBadge = null;
        this.badgeFormShowing = false;
        this.isFormChanged = false;
    }

    // Clears form information, except given values
    emptyForm() {
        this.editingBadge = null;
        this.badgeForm.reset({
            color: "gray",
            shape: "hexagon",
            context_group: this.selectedContextGroup,
        });
        this.isFormChanged = false;
    }

    // Titles instead of numbers in availableImages
    availableImages = [
        {id: 1, name: "Trophy"},
        {id: 2, name: "Winner"},
        {id: 3, name: "Teamwork"},
        {id: 4, name: "Code"},
        {id: 5, name: "Debug"},
        {id: 6, name: "On_fire"},
        {id: 7, name: "Rocket"},
        {id: 8, name: "Smile"},
        {id: 9, name: "Terminal"},
        {id: 10, name: "Deployed_code"},
        {id: 11, name: "Loop"},
        {id: 12, name: "100_points"},
    ];

    shapes = [
        {label: "Hexagon", value: "hexagon"},
        {label: "Flower", value: "flower"},
        {label: "Circle", value: "round"},
        {label: "Square", value: "square"},
    ];
    // color list for forms
    availableColors = [
        "yellow",
        "orange",
        "amber",
        "red",
        "wine",
        "pink",
        "peach",
        "purple",
        "purple-dark",
        "violet",
        "blue",
        "blue-dark",
        "skyblue",
        "green",
        "green-dark",
        "cyan",
        "lime",
        "mint",
        "brown",
        "gold",
    ];

    badgeForm = new FormGroup({
        id: new FormControl(0),
        image: new FormControl(0, [Validators.required]),
        title: new FormControl("", [Validators.required]),
        icon: new FormControl(""),
        description: new FormControl("", [Validators.required]),
        color: new FormControl("gray", [Validators.required]),
        shape: new FormControl("hexagon", [Validators.required]),
        context_group: new FormControl("", [Validators.required]),
    });

    // Saves newly created badge
    newBadge: any = null;
    async onSubmit() {
        if (this.badgeForm.valid) {
            this.newBadge = this.badgeForm.value;
            this.newBadge.created_by = this.userID;
            const response = toPromise(
                this.http.post<{ok: boolean}>("/create_badge", {
                    created_by: this.newBadge.created_by,
                    doc_id: this.currentDocumentID,
                    context_group: this.selectedContextGroup,
                    title: this.newBadge.title,
                    color: this.newBadge.color,
                    shape: this.newBadge.shape,
                    image: this.newBadge.image.toString(),
                    description: this.newBadge.description,
                })
            );
            //console.log("contextgroup: ", this.selectedContextGroup);
            const result = await response;
            //console.log("contextgroup: ", this.selectedContextGroup);
            if (result.ok) {
                while (this.all_badges.length > 0) {
                    this.all_badges.pop();
                }
                await this.getBadges();
                this.emptyForm();

                this.badgeService.triggerUpdateBadgeList();
                this.badgeFormShowing = false;
            }
            if (!result.ok) {
                this.badgeService.showError(
                    this.alerts,
                    {data: {error: result.result.error.error}},
                    "danger"
                );
                return;
            }
        }
    }

    // When cancelled, form information is cleared
    async onCancel() {
        this.emptyForm();
        await this.getBadges();
        this.badgeFormShowing = false;
        this.resetForm();
    }

    // Get all badges depending on if context group is selected
    private async getBadges() {
        let response;
        if (this.selectedContextGroup) {
            response = toPromise(
                this.http.get<[]>(
                    `/all_badges_in_context/${this.userID}/${this.currentDocumentID}/${this.selectedContextGroup}`
                )
            );
        } else {
            response = toPromise(this.http.get<[]>("/all_badges"));
        }

        const result = await response;

        if (result.ok) {
            if (result.result !== undefined) {
                const badges: IBadge[] = result.result;
                this.all_badges = badges;
                this.all_badges.reverse();
            }
        }
    }

    // Save changes on the badge that is being edited
    async saveBadgeChanges() {
        if (
            await showConfirm(
                `Edit ${this.editingBadge.title}`,
                "Are you sure you want to save the changes?"
            )
        ) {
            if (this.editingBadge) {
                Object.assign(this.editingBadge, this.badgeForm.value);
                const response = toPromise(
                    this.http.post<{ok: boolean}>("/modify_badge", {
                        badge_id: this.editingBadge.id,
                        modified_by: this.userID,
                        doc_id: this.currentDocumentID,
                        context_group: this.editingBadge.context_group,
                        title: this.editingBadge.title,
                        color: this.editingBadge.color,
                        shape: this.editingBadge.shape,
                        image: this.editingBadge.image,
                        description: this.editingBadge.description,
                    })
                );
                const result = await response;
                if (result.ok) {
                    while (this.all_badges.length > 0) {
                        this.all_badges.pop();
                    }
                    this.emptyForm();
                    await this.getBadges();
                }
                this.badgeService.triggerUpdateBadgeList();
                this.badgeFormShowing = false;
                this.clickedBadge = null;
            }
        }
    }

    // Delete badge
    async deleteBadge() {
        if (
            await showConfirm(
                `Delete ${this.editingBadge.title}`,
                `Are you sure you want to delete "${this.editingBadge.title}" badge?`
            )
        ) {
            if (this.editingBadge) {
                try {
                    const response = await toPromise(
                        this.http.post<{ok: boolean}>("/deactivate_badge", {
                            badge_id: this.editingBadge.id,
                            deleted_by: this.userID,
                            doc_id: this.currentDocumentID,
                            context_group: this.selectedContextGroup,
                        })
                    );

                    if (response.ok) {
                        while (this.all_badges.length > 0) {
                            this.all_badges.pop();
                        }

                        this.getBadges();
                        this.emptyForm();
                        this.resetForm();
                        this.clickedBadge = false;
                        this.isFormChanged = false;
                        // Lähetetään signaali onnistuneesta deletoinnista
                        this.badgeService.triggerUpdateBadgeList(); // Lähetetään signaali BadgeService:lle
                    } else {
                        console.log("Failed to delete badge");
                    }
                } catch (error) {
                    console.error("Error deleting badge", error);
                }
            }
        }
    }
}

@NgModule({
    declarations: [BadgeCreatorComponent],
    exports: [BadgeCreatorComponent],
    imports: [
        CommonModule,
        ReactiveFormsModule,
        BadgeModule,
        HttpClientModule,
        BadgeViewerModule,
        BadgeGiverModule,
        BadgeWithdrawModule,
        TimUtilityModule,
    ],
})
export class BadgeCreatorModule {}
