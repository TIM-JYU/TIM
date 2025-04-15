import type {OnInit} from "@angular/core";
import {ElementRef, ViewChild} from "@angular/core";
import {Input} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {ReactiveFormsModule, Validators} from "@angular/forms";
import {FormGroup, FormControl} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import {HttpClientModule} from "@angular/common/http";
import {BadgeViewerModule} from "tim/gamification/badge/badge-viewer.component";
import {BadgeGiverModule} from "tim/gamification/badge/badge-giver.component";
import {BadgeService} from "tim/gamification/badge/badge.service";
import type {IBadge} from "tim/gamification/badge/badge.interface";
import {Users} from "tim/user/userService";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {toPromise} from "tim/util/utils";
import {BadgeModule} from "tim/gamification/badge/badge.component";
import {BadgeWithdrawModule} from "tim/gamification/badge/badge-withdraw.component";
import {BadgeSelectedWithdrawModule} from "tim/gamification/badge/badge-selected-withdraw.component";
import {documentglobals} from "tim/util/globals";
import {TimUtilityModule} from "tim/ui/tim-utility.module";

@Component({
    selector: "tim-badge-creator",
    template: `
        <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">

        <ng-container *ngIf="!teacherPermission">
            <div class="badge-creator">
                <div class="all-badges">
                    <h2>{{ selectedContextGroup ? "All Badges (" + selectedContextGroup + ")" : "All Badges" }}</h2>
                    <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                        <div [innerHTML]="alert.msg"></div>
                    </tim-alert>                    
                </div>
            </div>
        </ng-container>
        <ng-container *ngIf="teacherPermission">
        <div class="badge-creator" [formGroup]="badgeForm" #allBadgesSection>
          <fieldset class="form-fieldset">
            <div class="all-badges">
                <fieldset>
                    <div class="creator-header">
                        <h2>{{ selectedContextGroup ? "All Badges (" + selectedContextGroup + ")" : "All Badges" }}</h2>
                        <div class="right-buttons">                       
                          <button title="Create Badge" id="showBadgeForm" type="button" (click)="clickCreate()">
                              <span class="material-icons" style="font-size: 30px">add</span></button>
                        </div> 
                    </div>                 
                    
                    <div class="badge-view">
                      <ng-container *ngIf="all_badges.length == 0">
                          <p class="no-badges-txt">This user/group does not have any badges yet.</p>
                      </ng-container>
                      <ng-container *ngIf="all_badges.length > 0">
                          <div class="badge-card" *ngFor="let badge of all_badges">
                            <tim-badge
                               [ngClass]="{'selected-badge': clickedBadge === badge}"
                                       title="{{badge.title}}"
                                       color="{{badge.color}}"
                                       shape="{{badge.shape}}"
                                       [image]="badge.image"
                                       (click)="selectBadge(badge);">
                            </tim-badge>
                              <div *ngIf="clickedBadge === badge" class="badge-buttons">
                                <button title="Assign Badge" id="giveBadgeButton" type="button" 
                                        (click)="showBadgeGiver(clickedBadge)" 
                                        [disabled]="!clickedBadge" 
                                        [ngClass]="{'disabled-btn': !clickedBadge}">
                                        <span class="material-icons">how_to_reg</span></button>
                                <button title="Withdraw Badge" id="giveBadgeButton" type="button"
                                        [disabled]="!clickedBadge" 
                                        (click)="showBadgeSelectedWithdraw(clickedBadge)"
                                        class="right-button">
                                        <span class="material-icons">person_remove</span></button>
                                <button title="Edit Badge" id="editButton" type="button" 
                                        (click)="editBadge(clickedBadge)" 
                                        [disabled]="!clickedBadge" 
                                        [ngClass]="{'disabled-btn': !clickedBadge}">
                                        <span class="material-icons">edit</span></button>
                                <button title="Delete Badge" id="deleteButton" type="button"
                                        [disabled]="!editingBadge" 
                                        (click)="deleteBadge()"
                                        class="right-button">
                                        <span class="material-icons">delete</span></button>
                              </div>
                          </div>
                      </ng-container>
                </div>
                    
                </fieldset>
                <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                    <div [innerHTML]="alert.msg"></div>
                </tim-alert>
            </div>
              
              <ng-container *ngIf="showGiver">
                  <timBadgeGiver (cancelEvent)="handleCancel()" [badgegroupContext]="badgegroupContext" [selectedBadge]="clickedBadge"></timBadgeGiver>                        
              </ng-container>
              
              <ng-container *ngIf="showSelectedWithdraw">
                  <tim-badge-selected-withdraw (cancelEvent)="handleCancel()" [badgegroupContext]="badgegroupContext" [selectedBadge]="clickedBadge"></tim-badge-selected-withdraw>             
              </ng-container>
              
            <div class="upper-form-group" *ngIf="this.badgeFormShowing">
                <h2>{{ editingBadge ? 'Edit "' + editingBadge.title + '" Badge' : 'Create a Badge' }}</h2>
                
                <p class="form-note">
                  Fields marked with <span class="required-asterisk">*</span> are required.
                </p>
                
                <form (ngSubmit)="onSubmit()" id="badgeForm" class="form-group">
                  <div class="form-group">
                      <label for="title">Badge Title <span class="required">*</span></label>
                      <input type="text" id="title" name="title" formControlName="title" [class.invalid]="badgeForm.controls.title.invalid && badgeForm.controls.title.touched">
                      <div *ngIf="badgeForm.controls.title.invalid && badgeForm.controls.title.touched" class="error-message">
                          <p *ngIf="badgeForm.controls.title.hasError('required')">Title is required.</p>
                      </div>
                  </div>
    
                  <div class="form-group">
                      <label for="description">Description <span class="required">*</span></label>
                      <textarea rows="3" cols="" maxlength="200" id="description" formControlName="description"></textarea>
                      
                      <div class="char-counter">
                        {{ badgeForm.get('description')?.value?.length || 0 }} / 200 characters
                      </div>
                      
                      <div *ngIf="badgeForm.controls.description.invalid && badgeForm.controls.description.touched" class="error-message">
                          <p *ngIf="badgeForm.controls.description.hasError('required')">Description is required.</p>
                          <p *ngIf="badgeForm.controls.description.hasError('maxlength')">Description is too long (max 200 characters).</p>
                      </div>
                  </div>
    
                  <div class="icon-color-group">
                    <div class="form-group">
                        <label for="image">Icon <span class="required">*</span></label>
                        <select id="image" formControlName="image">
                            <option *ngFor="let image of availableImages" [value]="image.id">{{ image.name }}</option>
                        </select>
                        <div *ngIf="badgeForm.controls.image.invalid && badgeForm.controls.image.touched" class="error-message">
                            <p *ngIf="badgeForm.controls.image.hasError('required')">Icon is required.</p>
                        </div>
                    </div>
    
                    <div class="form-group">
                        <label for="color">Color <span class="required">*</span></label>
                        <select id="color" formControlName="color">
                            <option *ngFor="let color of availableColors" [value]="color">{{ color }}</option>
                        </select>
                        <div *ngIf="badgeForm.controls.color.invalid && badgeForm.controls.color.touched" class="error-message">
                            <p>Color is required.</p>
                        </div>
                    </div>
    
                  </div>
                    <div class="shape-preview-group">
                        <div class="form-group">
                            <label>Shape <span class="required">*</span></label>
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
                          <button id="cancelButton" 
                                  title="Cancel and return without saving" 
                                  type="button" (click)="onCancel()">
                              Cancel
                          </button>
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
    @ViewChild("allBadgesSection") allBadgesSection!: ElementRef;

    constructor(
        private http: HttpClient,
        protected badgeService: BadgeService
    ) {}

    isFormChanged = false; // Flag to track form changes
    all_badges: IBadge[] = [];
    selectedContextGroup: string = "";
    badgeFormShowing = false;
    teacherPermission = false;
    availableImages: {id: number; name: string}[] = [];

    clickedBadge: any = null;
    editingBadge: any = null;

    showGiver = false;
    showSelectedWithdraw = false;
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
            return;
        }
        this.clickedBadge = badge;
        this.showEditingForm(badge);
    }

    // Initializes the component by loading badges and subscribing to form value changes.
    // It tracks changes to the context_group field and triggers a handler when the value changes.
    ngOnInit() {
        this.availableImages = this.badgeService.getAvailableImages();
        this.selectedContextGroup = this.badgegroupContext || "";
        this.getBadges();
        this.badgeForm.valueChanges.subscribe(() => {
            this.isFormChanged = true;
        });
    }

    // The handle for canceling an event conserning badge-creator.
    handleCancel() {
        this.resetForm();
        this.hideOtherViewsExcept(true);

        setTimeout(() => {
            this.centerToComponent();
        }, 100);
    }

    // Hides the other components when cancel is pressed
    hideOtherViewsExcept(thisView: boolean) {
        this.showGiver = false;
        this.showSelectedWithdraw = false;
        this.badgeFormShowing = false;
        return thisView;
    }

    // If user has pressed the create badge button, toggles the visibility of the badge creating form
    clickCreate() {
        this.clickedBadge = null;
        this.badgeFormShowing = this.hideOtherViewsExcept(
            this.badgeFormShowing
        );

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
        this.badgeFormShowing = this.hideOtherViewsExcept(
            this.badgeFormShowing
        );

        if (this.badgeFormShowing) {
            this.showEditingForm(badge);
        }
        setTimeout(() => {
            this.centerToComponent();
        }, 100);
    }

    // Opens badge-giver from creator
    showBadgeGiver(badge: IBadge) {
        this.showGiver = this.hideOtherViewsExcept(this.showGiver);
        this.showGiver = !this.showGiver;

        this.clickedBadge = badge;
        setTimeout(() => {
            this.centerToComponent();
        }, 100);
    }

    showBadgeSelectedWithdraw(clickedBadge: IBadge) {
        this.showSelectedWithdraw = this.hideOtherViewsExcept(
            this.showSelectedWithdraw
        );
        this.showSelectedWithdraw = !this.showSelectedWithdraw;
        setTimeout(() => {
            this.centerToComponent();
        }, 100);
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
        setTimeout(() => {
            this.centerToComponent();
        }, 100);
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
        setTimeout(() => {
            this.centerToComponent();
        }, 100);
    }

    // The available shapes for badges
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
        "red",
        "wine",
        "pink",
        "purple",
        "violet",
        "dark-blue",
        "skyblue",
        "green",
        "dark-green",
        "mint",
        "brown",
        "coral",
        "turquoise",
        "olive",
        "navy-vibrant",
        "red-vibrant",
        "green-vibrant",
        "purple-vibrant",
        "orange-vibrant",
        "yellow-vibrant",
        "black-vibrant",
    ];

    // Ensures that preset grey cannot be chosen as a color
    disallowGrayColor(control: FormControl) {
        return control.value === "gray" ? {grayNotAllowed: true} : null;
    }

    // The values of a badge
    badgeForm = new FormGroup({
        id: new FormControl(0),
        image: new FormControl(0, [Validators.required]),
        title: new FormControl("", [Validators.required]),
        icon: new FormControl(""),
        description: new FormControl("", [
            Validators.required,
            Validators.maxLength(200),
        ]),
        color: new FormControl("gray", [
            Validators.required,
            this.disallowGrayColor,
        ]),
        shape: new FormControl("hexagon", [Validators.required]),
        context_group: new FormControl("", [Validators.required]),
    });

    // Saves newly created badge
    newBadge: any = null;
    async onSubmit() {
        if (this.badgeForm.valid) {
            this.newBadge = this.badgeForm.value;
            const response = toPromise(
                this.http.post<{ok: boolean}>("/create_badge", {
                    context_group: this.selectedContextGroup,
                    title: this.newBadge.title,
                    color: this.newBadge.color,
                    shape: this.newBadge.shape,
                    image: this.newBadge.image.toString(),
                    description: this.newBadge.description,
                })
            );
            // console.log("contextgroup: ", this.selectedContextGroup);
            const result = await response;
            // console.log("contextgroup: ", this.selectedContextGroup);
            if (result.ok) {
                while (this.all_badges.length > 0) {
                    this.all_badges.pop();
                }
                await this.getBadges();
                this.emptyForm();

                this.badgeService.triggerUpdateBadgeList();
                this.badgeFormShowing = false;
                this.centerToComponent();
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
        setTimeout(() => {
            this.centerToComponent();
        }, 100);
    }

    // Get all badges depending on if context group is selected. Fails if not teacher.
    private async getBadges() {
        let response;
        if (this.selectedContextGroup) {
            response = toPromise(
                this.http.get<[]>(
                    `/all_badges_in_context/${this.selectedContextGroup}`
                )
            );
        } else {
            response = toPromise(this.http.get<[]>("/all_badges"));
        }

        const result = await response;

        if (result.ok) {
            this.teacherPermission = true;
            if (result.result !== undefined) {
                const badges: IBadge[] = result.result;
                this.all_badges = badges;
                this.all_badges.reverse();
            }
        }
        if (!result.ok) {
            this.badgeService.showError(
                this.alerts,
                {
                    data: {
                        error:
                            result.result.error.error +
                            ` If you are a teacher of ${this.badgegroupContext}, please contact TIM admin.`,
                    },
                },
                "danger"
            );
            return;
        }
    }

    // Save changes on the badge that is being edited
    async saveBadgeChanges() {
        if (this.editingBadge) {
            Object.assign(this.editingBadge, this.badgeForm.value);
            const response = toPromise(
                this.http.post<{ok: boolean}>("/modify_badge", {
                    badge_id: this.editingBadge.id,
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
            if (!result.ok) {
                this.badgeService.showError(
                    this.alerts,
                    {data: {error: result.result.error.error}},
                    "danger"
                );
                return;
            }
            this.badgeService.triggerUpdateBadgeList();
            this.badgeFormShowing = false;
            this.clickedBadge = null;
            this.centerToComponent();
        }
    }

    // Check if badge has been assigned to user_group
    async isBadgeAssigned() {
        const response = await toPromise(
            this.http.get<any>(`/badge_given/${this.clickedBadge.id}`)
        );
        if (response.result.length > 0) {
            return true;
        } else {
            return false;
        }
    }

    // Delete a badge from use
    async deleteBadge() {
        let confirmMessage = `Are you sure you want to delete "${this.editingBadge.title}" badge?`;

        if (await this.isBadgeAssigned()) {
            confirmMessage = `WARNING<br><br>This badge has been assigned to users, are you sure you want to delete "${this.editingBadge.title}" badge?`;
        }
        if (
            await showConfirm(
                `Delete ${this.editingBadge.title}`,
                confirmMessage
            )
        ) {
            if (this.editingBadge) {
                try {
                    const response = await toPromise(
                        this.http.post<{ok: boolean}>("/deactivate_badge", {
                            badge_id: this.editingBadge.id,
                            context_group: this.selectedContextGroup,
                        })
                    );

                    if (response.ok) {
                        while (this.all_badges.length > 0) {
                            this.all_badges.pop();
                            this.all_badges.pop();
                        }

                        this.getBadges();
                        this.emptyForm();
                        this.resetForm();
                        this.clickedBadge = false;
                        this.isFormChanged = false;
                        // Send a signel to badgeservice about succesful delete-action
                        this.badgeService.triggerUpdateBadgeList();
                    } else {
                        this.badgeService.showError(
                            this.alerts,
                            {data: {error: "Failed to delete badge"}},
                            "danger"
                        );
                        return;
                    }
                } catch (error) {
                    console.error("Error deleting badge", error);
                }
            }
        }
    }

    // Moves the screen back to the top, when other components are closed.
    centerToComponent() {
        if (this.allBadgesSection) {
            const element = this.allBadgesSection.nativeElement;
            const rect = element.getBoundingClientRect();

            const isVisible =
                rect.top >= 0 &&
                rect.left >= 0 &&
                rect.bottom <=
                    (window.innerHeight ||
                        document.documentElement.clientHeight) &&
                rect.right <=
                    (window.innerWidth || document.documentElement.clientWidth);

            if (!isVisible) {
                const offset = -100;
                const position = rect.top + window.scrollY + offset;
                window.scrollTo({
                    top: position,
                    behavior: "smooth",
                });
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
        BadgeSelectedWithdrawModule,
    ],
})
export class BadgeCreatorModule {}
