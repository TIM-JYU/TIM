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
import {showConfirm} from "tim/ui/showConfirmDialog";
import {toPromise} from "tim/util/utils";
import {BadgeModule} from "tim/gamification/badge/badge.component";
import {BadgeWithdrawModule} from "tim/gamification/badge/badge-withdraw.component";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {FormsModule} from "@angular/forms";
import {scrollToElement} from "tim/util/utils";
import {PurifyModule} from "tim/util/purify.module";

@Component({
    selector: "tim-badge-creator",
    template: `
        <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">

        <ng-container *ngIf="!teacherPermission">
            <div class="badge-creator">
                <div class="all-badges">
                    <h2>{{ selectedContextGroup ? "All Badges (" + selectedContextGroup + ")" : "All Badges" }}</h2>
                    <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                        <div [innerHTML]="alert.msg | purify"></div>
                    </tim-alert>                    
                </div>
            </div>
        </ng-container>
        <ng-container *ngIf="teacherPermission">
        <div class="badge-creator" #allBadgesSection>
          <fieldset class="form-fieldset">
            <div class="all-badges">
                <fieldset>
                    <div class="creator-header">
                        <h2>{{ selectedContextGroup ? "All Badges (" + selectedContextGroup + ")" : "All Badges" }}</h2>
                        <div class="right-buttons">                       
                          <button title="Create Badge" id="showBadgeForm" type="button" (click)="toggleBadgeCreateFromVisibility()">
                              <span class="material-icons" style="font-size: 30px">add</span></button>
                        </div> 
                    </div>                 
                    
                    <div class="sort-select">
                        <select id="sort-sel" 
                                [(ngModel)]="selectedSort" 
                                (change)="onSortChange(selectedSort)">
                          <option value="newest">Newest</option>
                          <option value="oldest">Oldest</option>
                          <option value="az">A-Z</option>
                          <option value="za">Z-A</option>
                        </select>
                    </div>
                    
                    <div class="badge-view">
                      <ng-container *ngIf="all_badges.length == 0">
                          <p class="no-badges-txt">This course does not have any badges yet.</p>
                      </ng-container>
                      <ng-container *ngIf="all_badges.length > 0">
                          <div class="badge-card" *ngFor="let badge of sortedBadges">
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
                                        <span class="material-icons">add</span></button>
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
                    <div [innerHTML]="alert.msg | purify"></div>
                </tim-alert>
            </div>
              
              <ng-container *ngIf="showGiver">
                  <timBadgeGiver (cancelEvent)="handleCancel()" [badgegroupContext]="badgegroupContext" [selectedBadge]="clickedBadge"></timBadgeGiver>                        
              </ng-container>
              
            <div class="upper-form-group" *ngIf="this.badgeFormShowing">
                <h2>{{ editingBadge ? 'Edit "' + editingBadge.title + '" Badge' : 'Create a Badge' }}</h2>
                
                <p class="form-note">
                  Fields marked with <span class="required-asterisk">*</span> are required.
                </p>
                
                <form [formGroup]="badgeForm" (ngSubmit)="editingBadge ? saveBadgeChanges() : onSubmit()" id="badgeForm" class="form-group">
                  <div class="form-group">
                      <label for="title">Badge Title <span class="required">*</span></label>
                      <input type="text" maxlength="30" id="title" name="title" formControlName="title" [class.invalid]="badgeForm.controls.title.invalid && badgeForm.controls.title.touched">
                      
                      <div class="char-counter">
                        {{ badgeForm.get('title')?.value?.length || 0 }} / 30 characters
                      </div>
                      
                      <div *ngIf="badgeForm.controls.title.invalid && badgeForm.controls.title.touched" class="error-message">
                          <p *ngIf="badgeForm.controls.title.hasError('required')">Title is required.</p>
                          <p *ngIf="badgeForm.controls.title.hasError('maxlength')">Title is too long (max 30 characters).</p>
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
                            <option *ngFor="let color of availableColors" [value]="color.id">{{ color.forCreatorList }}</option>
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
                              <label *ngFor="let shape of availableShapes">
                                <input type="radio" [id]="shape.id" formControlName="shape" [value]="shape.id">
                                {{ shape.value }}
                              </label>
                            </div>
                          </div>
                        <div class="form-group">
                            <label>Preview of the badge</label>
                            <div class="preview">
                                <fieldset>
                                  <tim-badge
                                    [title]="badgeForm.value.title || defaultBadgeValues.title"
                                    [color]="badgeForm.value.color || defaultBadgeValues.color"
                                    [image]="badgeForm.value.image || defaultBadgeValues.image "
                                    [description]="badgeForm.value.description || defaultBadgeValues.description"
                                    [shape]="badgeForm.value.shape || defaultBadgeValues.shape">
                                  </tim-badge>
                                </fieldset>
                            </div>
                        </div>
                    </div>
                    
                    <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                        <div [innerHTML]="alert.msg | purify"></div>
                    </tim-alert>
                    
                    <div class="button-container">
                      <button id="createButton"
                              type="submit"
                              [attr.title]="!badgeForm.valid ? 'Fill all the required fields' : null"
                              [disabled]="!badgeForm.valid">
                          {{ editingBadge ? 'Save Changes' : 'Create' }}
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
    selectedContextGroup: string | undefined = undefined;
    badgeFormShowing = false;
    teacherPermission = false;

    availableImages: {id: number; name: string}[] = [];
    availableShapes: {id: string; value: string}[] = [];
    availableColors: {id: string; forCreatorList: string}[] = [];

    clickedBadge: any = null;
    editingBadge: any = null;

    showGiver = false;
    @Input() badgegroupContext?: string;
    alerts: Array<{
        msg: string;
        type: "warning" | "danger";
        id?: string;
    }> = [];
    selectedSort: string = "newest";
    sortedBadges: IBadge[] = [];

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
        this.availableShapes = this.badgeService.getAvailableShapes();
        this.availableColors = this.badgeService.getAvailableColors();
        this.selectedContextGroup = this.badgegroupContext;
        this.getBadges();
        this.badgeForm.valueChanges.subscribe(() => {
            this.isFormChanged = true;
        });
    }

    // Access default badge values directly from the service
    get defaultBadgeValues() {
        return this.badgeService.getDefaultBadgeValues();
    }

    // The handle for canceling an event conserning badge-creator.
    handleCancel() {
        this.resetForm();
        this.hideOtherViewsExcept(true);

        setTimeout(() => {
            scrollToElement(this.allBadgesSection?.nativeElement);
        }, 100);
    }

    // Hides the other components when cancel is pressed
    hideOtherViewsExcept(thisView: boolean) {
        this.showGiver = false;
        this.badgeFormShowing = false;
        return thisView;
    }

    // If user has pressed the create badge button, toggles the visibility of the badge creating form
    toggleBadgeCreateFromVisibility() {
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

    /**
     * Toggles the visibility of the badge editing form and prepares it for the selected badge.
     *
     * @param {IBadge} badge - The badge object that needs to be edited, containing all relevant details.
     *
     */
    editBadge(badge: IBadge) {
        this.badgeFormShowing = !this.badgeFormShowing;
        this.badgeFormShowing = this.hideOtherViewsExcept(
            this.badgeFormShowing
        );

        if (this.badgeFormShowing) {
            this.showEditingForm(badge);
        }
        setTimeout(() => {
            scrollToElement(this.allBadgesSection?.nativeElement);
        }, 100);
    }

    // Opens badge-giver from creator
    showBadgeGiver(badge: IBadge) {
        this.showGiver = this.hideOtherViewsExcept(this.showGiver);
        this.showGiver = !this.showGiver;

        this.clickedBadge = badge;
        setTimeout(() => {
            scrollToElement(this.allBadgesSection?.nativeElement);
        }, 100);
    }

    // when create button is pressed, shows empty form
    showForm() {
        this.badgeFormShowing = true;
        this.isFormChanged = true;
    }

    /**
     * Prepares and displays the badge editing form with the given badge data.
     *
     * This function sets the provided `badge` object for editing, marks the form as changed,
     * and populates the form fields with the current values of the selected badge.
     *
     * @param {IBadge} badge - The badge object to be edited, containing all the necessary properties.
     *
     */
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

    resetForm() {
        this.resetBadgeForm("full");
    }
    emptyForm() {
        this.resetBadgeForm("empty");
    }

    /**
     * sets the form according to what function was called.
     * @param mode has two different modes full and empty
     */
    resetBadgeForm(mode: "full" | "empty") {
        this.editingBadge = null;
        this.isFormChanged = false;
        if (mode === "full") {
            this.clickedBadge = null;
            this.badgeFormShowing = false;
        } else {
            this.badgeForm.reset({
                color: "gray",
                shape: "hexagon",
                context_group: this.selectedContextGroup,
            });
        }
        setTimeout(() => {
            scrollToElement(this.allBadgesSection?.nativeElement);
        }, 100);
    }

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
            const result = await response;
            if (result.ok) {
                while (this.all_badges.length > 0) {
                    this.all_badges.pop();
                }
                await this.getBadges();
                this.emptyForm();

                this.badgeService.triggerUpdateBadgeList();
                this.badgeFormShowing = false;
                scrollToElement(this.allBadgesSection?.nativeElement);
            }
            if (await this.checkConnectionError()) {
                return;
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

    /**
     * Handles the cancellation of the current badge editing process.
     * It resets the form and fetches the latest badge information.
     *
     * @returns {Promise<void>} Resolves when the cancellation process is complete and the UI is updated.
     */
    async onCancel() {
        this.emptyForm();
        await this.getBadges();
        this.badgeFormShowing = false;
        this.resetForm();
        setTimeout(() => {
            scrollToElement(this.allBadgesSection?.nativeElement);
        }, 100);
    }

    // Get all badges in chosen context group. Fails if not teacher.
    private async getBadges() {
        let response;
        response = toPromise(
            this.http.get<[]>(`/all_badges/${this.selectedContextGroup}`)
        );

        const result = await response;

        if (result.ok) {
            this.teacherPermission = true;
            if (result.result !== undefined) {
                const badges: IBadge[] = result.result;
                this.all_badges = badges;
                this.all_badges.reverse();
                this.onSortChange(this.selectedSort);
            }
        }
        if (await this.checkConnectionError()) {
            return;
        }
        if (!result.ok) {
            this.badgeService.showError(
                this.alerts,
                {
                    data: {
                        error: result.result.error.error,
                    },
                },
                "danger"
            );
            return;
        }
    }

    /**
     * Saves the changes made to the badge and updates the server with the modified details.
     *
     * @async
     * @function saveBadgeChanges
     * @returns {Promise<void>} Resolves when the badge changes are successfully saved, or an error is handled.
     *
     */
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
            if (await this.checkConnectionError()) {
                return;
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
            scrollToElement(this.allBadgesSection?.nativeElement);
        }
    }

    /**
     * Checks if the currently selected badge is assigned to any users.
     *
     * This function sends a GET request to the `/badge_holders/{badge_id}` endpoint and checks if the badge
     * is assigned to any users or groups based on the response.
     *
     * @returns {Promise<boolean>} A promise that resolves to `true` if the badge is assigned to users or groups, or `false` otherwise.
     */
    async isBadgeAssigned() {
        const response = await toPromise(
            this.http.get<any>(`/badge_holders/${this.clickedBadge.id}`)
        );
        if (response.result[0].length > 0 || response.result[1].length > 0) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Handles the deletion of a badge, including user confirmation, server communication,
     * and client-side state updates. This method ensures flow for safely
     * deactivating a badge while managing various edge cases. Throws error if connection issues
     * arise.
     */
    async deleteBadge() {
        if (await this.checkConnectionError()) {
            return;
        }
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
                        // Send a signal to badgeservice about succesful delete-action
                        this.badgeService.triggerUpdateBadgeList();
                    } else {
                        if (await this.checkConnectionError()) {
                            return;
                        }
                        this.badgeService.showError(
                            this.alerts,
                            {data: {error: response.result.error.error}},
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

    /**
     * Handles the badge sorting logic triggered by user interaction.
     * Delegates sorting to the BadgeService based on the selected sort type.
     *
     * @param sortType The selected sort option ("az", "za", "newest", "oldest")
     */
    onSortChange(sortType: string) {
        this.sortedBadges = this.badgeService.sortBadges(
            this.all_badges,
            sortType
        );
    }

    /**
     * Tests connection with check_connection route.
     * If there is error with result, calls showError method via badge-service and returns true.
     * If no errors, returns false.
     */
    async checkConnectionError() {
        const result = await toPromise(this.http.get(`/check_connection/`));
        if (!result.ok) {
            this.badgeService.showError(
                this.alerts,
                {
                    data: {
                        error: "Unexpected error. Check your internet connection.",
                    },
                },
                "danger"
            );
            return true;
        }
        return false;
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
        FormsModule,
        PurifyModule,
    ],
})
export class BadgeCreatorModule {}
