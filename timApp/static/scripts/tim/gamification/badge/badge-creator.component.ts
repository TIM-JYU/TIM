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
import type {IBadge, IErrorAlert} from "tim/gamification/badge/badge.interface";
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
        <ng-container *ngIf="!hasPermissionToHandleBadges">
            <div class="badge-creator">
                <div class="all-badges">
                    <h2>{{ selectedContextGroup ? textAllBadges +" ("+ selectedContextGroup +")" : textAllBadges }}</h2>
                    <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                        <div [innerHTML]="alert.msg | purify"></div>
                    </tim-alert>                    
                </div>
            </div>
        </ng-container>
        <ng-container *ngIf="hasPermissionToHandleBadges">
        <div class="badge-creator" #allBadgesSection>
          <fieldset class="form-fieldset">
            <div class="all-badges">
                <fieldset>
                    <div class="creator-header">
                        <h2>{{ selectedContextGroup ? textAllBadges +" ("+ selectedContextGroup +")" : textAllBadges }}</h2>
                        <div class="right-buttons">                       
                          <button title="Create a new badge" id="showBadgeForm" type="button" (click)="toggleBadgeCreateFromVisibility()" i18n-title>
                              <img src="/static/scripts/vendor/material-design-icons/add.svg" alt="add" />
                          </button>
                        </div> 
                    </div>                 
                    
                    <div class="sort-select">
                        <select id="sort-sel" 
                                [(ngModel)]="selectedSort" 
                                (change)="onSortChange(selectedSort)">
                          <option value="newest" i18n>Newest</option>
                          <option value="oldest" i18n>Oldest</option>
                          <option value="az">A-Z</option>
                          <option value="za">Z-A</option>
                        </select>
                    </div>
                    
                    <div class="badge-view">
                      <ng-container *ngIf="all_badges.length == 0">
                          <p class="no-badges-txt" i18n>This course does not have any badges yet.</p>
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
                                <button title="Assign badge" id="giveBadgeButton" type="button" 
                                        (click)="showBadgeGiver(clickedBadge!)" 
                                        [disabled]="!clickedBadge" 
                                        [ngClass]="{'disabled-btn': !clickedBadge}" i18n-title>
                                        <img src="/static/scripts/vendor/material-design-icons/add.svg" alt="add" />
                                </button>
                                <button title="Edit badge" id="editButton" type="button" 
                                        (click)="editBadge(clickedBadge!)" 
                                        [disabled]="!clickedBadge" 
                                        [ngClass]="{'disabled-btn': !clickedBadge}" i18n-title>
                                        <img src="/static/scripts/vendor/material-design-icons/edit.svg" alt="edit" />
                                </button>
                                <button title="Delete badge" id="deleteButton" type="button"
                                        [disabled]="!editingBadge" 
                                        (click)="deleteBadge()"
                                        class="right-button" i18n-title>
                                        <img src="/static/scripts/vendor/material-design-icons/delete.svg" alt="delete" />
                                </button>
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
                  <tim-badge-giver (cancelEvent)="handleCancel()" [badgegroupContext]="badgegroupContext" [selectedBadge]="clickedBadge"></tim-badge-giver>                        
              </ng-container>
              
            <div class="upper-form-group" *ngIf="this.badgeFormShowing">
                <h2>{{ editingBadge ? textEdit +" "+ editingBadge.title : textCreateNewBadge }}</h2>
                
                <p class="form-note">
                  <ng-container i18n>Fields marked with</ng-container><span class="required-asterisk"> * </span><ng-container i18n>are required</ng-container>.
                </p>
                
                <form [formGroup]="badgeForm" (ngSubmit)="editingBadge ? saveBadgeChanges() : onSubmit()" id="badgeForm" class="form-group">
                  <div class="form-group">
                      <label for="title"><ng-container i18n>Badge title</ng-container><span class="required"> *</span></label>
                      <input type="text" maxlength="30" id="title" name="title" formControlName="title" [class.invalid]="badgeForm.controls.title.invalid && badgeForm.controls.title.touched">
                      
                      <div class="char-counter">
                        {{ badgeForm.get('title')?.value?.length || 0 }} / 30 <ng-container i18n>characters</ng-container>
                      </div>
                      
                      <div *ngIf="badgeForm.controls.title.invalid && badgeForm.controls.title.touched" class="error-message">
                          <p *ngIf="badgeForm.controls.title.hasError('required')" i18n>Title is required.</p>
                          <p *ngIf="badgeForm.controls.title.hasError('maxlength')" i18n>Title is too long (max 30 characters).</p>
                      </div>
                  </div>
    
                  <div class="form-group">
                      <label for="description"><ng-container i18n>Description</ng-container><span class="required"> *</span></label>
                      <textarea rows="3" cols="" maxlength="200" id="description" formControlName="description"></textarea>
                      
                      <div class="char-counter">
                        {{ badgeForm.get('description')?.value?.length || 0 }} / 200 <ng-container i18n>characters</ng-container>
                      </div>
                      
                      <div *ngIf="badgeForm.controls.description.invalid && badgeForm.controls.description.touched" class="error-message">
                          <p *ngIf="badgeForm.controls.description.hasError('required')" i18n>Description is required.</p>
                          <p *ngIf="badgeForm.controls.description.hasError('maxlength')" i18n>Description is too long (max 200 characters).</p>
                      </div>
                  </div>
    
                  <div class="icon-color-group">
                    <div class="form-group">
                        <label for="image"><ng-container i18n>Icon</ng-container><span class="required"> *</span></label>
                        <select id="image" formControlName="image">
                            <option *ngFor="let image of availableImages" [value]="image.id">{{ image.name }}</option>
                        </select>
                        <div *ngIf="badgeForm.controls.image.invalid && badgeForm.controls.image.touched" class="error-message">
                            <p *ngIf="badgeForm.controls.image.hasError('required')" i18n>Icon is required.</p>
                        </div>
                    </div>
    
                    <div class="form-group">
                        <label for="color"><ng-container i18n>Color</ng-container><span class="required"> *</span></label>
                        <select id="color" formControlName="color">
                            <option *ngFor="let color of availableColors" [value]="color.id">{{ color.forCreatorList }}</option>
                        </select>
                        <div *ngIf="badgeForm.controls.color.invalid && badgeForm.controls.color.touched" class="error-message">
                            <p i18n>Color is required.</p>
                        </div>
                    </div>
    
                  </div>
                    <div class="shape-preview-group">
                        <div class="form-group">
                            <label><ng-container i18n>Shape</ng-container><span class="required"> *</span></label>
                            <div class="shape">
                              <label *ngFor="let shape of availableShapes">
                                <input type="radio" [id]="shape.id" formControlName="shape" [value]="shape.id">
                                {{ shape.value }}
                              </label>
                            </div>
                          </div>
                        <div class="form-group">
                            <label i18n>Preview</label>
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
                              [attr.title]="!badgeForm.valid ? textFillRequired : null"
                              [disabled]="!badgeForm.valid">
                          {{ editingBadge ? textSave : textCreate }}
                      </button>
                      <div class="button-group">
                          <button id="cancelButton" 
                                  title="Cancel and return without saving" 
                                  type="button" (click)="onCancel()" i18n-title i18n>
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
      <p i18n>Access denied for students.</p>
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

    isFormChanged = false;
    all_badges: IBadge[] = [];
    selectedContextGroup?: string;
    badgeFormShowing = false;
    hasPermissionToHandleBadges = false;

    availableImages: {id: number; name: string}[] = [];
    availableShapes: {id: string; value: string}[] = [];
    availableColors: {id: string; forCreatorList: string}[] = [];

    clickedBadge: IBadge | null = null;
    editingBadge: IBadge | null = null;

    showGiver = false;
    @Input() badgegroupContext?: string;
    alerts: Array<IErrorAlert> = [];
    selectedSort: string = "newest";
    sortedBadges: IBadge[] = [];

    textCreate = $localize`:@@form.create:Create`;
    textSave = $localize`:@@form.save:Save Changes`;
    textFillRequired = $localize`:@@form.fillRequired:Fill all the required fields`;
    textAllBadges = $localize`:@@form.allBadges:All Badges`;
    textCreateNewBadge = $localize`:@@form.createNewBadge:Create a new badge`;
    textEdit = $localize`:@@form.edit:Edit`;

    /**
     * Method called when a badge is clicked
     * @param badge clicked badge by user
     */
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

    /**
     * Initializes the component by loading badges and subscribing to form value changes.
     * It tracks changes to the context_group field and triggers a handler when the value changes.
     **/
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

    /**
     * Access default badge values directly from the service
     */
    get defaultBadgeValues() {
        return this.badgeService.getDefaultBadgeValues();
    }

    /**
     * The handle for canceling an event concerning badge-creator.
     */
    handleCancel() {
        this.resetForm();
        this.hideOtherViewsExcept(true);

        setTimeout(() => {
            scrollToElement(this.allBadgesSection?.nativeElement);
        }, 100);
    }

    /**
     * Toggles other views (e.g. editing, assigning) false, except one given from argument.
     * @param thisView View you want to keep open.
     */
    hideOtherViewsExcept(thisView: boolean) {
        this.showGiver = false;
        this.badgeFormShowing = false;
        return thisView;
    }

    /**
     * If user has pressed the create badge button, toggles the visibility of the badge creating form
     */
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

    /**
     * Opens badge assigning view and hides other views.
     * @param badge Selected badge
     */
    showBadgeGiver(badge: IBadge) {
        this.showGiver = this.hideOtherViewsExcept(this.showGiver);
        this.showGiver = !this.showGiver;

        this.clickedBadge = badge;
        setTimeout(() => {
            scrollToElement(this.allBadgesSection?.nativeElement);
        }, 100);
    }

    /**
     * When the create button is pressed, shows empty form
     */
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

    /**
     * Ensures that preset grey cannot be chosen as a color
     */
    disallowGrayColor(control: FormControl) {
        return control.value === "gray" ? {grayNotAllowed: true} : null;
    }

    /**
     * The values of a badge
     */
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

    /**
     * Saves newly created badge with the information the user has set,
     * when the create badge button is pressed.
     */
    newBadge: IBadge | undefined;
    async onSubmit() {
        if (this.badgeForm.valid) {
            this.newBadge = this.badgeForm.value as IBadge;
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
            const error = await this.badgeService.checkConnectionError(
                this.alerts
            );
            if (error) {
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
    async onCancel(): Promise<void> {
        this.emptyForm();
        await this.getBadges();
        this.badgeFormShowing = false;
        this.resetForm();
        setTimeout(() => {
            scrollToElement(this.allBadgesSection?.nativeElement);
        }, 100);
    }

    /**
     * Get all badges in chosen context group. Fails if not teacher.
     */
    private async getBadges() {
        // FIXME: show error if there is no context group
        const response = await toPromise(
            this.http.get<IBadge[]>(`/badges`, {
                params: {
                    context_group: this.selectedContextGroup!,
                },
            })
        );

        if (response.ok) {
            this.hasPermissionToHandleBadges = true;
            this.all_badges = response.result.reverse();
            this.onSortChange(this.selectedSort);
            return;
        }

        this.badgeService.showError(
            this.alerts,
            {
                data: {
                    error: response.result.error.error,
                },
            },
            "danger"
        );
    }

    /**
     * Saves the changes made to the badge and updates the server with the modified details.
     *
     * @async
     * @function saveBadgeChanges
     * @returns {Promise<void>} Resolves when the badge changes are successfully saved, or an error is handled.
     *
     */
    async saveBadgeChanges(): Promise<void> {
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
            const error = await this.badgeService.checkConnectionError(
                this.alerts
            );
            if (error) {
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
    async isBadgeAssigned(): Promise<boolean> {
        const response = await toPromise(
            this.http.get<{
                ok: boolean;
                result: Array<
                    Array<{
                        id: number;
                        name: string;
                        personal_user: string | null;
                    }>
                >;
            }>(`/badge_holders/${this.clickedBadge!.id}`)
        );

        return (
            response &&
            Array.isArray(response.result) &&
            Array.isArray(response.result[1]) &&
            response.result[1].length > 0
        );
    }

    /**
     * Handles the deletion of a badge, including user confirmation, server communication,
     * and client-side state updates. This method ensures flow for safely
     * deactivating a badge while managing various edge cases. Throws error if connection issues
     * arise.
     */
    async deleteBadge() {
        // FIXME: show connection error instead of silently returning
        const error = await this.badgeService.checkConnectionError(this.alerts);
        if (error) {
            return;
        }
        let confirmMessage = `Are you sure you want to delete "${
            this.editingBadge!.title
        }" badge?`;

        if (await this.isBadgeAssigned()) {
            confirmMessage = `WARNING<br><br>This badge has been assigned to users, are you sure you want to delete "${
                this.editingBadge!.title
            }" badge?`;
        }
        if (
            await showConfirm(
                `Delete ${this.editingBadge!.title}`,
                confirmMessage
            )
        ) {
            if (this.editingBadge) {
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
                    this.clickedBadge = null;
                    this.isFormChanged = false;
                    // Send a signal to badgeservice about succesful delete-action
                    this.badgeService.triggerUpdateBadgeList();
                } else {
                    this.badgeService.showError(
                        this.alerts,
                        {data: {error: response.result.error.error}},
                        "danger"
                    );
                    return;
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
