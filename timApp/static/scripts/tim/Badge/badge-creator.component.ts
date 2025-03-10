import type {OnInit} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {ReactiveFormsModule, Validators} from "@angular/forms";
import {FormGroup, FormControl} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import {HttpClientModule} from "@angular/common/http";
import {BadgeComponent, BadgeModule} from "tim/Badge/Badge-component";
import {toPromise} from "tim/util/utils";
import {BadgeViewerModule} from "tim/Badge/badge-viewer-component";
import {BadgeGiverModule} from "tim/Badge/badge-giver.component";
import {cons} from "fp-ts/ReadonlyNonEmptyArray";
import {getFormBehavior} from "tim/plugin/util";
import {BadgeService} from "tim/Badge/badge.service";
import {IBadge} from "tim/Badge/badge.interface";
import {Users} from "tim/user/userService";

@Component({
    selector: "tim-badges",
    templateUrl: "./badge-creator.component.html",
    styleUrls: ["./badge-creator.component.scss"],
})
export class BadgeCreatorComponent implements OnInit {
    private userName?: string;
    private userID?: number;
    constructor(private http: HttpClient, private badgeService: BadgeService) {}

    isFormChanged = false; // Flag to track form changes
    all_badges: IBadge[] = [];
    selectedContextGroup: string = "";

    // Initializes the component by loading badges and subscribing to form value changes.
    // It tracks changes to the context_group field and triggers a handler when the value changes.
    ngOnInit() {
        let previousContextGroup = this.badgeForm.value.context_group;
        if (Users.isLoggedIn()) {
            this.userName = Users.getCurrent().name;
            this.userID = Users.getCurrent().id;
        }

        this.getBadges();
        this.badgeForm.valueChanges.subscribe(() => {
            this.isFormChanged = true;
            const currentContextGroup = this.badgeForm.value.context_group;
            if (currentContextGroup !== previousContextGroup) {
                previousContextGroup = currentContextGroup;
                if (currentContextGroup) {
                    this.onContextGroupChange(currentContextGroup);
                }
            }
        });
    }

    availableImages = [1, 2, 3, 4, 5];
    availableContext_groups = ["it_01", "it_02", "es_01"];
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
        "pink",
        "red",
        "purple",
        "teal",
        "blue",
        "blue-dark",
        "green",
        "green-dark",
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
                this.http.get<[]>(
                    "/create_badge_simple/" +
                        this.newBadge.created_by +
                        "/" +
                        this.newBadge.context_group +
                        "/" +
                        this.newBadge.title +
                        "/" +
                        this.newBadge.color +
                        "/" +
                        this.newBadge.shape +
                        "/" +
                        this.newBadge.image +
                        "/" +
                        this.newBadge.description
                )
            );
            const result = await response;
            if (result.ok) {
                while (this.all_badges.length > 0) {
                    this.all_badges.pop();
                }
                await this.getBadges();
                this.emptyForm();
                // Lähetetään signaali onnistuneesta luonnista
                this.badgeService.triggerUpdateBadgeList(); // Lähetetään signaali BadgeService:lle
            }
        }
    }

    // Helps when context group is changed
    onContextGroupChange(newContextGroup: string) {
        this.selectedContextGroup = newContextGroup;
        this.getBadges();
    }

    editingBadge: any = null;

    // Edit an existing badge, show attributes in input fields
    editBadge(badge: IBadge) {
        this.editingBadge = badge;
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

    // When cancelled, form information is cleared
    async onCancel() {
        this.selectedContextGroup = "";
        this.emptyForm();
        await this.getBadges();
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

    // Get all badges depending on if context group is selected
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
            if (result.result !== undefined) {
                const badges: IBadge[] = result.result;
                this.all_badges = badges;
                this.all_badges.reverse();
            }
        }
    }

    // Save changes on the badge that is being edited
    async saveBadgeChanges() {
        if (this.editingBadge) {
            Object.assign(this.editingBadge, this.badgeForm.value);
            const response = toPromise(
                this.http.get<[]>(
                    "/modify_badge_simple/" +
                        this.editingBadge.id +
                        "/" +
                        "1" + // Toistaiseksi kovakoodattuna
                        "/" +
                        this.editingBadge.context_group +
                        "/" +
                        this.editingBadge.title +
                        "/" +
                        this.editingBadge.color +
                        "/" +
                        this.editingBadge.shape +
                        "/" +
                        this.editingBadge.image +
                        "/" +
                        this.editingBadge.description
                )
            );
            const result = await response;
            if (result.ok) {
                while (this.all_badges.length > 0) {
                    this.all_badges.pop();
                }
                this.emptyForm();
                await this.getBadges();
            }
        }
    }

    // Delete badge
    async deleteBadge() {
        if (confirm("Are you sure you want to delete badge?")) {
            if (this.editingBadge) {
                try {
                    const response = await toPromise(
                        this.http.get(
                            `/deactivate_badge/${this.editingBadge.id}/1` // Kovakoodattu toistaiseksi
                        )
                    );

                    if (response.ok) {
                        while (this.all_badges.length > 0) {
                            this.all_badges.pop();
                        }

                        this.getBadges();
                        this.emptyForm();
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
    ],
})
export class BadgeCreatorModule {}
