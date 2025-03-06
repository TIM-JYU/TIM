import type {OnInit} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {ReactiveFormsModule} from "@angular/forms";
import {FormGroup, FormControl} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import {HttpClientModule} from "@angular/common/http";
import {BadgeComponent, BadgeModule} from "tim/Badge/Badge-component";
import {toPromise} from "tim/util/utils";
import {BadgeViewerModule} from "tim/Badge/badge-viewer-component";
import {BadgeGiverModule} from "tim/Badge/badge-giver.component";
import {cons} from "fp-ts/ReadonlyNonEmptyArray";

interface IBadge {
    id: number;
    title: string;
    color: string;
    image: number;
    shape: string;
    description: string;
    message: string;
    context_group: string;
}

@Component({
    selector: "tim-badges",
    templateUrl: "./badge-creator.component.html",
    styleUrls: ["./badge-creator.component.scss"],
})
export class BadgeCreatorComponent implements OnInit {
    constructor(private http: HttpClient) {}

    isFormChanged = false; // Flag to track form changes

    ngOnInit() {
        this.getAllBadges();
        this.badgeForm.valueChanges.subscribe(() => {
            this.isFormChanged = true; // Set flag to true when form changes
        });
    }

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

    availableImages = [1, 2, 3, 4, 5, 6, 100];

    availableContext_groups = ["it_01", "it_02", "es_01"];

    shapes = [
        {label: "Hexagon", value: "hexagon"},
        {label: "Flower", value: "flower"},
        {label: "Circle", value: "round"},
        {label: "Square", value: "square"},
    ];

    all_badges: IBadge[] = [];

    badgeForm = new FormGroup({
        id: new FormControl(0),
        image: new FormControl(0),
        title: new FormControl(""),
        icon: new FormControl(""),
        description: new FormControl(""),
        color: new FormControl("gray"),
        shape: new FormControl("hexagon"),
        context_group: new FormControl(""),
    });

    newBadge: any = null;
    async onSubmit() {
        this.newBadge = this.badgeForm.value;
        const response = toPromise(
            this.http.get<[]>(
                "/create_badge_simple/" +
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
            await this.getAllBadges();
            this.emptyForm();
        }
    }

    private async getAllBadges() {
        const response = toPromise(this.http.get<[]>("/all_badges"));

        const result = await response;
        if (result.ok) {
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    const json = JSON.stringify(alkio);
                    const obj = JSON.parse(json);
                    this.all_badges.push(obj);
                }
                this.all_badges.reverse();
            }
        }
    }

    editingBadge: any = null; // placeholder for selected badge

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

    emptyForm() {
        this.editingBadge = null;
        this.badgeForm.reset({
            color: "gray",
            shape: "hexagon",
        });
        this.isFormChanged = false; // Reset the change flag
    }

    // Save changes
    //TODO: ei tällä hetkellä koske itse databaseen
    async saveBadge() {
        if (this.editingBadge) {
            Object.assign(this.editingBadge, this.badgeForm.value);
            const response = toPromise(
                this.http.get<[]>(
                    "/modify_badge_simple/" +
                        this.editingBadge.id +
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
                await this.getAllBadges();
            }
        }
    }

    // Delete badge
    //TODO: varoitus, haluatko todella poistaa?
    async deleteBadge() {
        if (this.editingBadge) {
            try {
                const response = await toPromise(
                    this.http.get(`/deactivate_badge/${this.editingBadge.id}`)
                );

                if (response.ok) {
                    while (this.all_badges.length > 0) {
                        this.all_badges.pop();
                    }
                    this.getAllBadges();
                    this.editingBadge = null;
                    this.badgeForm.reset({
                        color: "gray",
                        shape: "hexagon",
                    });
                    this.isFormChanged = false;
                } else {
                    console.log("Failed to delete badge");
                }
            } catch (error) {
                console.error("Error deleting badge", error);
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
