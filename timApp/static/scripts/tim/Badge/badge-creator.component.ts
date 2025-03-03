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

interface IBadge {
    id: number;
    title: string;
    color: string;
    image: number;
    shape: string;
    description: string;
    message: string;
}

@Component({
    selector: "tim-badges",
    templateUrl: "./badge-creator.component.html",
    styleUrls: ["./badge-creator.component.scss"],
})
export class BadgeCreatorComponent implements OnInit {
    constructor(private http: HttpClient) {}

    ngOnInit() {
        this.getAllBadges();
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

    shapes = [
        {label: "Hexagon", value: "hexagon"},
        {label: "Flower", value: "flower"},
        {label: "Circle", value: "round"},
        {label: "Square", value: "square"},
    ];

    all_badges: IBadge[] = [];

    badgeForm = new FormGroup({
        id: new FormControl(""),
        image: new FormControl(0),
        title: new FormControl(""),
        icon: new FormControl(""),
        description: new FormControl(""),
        color: new FormControl("gray"),
        shape: new FormControl("hexagon"),
    });

    newBadge: any = null;
    async onSubmit() {
        this.newBadge = this.badgeForm.value;
        // console.log(this.newBadge);
        console.log("New badge: ", this.newBadge);

        const response = toPromise(
            this.http.get<[]>(
                "/create_badge_simple/" +
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
            this.getAllBadges();
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
            title: badge.title,
            description: badge.description,
            image: badge.image,
            color: badge.color,
            shape: badge.shape,
        });
    }

    // Save changes
    saveBadge() {
        if (this.editingBadge) {
            Object.assign(this.editingBadge, this.badgeForm.value);
            this.editingBadge = null;
            this.badgeForm.reset();
        }
    }

    // Delete badge
    deleteBadge() {
        if (this.editingBadge) {
            try {
                const response = await toPromise(
                    this.http.get(`/delete_badge/${this.editingBadge.id}`)
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
