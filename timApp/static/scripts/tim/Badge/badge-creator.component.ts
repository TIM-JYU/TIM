import type {OnInit} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {ReactiveFormsModule} from "@angular/forms";
import {FormGroup, FormControl} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import {HttpClientModule} from "@angular/common/http";
import {BadgeComponent, BadgeModule} from "tim/Badge/Badge-component";
import {toPromise} from "tim/util/utils";

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
    availableShapes = ["circle", "triangle", "hexagon"];

    all_badges: IBadge[] = [];

    badgeForm = new FormGroup({
        id: new FormControl(""),
        image: new FormControl(0),
        title: new FormControl(""),
        icon: new FormControl(""),
        description: new FormControl(""),
        color: new FormControl("#ff0000"),
        shape: new FormControl(""),
        message: new FormControl(""),
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
}

@NgModule({
    declarations: [BadgeCreatorComponent],
    exports: [BadgeCreatorComponent],
    imports: [CommonModule, ReactiveFormsModule, BadgeModule, HttpClientModule],
})
export class BadgeCreatorModule {}
