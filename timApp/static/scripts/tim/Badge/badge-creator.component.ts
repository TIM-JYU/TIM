import type {OnInit} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {ReactiveFormsModule} from "@angular/forms";
import {FormGroup, FormControl} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {BadgeComponent, BadgeModule} from "tim/Badge/Badge-component";
import {toPromise} from "tim/util/utils";

@Component({
    selector: "tim-badges",
    templateUrl: "./badge-creator.component.html",
    styleUrls: ["./badge-creator.component.scss"],
})
export class BadgeCreatorComponent implements OnInit {
    constructor(private http: HttpClient) {}

    ngOnInit() {}

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

    availableImages = [1, 2, 3, 4, 5];

    badgeForm = new FormGroup({
        id: new FormControl(""),
        image: new FormControl(1),
        title: new FormControl(""),
        icon: new FormControl(""),
        description: new FormControl(""),
        color: new FormControl("#ff0000"),
        shape: new FormControl(""),
    });

    newBadge: any = null;
    onSubmit() {
        this.newBadge = this.badgeForm.value;
        // console.log(this.newBadge);
        console.log("New badge: ", this.newBadge);

        toPromise(
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
    }
}

@NgModule({
    declarations: [BadgeCreatorComponent],
    exports: [BadgeCreatorComponent],
    imports: [CommonModule, ReactiveFormsModule, BadgeModule, HttpClientModule],
})
export class BadgeCreatorModule {}
