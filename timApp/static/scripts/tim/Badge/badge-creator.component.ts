import {Component, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {ReactiveFormsModule} from "@angular/forms";
import {FormGroup, FormControl} from "@angular/forms";
import {BadgeComponent, BadgeModule} from "./Badge-component";

@Component({
    selector: "tim-badges",
    templateUrl: "./badge-creator.component.html",
    styleUrls: ["./badge-creator.component.scss"],
})
export class BadgeCreatorComponent implements OnInit {
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

    availableImages = [1, 2, 3, 4, 5, 6, 100];
    availableShapes = ["circle", "triangle", "hexagon"];

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
    onSubmit() {
        this.newBadge = this.badgeForm.value;
        //console.log(this.newBadge);
        console.log("New badge: ", this.newBadge);
    }
}

@NgModule({
    declarations: [BadgeCreatorComponent],
    exports: [BadgeCreatorComponent],
    imports: [CommonModule, ReactiveFormsModule, BadgeModule],
})
export class BadgeCreatorModule {}
