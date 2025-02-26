import type {OnInit} from "@angular/core";
import {Input} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";

interface IBadge {
    id: number;
    text: string;
}

@Component({
    selector: "tim-badge-test",
    template: `
        <ng-container>
            <div class="main-wrapper">
                <div class="badge yellow">
                    <div class="circle"> <i class="fa fa-shield"></i></div>
                    <div class="ribbon">{{this.title}}</div>
                </div>
            </div>
        </ng-container>
    `,
    styleUrls: ["badge-viewer-component.scss"],
})
export class BadgeTestComponent implements OnInit {
    badges: IBadge[] = [
        {id: 0, text: "badge blue"},
        {id: 1, text: "badge yellow"},
        {id: 2, text: "badge orange"},
        {id: 3, text: "badge pink"},
        {id: 4, text: "badge red"},
        {id: 5, text: "badge blue-dark"},
        {id: 6, text: "badge green"},
        {id: 7, text: "badge silver"},
        {id: 8, text: "badge gold"},
    ];
    badgeid: number[] = [];
    filteredBadges: IBadge[] = [];

    @Input() title? = "";

    constructor() {}

    ngOnInit() {}

    addBadge() {}

    // public getBadge(badgeIDs: number[]) {
    //     // for (const id of badgeIDs) {
    //     //     const response = toPromise(this.http.get<[]>("/all_badges/" + id));
    //     // }
    //     this.badgeid = badgeIDs;
    //     this.filteredBadges = [
    //         ...this.badges.filter((b) => this.badgeid.includes(b.id)),
    //     ];
    // }
}

@NgModule({
    declarations: [BadgeTestComponent],
    imports: [CommonModule, FormsModule],
    exports: [BadgeTestComponent],
})
export class BadgeTestModule {}
