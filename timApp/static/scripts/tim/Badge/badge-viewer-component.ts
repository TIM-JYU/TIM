import type {OnInit} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {Users} from "tim/user/userService";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {to2} from "tim/util/utils";

@Component({
    selector: "tim-badge-viewer",
    template: `
        <ng-container>
            <p>{{userName}}'s badges: </p>
            <div class="main-wrapper">
              <div *ngFor='let badge of badges' class="{{badge.name}}">
                <div class="circle"> </div>
                <div class="ribbon">{{badge.text}}</div>
              </div>
            </div>
        </ng-container>
        `,
    styleUrls: ["badge-viewer-component.scss"],
})
export class BadgeViewerComponent implements OnInit {
    userName?: string;
    userID?: number;
    badges = [
        {name: "badge gold", text: "12"},
        {name: "badge silver", text: "11"},
        {name: "badge green-dark", text: "10"},
        {name: "badge green", text: "9"},
        {name: "badge blue-dark", text: "8"},
        {name: "badge blue", text: "7"},
        {name: "badge teal", text: "6"},
        {name: "badge purple", text: "5"},
        {name: "badge red", text: "4"},
        {name: "badge pink", text: "3"},
        {name: "badge orange", text: "2"},
        {name: "badge yellow", text: "1"},
    ];

    constructor(private http: HttpClient) {}

    private async getBadges() {
        const result = await to2(this.http.get("/all_badges").toPromise());
        if (result.ok) {
            console.log(result.result);
        }
    }

    ngOnInit() {
        if (Users.isLoggedIn()) {
            this.userName = Users.getCurrent().name;
            this.userID = Users.getCurrent().id;
        }
        this.getBadges();
    }
}

@NgModule({
    declarations: [BadgeViewerComponent],
    imports: [CommonModule, FormsModule, HttpClientModule],
    exports: [BadgeViewerComponent],
})
export class BadgeViewerModule {}
