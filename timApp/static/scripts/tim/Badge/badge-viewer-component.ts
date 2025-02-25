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
    userID: number;
    badges: IBadge[] = [];
    badgeIDs: number[] = [];

    constructor(private http: HttpClient) {
        this.userID = 0;
    }

    private async getBadges(id: number) {
        const response = toPromise(this.http.get<[]>("/groups_badges/" + id));

        const result = await response;

        if (result.ok) {
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    const json = JSON.stringify(alkio);
                    const obj = JSON.parse(json);
                    this.badges.push(obj);
                    this.badgeIDs.push(obj.id);
                }
            }
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
    imports: [
        CommonModule,
        FormsModule,
        HttpClientModule,
        BadgeModule,
        BadgeTestModule,
    ],
    exports: [BadgeViewerComponent],
})
export class BadgeViewerModule {}
