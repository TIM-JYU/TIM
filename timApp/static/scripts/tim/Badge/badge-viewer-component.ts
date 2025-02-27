import type {OnInit} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {Users} from "tim/user/userService";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {toPromise} from "tim/util/utils";
import {BadgeModule} from "tim/Badge/Badge-component";
import {BadgeTestModule} from "tim/Badge/badge-test-component";

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
    selector: "tim-badge-viewer",
    template: `
        <ng-container *ngIf="badges.length > 0">
            <p>{{userName}}'s badges: </p>
            <div class="user_badges">
                <tim-badge *ngFor="let badge of badges" 
                           title="{{badge.title}}" 
                           color="{{badge.color}}" 
                           shape="{{badge.shape}}"
                           [image]="badge.image"
                           description="{{badge.description}}">
                </tim-badge>
            </div>
        </ng-container>
        <ng-container *ngIf="badges.length == 0">
            <p>{{userName}}'s badges: </p>
            <div class="main-wrapper">
                <div class="badge yellow">
                    <div class="circle"> <i class="fa fa-shield"></i></div>
                    <div class="ribbon"> no badges </div>
                </div>
            </div>
        </ng-container>
        `,
    styleUrls: ["badge-viewer-component.scss"],
})
export class BadgeViewerComponent implements OnInit {
    userName?: string;
    userID: number = 0;
    badges: IBadge[] = [];

    constructor(private http: HttpClient) {}

    private async getBadges(id: number) {
        const response = toPromise(this.http.get<[]>("/groups_badges/" + id));

        const result = await response;
        if (result.ok) {
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    const json = JSON.stringify(alkio);
                    const obj = JSON.parse(json);
                    this.badges.push(obj);
                }
            }
        }
    }

    ngOnInit() {
        if (Users.isLoggedIn()) {
            this.userName = Users.getCurrent().name;
            this.userID = Users.getCurrent().id;
        }
        this.getBadges(this.userID);
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
