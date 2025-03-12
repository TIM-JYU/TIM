import type {OnInit, SimpleChanges} from "@angular/core";
import {Component, NgModule, Input, OnChanges} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {Users} from "tim/user/userService";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {toPromise} from "tim/util/utils";
import {BadgeModule} from "tim/Badge/Badge-component";
import {BadgeTestModule} from "tim/Badge/badge-test-component";
import {BadgeService} from "tim/Badge/badge.service";
import type {IBadge} from "tim/Badge/badge.interface";
import {Subscription} from "rxjs";

@Component({
    selector: "tim-badge-viewer",
    template: `
        <ng-container *ngIf="badges.length > 0">
            <p>{{this.userName}}'s badges</p>
            <div class="user_badges">
                <tim-badge *ngFor="let badge of badges" 
                           title="{{badge.title}}" 
                           color="{{badge.color}}" 
                           shape="{{badge.shape}}"
                           [image]="badge.image"
                           description="{{badge.description}}"
                           message="{{badge.message}}">
                </tim-badge>
            </div>
        </ng-container>
        <ng-container *ngIf="badges.length == 0">
         <p>user has no badges</p>
        </ng-container>
        `,
    styleUrls: ["badge-viewer-component.scss"],
})
export class BadgeViewerComponent implements OnInit {
    userName?: string;
    userID: number = 0;
    badges: IBadge[] = [];

    private subscription: Subscription = new Subscription();

    constructor(private http: HttpClient, private badgeService: BadgeService) {}

    async getBadges(id: number) {
        this.emptyBadges();
        this.badges = await this.badgeService.getUserBadges(id);
    }

    ngOnInit() {
        if (Users.isLoggedIn()) {
            this.userName = Users.getCurrent().name;
            this.userID = Users.getCurrent().id;
        }
        this.getBadges(this.userID);

        this.subscription = this.badgeService.updateBadgeList$.subscribe(() => {
            this.getBadges(this.userID);
        });
    }

    ngOnDestroy() {
        this.subscription.unsubscribe();
    }

    emptyBadges() {
        while (this.badges.length > 0) {
            this.badges.pop();
        }
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
