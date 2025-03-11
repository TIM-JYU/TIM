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
            <p>{{selectedUserName}}'s badges</p>
            <div class="user_badges">
                <tim-badge *ngFor="let badge of badges" 
                           title="{{badge.title}}" 
                           color="{{badge.color}}" 
                           shape="{{badge.shape}}"
                           [image]="badge.image"
                           description="{{badge.description}}"
                           message="{{badge.message}}"
                           (click)="selectBadge(badge)">
                </tim-badge>
            </div>
        </ng-container>
        <ng-container *ngIf="badges.length == 0">
         <p>{{selectedUserName}} has no badges</p>
        </ng-container>
        
        <!-- Preview of the selected badge -->
        <div *ngIf="selectedBadge" class="badge-preview">
            <h3>Selected Badge Preview</h3>
            <div class="badge-info">
                <h4>{{ selectedBadge.title }}</h4>
                <p><strong>Description:</strong> {{ selectedBadge.description }}</p>
                <p><strong>Message:</strong> {{ selectedBadge.message }}</p>
<!--                <img [src]="getBadgeImageUrl(selectedBadge.image)" alt="{{ selectedBadge.title }}" class="badge-image" />-->
            </div>
        </div>
        
        <!-- Delete button, only shown when a badge is selected -->
        <div *ngIf="showDeleteButton">
          <button (click)="removeBadge(selectedBadge?.badgegiven_id)">Delete</button>
        </div>
        `,
    styleUrls: ["badge-viewer-component.scss"],
})
export class BadgeViewerComponent implements OnInit {
    userName?: string;
    userID: number = 0;
    badges: IBadge[] = [];
    @Input() id?: number;
    @Input() selectedUserName?: string;

    // Track the selected badge and whether to show the delete button
    selectedBadge?: IBadge;
    showDeleteButton: boolean = false;

    private subscription: Subscription = new Subscription();

    constructor(private http: HttpClient, private badgeService: BadgeService) {}

    async getBadges(id: number) {
        while (this.badges.length > 0) {
            this.badges.pop();
        }
        this.badges = await this.badgeService.getUserBadges(id);
    }

    ngOnInit() {
        if (Users.isLoggedIn()) {
            this.userName = Users.getCurrent().name;
            this.userID = Users.getCurrent().id;

            if (this.id != undefined) {
                this.getBadges(this.id);
                return;
            }
        }
        this.getBadges(this.userID);

        // Subscribe to badge update events
        this.subscription.add(
            this.badgeService.updateBadgeList$.subscribe(() => {
                if (this.id != undefined) {
                    this.getBadges(this.id); // Refresh badges
                }
            })
        );
    }
    ngOnChanges(changes: SimpleChanges) {
        if (changes.id) {
            if (this.id != undefined) {
                this.getBadges(this.id);
            }
        }
    }

    ngOnDestroy() {
        this.subscription.unsubscribe();
    }

    // Select a badge to show the delete button
    selectBadge(badge?: IBadge) {
        this.selectedBadge = badge;
        this.showDeleteButton = true;
    }

    async removeBadge(badgegivenID?: number) {
        if (badgegivenID == undefined) {
            console.error("badgegivenID was undefined");
            return;
        }
        this.badgeService.withdrawBadge(badgegivenID, this.userID);
        //Päivitetään badge-viewerin näkymä.
        this.badges = this.badges.filter(
            (badge) => badge.badgegiven_id !== badgegivenID
        );
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
