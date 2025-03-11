import type {OnInit} from "@angular/core";
import {Component, NgModule, Input} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {Users} from "tim/user/userService";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {to2} from "tim/util/utils";

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
                <img [src]="getBadgeImageUrl(selectedBadge.image)" alt="{{ selectedBadge.title }}" class="badge-image" />
            </div>
        </div>
        
        <!-- Delete button, only shown when a badge is selected -->
        <div *ngIf="showDeleteButton">
          <button (click)="removeBadge()">Delete</button>
        </div>
        `,
    styleUrls: ["badge-viewer-component.scss"],
})
export class BadgeViewerComponent implements OnInit {
    userName?: string;
    userID: number;
    badges: IBadge[] = [];
    @Input() id?: number;
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
        this.getBadges();
    }

    // Select a badge to show the delete button
    selectBadge(badge: IBadge) {
        this.selectedBadge = badge;
        this.showDeleteButton = true;
    }

    removeBadge() {
        const response = toPromise(
            this.http.get(
                `/withdraw_badge/${this.selectedBadge}/${this.userID}`
            )
        );
        console.log("badgeId: ", this.selectedBadge);
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
