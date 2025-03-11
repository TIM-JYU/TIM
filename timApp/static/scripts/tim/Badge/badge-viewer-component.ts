import type {OnInit, SimpleChanges} from "@angular/core";
import {Component, NgModule, Input, OnChanges} from "@angular/core";
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
        <ng-container *ngIf="selectedUserName && badges.length > 0">
            <!-- Tämä otsikko näkyy vain, jos käyttäjä on valittu ja hänellä on badget -->
            <label for="user-badges">{{selectedUserName}}'s badges</label>
        </ng-container>
        
        <ng-container *ngIf="badges.length > 0">
            <div class="user_badges">
                <div *ngIf="id != null">
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
            </div>
        </ng-container>
        
        <!-- Delete button, only shown when a badge is selected -->
        <div *ngIf="showDeleteButton">
          <button (click)="removeBadge()">Delete</button>
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
    //@Output() selectedBadge?: IBadge;

    // Track the selected badge and whether to show the delete button
    selectedBadge?: IBadge;
    showDeleteButton: boolean = false;

    constructor(private http: HttpClient) {}

    private async getBadges(id: number) {
        while (this.badges.length > 0) {
            this.badges.pop();
        }
        const response = toPromise(this.http.get<[]>("/groups_badges/" + id));
        const result = await response;
        if (result.ok) {
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    const json = JSON.stringify(alkio);
                    const obj = JSON.parse(json);
                    this.badges.push(obj);
                }
                console.log("haettu käyttäjän " + id + " badget");
            }
            console.log("Miks tätä kutsutaan kahdesti: ", this.badges);
        }
    }

    ngOnInit() {
        if (Users.isLoggedIn()) {
            this.userName = Users.getCurrent().name;
            this.userID = Users.getCurrent().id;
        }
    }
    ngOnChanges(changes: SimpleChanges) {
        if (this.id != undefined) {
            console.log("haettu badget id:llä: " + this.id);
            this.getBadges(this.id);
        }
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
