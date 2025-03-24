import type {OnInit} from "@angular/core";
import {Input} from "@angular/core";
import {ElementRef, HostListener, ViewChild} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {BadgeModule} from "tim/gamification/badge/badge.component";
import {BadgeService} from "tim/gamification/badge/badge.service";
import type {IBadge, IGroup} from "tim/gamification/badge/badge.interface";
import {Subscription} from "rxjs";
import {Users} from "tim/user/userService";
import {toPromise} from "tim/util/utils";

@Component({
    selector: "tim-badge-viewer",
    template: `
        <div class="viewer-container">
            
            <ng-container *ngIf="badges.length > 0">
                <h2 class="badge-heading">{{this.fullname}}'s badges</h2>
                <div class="user_badges" (wheel)="onScroll($event)">
                    <div class="badge-card" *ngFor="let badge of badges">
                        <tim-badge
                               title="{{badge.title}}" 
                               color="{{badge.color}}" 
                               shape="{{badge.shape}}"
                               [image]="badge.image"
                               description="{{badge.description}}"
                               message="{{badge.message}}">
                        </tim-badge>
                    </div>
                </div>
            </ng-container>
            
            <ng-container *ngIf="userSubGroups.length > 0">
            <div class="subgroups" *ngFor="let group of userSubGroups">
                <h2 class="badge-heading">{{group.name}} badges</h2>
                <div class="users_group_badges" (wheel)="onScroll($event)">
                    <div class="badge-card" *ngFor="let badge of myMap.get(group.id)">
                        <tim-badge
                               title="{{badge.title}}" 
                               color="{{badge.color}}" 
                               shape="{{badge.shape}}"
                               [image]="badge.image"
                               description="{{badge.description}}"
                               message="{{badge.message}}">
                        </tim-badge>
                    </div>
                </div>
            </div>
            </ng-container>

            <ng-container *ngIf="badges.length == 0">
                <p>No badges</p>
            </ng-container>
        </div>
        `,
    styleUrls: ["badge-viewer.component.scss"],
})
export class BadgeViewerComponent implements OnInit {
    userName?: string;
    fullname?: string | null;
    userID: number = 0;
    badges: IBadge[] = [];
    userSubGroups: IGroup[] = [];
    groupBadges: IBadge[] = [];
    @Input() badgegroupContext?: string;
    private subscription: Subscription = new Subscription();
    myMap = new Map<number, IBadge[]>();

    constructor(private http: HttpClient, private badgeService: BadgeService) {}

    /**
     * Tyhjentää badge -taulukon ja kutsuu badge-servicen metodia joka hakee käyttäjälle kuuluvat badget.
     * @param id käyttäjän id (tällähetkellä käytetään sisäänkirjautuneen käyttäjän ID:tä)
     */
    async getBadges(id: number) {
        this.emptyBadges(this.badges);
        this.badges = await this.badgeService.getUserBadges(id);
    }

    async getUserSubGroups(groupContext: string, userid: number) {
        this.userSubGroups = await this.badgeService.getUserSubGroups(
            groupContext,
            userid
        );
        this.getGroupBadges();
    }

    async getGroupBadges() {
        this.myMap.clear();
        for (const group of this.userSubGroups) {
            this.myMap.set(
                group.id,
                await this.badgeService.getUserBadges(group.id)
            );
        }
        console.log(this.myMap);
    }

    //@ViewChild("scrollableDiv") scrollableDiv!: ElementRef;

    onScroll(event: WheelEvent) {
        const targetElement = event.currentTarget as HTMLElement;
        const scrollAmount = event.deltaY * 0.5;
        targetElement.scrollLeft += scrollAmount;
        event.preventDefault();
    }

    ngOnInit() {
        if (Users.isLoggedIn()) {
            this.userName = Users.getCurrent().name;
            this.fullname = Users.getCurrent().real_name;
            this.userID = Users.getCurrent().id;

            if (this.badgegroupContext != undefined) {
                this.getUserSubGroups(this.badgegroupContext, this.userID);
            }
        }
        this.getBadges(this.userID);

        this.subscription = this.badgeService.updateBadgeList$.subscribe(() => {
            this.getBadges(this.userID);
        });
    }

    ngOnDestroy() {
        this.subscription.unsubscribe();
    }

    /**
     * Tyhjentää attribuuttina annetun taulukon
     */
    emptyBadges(badges: IBadge[]) {
        while (badges.length > 0) {
            badges.pop();
        }
    }
}

@NgModule({
    declarations: [BadgeViewerComponent],
    imports: [CommonModule, FormsModule, HttpClientModule, BadgeModule],
    exports: [BadgeViewerComponent],
})
export class BadgeViewerModule {}
