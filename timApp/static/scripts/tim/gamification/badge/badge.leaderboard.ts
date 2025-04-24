import {Component, Input, NgModule, SimpleChanges} from "@angular/core";
import {OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {toPromise} from "tim/util/utils";
import type {IBadge, IGroup} from "tim/gamification/badge/badge.interface";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {firstValueFrom} from "rxjs";
import {Subscription} from "rxjs";
import {GroupService} from "tim/plugin/group-dashboard/group.service";

@Component({
    selector: "tim-badge-leaderboard",
    template: `
        <link href="https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined" rel="stylesheet" />
        <div class="viewer-container">
            <h2>{{badgegroupContext}} top5 Leaderboard</h2>
            <div class="leaderboard">        
                <ng-container *ngIf="top_five.length > 0; else noTeams">
                    <div *ngFor="let team of top_five; let i = index" class="position" [ngClass]="getPositionClass(i)">
                        <div class="icon">
                          <span class="material-symbols-outlined">{{ getIcon(i) }}</span>
                        </div>
                        <div class="trophy" [ngStyle]="{'height': calculateHeight(team.badge_count) }">{{ getPosition(i) }}</div>
                        <p class="team">{{ team.prettyName }}</p>
                        <p class="badge-count">{{ team.badge_count || 0 }}</p>
                    </div>
                </ng-container>
                <ng-template #noTeams>
                    <p class="no-badges-txt">No teams in the selected context group</p>
                </ng-template>               
            </div>               
        </div>        
    `,
    styleUrls: ["badge.leaderboard.scss"],
})
export class BadgeLeaderboardComponent implements OnInit {
    @Input() badgegroupContext?: string;
    top_five: {
        group_name: string;
        badge_count?: number;
        prettyName: string;
    }[] = [];
    baseHeight: number = 25;
    scaleFactor: number = 10;
    private subscription: Subscription = new Subscription();

    ngOnInit(): void {
        this.getTopFive();
        this.subscription = this.badgeService.updateBadgeList$.subscribe(() => {
            this.getTopFive();
        });
    }

    constructor(
        private http: HttpClient,
        protected badgeService: BadgeService,
        private groupService: GroupService
    ) {}

    // Gets the top five groups with most badges
    async getTopFive() {
        try {
            const result = await firstValueFrom(
                this.http.get<
                    {
                        group_name: string;
                        badge_count: number;
                        prettyName: string;
                    }[]
                >(`/podium/${this.badgegroupContext}`)
            );

            this.top_five = result || [];
            console.log("top5: ", this.top_five);

            for (const team of this.top_five) {
                const pretty = await this.groupService.getCurrentGroup(
                    team.group_name
                );
                team.prettyName = pretty?.description || team.group_name; // fallback
            }

            console.log("Fetched top_five: ", this.top_five);
        } catch (error) {
            console.error("Error fetching top five:", error);
            this.top_five = [];
        }
    }

    // Places the position of the group based on the number of badges
    getPositionClass(index: number): string {
        switch (index) {
            case 0:
                return "first";
            case 1:
                return "second";
            case 2:
                return "third";
            case 3:
                return "fourth";
            case 4:
                return "fifth";
            default:
                return "";
        }
    }

    // The icon of the position
    getIcon(index: number): string {
        switch (index) {
            case 0:
                return "trophy"; // 1st place
            case 1:
                return "counter_2"; // 2nd place
            case 2:
                return "counter_3"; // 3rd place
            case 3:
                return "sentiment_stressed"; // 4th place
            case 4:
                return "sentiment_very_dissatisfied"; // 5th place
            default:
                return "";
        }
    }

    // The number of the position
    getPosition(index: number): string {
        switch (index) {
            case 0:
                return "1st";
            case 1:
                return "2nd";
            case 2:
                return "3rd";
            case 3:
                return "4th";
            case 4:
                return "5th";
            default:
                return "";
        }
    }

    // Calculate the height of the columns
    calculateHeight(badgeCount?: number): string {
        const count = badgeCount || 0;
        const height = this.baseHeight + count * this.scaleFactor;
        return `${height}px`;
    }

    ngOnDestroy() {
        this.subscription.unsubscribe();
    }
}

@NgModule({
    declarations: [BadgeLeaderboardComponent],
    imports: [CommonModule, FormsModule],
    exports: [BadgeLeaderboardComponent],
})
export class BadgeLeaderboardModule {}
