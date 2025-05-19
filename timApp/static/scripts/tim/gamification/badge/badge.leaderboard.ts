import {Component, Input, NgModule} from "@angular/core";
import {OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {firstValueFrom} from "rxjs";
import {Subscription} from "rxjs";
import {GroupService} from "tim/plugin/group-dashboard/group.service";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {PurifyModule} from "tim/util/purify.module";

@Component({
    selector: "tim-badge-leaderboard",
    template: `
        <div class="viewer-container">             
            <h2>{{badgegroupContext}} <ng-container i18n>top5 Leaderboard</ng-container></h2>
            <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                <div [innerHTML]="alert.msg | purify"></div>
            </tim-alert>      
            <div *ngIf="alerts.length === 0 && top_five.length === 0">
                <p class="no-badges-txt" i18n>No groups with badges.</p>
            </div>
            <div class="leaderboard" *ngIf="alerts.length === 0">                  
                <div *ngFor="let team of top_five; let i = index" class="position" [ngClass]="getPositionClass(i)">
                    <div class="icon">
                      <img [src]="'/static/scripts/vendor/material-design-icons/' + getIcon(i) + '.svg'" alt="{{ getIcon(i) }}" />
                    </div>
                    <div class="trophy" [ngStyle]="{'height': calculateHeight(team.badge_count) }">{{ getPosition(i) }}</div>
                    <p class="team">{{ team.prettyName }}</p>
                    <p class="badge-count">{{ team.badge_count || 0 }}</p>
                </div>            
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
    alerts: Array<{
        msg: string;
        type: "warning" | "danger";
        id?: string;
    }> = [];
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

    /**
     * Fetches and processes the top five groups for the current badge group context.
     *
     * - Retrieves the top five groups via an HTTP GET request.
     * - Updates `this.top_five` with the results and sets a `prettyName` for each group.
     * - Handles errors by logging them and displaying appropriate messages to the user.
     *
     * @returns Promise<void> - Resolves when the process is complete.
     */
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

            for (const team of this.top_five) {
                const pretty = await this.groupService.getCurrentGroup(
                    team.group_name
                );
                team.prettyName = pretty?.description || team.group_name;
            }
        } catch (error: any) {
            console.error("Error fetching top five:", error);
            this.top_five = [];
        }
    }

    /**
     * Returns a CSS class name based on the given position index.
     *
     * @param index - The position index (0-based).
     * @returns string - The corresponding class name or an empty string for invalid indexes.
     */
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

    /**
     * Returns an icon name based on the given position index.
     *
     * @param index - The position index (0-based).
     * @returns string - The corresponding icon name or an empty string for invalid indexes.
     */
    getIcon(index: number): string {
        switch (index) {
            case 0:
                return "trophy_yellow";
            case 1:
                return "counter_2";
            case 2:
                return "counter_3";
            case 3:
                return "sentiment_stressed";
            case 4:
                return "sentiment_very_dissatisfied";
            default:
                return "";
        }
    }

    /**
     * Returns the position as a string based on the given index.
     *
     * @param index - The position index (0-based).
     * @returns string - The corresponding position string or an empty string for invalid indexes.
     */
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

    /**
     * Calculates the height based on the badge count.
     * Uses a base height and scales it by the badge count multiplied by a scale factor.
     *
     * @param badgeCount - The number of badges (optional, defaults to 0 if not provided).
     * @returns string - The calculated height in pixels.
     */
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
    imports: [CommonModule, FormsModule, TimUtilityModule, PurifyModule],
    exports: [BadgeLeaderboardComponent],
})
export class BadgeLeaderboardModule {}
