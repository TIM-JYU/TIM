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

@Component({
    selector: "tim-badge-leaderboard",
    template: `
        <link href="https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined" rel="stylesheet" />        
        <div class="viewer-container">             
            <h2>{{badgegroupContext}} top5 Leaderboard</h2>
            <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                <div [innerHTML]="alert.msg"></div>
            </tim-alert>               
            <div class="leaderboard" *ngIf="alerts.length === 0">                  
                <div *ngFor="let team of top_five; let i = index" class="position" [ngClass]="getPositionClass(i)">
                    <div class="icon">
                        <span class="material-symbols-outlined">{{ getIcon(i) }}</span>
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
                team.prettyName = pretty?.description || team.group_name;
            }

            console.log("Fetched top_five: ", this.top_five);
        } catch (error: any) {
            console.error("Error fetching top five:", error);
            this.top_five = [];

            if (error.error?.error === undefined) {
                // no nested `error.error` → probably a network/client issue
                this.badgeService.showError(
                    this.alerts,
                    {
                        data: {
                            error: "Unexpected error. Check your internet connection.",
                        },
                    },
                    "danger"
                );
            } else {
                // server responded with { error: { error: "some message" } }
                this.badgeService.showError(
                    this.alerts,
                    {data: {error: error.error.error}},
                    "danger"
                );
            }
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
    imports: [CommonModule, FormsModule, TimUtilityModule],
    exports: [BadgeLeaderboardComponent],
})
export class BadgeLeaderboardModule {}
