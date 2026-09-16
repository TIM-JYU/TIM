import {Component, Input, NgModule} from "@angular/core";
import type {OnDestroy, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {Subscription} from "rxjs";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {PurifyModule} from "tim/util/purify.module";
import {toPromise} from "tim/util/utils";
import type {IErrorAlert} from "tim/gamification/badge/badge.interface";

/**
 * One row of the /badges/user_podium response.
 */
interface IUserPodiumPlace {
    user_name: string;
    real_name: string;
    badge_count: number;
}

/**
 * How many places the podium shows. The route caps its own result at the same number;
 * this is here so the template and the position helpers agree on the length.
 */
const PODIUM_SIZE = 10;

/**
 * Class name and ordinal for each place, indexed by position. The classes drive the
 * medallion and bar colours in badge.user.leaderboard.scss.
 */
const PLACES: {className: string; ordinal: string}[] = [
    {className: "first", ordinal: "1st"},
    {className: "second", ordinal: "2nd"},
    {className: "third", ordinal: "3rd"},
    {className: "fourth", ordinal: "4th"},
    {className: "fifth", ordinal: "5th"},
    {className: "sixth", ordinal: "6th"},
    {className: "seventh", ordinal: "7th"},
    {className: "eighth", ordinal: "8th"},
    {className: "ninth", ordinal: "9th"},
    {className: "tenth", ordinal: "10th"},
];

@Component({
    selector: "tim-badge-user-leaderboard",
    template: `
        <div class="viewer-container">
            <h2><ng-container i18n>Leaderboard</ng-container> ({{badgegroupContext}})</h2>
            <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                <div [innerHTML]="alert.msg | purify"></div>
            </tim-alert>
            <div *ngIf="alerts.length === 0 && top_ten.length === 0">
                <p class="no-badges-txt" i18n>No users with badges.</p>
            </div>
            <div class="leaderboard" *ngIf="alerts.length === 0">
                <div *ngFor="let user of top_ten; let i = index" class="position" [ngClass]="getPositionClass(i)">
                    <div class="rank">{{ i + 1 }}</div>
                    <div class="trophy" [ngStyle]="{'height': calculateHeight(user.badge_count) }">{{ getPosition(i) }}</div>
                    <p class="user-name">{{ user.real_name }}</p>
                    <p class="badge-count">{{ user.badge_count || 0 }}</p>
                </div>
            </div>
        </div>
    `,
    styleUrls: ["badge.user.leaderboard.scss"],
})
export class BadgeUserLeaderboardComponent implements OnInit, OnDestroy {
    @Input() badgegroupContext?: string;
    top_ten: IUserPodiumPlace[] = [];
    baseHeight: number = 25;
    scaleFactor: number = 10;
    alerts: Array<IErrorAlert> = [];
    private subscription: Subscription = new Subscription();

    ngOnInit(): void {
        void this.getTopTen();
        this.subscription = this.badgeService.updateBadgeList$.subscribe(() => {
            void this.getTopTen();
        });
    }

    constructor(
        private http: HttpClient,
        protected badgeService: BadgeService
    ) {}

    /**
     * Fetches the members of the current badge group context that hold the most badges.
     *
     * The route already returns the places in order and resolves each member's display
     * name, so nothing has to be looked up per row afterwards. A failed request is
     * shown as an alert, which also hides the podium.
     *
     * @returns Promise<void> - Resolves when the process is complete.
     */
    async getTopTen() {
        if (!this.badgegroupContext) {
            this.top_ten = [];
            return;
        }
        const result = await toPromise(
            this.http.get<IUserPodiumPlace[]>(
                `/badges/user_podium/${this.badgegroupContext}`
            )
        );
        if (!result.ok) {
            this.top_ten = [];
            this.badgeService.showError(
                this.alerts,
                {
                    data: {
                        error: result.result.error.error,
                    },
                },
                "danger"
            );
            return;
        }
        // A request that succeeds after a failed one clears the alert; otherwise the
        // podium would stay hidden behind an error that no longer applies.
        this.alerts = [];
        this.top_ten = result.result.slice(0, PODIUM_SIZE);
    }

    /**
     * Returns a CSS class name based on the given position index.
     *
     * @param index - The position index (0-based).
     * @returns string - The corresponding class name or an empty string for invalid indexes.
     */
    getPositionClass(index: number): string {
        return PLACES[index]?.className ?? "";
    }

    /**
     * Returns the position as a string based on the given index.
     *
     * @param index - The position index (0-based).
     * @returns string - The corresponding position string or an empty string for invalid indexes.
     */
    getPosition(index: number): string {
        return PLACES[index]?.ordinal ?? "";
    }

    /**
     * Calculates the height based on the badge count.
     * Uses a base height and scales it by the badge count multiplied by a scale factor.
     *
     * @param badgeCount - The number of badges (optional, defaults to 0 if not provided).
     * @returns string - The calculated height in pixels.
     */
    calculateHeight(badgeCount?: number): string {
        const count = badgeCount ?? 0;
        const height = this.baseHeight + count * this.scaleFactor;
        return `${height}px`;
    }

    ngOnDestroy() {
        this.subscription.unsubscribe();
    }
}

@NgModule({
    declarations: [BadgeUserLeaderboardComponent],
    imports: [CommonModule, FormsModule, TimUtilityModule, PurifyModule],
    exports: [BadgeUserLeaderboardComponent],
})
export class BadgeUserLeaderboardModule {}
