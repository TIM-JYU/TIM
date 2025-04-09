import {Component, Input, NgModule, SimpleChanges} from "@angular/core";
import {OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {toPromise} from "tim/util/utils";
import type {IBadge, IGroup} from "tim/gamification/badge/badge.interface";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {firstValueFrom} from "rxjs";

@Component({
    selector: "tim-badge-leaderboard",
    template: `
        <link href="https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined" rel="stylesheet" />
        <div class="viewer-container">
            <div class="leaderboard">
              <div class="position fourth">
                  <div class="icon">
                      <span class="material-symbols-outlined">sentiment_stressed</span>
                  </div>
                <div class="trophy">4th</div>   
                <p class="team">{{ top_five[3] ? top_five[3].prettyName : "no team"}}</p>
                <p class="badge-count">{{ top_five[3] ? top_five[3].badge_count : "nothing"}}</p>
              </div>
              <div class="position second">
                  <div class="icon">
                        <span class="material-symbols-outlined">counter_2</span>
                    </div>
                <div class="trophy silver">2nd</div>
                <p class="team">{{ top_five[1] ? top_five[1].prettyName : "no team"}}</p>
                <p class="badge-count">{{ top_five[1] ? top_five[1].badge_count : "nothing"}}</p>
              </div>
                <div class="position first">
                    <div class="icon">
                        <span class="material-symbols-outlined">trophy</span>
                    </div>
                <div class="trophy gold">1st</div>
                <p class="team">{{ top_five[0] ? top_five[0].prettyName : "no team" }}</p>
                <p class="badge-count">{{ top_five[0] ? top_five[0].badge_count : "nothing"}}</p>
              </div>
                
              <div class="position third">
                  <div class="icon">
                      <span class="material-symbols-outlined">counter_3</span>
                  </div>
                <div class="trophy bronze">3rd</div>
                <p class="team">{{ top_five[2] ? top_five[2].prettyName : "no team"}}</p>
                <p class="badge-count">{{ top_five[2] ? top_five[2].badge_count : "nothing"}}</p>
              </div>
              
              <div class="position fifth">
                  <div class="icon">
                      <span class="material-symbols-outlined">sentiment_very_dissatisfied</span>
                  </div>
                <div class="trophy">5th</div>
                <p class="team">{{ top_five[4] ? top_five[4].prettyName : "no team"}}</p>
                <p class="badge-count">{{ top_five[4] ? top_five[4].badge_count : "nothing"}}</p>
              </div>
            </div>               
        </div>        
    `,
    styleUrls: ["badge.leaderboard.scss"],
})
export class BadgeLeaderboardComponent implements OnInit {
    @Input() badgegroupContext?: string;
    top_five: {group_name: string; badge_count: number; prettyName: string}[] =
        [];

    ngOnInit(): void {
        this.getTopFive();
    }

    constructor(
        private http: HttpClient,
        protected badgeService: BadgeService
    ) {}

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
                const pretty = await this.badgeService.getCurrentGroup(
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
}

@NgModule({
    declarations: [BadgeLeaderboardComponent],
    imports: [CommonModule, FormsModule],
    exports: [BadgeLeaderboardComponent],
})
export class BadgeLeaderboardModule {}
