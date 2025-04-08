import {Component, Input, NgModule, SimpleChanges} from "@angular/core";
import {OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {toPromise} from "tim/util/utils";
import type {IBadge} from "tim/gamification/badge/badge.interface";
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
                      <span class="material-symbols-outlined">delete</span>
                  </div>
                <div class="trophy">4th</div>   
                <p class="team">{{ top_five[3] ? top_five[3].group_name : "no team"}}</p>
                <p class="badge-count">{{ top_five[3] ? top_five[3].badge_count : "no badges"}}</p>
              </div>
              <div class="position second">
                  <div class="icon">
                        <span class="material-symbols-outlined">counter_2</span>
                    </div>
                <div class="trophy silver">2nd</div>
                <p class="team">{{ top_five[1] ? top_five[1].group_name : "no team"}}</p>
                <p class="badge-count">{{ top_five[1] ? top_five[1].badge_count : "no badges"}}</p>
              </div>
                <div class="position first">
                    <div class="icon">
                        <span class="material-symbols-outlined">trophy</span>
                    </div>
                <div class="trophy gold">1st</div>
                <p class="team">{{ top_five[0] ? top_five[0].group_name : "no team"}}</p>
                <p class="badge-count">{{ top_five[0] ? top_five[0].badge_count : "no badges"}}</p>
              </div>
                
              <div class="position third">
                  <div class="icon">
                      <span class="material-symbols-outlined">counter_3</span>
                  </div>
                <div class="trophy bronze">3rd</div>
                <p class="team">{{ top_five[2] ? top_five[2].group_name : "no team"}}</p>
                <p class="badge-count">{{ top_five[2] ? top_five[2].badge_count : "no badges"}}</p>
              </div>
              
              <div class="position fifth">
                  <div class="icon">
                      <!--<span class="material-symbols-outlined">sentiment_sad</span> -->
                      <img src="https://i.pinimg.com/236x/f1/9a/51/f19a5199180cc1f5c82bb5367fca65b8.jpg" alt="no Image">
                  </div>
                <div class="trophy">5th</div>
                <p class="team">{{ top_five[4] ? top_five[4].group_name : "no team"}}</p>
                <p class="badge-count">{{ top_five[4] ? top_five[4].badge_count : "no badges"}}</p>
              </div>
            </div>               
        </div>        
    `,
    styleUrls: ["badge.leaderboard.scss"],
})
export class BadgeLeaderboardComponent implements OnInit {
    @Input() nimi: string | undefined;
    top_five: {group_name: string; badge_count: number}[] = [];

    ngOnInit(): void {
        this.nimi = "Seppo";
        this.getTopFive();
    }

    constructor(
        private http: HttpClient,
        protected badgeService: BadgeService
    ) {}

    async getTopFive() {
        try {
            const result = await firstValueFrom(
                this.http.get<{group_name: string; badge_count: number}[]>(
                    "/podium/it_01"
                )
            );
            this.top_five = result || [];
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
