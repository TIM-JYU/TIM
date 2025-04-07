import {Component, Input, NgModule, SimpleChanges} from "@angular/core";
import {OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";

@Component({
    selector: "tim-badge-leaderboard",
    template: `
        <div class="viewer-container">
            <div class="leaderboard">      
                <div class="position fourth">
                <div class="trophy">joku</div>
                
                <p class="team">Team D</p>
              </div>
              <div class="position second">
                <div class="trophy silver"></div>
                <p class="rank">2nd</p>
                <p class="team">Team B</p>
              </div>
                <div class="position first">
                <div class="trophy gold"></div>
                <p class="rank">1st</p>
                <p class="team">Team A</p>
              </div>
                
              <div class="position third">
                <div class="trophy bronze"></div>
                <p class="rank">3rd</p>
                <p class="team">Team C</p>
              </div>
              
              <div class="position fifth">
                <div class="trophy"></div>
                <p class="rank">5th</p>
                <p class="team">Team E</p>
              </div>
            </div>

                  
        </div>
        
    `,
    styleUrls: ["badge.leaderboard.scss"],
})
export class BadgeLeaderboardComponent implements OnInit {
    @Input() nimi: string | undefined;

    ngOnInit(): void {
        this.nimi = "Seppo";
    }
}

@NgModule({
    declarations: [BadgeLeaderboardComponent],
    imports: [CommonModule, FormsModule],
    exports: [BadgeLeaderboardComponent],
})
export class BadgeLeaderboardModule {}
