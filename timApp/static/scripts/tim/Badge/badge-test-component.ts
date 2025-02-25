import type {OnInit} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";

interface IBadge {
    id: number;
    text: string;
}

@Component({
    selector: "tim-badge-test",
    template: `
        <ng-container *ngIf="filteredBadges.length == 0">
            <div class="main-wrapper">
              <div class="badge yellow">
                <div class="circle"> <i class="fa fa-bolt"></i></div>
                <div class="ribbon">Initiator</div>
              </div>
              <div class="badge orange">
                <div class="circle"> <i class="fa fa-wheelchair-alt"></i></div>
                <div class="ribbon">Disabler</div>
              </div>
              <div class="badge pink">
                <div class="circle"> <i class="fa fa-pied-piper"></i></div>
                <div class="ribbon">Nuker</div>
              </div>
              <div class="badge red">
                <div class="circle"> <i class="fa fa-shield"></i></div>
                <div class="ribbon">Ganker</div>
              </div>
              <div class="badge purple">
                <div class="circle"> <i class="fa fa-anchor"></i></div>
                <div class="ribbon">Durable</div>
              </div>
              <div class="badge teal">
                <div class="circle"> <i class="fa fa-bicycle"></i></div>
                <div class="ribbon">Roamer</div>
              </div>
              <div class="badge blue">
                <div class="circle"> <i class="fa fa-users"></i></div>
                <div class="ribbon">Pusher</div>
              </div>
              <div class="badge blue-dark">
                <div class="circle"> <i class="fa fa-rocket"></i></div>
                <div class="ribbon">Escape</div>
              </div>
              <div class="badge green">
                <div class="circle"> <i class="fa fa-tree"></i></div>
                <div class="ribbon">Jungler</div>
              </div>
              <div class="badge green-dark">
                <div class="circle"> <i class="fa fa-user fa-street-view"></i></div>
                <div class="ribbon">Offlaner</div>
              </div>
              <div class="badge silver">
                <div class="circle"> <span class="font">AFK</span></div>
                <div class="ribbon">Carry</div>
              </div>
              <div class="badge gold">
                <div class="circle"> <i class="fa fa-magic"></i></div>
                <div class="ribbon">Support</div>
              </div>
            </div>
        </ng-container>
        <ng-container *ngIf="filteredBadges.length > 0">
            <div class="main-wrapper">
                <div *ngFor="let badge of this.filteredBadges" class="{{badge.text}}">
                    <div class="circle"> <i class="fa fa-shield"></i></div>
                    <div class="ribbon"></div>
                </div>
            </div>
        </ng-container>
    `,
    styleUrls: ["badge-viewer-component.scss"],
})
export class BadgeTestComponent implements OnInit {
    badges: IBadge[] = [
        {id: 0, text: "badge blue"},
        {id: 1, text: "badge yellow"},
        {id: 2, text: "badge orange"},
        {id: 3, text: "badge pink"},
        {id: 4, text: "badge red"},
        {id: 5, text: "badge blue-dark"},
        {id: 6, text: "badge green"},
        {id: 7, text: "badge silver"},
        {id: 8, text: "badge gold"},
    ];
    badgeid: number[] = [];
    filteredBadges: IBadge[] = [];

    constructor() {}

    ngOnInit() {}

    addBadge() {}

    public getBadge(badgeIDs: number[]) {
        // for (const id of badgeIDs) {
        //     const response = toPromise(this.http.get<[]>("/all_badges/" + id));
        // }
        this.badgeid = badgeIDs;
        this.filteredBadges = [
            ...this.badges.filter((b) => this.badgeid.includes(b.id)),
        ];
    }
}

@NgModule({
    declarations: [BadgeTestComponent],
    imports: [CommonModule, FormsModule],
    exports: [BadgeTestComponent],
})
export class BadgeTestModule {}
