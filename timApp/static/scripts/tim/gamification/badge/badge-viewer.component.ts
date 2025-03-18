import {ElementRef, HostListener, OnInit, ViewChild} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {Users} from "../../user/userService";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {BadgeModule} from "./badge.component";
import {BadgeService} from "./badge.service";
import type {IBadge} from "./badge.interface";
import {Subscription} from "rxjs";

@Component({
    selector: "tim-badge-viewer",
    template: `
        <div class="viewer-container">
            <ng-container *ngIf="badges.length > 0">
            <h2 class="badge-heading">{{this.fullname}}'s badges</h2>
            <div class="user_badges" #scrollableDiv>
                <tim-badge *ngFor="let badge of badges" 
                           title="{{badge.title}}" 
                           color="{{badge.color}}" 
                           shape="{{badge.shape}}"
                           [image]="badge.image"
                           description="{{badge.description}}"
                           message="{{badge.message}}">
                </tim-badge>
            </div>
            </ng-container>
            <ng-container *ngIf="badges.length == 0">
                <p>{{this.userName}} has no badges</p>
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

    private subscription: Subscription = new Subscription();

    constructor(private http: HttpClient, private badgeService: BadgeService) {}

    /**
     * Tyhjentää badge -taulukon ja kutsuu badge-servicen metodia joka hakee käyttäjälle kuuluvat badget.
     * @param id käyttäjän id (tällähetkellä käytetään sisäänkirjautuneen käyttäjän ID:tä)
     */
    async getBadges(id: number) {
        this.emptyBadges();
        this.badges = await this.badgeService.getUserBadges(id);
    }

    @ViewChild("scrollableDiv") scrollableDiv!: ElementRef;

    @HostListener("wheel", ["$event"])
    onScroll(event: WheelEvent) {
        if (this.scrollableDiv) {
            const scrollAmount = event.deltaY * 0.5;
            this.scrollableDiv.nativeElement.scrollLeft += scrollAmount;
            event.preventDefault();
        }
    }

    ngOnInit() {
        if (Users.isLoggedIn()) {
            this.userName = Users.getCurrent().name;
            this.fullname = Users.getCurrent().real_name;
            this.userID = Users.getCurrent().id;
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
     * Tyhjentää this.badges -taulukon
     */
    emptyBadges() {
        while (this.badges.length > 0) {
            this.badges.pop();
        }
    }
}

@NgModule({
    declarations: [BadgeViewerComponent],
    imports: [CommonModule, FormsModule, HttpClientModule, BadgeModule],
    exports: [BadgeViewerComponent],
})
export class BadgeViewerModule {}
