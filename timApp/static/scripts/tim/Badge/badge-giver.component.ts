import type {OnInit} from "@angular/core";
import {Input} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import {FormsModule, ReactiveFormsModule} from "@angular/forms";
import {BadgeService} from "tim/Badge/badge.service";
import {toPromise} from "tim/util/utils";
import {Subscription} from "rxjs";
import {BadgeViewerModule} from "tim/Badge/badge-viewer-component";
import {Users} from "tim/user/userService";
import {IBadge} from "tim/Badge/badge.interface";
import {BadgeComponent, BadgeModule} from "tim/Badge/Badge-component";

interface Badge {
    id: number;
    title: string;
    description: string;
    color: string;
    shape: string;
    image: number;
}

interface User {
    id: number;
    name: string;
    real_name: string;
    email: string;
}

interface BadgeGiven {
    id: number;
    badge: Badge;
    user_id: number;
    removed: boolean;
    message: string;
}

@Component({
    selector: "timBadgeGiver",
    templateUrl: "./badge-giver.component.html",
    styleUrls: ["./badge-giver.component.scss"],
})
export class BadgeGiverComponent implements OnInit {
    private subscription: Subscription = new Subscription();

    users: User[] = [];
    badges: any = [];
    selectedUser?: User | null = null;
    userBadges: BadgeGiven[] = [];
    selectedBadge?: Badge | null = null;
    message = "";
    badgeGiver = 0;

    constructor(private http: HttpClient, private badgeService: BadgeService) {}

    ngOnInit() {
        // Tilataan updateBadgelist-tapahtuma BadgeService:ltä
        this.subscription.add(
            this.badgeService.updateBadgeList$.subscribe(() => {
                this.fetchBadges(); // Kutsutaan fetchBadges-metodia updaten jälkeen
            })
        );
        this.fetchUsers();
        this.fetchBadges();
    }

    emptyForm() {
        this.selectedUser = null;
        this.selectedBadge = null;
        this.message = "";
    }

    private async fetchUsers() {
        const response = toPromise(this.http.get<[]>("/groups/show/newgroup1"));
        const result = await response;
        if (result.ok) {
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    this.users.push(alkio);
                }
            }
        }
    }

    async fetchBadges() {
        this.badges = await this.badgeService.getAllBadges();
        console.log("näyttää kaikki badget: ", this.badges);
        console.log("Selected Badge:", this.selectedBadge);
    }

    fetchUserBadges(userId: number) {
        this.http
            .get<BadgeGiven[]>(`/api/badge-given/${userId}`)
            .subscribe((data) => {
                this.userBadges = data;
            });
    }

    async assignBadge(message: string) {
        if (Users.isLoggedIn()) {
            this.badgeGiver = Users.getCurrent().id;
        }
        const currentId = this.selectedUser?.id;

        const response = toPromise(
            this.http.get<[]>(
                "/give_badge/" +
                    this.badgeGiver +
                    "/" +
                    this.selectedUser?.id +
                    "/" +
                    this.selectedBadge?.id +
                    "/" +
                    message
            )
        );

        const result = await response;
        if (result.ok) {
            if (result.result != undefined) {
                console.log(
                    "badge " +
                        this.selectedBadge?.title +
                        " annettu käyttäjälle: " +
                        this.selectedUser?.name
                );
            }
        }
        this.selectedBadge = undefined;
        this.message = "";
        if (currentId) {
            this.badgeService.getUserBadges(currentId);
            this.badgeService.notifyBadgeViewerUpdate();
        }
    }
    removeBadge() {
        this.badgeGiver = Users.getCurrent().id;
        const response = toPromise(
            this.http.get(
                `/withdraw_badge/${this.badges.id}/${this.badgeGiver}`
            )
        );
    }

    protected readonly console = console;
    protected readonly alert = alert;
}

@NgModule({
    declarations: [BadgeGiverComponent],
    exports: [BadgeGiverComponent],
    imports: [CommonModule, FormsModule, BadgeViewerModule, BadgeModule],
})
export class BadgeGiverModule {}
