import type {OnInit} from "@angular/core";
import {Input} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import {FormsModule, ReactiveFormsModule} from "@angular/forms";
import {BadgeModule} from "tim/Badge/Badge-component";
import {BadgeViewerModule} from "tim/Badge/badge-viewer-component";
import {BadgeCreatorComponent} from "tim/Badge/badge-creator.component";
import {BadgeService} from "tim/Badge/badge.service";
import {toPromise} from "tim/util/utils";
import {Subscription} from "rxjs";
import {Users} from "tim/user/userService";
import {IBadge} from "tim/Badge/badge.interface";

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
    badges: Badge[] = [];
    selectedUser?: User;
    userBadges: BadgeGiven[] = [];
    selectedBadge?: Badge;
    message = "";

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
    }

    fetchUserBadges(userId: number) {
        this.http
            .get<BadgeGiven[]>(`/badge-given/${userId}`)
            .subscribe((data) => {
                this.userBadges = data;
            });
    }

    async assignBadge(message: string) {
        const response = toPromise(
            this.http.get<[]>(
                "/give_badge/" +
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
    }

    removeBadge(badgeId: number) {
        if (this.selectedUser) {
            const payload = {
                user_id: this.selectedUser.id,
                badge_id: badgeId,
                removed: true,
            };
            this.http.put("/api/badge-given/remove", payload).subscribe(() => {
                this.fetchUserBadges(this.selectedUser!.id);
            });
        }
    }

    protected readonly console = console;
}

@NgModule({
    declarations: [BadgeGiverComponent],
    exports: [BadgeGiverComponent],
    imports: [
        CommonModule,
        ReactiveFormsModule,
        BadgeModule,
        HttpClientModule,
        BadgeViewerModule,
        FormsModule,
    ],
})
export class BadgeGiverModule {}
