import {Component, NgModule} from "@angular/core";
import type {OnInit} from "@angular/core";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import {FormsModule, ReactiveFormsModule} from "@angular/forms";
import {BadgeModule} from "tim/Badge/Badge-component";
import {BadgeViewerModule} from "tim/Badge/badge-viewer-component";

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
    users: User[] = [];
    badges: Badge[] = [];
    selectedUser?: User;
    userBadges: BadgeGiven[] = [];
    selectedBadge?: Badge;
    message = "";

    constructor(private http: HttpClient) {}

    ngOnInit() {
        this.fetchUsers();
        this.fetchBadges();
    }

    fetchUsers() {
        this.http.get<User[]>("/groups/").subscribe((data) => {
            this.users = data;
        });
    }

    fetchBadges() {
        this.http.get<Badge[]>("/badges/").subscribe((data) => {
            this.badges = data;
        });
    }

    fetchUserBadges(userId: number) {
        this.http
            .get<BadgeGiven[]>(`/badge-given/${userId}`)
            .subscribe((data) => {
                this.userBadges = data;
            });
    }

    assignBadge() {
        if (this.selectedUser && this.selectedBadge) {
            const payload = {
                badge_id: this.selectedBadge.id,
                user_id: this.selectedUser.id,
            };
            this.http.post("/badge-given/", payload).subscribe(() => {
                this.fetchUserBadges(this.selectedUser!.id);
            });
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
