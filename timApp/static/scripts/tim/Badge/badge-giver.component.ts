import {Component, NgModule} from "@angular/core";
import type {OnInit} from "@angular/core";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import {FormsModule, ReactiveFormsModule} from "@angular/forms";
import {BadgeModule} from "tim/Badge/Badge-component";
import {BadgeViewerModule} from "tim/Badge/badge-viewer-component";
import {BadgeCreatorComponent} from "tim/Badge/badge-creator.component";
import {BadgeService} from "tim/Badge/badge.service";
import {toPromise} from "tim/util/utils";

interface Badge {
    id: number;
    title: string;
    description: string;
    color: string;
    shape: string;
    image: number;
}

interface IBadge {
    id: number;
    title: string;
    color: string;
    image: number;
    shape: string;
    description: string;
    message: string;
    context_group: string;
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
    selector: "badge-giver",
    templateUrl: "./badge-giver.component.html",
    styleUrls: ["./badge-giver.component.scss"],
})
export class BadgeGiverComponent implements OnInit {
    users: User[] = [];
    badges: Badge[] = [];
    selectedUser?: User;
    userBadges: BadgeGiven[] = [];
    selectedBadge?: Badge;
    message: any;

    constructor(private http: HttpClient) {}

    ngOnInit() {
        this.fetchUsers();
        this.fetchBadges();
    }

    private async fetchUsers() {
        // this.users = [
        //     {id: 0, name: "test1"},
        //     {id: 1, name: "test2"},
        //     {id: 2, name: "test3"},
        //     {id: 3, name: "test4"},
        //     {id: 4, name: "test5"},
        // ];
        const response = toPromise(this.http.get<[]>("/groups/show/newgroup1"));
        const result = await response;
        if (result.ok) {
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    this.users.push(alkio);
                }
            }
        }
        console.log(this.users);
    }

    fetchBadges() {}

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
