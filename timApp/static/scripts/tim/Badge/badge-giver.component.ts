import type {OnInit} from "@angular/core";
import {Input} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import {FormsModule, ReactiveFormsModule} from "@angular/forms";
import {BadgeService} from "tim/Badge/badge.service";
import {toPromise} from "tim/util/utils";
import {Subscription} from "rxjs";
import {Users} from "tim/user/userService";
import type {IBadge} from "tim/Badge/badge.interface";
import {BadgeModule} from "tim/Badge/Badge-component";

interface User {
    id: number;
    name: string;
    real_name: string;
    email: string;
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
    userBadges: IBadge[] = [];
    selectedBadge?: IBadge | null = null;
    message = "";
    badgeGiver = 0;
    showDeleteButton: boolean = false;

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

        // Subscribe to badge update events
        this.subscription.add(
            this.badgeService.updateBadgeList$.subscribe(() => {
                if (this.selectedUser?.id != undefined) {
                    this.fetchUserBadges(this.selectedUser.id); // Refresh badges
                }
            })
        );
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

    async fetchUserBadges(userId?: number) {
        if (userId == undefined) {
            console.error("userid was undefined");
            return;
        }
        while (this.userBadges.length > 0) {
            this.userBadges.pop();
        }
        this.userBadges = await this.badgeService.getUserBadges(userId);
    }

    async assignBadge(message: string) {
        if (Users.isLoggedIn()) {
            this.badgeGiver = Users.getCurrent().id;
        }
        const currentId = this.selectedUser?.id;

        const response = toPromise(
            this.http.post<{ok: boolean}>("/give_badge", {
                given_by: this.badgeGiver,
                group_id: this.selectedUser?.id,
                badge_id: this.selectedBadge?.id,
                message: message,
            })
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
        this.selectedBadge = null;
        this.message = "";
        if (currentId) {
            this.badgeService.getUserBadges(currentId);
            this.badgeService.notifyBadgeViewerUpdate();
        }
    }

    async removeBadge(badgegivenID?: number) {
        this.badgeGiver = Users.getCurrent().id;
        if (badgegivenID == undefined) {
            console.error("badgegived id was undefined");
            return;
        }
        await this.badgeService.withdrawBadge(badgegivenID, this.badgeGiver);
        this.fetchUserBadges(this.selectedUser?.id);
    }

    selectBadge(badge?: IBadge) {
        this.selectedBadge = badge;
        this.showDeleteButton = true;
    }

    ngOnDestroy() {
        this.subscription.unsubscribe();
    }

    protected readonly console = console;
    protected readonly alert = alert;
}

@NgModule({
    declarations: [BadgeGiverComponent],
    exports: [BadgeGiverComponent],
    imports: [CommonModule, FormsModule, BadgeModule],
})
export class BadgeGiverModule {}
