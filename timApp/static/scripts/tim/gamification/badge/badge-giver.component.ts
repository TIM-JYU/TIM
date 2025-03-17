import type {OnInit} from "@angular/core";
import {Input} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {Subscription} from "rxjs";
import {Users} from "tim/user/userService";
import type {
    IBadge,
    IGroup,
    IUser,
} from "tim/gamification/badge/badge.interface";
import {BadgeModule} from "tim/gamification/badge/badge.component";
import {toPromise} from "tim/util/utils";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {showConfirm} from "tim/ui/showConfirmDialog";

@Component({
    selector: "timBadgeGiver",
    template: `
        <ng-container *ngIf="this.hasPermission; else noPermissionView">
        <div class="badge-giver">
            <h2>Badge Giver</h2>
        
            <div class="user-selection">
                <label for="select-user">User</label>
                <select id="select-user" [(ngModel)]="selectedUser">
                    <option [ngValue]="null" disabled selected>Select an user</option>
                    <option *ngFor="let user of users" [ngValue]="user" (click)="fetchUserBadges(user.id)">
                    {{ user.real_name }}
                </option>
                </select>
            </div>
        
            <ng-container *ngIf="userBadges.length > 0">
               <p *ngIf="selectedUser?.name != undefined">{{selectedUser?.real_name}}'s badges</p>
                    <div class="user_badges">
                        <tim-badge *ngFor="let badge of userBadges"
                                   title="{{badge.title}}"
                                   color="{{badge.color}}"
                                   shape="{{badge.shape}}"
                                   [image]="badge.image"
                                   description="{{badge.description}}"
                                   message="{{badge.message}}"
                                   (click)="selectBadge(badge)">
                        </tim-badge>
                    </div>
                </ng-container>
        
        
        
        <!--         Delete button, only shown when a badge is selected -->
            <div *ngIf="showDeleteButton">
              <button (click)="removeBadge(selectedBadge?.badgegiven_id)">Delete</button>
            </div>
        
            <div class="groups">
                <label for="select_group">Group</label>
                <select id="select_group" [(ngModel)]="selectedGroup">¨
                    <option [ngValue]="null" disabled selected>Select a group</option>
                    <option *ngFor="let group of groups" [ngValue]="group" (click)="fetchGroupBadges(group.id)">{{ group.name }}</option>
                </select>
            </div>
            
            <ng-container *ngIf="groupBadges.length > 0">
                <div class="group_badges">
                    <tim-badge *ngFor="let badge of groupBadges"
                               title="{{badge.title}}"
                               color="{{badge.color}}"
                               shape="{{badge.shape}}"
                               [image]="badge.image"
                               description="{{badge.description}}"
                               message="{{badge.message}}"
                               (click)="selectBadge(badge)">
                    </tim-badge>
                </div>
            </ng-container>
        
            <div class="form-group">
                <label for="badge_to_assign">Badge to Assign</label>
                <select id="badge_to_assign" [(ngModel)]="selectedBadge">¨
                    <option [ngValue]="null" disabled selected>Select a badge</option>
                    <option *ngFor="let badge of badges" [ngValue]="badge">{{ badge.title }}</option>
                </select>
            </div>
        
            <!-- Preview of the selected badge -->
            <div *ngIf="selectedBadge" class="badge-preview">
                <label for="selected-badge-preview">Selected Badge Preview</label>
                <div id="selected-badge-preview">
                    <tim-badge *ngIf="selectedBadge"
                           title="{{ selectedBadge!.title  }}"
                           color="{{ selectedBadge!.color }}"
                           shape="{{ selectedBadge!.shape }}"
                           [image]="selectedBadge!.image"
                           description="{{ selectedBadge!.description }}"
                           message="{{ message }}">
                </tim-badge>
                </div>
            </div>
        
            <div class="form-group">
                <label for="message">Message</label>
                <textarea id="message" rows="3" [(ngModel)]="message" placeholder="Enter a message..."></textarea>
            </div>
        
            <div class="button-container">
                <button id="assignButton" (click)="assignBadge(message)" [disabled]="selectedUser && selectedGroup || !selectedGroup && !selectedUser || !selectedBadge">
                    Give Badge
                </button>
                <button id="cancelButton" (click)="emptyForm()" [disabled]="!selectedUser && !selectedBadge && !message">Cancel</button>
            </div>
        </div>
    </ng-container>
    <ng-template #noPermissionView>
      <p>Access denied for students.</p>
    </ng-template>
    `,
    styleUrls: ["./badge-giver.component.scss"],
})
export class BadgeGiverComponent implements OnInit {
    private subscription: Subscription = new Subscription();

    users: IUser[] = [];
    badges: any = [];
    selectedUser?: IUser | null = null;
    userBadges: IBadge[] = [];
    selectedBadge?: IBadge | null = null;
    message = "";
    badgeGiver = 0;
    showDeleteButton: boolean = false;
    hasPermission: boolean = false;
    @Input() badgegroupContext?: string;
    groups: IGroup[] = [];
    selectedGroup?: IGroup | null = null;
    groupBadges: IBadge[] = [];

    constructor(private http: HttpClient, private badgeService: BadgeService) {}

    ngOnInit() {
        if (Users.isLoggedIn()) {
            if (Users.belongsToGroup("Administrators")) {
                this.hasPermission = true; // Tarkistetaan onko käyttäjällä oikeus käyttää komponenttia
                this.addListeners();
                this.fetchUsers();
                this.fetchGroups();
                this.fetchBadges();
            }
        }
    }

    private addListeners() {
        // Tilataan updateBadgelist-tapahtuma BadgeService:ltä
        this.subscription.add(
            this.badgeService.updateBadgeList$.subscribe(() => {
                this.fetchBadges(); // Kutsutaan fetchBadges-metodia updaten jälkeen
            })
        );

        // Subscribe to badge update events
        this.subscription.add(
            this.badgeService.updateBadgeList$.subscribe(() => {
                if (this.selectedUser?.id != undefined) {
                    this.fetchUserBadges(this.selectedUser.id); // Refresh badges
                }
                if (this.selectedGroup?.id != undefined) {
                    this.fetchGroupBadges(this.selectedGroup.id); // Refresh group badges
                }
            })
        );
    }

    emptyForm() {
        this.selectedUser = null;
        this.selectedBadge = null;
        this.message = "";
    }

    /**
     * Hakee käyttäjät, jotka kuuluvat badgegroupContext ryhmään. badgegroupContext annetaan TIM:n puolelta.
     */
    private async fetchUsers() {
        const response = toPromise(
            this.http.get<[]>("/groups/show/" + this.badgegroupContext)
        );
        const result = await response;
        if (result.ok) {
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    this.users.push(alkio);
                }
            }
        }
    }

    private async fetchGroups() {
        const response = toPromise(
            this.http.get<[]>("/subgroups/" + this.badgegroupContext)
        );
        const result = await response;
        if (result.ok) {
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    this.groups.push(alkio);
                }
            }
        }
    }

    /**
     * Kutsuu badge-servicen metodia, joka hakee kaikki badget
     */
    async fetchBadges() {
        this.badges = await this.badgeService.getAllBadges();
        console.log("näyttää kaikki badget: ", this.badges);
        console.log("Selected badge:", this.selectedBadge);
    }

    /**
     * Tarkistaa onko annettu parametri undefined. Jos true niin lähdetään pois.
     * Tyhjentää this.userBadges -taulukon
     * Kutsuu badge-servicen metodia, joka hakee käyttäjälle kuuluvat badget.
     * @param userId käyttäjän id
     */
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

    async fetchGroupBadges(groupId?: number) {
        if (groupId == undefined) {
            console.error("groupid was undefined");
            return;
        }
        while (this.groupBadges.length > 0) {
            this.groupBadges.pop();
        }
        this.groupBadges = await this.badgeService.getUserBadges(groupId);
    }

    /**
     * Antaa valitulle käyttäjälle valitun badgen
     * @param message viesti, joka antamisen yhteydessä voidaan antaa
     */
    async assignBadge(message: string) {
        if (Users.isLoggedIn()) {
            this.badgeGiver = Users.getCurrent().id;
        }
        let currentId = this.selectedUser?.id;
        if (this.selectedGroup != undefined) {
            currentId = this.selectedGroup.id;
        }

        // Show confirmation dialog before removing the badge
        const confirmed = await showConfirm(
            "Confirm badge assigning event",
            "Are you sure you want to assign this badge?"
        );

        if (!confirmed) {
            return; // Exit if user cancels the confirmation dialog
        }

        const response = toPromise(
            this.http.post<{ok: boolean}>("/give_badge", {
                given_by: this.badgeGiver,
                group_id: currentId,
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

    /**
     * Kutsuu badge-servicen metodia, joka ottaa valitun badgen pois käyttäjältä.
     * @param badgegivenID ID, jonka perustella badgesgiven taulukosta voidaan ottaa pois käyttäjälle annettu badge
     */
    async removeBadge(badgegivenID?: number) {
        this.badgeGiver = Users.getCurrent().id;
        if (badgegivenID == undefined) {
            console.error("badgegived id was undefined");
            return;
        }

        // Show confirmation dialog before removing the badge
        const confirmed = await showConfirm(
            "Confirm badge removal event",
            "Are you sure you want to remove this badge?"
        );
        if (!confirmed) {
            return; // Exit if user cancels the confirmation dialog
        }

        await this.badgeService.withdrawBadge(badgegivenID, this.badgeGiver);
        this.fetchUserBadges(this.selectedUser?.id);
        // Poistaa deletenapin näkyvistä deleten jälkeen
        this.selectedBadge = null;
        this.showDeleteButton = false;
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
