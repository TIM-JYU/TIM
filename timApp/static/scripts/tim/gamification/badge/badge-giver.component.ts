import {ElementRef, HostListener, OnInit, ViewChild} from "@angular/core";
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
        <ng-container *ngIf="hasPermission; else noPermissionView">
            <div *ngIf="showComponent" class="badge-giver">
                <h2>Badge Giver</h2>
                
                <!-- Preview of the selected badge -->
                <div *ngIf="selectedBadge" class="badge-preview">
                    <label for="preview">Preview</label>
                    <div class="preview">
                        <tim-badge *ngIf="selectedBadge"
                                   [title]="selectedBadge!.title"
                                   [color]="selectedBadge!.color"
                                   [shape]="selectedBadge!.shape"
                                   [image]="selectedBadge!.image"
                                   [description]="selectedBadge!.description"
                                   [message]="message">
                        </tim-badge>
                    </div>
                </div>
                
                <div class="user-group-button-container">
                    <button (click)="userAssign = true; groupAssign = false">
                        Assign badge to a user
                    </button>
                    <button (click)="groupAssign = true; userAssign = false">
                        Assign badge to a group
                    </button>
                </div>
                
                <div *ngIf="userAssign" class="form-group">
                    <label for="select-user">User</label>
                    <select id="select-user" [(ngModel)]="selectedUser">
                        <option [ngValue]="null" disabled selected>Select a user to assign a badge</option>
                        <option *ngFor="let user of users" [ngValue]="user" (click)="fetchUserBadges(user.id)">
                            {{ user.real_name }}
                        </option>
                    </select>
                </div>
                
                <div *ngIf="groupAssign" class="form-group">
                    <label for="select_group">Group</label>
                    <select id="select_group" [(ngModel)]="selectedGroup">
                        <option [ngValue]="null" disabled selected>Select a group to assign a badge</option>
                        <option *ngFor="let group of groups" [ngValue]="group" (click)="fetchGroupBadges(group.id)">
                            {{ group.name }}
                        </option>
                    </select>
                </div>
                
                <div *ngIf="groupAssign || userAssign">
                    <div class="message-container">
                        
                        <!-- Message Box -->
                        <div class="message-box">
                            <label for="message">Message</label>
                            <textarea id="message" rows="6" [(ngModel)]="message" placeholder="Enter a message..."></textarea>
                        </div>
                        
                        <!-- Assigned Badges -->
                        <!-- Group -->
                        <div class="badges-box" *ngIf="groupAssign">
                            <label for="user_badges">
                                Assigned Badges {{selectedGroup ? selectedGroup.name : ''}}
                            </label>
                            <ng-container *ngIf="groupBadges.length > 0">
                                <div class="user_badges_scroll">
                                    <div class="badge-card" *ngFor="let badge of groupBadges">
                                        <tim-badge
                                                   [title]="badge.title"
                                                   [color]="badge.color"
                                                   [shape]="badge.shape"
                                                   [image]="badge.image"
                                                   [description]="badge.description"
                                                   [message]="badge.message">
                                        </tim-badge>
                                    </div>
                                </div>
                            </ng-container>
                            <ng-container *ngIf="groupBadges.length == 0">
                                <p>No assigned badges</p>
                            </ng-container>
                        </div>
                        
                        <!-- User -->
                        <div class="badges-box" *ngIf="userAssign">
                            <label for="user_badges" *ngIf="selectedUser?.name != undefined">
                                Assigned Badges {{ selectedUser?.real_name }}
                            </label>
                            <label for="user_badges" *ngIf="selectedUser?.name == undefined">
                                Assigned Badges
                            </label>
                            <div class="viewer-container">
                                <ng-container *ngIf="userBadges.length > 0">
                                <div class="user_badges_scroll">
                                    <div class="badge-card" *ngFor="let badge of userBadges">
                                        <tim-badge
                                                   title="{{badge.title}}" 
                                                   color="{{badge.color}}" 
                                                   shape="{{badge.shape}}"
                                                   [image]="badge.image"
                                                   description="{{badge.description}}"
                                                   message="{{badge.message}}">
                                        </tim-badge>
                                    </div>
                                </div>
                                </ng-container>
                                <ng-container *ngIf="userBadges.length == 0">
                                    <p>No assigned badges</p>
                                </ng-container>
                            </div>
                        </div>
                    </div>
                    
                    <div class="button-container">
                        <button (click)="assignBadge(message)" [disabled]="!selectedUser && !selectedGroup || !selectedBadge">
                            Assign Badge
                        </button>
                        <button id="cancelGiveButton" (click)="emptyForm()">Cancel</button>
                    </div>
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
    @Input() selectedBadge?: IBadge | null = null;
    message = "";
    badgeGiver = 0;
    showDeleteButton: boolean = false;
    hasPermission: boolean = true;
    showComponent: boolean = true;
    @Input() badgegroupContext?: string;
    groups: IGroup[] = [];
    selectedGroup?: IGroup | null = null;
    groupBadges: IBadge[] = [];
    userAssign: boolean = false;
    groupAssign: boolean = false;

    constructor(private http: HttpClient, private badgeService: BadgeService) {}

    ngOnInit() {
        if (Users.isLoggedIn()) {
            this.addListeners();
            this.fetchUsers();
            this.fetchGroups();
            this.fetchBadges();
            if (Users.belongsToGroup("Administrators")) {
                this.hasPermission = true; // Tarkistetaan onko käyttäjällä oikeus käyttää komponenttia
                this.showComponent = true;
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
        this.selectedGroup = null;
        this.message = "";
        this.showDeleteButton = false;
        this.showComponent = false;
        this.userBadges = [];
        this.groupBadges = [];
    }

    /**
     * Hakee käyttäjät, jotka kuuluvat badgegroupContext ryhmään. badgegroupContext annetaan TIM:n puolelta.
     */
    private async fetchUsers() {
        if (this.badgegroupContext) {
            this.users = await this.badgeService.getUsersFromGroup(
                this.badgegroupContext
            );
        }
    }

    private async fetchGroups() {
        if (this.badgegroupContext) {
            this.groups = await this.badgeService.getSubGroups(
                this.badgegroupContext
            );
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

        // Reset group selection and hide badges
        //if (this.selectedGroup) {
        //    this.selectedBadge = null;
        //}
        this.selectedGroup = null;
        this.groupBadges = [];
        this.showDeleteButton = false;

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

        //if (this.selectedUser) {
        //    this.selectedBadge = null;
        //}
        // Reset user selection and hide badges
        this.selectedUser = null;
        this.userBadges = [];
        this.showDeleteButton = false;

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
                context_group: this.badgegroupContext,
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

        if (this.badgegroupContext == undefined) {
            console.error("group_context was undefined");
            return;
        }
        await this.badgeService.withdrawBadge(
            badgegivenID,
            this.badgeGiver,
            this.badgegroupContext
        );
        this.fetchUserBadges(this.selectedUser?.id);
        // Poistaa deletenapin näkyvistä deleten jälkeen
        this.selectedBadge = null;
        this.showDeleteButton = false;
    }

    selectBadge(
        badge?: IBadge | null | undefined,
        fromGroup: boolean = false,
        fromAssignList: boolean = false
    ) {
        this.selectedBadge = badge ?? null;
        this.showDeleteButton =
            !fromAssignList && badge !== null && badge !== undefined; // Nappi näkyy vain, jos badge on valittu

        // Päivitetään badge-listat valinnasta riippuen
        if (!fromAssignList) {
            if (fromGroup) {
                this.selectedUser = null;
            } else {
                this.selectedGroup = null;
            }
        }
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
