import type {OnInit} from "@angular/core";
import {
    ElementRef,
    EventEmitter,
    HostListener,
    Output,
    ViewChild,
} from "@angular/core";
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
import {documentglobals} from "tim/util/globals";
import {TimUtilityModule} from "tim/ui/tim-utility.module";

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
                                   [message]="message"
                                   [preventDialog]="true">
                        </tim-badge>
                    </div>
                </div>
                
                <div class="user-group-button-container">
                    <button (click)="userAssign = true; groupAssign = false; handleSwap()" [disabled]="userAssign === true">
                        Assign badge to a user
                    </button>
                    <button (click)="userAssign = false; groupAssign = true; handleSwap()" [disabled]="userAssign === false">
                        Assign badge to a group
                    </button>
                </div>
                
                <div *ngIf="userAssign" class="form-group">
                    <label>Users</label>
                    <div *ngFor="let user of users" class="option-item">
                        <input 
                            type="checkbox" 
                            [value]="user" 
                            (change)="toggleUserSelection(user, $event)"
                        />
                        <span class="option-name" (click)="selectedUser = user; fetchUserBadges(user)" [ngClass]="{'selected-option': selectedUser?.id === user.id}">
                            {{user.real_name}}
                        </span>
                    </div>
                </div>
                
                <div *ngIf="groupAssign" class="form-group">
                    <label>Groups</label>
                    <div *ngFor="let group of groups" class="option-item">
                        <input 
                            type="checkbox" 
                            [value]="group" 
                            (change)="toggleGroupSelection(group, $event)"
                        />
                        <span class="option-name" (click)="selectedGroup = group; fetchGroupBadges(group.id)" [ngClass]="{'selected-option': selectedGroup?.id === group.id}">
                            {{ prettyGroupName(group.name) }}
                        </span>
                    </div>
                </div>
                
                <div *ngIf="userAssign || groupAssign">
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
                            <ng-container *ngIf="groupBadges.length > 0 && selectedGroup">
                                <div class="user_badges_scroll">
                                    <div class="badge-card" *ngFor="let badge of groupBadges">
                                        <tim-badge
                                                   [title]="badge.title"
                                                   [color]="badge.color"
                                                   [shape]="badge.shape"
                                                   [image]="badge.image"
                                                   [description]="badge.description"
                                                   [message]="badge.message"
                                                   [preventDialog]="true">
                                        </tim-badge>
                                    </div>
                                </div>
                            </ng-container>
                            <ng-container *ngIf="groupBadges.length == 0 && selectedGroup">
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
                                <ng-container *ngIf="userBadges.length > 0 && selectedUser">
                                <div class="user_badges_scroll">
                                    <div class="badge-card" *ngFor="let badge of userBadges">
                                        <tim-badge
                                                   title="{{badge.title}}" 
                                                   color="{{badge.color}}" 
                                                   shape="{{badge.shape}}"
                                                   [image]="badge.image"
                                                   description="{{badge.description}}"
                                                   message="{{badge.message}}"
                                                   [preventDialog]="true">
                                        </tim-badge>
                                    </div>
                                </div>
                                </ng-container>
                                <ng-container *ngIf="selectedUser && userBadges.length == 0">
                                    <p>No assigned badges</p>
                                </ng-container>
                            </div>
                        </div>
                    </div>
                    
                    <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                        <div [innerHTML]="alert.msg"></div>
                    </tim-alert>
                    
                    <div class="button-container">
                        <button (click)="assignBadge(message)" [disabled]="selectedUsers.length === 0 && selectedGroups.length === 0">
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

    currentDocumentID = documentglobals().curr_item.id;
    users: IUser[] = [];
    badges: any = [];
    selectedUser?: IUser | null = null;
    selectedUsers: IUser[] = [];
    userBadges: IBadge[] = [];
    @Input() selectedBadge?: IBadge | null = null;
    message = "";
    showDeleteButton: boolean = false;
    hasPermission: boolean = true;
    showComponent: boolean = true;
    @Input() badgegroupContext?: string;
    groups: IGroup[] = [];
    selectedGroup?: IGroup | null = null;
    selectedGroups: IGroup[] = [];
    groupBadges: IBadge[] = [];
    userAssign?: boolean;
    groupAssign: boolean = false;
    badgeGiver = 0;
    currentId = null;
    alerts: Array<{
        msg: string;
        type: "warning" | "danger";
        id?: string;
    }> = [];

    @Output() cancelEvent = new EventEmitter<void>();
    private userAndPersonalGroup: unknown;
    private userName: string | undefined;

    constructor(
        private http: HttpClient,
        protected badgeService: BadgeService
    ) {}

    ngOnInit() {
        if (Users.isLoggedIn()) {
            this.userName = Users.getCurrent().name;
            this.addListeners();
            this.fetchUsers();
            this.fetchGroups();
            this.fetchBadges();
            this.fetchPersonalGroup();
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
                    this.fetchUserBadges(this.selectedUser); // Refresh badges
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
        this.cancelEvent.emit(); // Lähettää tiedon vanhemmalle
    }

    toggleUserSelection(user: IUser, event: Event) {
        const isChecked = (event.target as HTMLInputElement).checked;
        if (isChecked) {
            this.selectedUsers.push(user);
            return;
        }
        this.selectedUsers = this.selectedUsers.filter((u) => u.id !== user.id);
    }
    toggleGroupSelection(group: IGroup, event: Event) {
        const isChecked = (event.target as HTMLInputElement).checked;
        if (isChecked) {
            this.selectedGroups.push(group);
            return;
        }
        this.selectedGroups = this.selectedGroups.filter(
            (g) => g.id !== group.id
        );
    }

    handleSwap() {
        while (this.selectedUsers.length > 0) {
            this.selectedUsers.pop();
        }
        while (this.selectedGroups.length > 0) {
            this.selectedGroups.pop();
        }
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
        // console.log("näyttää kaikki badget: ", this.badges);
        // console.log("Selected badge:", this.selectedBadge);
    }

    /**
     * Tarkistaa onko annettu parametri undefined. Jos true niin lähdetään pois.
     * Tyhjentää this.userBadges -taulukon
     * Kutsuu badge-servicen metodia, joka hakee käyttäjälle kuuluvat badget.
     * @param selectedUser valittu käyttäjä
     *
     */
    async fetchUserBadges(selectedUser: IUser) {
        this.selectedGroup = null;
        this.groupBadges = [];
        this.showDeleteButton = false;

        while (this.userBadges.length > 0) {
            this.userBadges.pop();
        }
        if (!selectedUser) {
            console.error("Selected user was undefined");
            return;
        }
        const userAndPersonalGroup =
            await this.badgeService.getUserAndPersonalGroup(selectedUser.name);
        if (!userAndPersonalGroup) {
            console.error("Failed to retrieve the user's personal group ID.");
            return;
        }
        this.userBadges = await this.badgeService.getUserBadges(
            userAndPersonalGroup[1].id
        );
    }

    async fetchGroupBadges(groupId?: number) {
        if (groupId == undefined) {
            console.error("groupid was undefined");
            return;
        }

        this.selectedUser = null;
        this.userBadges = [];
        this.showDeleteButton = false;

        while (this.groupBadges.length > 0) {
            this.groupBadges.pop();
        }
        this.groupBadges = await this.badgeService.getUserBadges(groupId);
    }

    async fetchPersonalGroup() {
        this.userAndPersonalGroup =
            await this.badgeService.getUserAndPersonalGroup(this.userName);
        if (this.userAndPersonalGroup) {
            console.log("User and personal group:", this.userAndPersonalGroup);
        }
    }

    /**
     * Antaa valitulle käyttäjälle valitun badgen
     * @param message viesti, joka antamisen yhteydessä voidaan antaa
     */
    async assignBadge(message: string) {
        if (Users.isLoggedIn()) {
            this.badgeGiver = Users.getCurrent().id;
        }

        if (this.selectedUsers.length > 0) {
            for (const user of this.selectedUsers) {
                const userAndPersonalGroup =
                    await this.badgeService.getUserAndPersonalGroup(user.name);
                await this.badgeService.assignBadges({
                    given_by: this.badgeGiver,
                    doc_id: this.currentDocumentID,
                    context_group: this.badgegroupContext,
                    group_id: userAndPersonalGroup[1].id,
                    badge_id: this.selectedBadge?.id,
                    message: message,
                });
            }
        }
        if (this.selectedGroups.length > 0) {
            for (const group of this.selectedGroups) {
                await this.badgeService.assignBadges({
                    given_by: this.badgeGiver,
                    doc_id: this.currentDocumentID,
                    context_group: this.badgegroupContext,
                    group_id: group.id,
                    badge_id: this.selectedBadge?.id,
                    message: message,
                });
            }
        }
        this.emptyForm();
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

    // Removes context group (main group) from the group's name in group listing
    prettyGroupName(groupName: string): string {
        if (!groupName || !this.badgegroupContext) return groupName;

        return groupName.startsWith(this.badgegroupContext + "-")
            ? groupName.slice(this.badgegroupContext.length + 1)
            : groupName;
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
    imports: [CommonModule, FormsModule, BadgeModule, TimUtilityModule],
})
export class BadgeGiverModule {}
