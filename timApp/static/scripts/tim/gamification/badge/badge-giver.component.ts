import type {OnInit} from "@angular/core";
import {EventEmitter, Output} from "@angular/core";
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
    IPersonalGroup,
    IUser,
} from "tim/gamification/badge/badge.interface";
import {BadgeModule} from "tim/gamification/badge/badge.component";
import {BadgeService} from "tim/gamification/badge/badge.service";
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
                                   [message]="message">
                        </tim-badge>
                    </div>
                </div>
                
                <div class="user-group-button-container">
                    <button (click)="handleSwap(true)" [disabled]="userAssign === true">
                        Assign badge to a user
                    </button>
                    <button (click)="handleSwap(false)" [disabled]="userAssign === false">
                        Assign badge to a group
                    </button>
                </div>
                
                <div *ngIf="userAssign === true" class="form-group">
                    <label>Users</label>
                    <div *ngFor="let group of groups" class="option-item">
                        <span class="option-name" (click)="handleGroupSelection(group)" [ngClass]="{'selected-option': selectedGroup?.id === group.id}">
                            {{ prettyGroupName(group.name) }}
                        </span>
                        <div class="group-users" *ngIf="selectedGroup === group">
                            <div *ngFor="let user of users" class="option-item">
                                <input class="user-checkbox"
                                    type="checkbox" 
                                    [value]="user" 
                                    [checked]="isUserSelected(user)" 
                                    (change)="toggleUserSelection(user, $event)"
                                />
                                <div class="option-name" (click)="handleUserSelection(user)" [ngClass]="{'selected-option': selectedUser?.id === user.id}">
                                    {{user.real_name}}
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
                
                <div *ngIf="userAssign === false" class="form-group">
                    <label>Groups</label>
                    <div *ngFor="let group of groups" class="group-item">
                        <input class="group-checkbox"
                            type="checkbox" 
                            [value]="group" 
                            (change)="toggleGroupSelection(group, $event)"
                        />
                        <span class="option-name" (click)="handleGroupSelection(group); fetchGroupBadges(group.id);" [ngClass]="{'selected-option': selectedGroup?.id === group.id}">
                            {{ prettyGroupName(group.name) }}
                        </span>
                        <div class="group-users" *ngIf="selectedGroup === group">
                            <div *ngFor="let user of users">
                                <div class="user-name">{{user.real_name}}</div>
                            </div>
                        </div>
                    </div>
                </div>
                
                <div *ngIf="userAssign != undefined">
                    <div class="message-container">
                        
                        <!-- Message Box -->
                        <div class="message-box">
                            <label for="message">Message</label>
                            <textarea id="message" rows="6" [(ngModel)]="message" placeholder="Enter a message..."></textarea>
                        </div>
                        
                        <!-- Assigned Badges -->
                        <!-- Group -->
                        <div class="badges-box" *ngIf="userAssign === false">
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
                                                   [message]="badge.message">
                                        </tim-badge>
                                    </div>
                                </div>
                            </ng-container>
                            <ng-container *ngIf="groupBadges.length == 0 && selectedGroup">
                                <p>No assigned badges</p>
                            </ng-container>
                        </div>
                        
                        <!-- User -->
                        <div class="badges-box" *ngIf="userAssign === true">
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
                                                   message="{{badge.message}}">
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
    hasPermission: boolean = true;
    showComponent: boolean = true;
    message = "";

    users: IUser[] = [];
    selectedUser?: IUser | null = null;
    selectedUsers: IUser[] = [];
    userBadges: IBadge[] = [];

    groups: IGroup[] = [];
    selectedGroup?: IGroup | null = null;
    selectedGroups: IGroup[] = [];
    groupBadges: IBadge[] = [];

    @Input() selectedBadge?: IBadge | null = null;
    @Input() badgegroupContext?: string;

    userAssign?: boolean = undefined;

    alerts: Array<{
        msg: string;
        type: "warning" | "danger";
        id?: string;
    }> = [];
    private subscription: Subscription = new Subscription();
    @Output() cancelEvent = new EventEmitter<void>();

    constructor(
        private http: HttpClient,
        protected badgeService: BadgeService
    ) {}

    ngOnInit() {
        if (Users.isLoggedIn()) {
            this.addListeners();
            this.fetchUsers(this.badgegroupContext);
            this.fetchGroups();
            if (Users.belongsToGroup("Administrators")) {
                this.hasPermission = true; // Tarkistetaan onko käyttäjällä oikeus käyttää komponenttia
                this.showComponent = true;
            }
        }
    }

    private addListeners() {
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

    handleSwap(bool: boolean) {
        this.userAssign = bool;
        this.selectedGroup = null;
        while (this.selectedUsers.length > 0) {
            this.selectedUsers.pop();
        }
        while (this.selectedGroups.length > 0) {
            this.selectedGroups.pop();
        }
    }

    isUserSelected(user: IUser) {
        if (this.selectedUser === user) {
            return true;
        }
    }
    handleUserSelection(user: IUser) {
        if (this.selectedUser === user) {
            this.selectedUser = null;
        }
        this.selectedUser = user;
        this.fetchUserBadges(user);
    }

    handleGroupSelection(group: IGroup) {
        this.selectedUser = null;
        if (this.selectedGroup === group) {
            this.selectedGroup = null;
            return;
        }
        this.selectedGroup = group;
        this.fetchUsers(group.name);
    }

    /**
     * Hakee käyttäjät, jotka kuuluvat badgegroupContext ryhmään. badgegroupContext annetaan TIM:n puolelta.
     */
    async fetchUsers(groupContext?: string) {
        if (groupContext) {
            this.users = await this.badgeService.getUsersFromGroup(
                groupContext
            );
        }
    }

    async fetchGroups() {
        if (this.badgegroupContext) {
            this.groups = await this.badgeService.getSubGroups(
                this.badgegroupContext
            );
        }
    }

    /**
     * Tarkistaa onko annettu parametri undefined. Jos true niin lähdetään pois.
     * Tyhjentää this.userBadges -taulukon
     * Kutsuu badge-servicen metodia, joka hakee käyttäjälle kuuluvat badget.
     * @param selectedUser valittu käyttäjä
     *
     */
    async fetchUserBadges(selectedUser: IUser) {
        this.emptyBadges(this.userBadges);
        if (!selectedUser) {
            console.error("Selected user was undefined");
            return;
        }
        const pGroup: IPersonalGroup =
            await this.badgeService.getUserAndPersonalGroup(selectedUser.name);
        if (!pGroup) {
            console.error("Failed to retrieve the user's personal group ID.");
            return;
        }
        this.userBadges = await this.badgeService.getUserBadges(pGroup["1"].id);
    }

    async fetchGroupBadges(groupId?: number) {
        if (groupId == undefined) {
            console.error("groupid was undefined");
            return;
        }
        this.emptyBadges(this.groupBadges);
        this.groupBadges = await this.badgeService.getUserBadges(groupId);
    }

    /**
     * Antaa valitulle käyttäjälle valitun badgen
     * @param message viesti, joka antamisen yhteydessä voidaan antaa
     */
    async assignBadge(message: string) {
        let givenByID = 0;
        const currentDocumentID = documentglobals().curr_item.id;
        if (Users.isLoggedIn()) {
            givenByID = Users.getCurrent().id;
        }

        if (this.selectedUsers.length > 0) {
            for (const user of this.selectedUsers) {
                const pGroup: IPersonalGroup =
                    await this.badgeService.getUserAndPersonalGroup(user.name);
                await this.badgeService.assignBadges({
                    given_by: givenByID,
                    doc_id: currentDocumentID,
                    context_group: this.badgegroupContext,
                    group_id: pGroup["1"].id,
                    badge_id: this.selectedBadge?.id,
                    message: message,
                });
            }
        }
        if (this.selectedGroups.length > 0) {
            for (const group of this.selectedGroups) {
                await this.badgeService.assignBadges({
                    given_by: givenByID,
                    doc_id: currentDocumentID,
                    context_group: this.badgegroupContext,
                    group_id: group.id,
                    badge_id: this.selectedBadge?.id,
                    message: message,
                });
            }
        }
        this.emptyForm();
    }

    // Removes context group (main group) from the group's name in group listing
    prettyGroupName(groupName: string): string {
        if (!groupName || !this.badgegroupContext) {
            return groupName;
        }

        return groupName.startsWith(this.badgegroupContext + "-")
            ? groupName.slice(this.badgegroupContext.length + 1)
            : groupName;
    }

    /**
     * Tyhjentää attribuuttina annetun taulukon
     */
    emptyBadges(badges: IBadge[]) {
        while (badges.length > 0) {
            badges.pop();
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
    imports: [CommonModule, FormsModule, BadgeModule, TimUtilityModule],
})
export class BadgeGiverModule {}
