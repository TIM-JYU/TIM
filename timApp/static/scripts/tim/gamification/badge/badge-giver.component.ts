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
import {GroupService} from "tim/plugin/group-dashboard/group.service";

@Component({
    selector: "timBadgeGiver",
    template: `
        <ng-container *ngIf="hasPermission; else noPermissionView">
            <div *ngIf="showComponent" class="badge-giver">
                <h2>Assign {{ selectedBadge?.title }} to user(s) or group(s)</h2>

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
                    <button (click)="handleSwap(true); fetchUsersFromGroups()" [disabled]="userAssign === true">
                        View users
                    </button>
                    <button (click)="handleSwap(false)" [disabled]="userAssign === false">
                        View groups
                    </button>
                </div>

                <div *ngIf="userAssign === true" class="form-group">
                    <label>Users</label>
                    
                    <div class="search-wrapper">
                      <div class="search-users">
                        <label for="user-search">Search user:</label>
                        <input type="search" id="user-search" name="q"
                               [(ngModel)]="userSearchTerm"
                               (ngModelChange)="filterUsers()" />
                      </div>
                    
                      <div *ngIf="searchingUsers" class="search-results">
                        <div *ngFor="let user of searchResults" class="option-item">
                          <input type="checkbox"
                                 [checked]="isSelected(user, selectedUsers)"
                                 (change)="toggleUserSelection(user, $event)" />
                          <span class="searched-name">{{ user.real_name }}</span>
                        </div>
                      </div>
                    </div>
                    
                    <div>
                        <input class="user-checkbox" type="checkbox" (change)="toggleSelectAllItems($event, selectedUsers, users)">Select all
                        users
                    </div>
                    <div class="list-scroll-container" (wheel)="onScrollList($event)">
                        <div *ngFor="let group of groups" class="group">
                            <span *ngIf="groupUsersMap.get(group.id)?.length">
                                {{ groupPrettyNames.get(group.id) || group.name }}
                            </span>
                            <div *ngIf="groupUsersMap.get(group.id)?.length">
                                <input class="user-checkbox" type="checkbox" (change)="toggleSelectAll(group, $event)">Select
                                all
                            </div>
                            <div *ngFor="let user of groupUsersMap.get(group.id)" class="option-item">
                                <input class="user-checkbox"
                                       type="checkbox"
                                       [value]="user"
                                       (change)="toggleUserSelection(user, $event)"
                                       [checked]="isSelected(user, selectedUsers)"
                                />
                                <div class="option-name" (click)="handleUserSelection(user)"
                                     [ngClass]="{'selected-option': selectedUser?.id === user.id}">
                                    {{ user.real_name }}
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
                
               
                <div *ngIf="userAssign === false" class="form-group">
                    <label>Groups</label>
                    
                    <div class="search-wrapper">
                      <div class="search-groups">
                        <label for="group-search">Search group:</label>
                        <input type="search" id="group-search" name="q"
                               [(ngModel)]="groupSearchTerm"
                               (ngModelChange)="filterGroups()" />
                      </div>
                    
                      <div *ngIf="searchingGroups" class="search-results">
                          <div *ngFor="let group of filteredGroups" class="option-item">
                            <input type="checkbox"
                                   [checked]="isSelected(group, selectedGroups)"
                                   (change)="toggleGroupSelection(group, $event)" />
                            <span class="searched-name">{{ groupPrettyNames.get(group.id) || group.name }}</span>
                          </div>
                      </div>
                        
                    </div>
                    
                    <div>
                        <input class="group-checkbox" type="checkbox" (change)="toggleSelectAllItems($event, selectedGroups, groups)">Select all 
                        groups
                    </div>
                    
                    <div class="list-scroll-container" (wheel)="onScrollList($event)">
                        <div *ngFor="let group of groups" class="group-item">
                            <input class="group-checkbox"
                                   type="checkbox"
                                   [value]="group"
                                   (change)="toggleGroupSelection(group, $event)"
                                   [checked]="isSelected(group, selectedGroups)"
                            />
                            <span class="option-name" (click)="handleGroupSelection(group); fetchGroupBadges(group.id);"
                                  [ngClass]="{'selected-option': selectedGroup?.id === group.id}">
                                {{ groupPrettyNames.get(group.id) || group.name }}
                            </span>
                        </div>
                    </div>
            
                     <div class="group-users">
                        <ng-container *ngIf="selectedGroup">
                            <p>{{selectedGroup.name}}</p>
                            <div *ngFor="let user of users">
                                <div class="user-name">{{ user.real_name }}</div>
                            </div>
                        </ng-container>
                        <ng-container *ngIf="selectedGroup && users.length === 0">
                            <p class="user-name">No users found</p>
                        </ng-container>
                        <ng-container *ngIf="!selectedGroup">
                            <p>Select group to view users</p>
                        </ng-container>
                     </div>
                    
                </div>
                <div *ngIf="userAssign != undefined">
                   
                    <!-- Message Box -->
                    <div class="message-box">
                        <label for="message">Message</label>
                        <textarea id="message" 
                                  rows="4" 
                                  maxlength="200"
                                  [(ngModel)]="message">
                        </textarea>
                        <div class="char-counter">
                          {{ message.length }} / 200 characters
                        </div>
                    </div>
                    
                    <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true"
                               (closing)="badgeService.closeAlert(this.alerts, i)">
                        <div [innerHTML]="alert.msg"></div>
                    </tim-alert>
    
                    <div class="button-container">
                        <button (click)="assignBadge(message)"
                                [disabled]="selectedUsers.length === 0 && selectedGroups.length === 0">
                            Assign
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
    groupUsersMap = new Map<number, IUser[]>();
    groupPrettyNames: Map<number, string> = new Map();

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

    userSearchTerm: string = "";
    filteredUsersByGroup = new Map<number, IUser[]>();
    searchingUsers = false;
    groupSearchTerm: string = "";
    searchingGroups = false;
    filteredGroups: IGroup[] = [];

    constructor(
        private http: HttpClient,
        protected badgeService: BadgeService,
        private groupService: GroupService
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

    filterUsers() {
        this.filteredUsersByGroup.clear();

        const searchTerm = this.userSearchTerm.trim().toLowerCase();
        this.searchingUsers = searchTerm.length > 0;

        for (const [groupId, users] of this.groupUsersMap.entries()) {
            const filtered = users.filter((user) =>
                user.real_name!.toLowerCase().includes(searchTerm)
            );
            if (filtered.length > 0 || searchTerm === "") {
                this.filteredUsersByGroup.set(groupId, filtered);
            }
        }
    }

    filterGroups() {
        const searchTerm = this.groupSearchTerm.trim().toLowerCase();
        this.searchingGroups = searchTerm.length > 0;

        if (searchTerm === "") {
            this.filteredGroups = [...this.groups];
        } else {
            this.filteredGroups = this.groups.filter(
                (group) =>
                    group.name.toLowerCase().includes(searchTerm) ||
                    this.groupPrettyNames
                        .get(group.id)
                        ?.toLowerCase()
                        .includes(searchTerm)
            );
        }
    }

    get searchResults(): IUser[] {
        const seen = new Map<number, IUser>();
        for (const users of this.filteredUsersByGroup.values()) {
            for (const user of users) {
                if (!seen.has(user.id)) {
                    seen.set(user.id, user);
                }
            }
        }
        return Array.from(seen.values());
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

    handleUserSelection(user: IUser) {
        if (this.selectedUser === user) {
            this.selectedUser = null;
            this.selectedUsers = this.selectedUsers.filter(
                (u) => u.id !== user.id
            );
            return;
        }
        this.selectedUser = user;
        this.toggleUserSelection(user);
        this.fetchUserBadges(user);
    }

    handleGroupSelection(group: IGroup) {
        this.selectedUser = null;
        if (this.selectedGroup === group) {
            this.selectedGroup = null;
            this.selectedGroups = this.selectedGroups.filter(
                (g) => g.id !== group.id
            );
            return;
        }
        this.selectedGroup = group;
        this.toggleGroupSelection(group);
        this.fetchUsers(group.name);
    }

    toggleItemSelection<T extends {id: number}>(
        item: T,
        selectedItems: T[],
        event?: Event
    ): T[] {
        if (!event) {
            const isUserSelected = selectedItems.includes(item);
            if (!isUserSelected) {
                selectedItems.push(item);
            }
            return selectedItems;
        }
        const isChecked = (event.target as HTMLInputElement).checked;
        if (isChecked) {
            selectedItems.push(item);
            return selectedItems;
        }
        return selectedItems.filter((u) => u.id !== item.id);
    }

    toggleUserSelection(user: IUser, event?: Event) {
        this.selectedUsers = this.toggleItemSelection(
            user,
            this.selectedUsers,
            event
        );
    }
    toggleGroupSelection(group: IGroup, event?: Event) {
        this.selectedGroups = this.toggleItemSelection(
            group,
            this.selectedGroups,
            event
        );
    }

    toggleSelectAll(group: IGroup, event: Event) {
        const isChecked = (event.target as HTMLInputElement).checked;
        const groupUsers = this.groupUsersMap.get(group.id);
        if (groupUsers === undefined) {
            return;
        }
        if (isChecked) {
            for (const user of groupUsers) {
                const hasSameUser = this.selectedUsers.some(
                    (u) => u.id === user.id
                );
                if (!hasSameUser) {
                    this.selectedUsers.push(user);
                }
            }
            return;
        }

        for (const user of groupUsers) {
            this.selectedUsers = this.selectedUsers.filter(
                (u) => u.id !== user.id
            );
        }
    }

    toggleSelectAllItems<T>(
        event: Event,
        selectedItems: T[],
        itemList: T[]
    ): void {
        const isChecked = (event.target as HTMLInputElement).checked;
        if (isChecked) {
            this.emptyTable(selectedItems);
            for (const item of itemList) {
                selectedItems.push(item);
            }
            return;
        }
        this.emptyTable(selectedItems);
    }

    isSelected<Valinta extends {id: number}>(
        item: Valinta,
        selectedItems: Valinta[]
    ): boolean {
        return selectedItems.some((i) => i.id === item.id);
    }

    handleSwap(bool: boolean) {
        this.userAssign = bool;
        this.selectedGroup = null;
        this.emptyTable(this.selectedUsers);
        this.emptyTable(this.selectedGroups);
        this.emptyTable(this.users);
        this.fetchUsers(this.badgegroupContext);
    }

    emptyForm() {
        this.selectedUser = null;
        this.selectedBadge = null;
        this.selectedGroup = null;
        this.message = "";
        this.showComponent = false;
        this.emptyTable(this.userBadges);
        this.emptyTable(this.groupBadges);
        this.emptyTable(this.selectedUsers);
        this.emptyTable(this.selectedGroups);
        this.groupUsersMap.clear();
        this.cancelEvent.emit(); // Lähettää tiedon vanhemmalle
    }

    /**
     * Hakee käyttäjät, jotka kuuluvat badgegroupContext ryhmään. badgegroupContext annetaan TIM:n puolelta.
     */
    async fetchUsers(groupContext?: string) {
        if (groupContext) {
            this.users = await this.groupService.getUsersFromGroup(
                groupContext
            );
        }
    }

    async fetchGroups() {
        if (this.badgegroupContext) {
            this.groups = await this.groupService.getSubGroups(
                this.badgegroupContext
            );
            for (const group of this.groups) {
                const prettyName = await this.groupService.getCurrentGroup(
                    group.name
                );
                if (prettyName) {
                    this.groupPrettyNames.set(group.id, prettyName.description);
                }
            }
        }
    }

    async fetchUsersFromGroups() {
        this.groupUsersMap.clear();
        for (const group of this.groups) {
            this.groupUsersMap.set(
                group.id,
                await this.groupService.getUsersFromGroup(group.name)
            );
        }
        this.filterUsers();
    }
    /**
     * Tarkistaa onko annettu parametri undefined. Jos true niin lähdetään pois.
     * Tyhjentää this.userBadges -taulukon
     * Kutsuu badge-servicen metodia, joka hakee käyttäjälle kuuluvat badget.
     * @param selectedUser valittu käyttäjä
     *
     */
    async fetchUserBadges(selectedUser: IUser) {
        this.emptyTable(this.userBadges);
        if (!selectedUser) {
            console.error("Selected user was undefined");
            return;
        }
        const pGroup: IPersonalGroup =
            await this.groupService.getUserAndPersonalGroup(selectedUser.name);
        if (!pGroup) {
            console.error("Failed to retrieve the user's personal group ID.");
            return;
        }
        if (!this.badgegroupContext) {
            console.error("Failed to retrieve the context group.");
            return;
        }
        this.userBadges = await this.badgeService.getUserBadges(
            pGroup["1"].id,
            this.badgegroupContext
        );
    }

    async fetchGroupBadges(groupId?: number) {
        if (groupId == undefined) {
            console.error("groupid was undefined");
            return;
        }
        if (!this.badgegroupContext) {
            console.error("Failed to retrieve the context group.");
            return;
        }
        this.emptyTable(this.groupBadges);
        this.groupBadges = await this.badgeService.getUserBadges(
            groupId,
            this.badgegroupContext
        );
    }

    /**
     * Antaa valitulle käyttäjälle valitun badgen
     * @param message viesti, joka antamisen yhteydessä voidaan antaa
     */
    async assignBadge(message: string) {
        if (this.selectedUsers.length > 0) {
            for (const user of this.selectedUsers) {
                const pGroup: IPersonalGroup =
                    await this.groupService.getUserAndPersonalGroup(user.name);
                await this.badgeService.assignBadges({
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
                    context_group: this.badgegroupContext,
                    group_id: group.id,
                    badge_id: this.selectedBadge?.id,
                    message: message,
                });
            }
        }
        this.emptyForm();
    }

    // Bring pretty group names (descriptions)
    prettyGroupNames() {}

    onScrollList(event: WheelEvent) {
        const element = event.currentTarget as HTMLElement;
        const scrollable = element.scrollHeight > element.clientHeight;
        if (scrollable) {
            const targetElement = event.currentTarget as HTMLElement;
            const scrollAmount = event.deltaY * 0.5;
            targetElement.scrollTop += scrollAmount;
            event.preventDefault();
        }
    }
    /**
     * Tyhjentää attribuuttina annetun taulukon
     */
    emptyTable<T>(table: T[]) {
        while (table.length > 0) {
            table.pop();
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
