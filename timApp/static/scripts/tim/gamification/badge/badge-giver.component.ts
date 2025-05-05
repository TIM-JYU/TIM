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
                    </div>
                    <div class="list-scroll-container" (wheel)="onScrollList($event)">
                        <div *ngFor="let group of groups" class="group">
                            <span *ngIf="groupUsersMap.get(group.id)?.length">
                                <input class="user-checkbox" type="checkbox" (change)="toggleSelectAll(group, $event)">{{ groupPrettyNames.get(group.id) || group.name }}
                            </span>
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

    /**
     * Adds listeners to fetchUserBadges and fetchGroupBadges functions.
     */
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

    /**
     * When user's name is clicked, method checks if clicked user is already selected.
     * If user is selected, it sets new filtered table from selectedUsers without selected user.
     *
     * If user is not already selected, it sets it selected and calls toggleUserSelection method and fetches
     * user's badges.
     * @param user Clicked user
     */
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

    /**
     * When group's name is clicked, method checks if clicked group is already selected.
     * If group is selected, it sets new filtered table from selectedGroups without selected group.
     *
     * If group is not already selected, it sets it selected and calls toggleGroupSelection method and fetches
     * group's users.
     * @param group Clicked group
     */
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

    /**
     * Adds selected user or group to table and returns it.
     * If user / group is unchecked, function returns new table without unchecked user / group.
     *
     * @param item type T, that must contain "id" field. This can be IUser or IGroup.
     * @param selectedItems type T[], that must contain "id" field. This can be IUser[] or IGroup[]
     * @param event Allows to find out, where function is called from. If it is called from html, event is truthy.
     * Event is falsy when called from typescript.
     */
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

    /**
     * Calls toggleItemSelection function and sets returned pointer to point to this.selectedUser table.
     * @param user Selected user.
     * @param event Contains information from checkbox state.
     */
    toggleUserSelection(user: IUser, event?: Event) {
        this.selectedUsers = this.toggleItemSelection(
            user,
            this.selectedUsers,
            event
        );
    }

    /**
     * Calls toggleItemSelection function and sets returned pointer to point to this.selectedGroup table.
     * @param group Selected group.
     * @param event Contains information from checkbox state.
     */
    toggleGroupSelection(group: IGroup, event?: Event) {
        this.selectedGroups = this.toggleItemSelection(
            group,
            this.selectedGroups,
            event
        );
    }

    /**
     * Checks all boxes from group's users or unchecks all boxes from group's users.
     * @param group Group that users belong to.
     * @param event Contains information from checkbox state.
     */
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

    /**
     * Checks all boxes from users or groups.
     * Unchecks all boxes from users or groups if "Select All" checkbox is not checked.
     *
     * @param event Contains information from checkbox state.
     * @param selectedItems Selected groups or users, that can be type IUser[] or IGroup[].
     * @param itemList Group or user list, that can be type IUser[] or IGroup[].
     */
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

    /**
     * Checks if any user or group can be found from selectedUsers or selectedGroups table.
     * @param item User or group, that is type T, where field "id" must be found.
     * @param selectedItems Selected users/groups that are type T[].
     * @return true, if user/group can found from table, else false.
     */
    isSelected<T extends {id: number}>(item: T, selectedItems: T[]): boolean {
        return selectedItems.some((i) => i.id === item.id);
    }

    /**
     * Sets boolean value argument to userAssign.
     * Resets tables.
     * Fetches all users in the end.
     * @param bool boolean value, that defines user/group view.
     */
    handleSwap(bool: boolean) {
        this.userAssign = bool;
        this.selectedGroup = null;
        this.emptyTable(this.selectedUsers);
        this.emptyTable(this.selectedGroups);
        this.emptyTable(this.users);
        this.fetchUsers(this.badgegroupContext);
    }

    /**
     * Empties attributes. Method is called when pressing cancel.
     */
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
     * Finds users, that belongs to badgegroupContext group. badgegroupContext is given from TIM.
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

    /**
     * Sets every group's ID and users to groupUsersMap.
     * Calls filterUsers method.
     */
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
     * Resets this.userBadges and checks if selectedUser is falsy.
     * Gets user's PersonalGroup, that allows to use user's correct ID to fetch badges.
     * Sets getUserBadges function's returned pointer to point to this.userBadges.
     * @param selectedUser Selected user, whose badges are fetched.
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

    /**
     * Checks if groupId or badgegroupContext is falsy.
     * Resets badges.
     * Sets getUserBadges function returned pointer to point to this.groupBadges.
     * @param groupId Selected group's ID, which is used to fetch group's badges.
     */
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
     * Assigns selected badge to selected groups or users.
     * Calls emptyForm method for a reset.
     * @param message optional message, that can be given when assigning badge.
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
     * Resets table from argument
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
