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
                    <div>
                        <input class="user-checkbox" type="checkbox" (change)="toggleSelectAllItems($event, selectedUsers, users)">Select all
                        users
                    </div>
                    <div class="list-scroll-container" (wheel)="onScrollList($event)">
                        <div *ngFor="let group of groups">
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
                    <div>
                        <input class="user-checkbox" type="checkbox" (change)="toggleSelectAllItems($event, selectedGroups, groups)">Select all
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
                            <div class="group-users" *ngIf="selectedGroup === group">
                                <div *ngFor="let user of users">
                                    <div class="user-name">{{ user.real_name }}</div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>

                <div *ngIf="userAssign != undefined">
                    <div class="message-container">

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

                        <!-- Assigned Badges -->
                        <!-- Group -->
                        <div class="badges-box" *ngIf="userAssign === false">
                            <label for="user_badges">
                                Assigned Badges {{ selectedGroup ? selectedGroup.name : '' }}
                            </label>
                            <ng-container *ngIf="groupBadges.length > 0 && selectedGroup">
                                <div class="user_badges_scroll">
                                    <div class="badge-card" *ngFor="let badge of groupBadges">
                                        <tim-badge
                                                [title]="badge.title"
                                                [color]="badge.color"
                                                [shape]="badge.shape"
                                                [image]="badge.image">
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
                                                    [image]="badge.image">
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

                    <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true"
                               (closing)="badgeService.closeAlert(this.alerts, i)">
                        <div [innerHTML]="alert.msg"></div>
                    </tim-alert>

                    <div class="button-container">
                        <button (click)="assignBadge(message)"
                                [disabled]="selectedUsers.length === 0 && selectedGroups.length === 0">
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
            for (const group of this.groups) {
                const prettyName = await this.badgeService.getCurrentGroup(
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
                await this.badgeService.getUsersFromGroup(group.name)
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
        this.emptyTable(this.userBadges);
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
                    await this.badgeService.getUserAndPersonalGroup(user.name);
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
