import type {OnInit, SimpleChanges} from "@angular/core";
import {EventEmitter, Output} from "@angular/core";
import {Input} from "@angular/core";
import {Component, NgModule, OnChanges} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {Subscription} from "rxjs";
import {Users} from "tim/user/userService";
import {IBadge} from "tim/gamification/badge/badge.interface";
import type {
    IGroup,
    IPersonalGroup,
    IUser,
    IBadgeHolders,
} from "tim/gamification/badge/badge.interface";
import {BadgeModule} from "tim/gamification/badge/badge.component";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {documentglobals} from "tim/util/globals";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {cons} from "fp-ts/ReadonlyNonEmptyArray";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {GroupService} from "tim/plugin/group-dashboard/group.service";

@Component({
    selector: "tim-badge-selected-withdraw",
    template: `
        <ng-container *ngIf="hasPermission; else noPermissionView">
            <div *ngIf="showComponent" class="badge-giver">
                <h2>Withdraw {{ selectedBadge?.title }} from user(s) or group(s)</h2>

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

                <ng-container *ngIf="groups.length === 0 && users.length === 0">
                    <span>Selected badge is not assigned to anyone</span>
                </ng-container>

                <ng-container *ngIf="groups.length > 0 || users.length > 0">
                    <div class="user-group-button-container">
                        <button (click)="handleSwap(true); fetchUsersFromGroups()"
                                [disabled]="userAssign === true || users.length === 0">
                            View users
                        </button>
                        <button (click)="handleSwap(false)" [disabled]="userAssign === false || groups.length === 0">
                            View groups
                        </button>
                    </div>

                    <div *ngIf="userAssign === true" class="form-group">
                        <label>Users</label>
                        
                        <div class="search-wrapper">
                          <div class="search-users">
                            <label for="user-search">Search user:</label>
                            <input type="search" id="user-search" name="q"
                                   [(ngModel)]="searchTerm"
                                   (ngModelChange)="onSearchChange($event)" />
                          </div>
                        
                          <div *ngIf="searchingUsers" class="search-results">
                            <div *ngFor="let user of searchResults" class="option-item">
                              <input type="checkbox"
                                       [checked]="isChecked(user)"
                                       (change)="toggleUserSelection(user, $event)" />
                              <span class="searched-name">{{ user.real_name }}</span>
                            </div>
                          </div>
                        </div>
                        
                        <div class="list-scroll-container" (wheel)="onScrollList($event)">
                            <div *ngFor="let user of users" class="option-item">
                                <input class="user-checkbox"
                                       type="checkbox"
                                       [value]="user"
                                       [checked]="isChecked(user)"
                                       (change)="toggleUserSelection(user, $event)" />
                                <div class="option-name" (click)="handleUserSelection(user)"
                                     [ngClass]="{'selected-option': selectedUser?.id === user.id}">
                                    {{ user.real_name }}
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
                                   (ngModelChange)="onGroupSearchChange($event)" />
                          </div>
                        
                          <div *ngIf="searchingGroups" class="search-results">
                                <div *ngFor="let group of groupSearchResults" class="option-item">
                                    <input type="checkbox"
                                           [checked]="isGroupChecked(group)"
                                           (change)="toggleGroupSelection(group, $event)" />
                                    <span class="searched-name">{{ group.name }}</span>
                                </div>
                          </div>
                        </div>
                        
                        <div class="list-scroll-container" (wheel)="onScrollList($event)">
                            <div *ngFor="let group of groups" class="group-item">
                                <input class="group-checkbox"
                                       type="checkbox"
                                       [checked]="isGroupChecked(group)"
                                       (change)="toggleGroupSelection(group, $event)" />
                                <span class="option-name" (click)="handleGroupSelection(group)"
                                      [ngClass]="{'selected-option': selectedGroup?.id === group.id}">
                                    {{ group.name }}
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
                        <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type"
                                   [closeable]="true"
                                   (closing)="badgeService.closeAlert(this.alerts, i)">
                            <div [innerHTML]="alert.msg"></div>
                        </tim-alert>

                        <div class="button-container">
                            <button (click)="withdrawBadge()"
                                    [disabled]="selectedUsers.length === 0 && selectedGroups.length === 0">
                                Withdraw badge
                            </button>
                            <button id="cancelGiveButton" (click)="emptyForm()">Cancel</button>
                        </div>
                    </div>
                </ng-container>
            </div>
        </ng-container>
        <ng-template #noPermissionView>
            <p>Access denied for students.</p>
        </ng-template>
    `,
    styleUrls: ["./badge-selected-withdraw.component.scss"],
})
export class BadgeSelectedWithdrawComponent implements OnInit {
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
    isAllSelectedMap = new Map<number, boolean>();

    @Input() selectedBadge?: IBadge;
    @Input() badgegroupContext?: string;

    userAssign?: boolean = undefined;

    searchTerm: string = "";
    searchResults: IUser[] = [];
    searchingUsers: boolean = false;
    userSelectionMap = new Map<number, boolean>();
    groupSearchTerm: string = "";
    groupSearchResults: IGroup[] = [];
    searchingGroups: boolean = false;
    groupSelectionMap = new Map<number, boolean>();

    alerts: Array<{
        msg: string;
        type: "warning" | "danger";
        id?: string;
    }> = [];
    private subscription: Subscription = new Subscription();
    @Output() cancelEvent = new EventEmitter<void>();

    constructor(
        private http: HttpClient,
        protected badgeService: BadgeService,
        private groupService: GroupService
    ) {}

    ngOnInit() {
        if (Users.isLoggedIn()) {
            if (Users.belongsToGroup("Administrators")) {
                this.hasPermission = true; // Tarkistetaan onko käyttäjällä oikeus käyttää komponenttia
                this.showComponent = true;
            }
        }
    }

    ngOnChanges(changes: SimpleChanges) {
        if (changes.selectedBadge) {
            this.fetchBadgeHolders(this.selectedBadge);
        }
    }

    onSearchChange(term: string) {
        this.searchTerm = term;
        if (!term.trim()) {
            this.searchResults = [];
            this.searchingUsers = false;
            return;
        }

        this.searchingUsers = true;
        const termLower = term.toLowerCase();
        this.searchResults = this.users.filter((user) =>
            user.real_name!.toLowerCase().includes(termLower)
        );
    }

    isSelected<T extends {id: number}>(item: T, list: T[]): boolean {
        return list.some((i) => i.id === item.id);
    }

    onGroupSearchChange(term: string) {
        this.groupSearchTerm = term;
        if (!term.trim()) {
            this.groupSearchResults = [];
            this.searchingGroups = false;
            return;
        }

        this.searchingGroups = true;
        const termLower = term.toLowerCase();
        this.groupSearchResults = this.groups.filter((group) =>
            group.name.toLowerCase().includes(termLower)
        );
    }

    isGroupChecked(group: IGroup): boolean {
        return this.selectedGroups.some((g) => g.id === group.id);
    }

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

    emptyForm() {
        this.selectedUser = null;
        this.selectedBadge = undefined;
        this.selectedGroup = null;
        this.message = "";
        this.showComponent = false;
        this.emptyTable(this.userBadges);
        this.emptyTable(this.groupBadges);
        this.emptyTable(this.selectedUsers);
        this.emptyTable(this.selectedGroups);
        this.groupUsersMap.clear();
        this.isAllSelectedMap.clear();
        this.cancelEvent.emit(); // Lähettää tiedon vanhemmalle
    }

    isChecked(user: IUser): boolean {
        return this.userSelectionMap.get(user.id) ?? false;
    }

    toggleUserSelection(user: IUser, event: Event) {
        const isChecked = (event.target as HTMLInputElement).checked;

        this.userSelectionMap.set(user.id, isChecked);

        if (isChecked) {
            this.selectedUsers = [...this.selectedUsers, user];
        } else {
            this.selectedUsers = this.selectedUsers.filter(
                (u) => u.id !== user.id
            );
        }
    }

    toggleGroupSelection(group: IGroup, event: Event) {
        const isChecked = (event.target as HTMLInputElement).checked;

        if (isChecked) {
            const alreadySelected = this.selectedGroups.some(
                (g) => g.id === group.id
            );
            if (!alreadySelected) {
                this.selectedGroups.push(group);
            }
        } else {
            this.selectedGroups = this.selectedGroups.filter(
                (g) => g.id !== group.id
            );
        }
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
            this.isAllSelectedMap.set(group.id, true);
            return;
        }

        for (const user of groupUsers) {
            this.selectedUsers = this.selectedUsers.filter(
                (u) => u.id !== user.id
            );
        }
        this.isAllSelectedMap.set(group.id, false);
    }

    handleSwap(bool: boolean) {
        this.userAssign = bool;
        this.selectedGroup = null;
        this.emptyTable(this.selectedUsers);
        this.emptyTable(this.selectedGroups);
    }

    handleUserSelection(user: IUser) {
        if (this.selectedUser === user) {
            this.selectedUser = null;
            return;
        }
        this.selectedUser = user;
    }

    handleGroupSelection(group: IGroup) {
        this.selectedUser = null;
        if (this.selectedGroup === group) {
            this.selectedGroup = null;
            return;
        }
        this.selectedGroup = group;
    }

    async fetchUsersFromGroups() {
        this.groupUsersMap.clear();
        for (const group of this.groups) {
            this.groupUsersMap.set(
                group.id,
                await this.groupService.getUsersFromGroup(group.name)
            );
            this.isAllSelectedMap.set(group.id, false);
        }
    }

    async fetchBadgeHolders(selectedBadge?: IBadge) {
        if (selectedBadge == undefined) {
            console.error("selected badge was undefined");
            return;
        }
        const holders: IBadgeHolders | null =
            await this.badgeService.getBadgeHolders(selectedBadge.id);

        if (!holders) {
            return;
        }

        this.emptyTable(this.users);
        this.emptyTable(this.groups);

        for (const user of holders["0"]) {
            this.users.push(user);
        }

        for (const group of holders["1"]) {
            const prettyName = await this.groupService.getCurrentGroup(
                group.name
            );
            if (prettyName) {
                group.name = prettyName.description || group.name;
            }
            this.groups.push(group);
        }
    }

    async withdrawBadge() {
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
        if (this.selectedBadge == undefined) {
            console.error("selected badge was undefined");
            return;
        }

        if (this.selectedUsers.length > 0) {
            for (const user of this.selectedUsers) {
                const pGroup: IPersonalGroup =
                    await this.groupService.getUserAndPersonalGroup(user.name);

                await this.badgeService
                    .withdrawSelectedBadge(
                        pGroup["1"].id,
                        this.selectedBadge?.id,
                        this.badgegroupContext
                    )
                    .then((r) => {
                        if (!r.ok) {
                            this.badgeService.showError(
                                this.alerts,
                                {
                                    data: {
                                        error: r.data?.result.error.error || "",
                                    },
                                },
                                "danger"
                            );
                        }
                    });
            }
        }

        if (this.selectedGroups.length > 0) {
            for (const group of this.selectedGroups) {
                await this.badgeService
                    .withdrawSelectedBadge(
                        group.id,
                        this.selectedBadge?.id,
                        this.badgegroupContext
                    )
                    .then((r) => {
                        if (!r.ok) {
                            this.badgeService.showError(
                                this.alerts,
                                {
                                    data: {
                                        error: r.data?.result.error.error || "",
                                    },
                                },
                                "danger"
                            );
                        }
                    });
            }
        }
        this.emptyForm();
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
    declarations: [BadgeSelectedWithdrawComponent],
    exports: [BadgeSelectedWithdrawComponent],
    imports: [CommonModule, FormsModule, BadgeModule, TimUtilityModule],
})
export class BadgeSelectedWithdrawModule {}
