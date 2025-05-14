import type {OnInit} from "@angular/core";
import {ViewChild} from "@angular/core";
import {Input} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {HostListener, ElementRef} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {BadgeModule} from "tim/gamification/badge/badge.component";
import type {
    IBadge,
    IBadgeGroup,
    IPersonalGroup,
} from "tim/gamification/badge/badge.interface";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {Users} from "tim/user/userService";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {Subscription} from "rxjs";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {toPromise} from "tim/util/utils";
import {GroupService} from "tim/plugin/group-dashboard/group.service";
import {scrollToElement} from "tim/util/utils";
import type {IUser} from "tim/user/IUser";
import {PurifyModule} from "tim/util/purify.module";

@Component({
    selector: "tim-badge-withdraw",
    template: `

        <div class="badge-withdraw" #startSection>
            <h2>View users or groups ({{ badgegroupContext }})</h2>

            <ng-container *ngIf="!hasPermissionToHandleBadges">
                <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true"
                           (closing)="badgeService.closeAlert(this.alerts, i)">
                    <div [innerHTML]="alert.msg | purify"></div>
                </tim-alert>
            </ng-container>

            <ng-container *ngIf="hasPermissionToHandleBadges">
                <div class="user-group-button-container">
                    <button (click)="handleSwap(true); fetchUsersFromGroups()"
                            [disabled]="userAssign || users.length === 0"
                            [title]="users.length === 0 && userAssign == undefined ? 'No users available' : ''">
                        Users
                    </button>
                    <button (click)="handleSwap(false)" [disabled]="userAssign === false || groups.length === 0"
                            [title]="groups.length === 0 && userAssign == undefined ? 'No groups available' : ''">
                        Groups
                    </button>
                </div>

                <div *ngIf="showSelection">
                    <div *ngIf="userAssign" class="form-group">
                        <div class="search-wrapper">
                            <div class="search-groups">
                                <label for="group-search">Search user:</label>
                                <input type="search" id="group-search" name="q"
                                       [(ngModel)]="searchTerm"
                                       (ngModelChange)="onUserSearchChange()"/>
                            </div>

                            <div *ngIf="userSearchResults.length > 0" class="search-results">
                                <div *ngFor="let user of userSearchResults"
                                     class="option-item"
                                     [ngClass]="{'selected-option': selectedUser?.id === user.id}"
                                     (click)="selectUserFromSearch(user)">
                                    <span class="searched-name">{{ user.real_name }}</span>
                                </div>
                            </div>
                        </div>

                        <div class="users-list">
                            <div class="list-scroll-container" (wheel)="onScrollList($event)">

                                <div *ngFor="let group of groups" class="group">
                                        <span *ngIf="groupUsersMap.get(group.id)?.length">
                                            {{ group.description }}
                                        </span>
                                    <div *ngFor="let user of groupUsersMap.get(group.id)" class="option-item">
                                            <span class="user-name"
                                                  (click)="selectedUser = user; fetchUserBadges(user.id)"
                                                  [ngClass]="{'selected-option': selectedUser?.id === user.id}">
                                                {{ user.real_name }}
                                            </span>
                                    </div>
                                </div>

                                <span>Users without group</span>
                                <div *ngFor="let user of usersWithoutGroup" class="option-item">
                                        <span class="user-name" (click)="selectedUser = user; fetchUserBadges(user.id)"
                                              [ngClass]="{'selected-option': selectedUser?.id === user.id}">
                                            {{ user.real_name }}
                                        </span>
                                </div>

                            </div>
                        </div>
                        <div class="badges-preview">
                            <ng-container *ngIf="userBadges.length > 0">
                                <div *ngIf="selectedUser?.name != undefined">
                                    <p>{{ selectedUser?.real_name }}'s badges</p>
                                    <div class="user_badges">
                                        <div class="card" *ngFor="let badge of userBadges">
                                            <tim-badge
                                                    [ngClass]="{'selected-badge': selectedBadge === badge}"
                                                    title="{{badge.title}}"
                                                    color="{{badge.color}}"
                                                    shape="{{badge.shape}}"
                                                    [image]="badge.image"
                                                    (click)="selectBadge(badge, false, false)">
                                            </tim-badge>
                                        </div>
                                    </div>
                                </div>
                            </ng-container>
                            <ng-container *ngIf="!hasBadges && selectedUser">
                                <p>{{ selectedUser!.real_name }}</p>
                                <p>No badges assigned</p>
                            </ng-container>

                            <ng-container *ngIf="!selectedUser">
                                <p>Select user to view badges</p>
                            </ng-container>
                        </div>
                    </div>

                    <div *ngIf="userAssign === false" class="form-group">
                        <div class="search-wrapper">
                            <div class="search-groups">
                                <label for="group-search">Search group:</label>
                                <input type="search" id="group-search" name="q"
                                       [(ngModel)]="groupSearchTerm"
                                       (ngModelChange)="onGroupSearchChange()"/>
                            </div>

                            <div *ngIf="groupSearchResults.length > 0" class="search-results">
                                <div *ngFor="let group of groupSearchResults"
                                     class="option-item"
                                     [ngClass]="{'selected-option': selectedGroup?.id === group.id}"
                                     (click)="selectGroupFromSearch(group)">
                                    <span class="searched-name">{{ group.description }}</span>
                                </div>
                            </div>
                        </div>

                        <div class="users-list">
                            <div class="list-scroll-container" (wheel)="onScrollList($event)">
                                <div *ngFor="let group of groups" class="option-item">
                                        <span class="group-name"
                                              (click)="selectedGroup = group; fetchGroupBadges(group.id)"
                                              [ngClass]="{'selected-option': selectedGroup?.id === group.id}">
                                            {{ group.description }}
                                        </span>
                                </div>
                            </div>
                        </div>
                        <div class="badges-preview">

                            <ng-container *ngIf="groupBadges.length > 0">
                                <div *ngIf="selectedGroup?.name != undefined">
                                    <p>{{ selectedGroup?.description }}</p>
                                    <div class="group_badges">
                                        <div class="card" *ngFor="let badge of groupBadges">
                                            <tim-badge
                                                    [ngClass]="{'selected-badge': selectedBadge === badge}"
                                                    title="{{badge.title}}"
                                                    color="{{badge.color}}"
                                                    shape="{{badge.shape}}"
                                                    [image]="badge.image"
                                                    (click)="selectBadge(badge, true, false)">
                                            </tim-badge>
                                        </div>
                                    </div>
                                </div>
                            </ng-container>

                            <ng-container *ngIf="!hasBadges && selectedGroup">
                                <p>{{ selectedGroup!.name }}</p>
                                <p>No badges assigned</p>
                            </ng-container>

                            <ng-container *ngIf="!selectedGroup">
                                <p>Select group to view badges</p>
                            </ng-container>

                        </div>

                    </div>

                    <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type"
                               [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                        <div [innerHTML]="alert.msg | purify"></div>
                    </tim-alert>


                    <ng-container *ngIf="selectedBadge">
                        <h5>Details of the selected badge</h5>
                        <p class="word-wrap">
                            Title: {{ selectedBadge!.title || "No title" }} <br>
                            Description: {{ selectedBadge!.description || "No description" }} <br>
                            Message: {{ selectedBadge!.message || "No message" }} <br>
                            Given By: {{ selectedBadge!.given_by_name || "No name" }} <br>
                            Given Time: {{ selectedBadge!.given || "No date" }} <br>
                        </p>
                    </ng-container>

                    <ng-container *ngIf="userAssign != undefined">
                        <div class="button-container">
                            <button id="assignButton" (click)="removeBadge(selectedBadge?.badgegiven_id)"
                                    [disabled]="isWithdrawButtonDisabled()"
                                    [title]="isWithdrawButtonDisabled() ? 'Select badge to withdraw' : ''">
                                Withdraw
                            </button>
                            <button id="cancelButton" (click)="emptyForm()">Cancel</button>
                        </div>
                    </ng-container>
                </div>
            </ng-container>
        </div>
    `,
    styleUrls: ["badge-withdraw.component.scss"],
})
export class BadgeWithdrawComponent implements OnInit {
    private subscription: Subscription = new Subscription();

    userAssign?: boolean = undefined;
    hasPermissionToHandleBadges = true;
    showSelection: boolean = true;
    hasBadges: boolean = false;

    users: IUser[] = [];
    selectedUser?: IUser | null = null;
    usersWithoutGroup: IUser[] = [];
    userBadges: IBadge[] = [];

    badges: any = [];
    selectedBadge?: IBadge | null = null;

    @Input() badgegroupContext?: string;
    @ViewChild("startSection") startSection!: ElementRef;

    groups: IBadgeGroup[] = [];
    selectedGroup?: IBadgeGroup | null = null;
    groupBadges: IBadge[] = [];
    groupUsersMap = new Map<number, IUser[]>();

    searchTerm = "";
    userSearchResults: IUser[] = [];
    groupSearchTerm = "";
    groupSearchResults: IBadgeGroup[] = [];

    alerts: Array<{
        msg: string;
        type: "warning" | "danger";
        id?: string;
    }> = [];

    constructor(
        private http: HttpClient,
        protected badgeService: BadgeService,
        private groupService: GroupService,
        private elRef: ElementRef
    ) {}

    ngOnInit() {
        if (Users.isLoggedIn()) {
            this.subscribeToBadgeUpdates();
            this.fetchUsers();
            this.fetchGroups();
        }
    }

    /**
     * Performs a search on the list of users using the current search term.
     * Updates `userSearchResults` with users whose `real_name` matches the search pattern.
     */
    onUserSearchChange() {
        const term = this.searchTerm.toLowerCase().trim();
        if (!term) {
            this.userSearchResults = [];
            return;
        }

        try {
            const regex = new RegExp(term, "i");
            this.userSearchResults = this.users.filter((user) =>
                regex.test(user.real_name ?? "")
            );
        } catch (e) {
            this.userSearchResults = [];
        }
    }

    /**
     * Performs a search on the groups array using the `groupSearchTerm` field.
     * Updates `groupSearchResults` with groups whose `description` matches the search term.
     */
    onGroupSearchChange() {
        const term = this.groupSearchTerm.toLowerCase().trim();
        if (!term) {
            this.groupSearchResults = [];
            return;
        }

        try {
            const regex = new RegExp(term, "i");
            this.groupSearchResults = this.groups.filter((group) =>
                regex.test(group.description ?? "")
            );
        } catch (e) {
            this.groupSearchResults = [];
        }
    }

    /**
     * Selects a user from the search results, clears the search input,
     * and fetches badges associated with the selected user.
     * @param user The selected user
     */
    selectUserFromSearch(user: IUser) {
        this.selectedUser = user;
        this.searchTerm = "";
        this.userSearchResults = [];
        this.fetchUserBadges(user.id);
    }

    /**
     * Selects a group from the search results, clears the search input,
     * and fetches badges associated with the selected group.
     * @param group The selected group
     */
    selectGroupFromSearch(group: IBadgeGroup) {
        this.selectedGroup = group;
        this.groupSearchTerm = "";
        this.groupSearchResults = [];
        this.fetchGroupBadges(group.id);
    }

    /**
     * Closes the user search results dropdown when clicking outside the search area.
     * @param event The mouse click event
     */
    @HostListener("document:click", ["$event"])
    onDocumentClick(event: MouseEvent): void {
        const searchResults =
            this.elRef.nativeElement.querySelector(".search-results");
        const searchWrapper =
            this.elRef.nativeElement.querySelector(".search-wrapper");

        if (
            searchResults &&
            searchWrapper &&
            !searchWrapper.contains(event.target)
        ) {
            this.userSearchResults = [];
        }
    }

    /**
     * Function is triggered when a wheel scroll event occurs.
     * Checks if the element can actually scroll horizontally.
     * Apply custom scroll logic if the element can scroll.
     * @param event Browser's scroll-wheel event
     */
    onScroll(event: WheelEvent) {
        const element = event.currentTarget as HTMLElement;
        const scrollable = element.scrollWidth > element.clientWidth;
        if (scrollable) {
            const targetElement = event.currentTarget as HTMLElement;
            const scrollAmount = event.deltaY * 0.5;
            targetElement.scrollLeft += scrollAmount;
            event.preventDefault();
        }
    }

    /**
     * Calls onScrollList method via badgeService.
     * @param event Browser's scroll-wheel event.
     */
    onScrollList(event: WheelEvent) {
        this.badgeService.onScrollList(event);
    }

    /**
     * Adds listeners to fetchUserBadges and fetchGroupBadges functions.
     * Subscriptions allow badges to refresh live after changes.
     */
    private subscribeToBadgeUpdates() {
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

    /**
     * Resets attributes. Method is called when pressing cancel.
     */
    emptyForm() {
        this.selectedUser = null;
        this.selectedBadge = null;
        this.selectedGroup = null;
        this.showSelection = false;
        this.userAssign = undefined;
        this.emptyTable(this.userBadges);
        this.emptyTable(this.userBadges);
        this.groupUsersMap.clear();
        setTimeout(() => {
            scrollToElement(this.startSection?.nativeElement);
        }, 100);
    }

    /**
     * Resets attributes.
     * Sets boolean value from argument to userAssign.
     * @param bool boolean value, that defines user/group view.
     */
    handleSwap(bool: boolean) {
        this.showSelection = true;
        this.selectedGroup = null;
        this.selectedUser = null;
        this.selectedBadge = null;
        this.hasBadges = false;
        this.userAssign = bool;
    }

    /**
     * Sets every group's ID and users to groupUsersMap.
     * Adds every user's ID's, that belong to any group to groupedUserIds table.
     * Filters every user, that does not belong to any group to usersWithoutGroup table.
     */
    async fetchUsersFromGroups() {
        this.groupUsersMap.clear();
        for (const group of this.groups) {
            this.groupUsersMap.set(
                group.id,
                await this.groupService.getUsersFromGroup(group.name)
            );
        }

        const groupedUserIds = new Set<number>(
            Array.from(this.groupUsersMap.values())
                .flat()
                .map((user) => user.id)
        );

        this.usersWithoutGroup = this.users.filter(
            (user) => !groupedUserIds.has(user.id)
        );
    }

    /**
     * Fetches users, that belong to badgegroupContext group. badgegroupContext is received from TIM.
     * If http get request returns error, showError method is called via badgeService.
     */
    private async fetchUsers() {
        const result = await toPromise(
            this.http.get<[]>(
                `/groups/usergroups_members/${this.badgegroupContext}`
            )
        );
        const users: IUser[] = [];
        if (result.ok) {
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    users.push(alkio);
                }
            }
        }
        if (await this.checkConnectionError()) {
            return;
        }
        if (!result.ok) {
            this.hasPermissionToHandleBadges = false;
            this.badgeService.showError(
                this.alerts,
                {
                    data: {
                        error: result.result.error.error,
                    },
                },
                "danger"
            );
            return;
        }
        this.users = users;
    }

    /**
     * Fetches groups, that belong to badgegroupContext group. badgegroupContext is received from TIM.
     * If http get request returns error, hasPermissionToHandleBadges is set to false and showError method is called via badgeService.
     *
     * **Comment updateGroups section**
     */
    private async fetchGroups() {
        if (this.badgegroupContext) {
            const result = await toPromise(
                this.http.get<[]>(`/groups/subgroups/${this.badgegroupContext}`)
            );
            const subGroups: IBadgeGroup[] = [];
            if (result.ok) {
                if (result.result != undefined) {
                    for (const alkio of result.result) {
                        subGroups.push(alkio);
                    }
                }
            }
            if (await this.checkConnectionError()) {
                return;
            }
            if (!result.ok) {
                this.hasPermissionToHandleBadges = false;
                this.badgeService.showError(
                    this.alerts,
                    {
                        data: {
                            error: result.result.error.error,
                        },
                    },
                    "danger"
                );
                return;
            }
            this.groups = subGroups;
        }

        const updatesGroups = [];
        for (const group of this.groups) {
            const prettyName = await this.groupService.getCurrentGroup(
                group.name
            );
            if (prettyName) {
                group.description = prettyName.description || group.name;
                updatesGroups.push(group);
            }
        }
        this.groups = updatesGroups;
    }

    /**
     * Checks if userId is undefined.
     * Resets selections and groupBadges & userBadges.
     * Checks if selectedUser or badgegroupContext are falsy.
     * Fetches user's personal group, which is used to fetch user's badges
     * Calls getBadges method via badge-service.
     * @param userId User's ID
     */
    async fetchUserBadges(userId?: number) {
        if (await this.checkConnectionError()) {
            this.hasBadges = false;
            return;
        }
        if (userId == undefined) {
            console.error("userid was undefined");
            this.hasBadges = false;
            return;
        }

        this.selectedGroup = null;
        this.selectedBadge = null;
        this.emptyTable(this.groupBadges);
        this.emptyTable(this.userBadges);

        if (!this.selectedUser) {
            console.error("Failed to retrieve the user's personal group ID.");
            this.hasBadges = false;
            return;
        }
        if (!this.badgegroupContext) {
            console.error("Failed to retrieve the context group.");
            this.hasBadges = false;
            return;
        }
        const pGroup: IPersonalGroup =
            await this.groupService.getUserAndPersonalGroup(
                this.selectedUser.name
            );
        this.userBadges = await this.badgeService.getBadges(
            pGroup["1"].id,
            this.badgegroupContext
        );
        this.hasBadges = this.userBadges.length > 0;
    }

    /**
     * Checks if groupId or badgegroupContext is falsy.
     * Resets badges.
     * Sets getBadges method's returned pointer to point to this.groupBadges.
     * @param groupId Selected group's ID, which is used to fetch group's badges.
     */
    async fetchGroupBadges(groupId?: number) {
        if (await this.checkConnectionError()) {
            return;
        }
        if (groupId == undefined) {
            console.error("groupid was undefined");
            this.hasBadges = false;
            return;
        }
        if (!this.badgegroupContext) {
            console.error("Failed to retrieve the context group.");
            this.hasBadges = false;
            return;
        }

        this.selectedUser = null;
        this.selectedBadge = null;
        this.emptyTable(this.userBadges);
        this.emptyTable(this.groupBadges);

        this.groupBadges = await this.badgeService.getBadges(
            groupId,
            this.badgegroupContext
        );
        this.hasBadges = this.groupBadges.length > 0;
    }

    /**
     * Calls withdrawBadge method via badge-service, that removes selected badge from user.
     * If withdrawBadge method returns error, showError method is called via badge-service.
     * @param badgegivenID ID, that is used to withdraw badge from user.
     */
    async removeBadge(badgegivenID?: number) {
        if (badgegivenID == undefined) {
            console.error("badgegivenID id was undefined");
            return;
        }

        // Show confirmation dialog before removing the badge
        const confirmed = await showConfirm(
            "Confirm badge removal event",
            "Are you sure you want to remove this badge?"
        );
        if (!confirmed) {
            this.checkConnectionError();
            return; // Exit if user cancels the confirmation dialog
        }

        if (this.badgegroupContext == undefined) {
            console.error("group_context was undefined");
            return;
        }

        this.badgeService
            .withdrawBadge(badgegivenID, this.badgegroupContext)
            .then(async (r) => {
                if (await this.checkConnectionError()) {
                    return;
                }
                if (!r.ok) {
                    this.badgeService.showError(
                        this.alerts,
                        {data: {error: r.data?.result.error.error || ""}},
                        "danger"
                    );
                }
            });
        this.selectedBadge = null;
    }

    /**
     * Checks when Withdraw button is supposed to be disabled.
     * If badge is not selected, returns true.
     * If user or group is not selected, returns true.
     * Otherwise, returns false.
     */
    isWithdrawButtonDisabled(): boolean {
        if (!this.selectedBadge) {
            return true;
        }
        if (!this.selectedUser && !this.selectedGroup) {
            return true;
        }
        return false;
    }

    selectBadge(
        badge?: IBadge | null | undefined,
        fromGroup: boolean = false,
        fromAssignList: boolean = false
    ) {
        this.selectedBadge = badge ?? null;
        // Päivitetään badge-listat valinnasta riippuen
        if (!fromAssignList) {
            if (fromGroup) {
                this.selectedUser = null;
            } else {
                this.selectedGroup = null;
            }
        }
    }

    /**
     * Tests connection with check_connection route.
     * If there is error with result, calls showError method via badge-service and returns true.
     * If no errors, returns false.
     */
    async checkConnectionError() {
        const result = await toPromise(this.http.get(`/check_connection/`));
        if (!result.ok) {
            this.badgeService.showError(
                this.alerts,
                {
                    data: {
                        error: "Unexpected error. Check your internet connection.",
                    },
                },
                "danger"
            );
            return true;
        }
        return false;
    }

    /**
     * Resets table from argument.
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
    declarations: [BadgeWithdrawComponent],
    imports: [
        CommonModule,
        FormsModule,
        HttpClientModule,
        BadgeModule,
        TimUtilityModule,
        PurifyModule,
    ],
    exports: [BadgeWithdrawComponent],
})
export class BadgeWithdrawModule {}
