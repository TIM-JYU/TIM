import type {OnInit} from "@angular/core";
import {Input} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {HostListener, ElementRef} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {BadgeModule} from "tim/gamification/badge/badge.component";
import type {
    IBadge,
    IGroup,
    IPersonalGroup,
    IUser,
} from "tim/gamification/badge/badge.interface";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {Users} from "tim/user/userService";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {Subscription} from "rxjs";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {toPromise} from "tim/util/utils";
import {GroupService} from "tim/plugin/group-dashboard/group.service";

@Component({
    selector: "tim-badge-withdraw",
    template: `
        <ng-container *ngIf="hasPermission; else noPermissionView">
            <div class="badge-withdraw">
                <h2>View users or groups ({{badgegroupContext}})</h2>
                
                <ng-container *ngIf="!teacherPermission">
                    <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                        <div [innerHTML]="alert.msg"></div>
                    </tim-alert>
                </ng-container>
                
                <ng-container *ngIf="teacherPermission">
                    <div class="user-group-button-container">
                        <button (click)="handleSwap(true); fetchUsersFromGroups()" [disabled]="userAssign || users.length === 0" [title]="users.length === 0 && userAssign == undefined ? 'No users available' : ''">
                            Users
                        </button>
                        <button (click)="handleSwap(false)" [disabled]="userAssign === false || groups.length === 0" [title]="groups.length === 0 && userAssign == undefined ? 'No groups available' : ''">
                            Groups
                        </button>
                    </div>
                
                    <div *ngIf="showComponent">
                        <div *ngIf="userAssign" class="form-group">
                            <label>Users</label>
                            
                            <div class="search-wrapper">
                              <div class="search-groups">
                                <label for="group-search">Search user:</label>
                                <input type="search" id="group-search" name="q"
                                       [(ngModel)]="searchTerm"
                                       (ngModelChange)="onUserSearchChange()" />
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
                                            {{group.description}}
                                        </span>
                                        <div *ngFor="let user of groupUsersMap.get(group.id)" class="option-item">
                                            <span class="user-name" (click)="selectedUser = user; fetchUserBadges(user.id)" [ngClass]="{'selected-option': selectedUser?.id === user.id}">
                                                {{user.real_name}}
                                            </span>
                                        </div>
                                    </div>
                                    
                                    <span>Users with no group</span>
                                    <div *ngFor="let user of filteredUsers" class="option-item">
                                        <span class="user-name" (click)="selectedUser = user; fetchUserBadges(user.id)" [ngClass]="{'selected-option': selectedUser?.id === user.id}">
                                            {{user.real_name}}
                                        </span>
                                    </div>
                                    
                                </div>
                            </div>
                            <div class="badges-preview">
                                <ng-container *ngIf="userBadges.length > 0">
                                    <div *ngIf="selectedUser?.name != undefined">
                                        <p>{{ selectedUser?.real_name }}'s badges</p>
                                        <div class="user_badges" (wheel)="onScroll($event)">
                                            <div class="badge-card" *ngFor="let badge of userBadges">
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
                                    <p>{{selectedUser.real_name}}</p>
                                    <p>No badges assigned</p>
                                </ng-container>
                                
                                <ng-container *ngIf="!selectedUser">
                                    <p>Select user to view badges</p>
                                </ng-container>
                            </div>
                        </div>
    
                        <div *ngIf="userAssign === false" class="form-group">
                            <label>Groups</label>
                            
                            <div class="search-wrapper">
                                <div class="search-groups">
                                    <label for="group-search">Search group:</label>
                                    <input type="search" id="group-search" name="q"
                                           [(ngModel)]="groupSearchTerm"
                                           (ngModelChange)="onGroupSearchChange()" />
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
                                        <span class="group-name" (click)="selectedGroup = group; fetchGroupBadges(group.id)" [ngClass]="{'selected-option': selectedGroup?.id === group.id}">
                                            {{ group.description }}
                                        </span>
                                    </div>
                                </div>
                            </div>
                            <div class="badges-preview">
                                    
                                <ng-container *ngIf="groupBadges.length > 0">
                                    <div *ngIf="selectedGroup?.name != undefined">
                                        <p>{{ selectedGroup?.name }}</p>
                                        <div class="group_badges" (wheel)="onScroll($event)">
                                            <div class="badge-card" *ngFor="let badge of groupBadges">
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
                                    <p>{{selectedGroup.name}}</p>
                                    <p>No badges assigned</p>
                                </ng-container>
                                
                                <ng-container *ngIf="!selectedGroup">
                                    <p>Select group to view badges</p>
                                </ng-container>
                                
                            </div>
                            
                        </div>
                    
                        <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                            <div [innerHTML]="alert.msg"></div>
                        </tim-alert>
                        
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
        </ng-container>
        <ng-template #noPermissionView>
            <p>Access denied for students.</p>
        </ng-template>
    `,
    styleUrls: ["badge-withdraw.component.scss"],
})
export class BadgeWithdrawComponent implements OnInit {
    private subscription: Subscription = new Subscription();

    userAssign?: boolean = undefined;
    teacherPermission = false;
    hasPermission: boolean = true;
    showComponent: boolean = true;
    hasBadges: boolean = false;

    users: IUser[] = [];
    selectedUser?: IUser | null = null;
    filteredUsers: IUser[] = [];
    userBadges: IBadge[] = [];

    badges: any = [];
    selectedBadge?: IBadge | null = null;

    @Input() badgegroupContext?: string;

    groups: IGroup[] = [];
    selectedGroup?: IGroup | null = null;
    groupBadges: IBadge[] = [];
    groupUsersMap = new Map<number, IUser[]>();

    searchTerm = "";
    userSearchResults: IUser[] = [];
    groupSearchTerm = "";
    groupSearchResults: IGroup[] = [];

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

    onUserSearchChange() {
        const term = this.searchTerm.toLowerCase().trim();
        if (!term) {
            this.userSearchResults = [];
            return;
        }

        this.userSearchResults = this.users.filter((user) =>
            user.real_name!.toLowerCase().includes(term)
        );
    }

    onGroupSearchChange() {
        const term = this.groupSearchTerm.toLowerCase().trim();
        if (!term) {
            this.groupSearchResults = [];
            return;
        }

        this.groupSearchResults = this.groups.filter((group) =>
            group.description.toLowerCase().includes(term)
        );
    }

    selectUserFromSearch(user: IUser) {
        this.selectedUser = user;
        this.searchTerm = "";
        this.userSearchResults = [];
        this.fetchUserBadges(user.id);
    }

    selectGroupFromSearch(group: IGroup) {
        this.selectedGroup = group;
        this.groupSearchTerm = "";
        this.groupSearchResults = [];
        this.fetchGroupBadges(group.id);
    }

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

    ngOnInit() {
        if (Users.isLoggedIn()) {
            this.addListeners();
            this.fetchUsers();
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
        this.showComponent = false;
        this.userAssign = undefined;
        this.emptyTable(this.userBadges);
        this.emptyTable(this.userBadges);
        this.groupUsersMap.clear();
    }

    handleSwap(bool: boolean) {
        this.showComponent = true;
        this.selectedGroup = null;
        this.selectedUser = null;
        this.selectedBadge = null;
        this.hasBadges = false;
        this.userAssign = bool;
    }

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
        this.filteredUsers = this.users.filter(
            (user) => !groupedUserIds.has(user.id)
        );
    }
    /**
     * Hakee käyttäjät, jotka kuuluvat badgegroupContext ryhmään. badgegroupContext annetaan TIM:n puolelta.
     */
    private async fetchUsers() {
        const result = await toPromise(
            this.http.get<[]>(`/usergroups_members/${this.badgegroupContext}`)
        );
        const users: IUser[] = [];
        if (result.ok) {
            this.teacherPermission = true;
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    users.push(alkio);
                }
            }
        }
        if (!result.ok) {
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

    private async fetchGroups() {
        if (this.badgegroupContext) {
            this.groups = await this.groupService.getSubGroups(
                this.badgegroupContext
            );
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
     * Tarkistaa onko annettu parametri undefined. Jos true niin lähdetään pois.
     * Tyhjentää this.userBadges -taulukon
     * Kutsuu badge-servicen metodia, joka hakee käyttäjälle kuuluvat badget.
     * @param userId käyttäjän id
     */
    async fetchUserBadges(userId?: number) {
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
        this.userBadges = await this.badgeService.getUserBadges(
            pGroup["1"].id,
            this.badgegroupContext
        );
        this.hasBadges = this.userBadges.length > 0;
    }

    async fetchGroupBadges(groupId?: number) {
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

        this.groupBadges = await this.badgeService.getUserBadges(
            groupId,
            this.badgegroupContext
        );
        this.hasBadges = this.groupBadges.length > 0;
    }

    /**
     * Kutsuu badge-servicen metodia, joka ottaa valitun badgen pois käyttäjältä.
     * @param badgegivenID ID, jonka perustella badgesgiven taulukosta voidaan ottaa pois käyttäjälle annettu badge
     */
    async removeBadge(badgegivenID?: number) {
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
        this.badgeService
            .withdrawBadge(badgegivenID, this.badgegroupContext)
            .then((r) => {
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
    ],
    exports: [BadgeWithdrawComponent],
})
export class BadgeWithdrawModule {}
