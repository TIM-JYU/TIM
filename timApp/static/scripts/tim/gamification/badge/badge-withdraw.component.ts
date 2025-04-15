import type {OnInit} from "@angular/core";
import {Input} from "@angular/core";
import {Component, NgModule} from "@angular/core";
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
import {documentglobals} from "tim/util/globals";

@Component({
    selector: "tim-badge-withdraw",
    template: `
        <ng-container *ngIf="hasPermission; else noPermissionView">
            <div class="badge-withdraw">
                <h2>View users or groups</h2>
                
                <ng-container *ngIf="!teacherPermission">
                    <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                        <div [innerHTML]="alert.msg"></div>
                    </tim-alert>
                </ng-container>
                
                <ng-container *ngIf="teacherPermission">
                    <div class="user-group-button-container">
                        <button (click)="handleSwap(true)" [disabled]="userAssign || users.length === 0" [title]="users.length === 0 && userAssign == undefined ? 'No users available' : ''">
                            Users
                        </button>
                        <button (click)="handleSwap(false)" [disabled]="userAssign === false || groups.length === 0" [title]="groups.length === 0 && userAssign == undefined ? 'No groups available' : ''">
                            Groups
                        </button>
                    </div>
                
                    <div *ngIf="showComponent">
                        <div *ngIf="userAssign" class="form-group">
                            <label>Users</label>
                            
                            <div class="list-scroll-container" (wheel)="onScrollList($event)">
                                <div *ngFor="let user of users" class="option-item">
                                    <span class="option-name" (click)="selectedUser = user; fetchUserBadges(user.id)" [ngClass]="{'selected-option': selectedUser?.id === user.id}">
                                        {{user.real_name}}
                                    </span>
                                </div>
                            </div>
                            
                            <ng-container *ngIf="userBadges.length > 0">
                                <p *ngIf="selectedUser?.name != undefined">{{ selectedUser?.real_name }}'s badges</p>
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
                            </ng-container>
                            <ng-container *ngIf="userBadges.length === 0 && selectedUser">
                                <p> no assigned badges </p>
                            </ng-container>
                        </div>
    
                        <div *ngIf="userAssign === false" class="form-group">
                            <label>Groups</label>
                            
                            <div class="list-scroll-container" (wheel)="onScrollList($event)">
                                <div *ngFor="let group of groups" class="option-item">
                                    <span class="option-name" (click)="selectedGroup = group; fetchGroupBadges(group.id)" [ngClass]="{'selected-option': selectedGroup?.id === group.id}">
                                        {{ prettyGroupName(group.name) }}
                                    </span>
                                </div>
                            </div>
                            
                                <ng-container *ngIf="groupBadges.length > 0">
                                    <p *ngIf="selectedGroup?.name != undefined">{{ selectedGroup?.name }} badges</p>
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
                                </ng-container>
                        
                            <ng-container *ngIf="groupBadges.length === 0 && selectedGroup">
                                <p>No assigned badges</p>
                            </ng-container>
                            
                        </div>
                    
                        <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                            <div [innerHTML]="alert.msg"></div>
                        </tim-alert>
                        
                        <ng-container *ngIf="userAssign != undefined">
                            <div class="button-container">
                                    <button id="assignButton" (click)="removeBadge(selectedBadge?.badgegiven_id)" 
                                            [disabled]="isWithdrawButtonDisabled()"
                                            [title]="isWithdrawButtonDisabled() ? 'Select badge to withdraw' : ''">
                                            Withdraw Badge
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
    users: IUser[] = [];
    badges: any = [];
    selectedUser?: IUser | null = null;
    userBadges: IBadge[] = [];
    selectedBadge?: IBadge | null = null;
    hasPermission: boolean = true;
    showComponent: boolean = true;
    @Input() badgegroupContext?: string;
    groups: IGroup[] = [];
    selectedGroup?: IGroup | null = null;
    groupBadges: IBadge[] = [];
    alerts: Array<{
        msg: string;
        type: "warning" | "danger";
        id?: string;
    }> = [];

    constructor(
        private http: HttpClient,
        protected badgeService: BadgeService
    ) {}

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
    }

    handleSwap(bool: boolean) {
        this.showComponent = true;
        this.selectedBadge = null;
        this.userAssign = bool;
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
                        error:
                            result.result.error.error +
                            `. If you are a teacher of ${this.badgegroupContext}, please contact TIM admin.`,
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
            this.groups = await this.badgeService.getSubGroups(
                this.badgegroupContext
            );
        }
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

        this.selectedGroup = null;
        this.selectedBadge = null;
        this.emptyTable(this.groupBadges);
        this.emptyTable(this.userBadges);

        if (!this.selectedUser) {
            console.error("Failed to retrieve the user's personal group ID.");
            return;
        }
        if (!this.badgegroupContext) {
            console.error("Failed to retrieve the context group.");
            return;
        }
        const pGroup: IPersonalGroup =
            await this.badgeService.getUserAndPersonalGroup(
                this.selectedUser.name
            );
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

        this.selectedUser = null;
        this.selectedBadge = null;
        this.emptyTable(this.userBadges);
        this.emptyTable(this.groupBadges);

        this.groupBadges = await this.badgeService.getUserBadges(
            groupId,
            this.badgegroupContext
        );
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

    prettyGroupName(groupName: string): string {
        if (!groupName || !this.badgegroupContext) {
            return groupName;
        }

        return groupName.startsWith(this.badgegroupContext + "-")
            ? groupName.slice(this.badgegroupContext.length + 1)
            : groupName;
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
