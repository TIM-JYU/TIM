import type {OnInit} from "@angular/core";
import {EventEmitter, Output} from "@angular/core";
import {Input} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {BadgeModule} from "tim/gamification/badge/badge.component";
import type {
    IBadge,
    IGroup,
    IUser,
} from "tim/gamification/badge/badge.interface";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {Users} from "tim/user/userService";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {Subscription} from "rxjs";
import answer from "../../../../../modules/jsrunner/server/routes/answer";
import {TimUtilityModule} from "tim/ui/tim-utility.module";

@Component({
    selector: "tim-badge-withdraw",
    template: `
        <ng-container *ngIf="hasPermission; else noPermissionView">
            <div *ngIf="showComponent" class="badge-giver">
                <h2>Badge Withdraw</h2>

                <div class="user-selection">
                    <label for="select-user">User</label>
                    <select id="select-user" [(ngModel)]="selectedUser">
                        <option [ngValue]="null" disabled selected>Select an user to withdraw badge</option>
                        <option *ngFor="let user of users" [ngValue]="user" (click)="fetchUserBadges(user.id)">
                            {{ user.real_name }}
                        </option>
                    </select>
                </div>
                
                <ng-container *ngIf="userBadges.length == 0 && selectedUser">
                    <p>No assigned badges</p>
                </ng-container>

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
                                       [preventDialog]="true"
                                       (click)="selectBadge(badge, false, false)">
                            </tim-badge>
                        </div>
                    </div>
                </ng-container>

                <div class="groups">
                    <label for="select_group">Group</label>
                    <select id="select_group" [(ngModel)]="selectedGroup">
                        <option [ngValue]="null" disabled selected>Select a group to withdraw a badge</option>
                        <option *ngFor="let group of groups" [ngValue]="group"
                                (click)="fetchGroupBadges(group.id)">{{ group.name }}
                        </option>
                    </select>
                </div>
                
                <ng-container *ngIf="groupBadges.length == 0 && selectedGroup">
                    <p>No assigned badges</p>
                </ng-container>

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
                                       [preventDialog]="true"
                                       (click)="selectBadge(badge, true, false)">
                            </tim-badge>
                        </div>
                    </div>
                </ng-container>
                
                <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                    <div [innerHTML]="alert.msg"></div>
                </tim-alert>

                <div class="button-container">
                    <button id="assignButton" (click)="removeBadge(selectedBadge?.badgegiven_id)"
                            [disabled]="selectedUser && selectedGroup || !selectedGroup && !selectedUser || !selectedBadge">
                        Withdraw Badge
                    </button>
                    <button id="cancelButton" (click)="emptyForm()">Cancel</button>
                </div>
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

    users: IUser[] = [];
    badges: any = [];
    selectedUser?: IUser | null = null;
    userBadges: IBadge[] = [];
    @Input() selectedBadge?: IBadge | null = null;
    badgeGiver = 0;
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

    @Output() cancelEvent = new EventEmitter<void>();
    constructor(
        private http: HttpClient,
        protected badgeService: BadgeService
    ) {}

    onScroll(event: WheelEvent) {
        const targetElement = event.currentTarget as HTMLElement;
        const scrollAmount = event.deltaY * 0.5;
        targetElement.scrollLeft += scrollAmount;
        event.preventDefault();
    }

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
        this.showComponent = false;
        this.userBadges = [];
        this.groupBadges = [];
        this.cancelEvent.emit();
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
        //console.log("näyttää kaikki badget: ", this.badges);
        //console.log("Selected badge:", this.selectedBadge);
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
        if (this.selectedGroup) {
            this.selectedBadge = null;
        }
        this.selectedGroup = null;
        this.groupBadges = [];

        while (this.userBadges.length > 0) {
            this.userBadges.pop();
        }

        let currentId = this.selectedUser?.id;
        if (!this.selectedUser) {
            return;
        }
        const userAndPersonalGroup =
            await this.badgeService.getUserAndPersonalGroup(
                this.selectedUser.name
            );
        if (userAndPersonalGroup) {
            currentId = userAndPersonalGroup[1].id;
        } else {
            console.error("Failed to retrieve the user's personal group ID.");
        }
        if (!currentId) {
            return;
        }
        this.userBadges = await this.badgeService.getUserBadges(currentId);
    }

    async fetchGroupBadges(groupId?: number) {
        if (groupId == undefined) {
            console.error("groupid was undefined");
            return;
        }

        if (this.selectedUser) {
            this.selectedBadge = null;
        }
        // Reset user selection and hide badges
        this.selectedUser = null;
        this.userBadges = [];

        while (this.groupBadges.length > 0) {
            this.groupBadges.pop();
        }
        this.groupBadges = await this.badgeService.getUserBadges(groupId);
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
        await this.badgeService
            .withdrawBadge(
                badgegivenID,
                this.badgeGiver,
                this.badgegroupContext
            )
            .then((r) => {
                if (!r.ok) {
                    this.badgeService.showError(
                        this.alerts,
                        {data: {error: r.data?.result.error.error || ""}},
                        "danger"
                    );
                }
            });
        this.fetchUserBadges(this.selectedUser?.id);
        // Poistaa deletenapin näkyvistä deleten jälkeen
        this.selectedBadge = null;
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
