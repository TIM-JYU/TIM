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

@Component({
    selector: "tim-badge-selected-withdraw",
    template: `
        <ng-container *ngIf="hasPermission; else noPermissionView">
            <div *ngIf="showComponent" class="badge-giver">
                <h2>Withdraw selected badge</h2>

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
                        Withdraw badge from a user
                    </button>
                    <button (click)="handleSwap(false)" [disabled]="userAssign === false">
                        Withdraw badge from a group
                    </button>
                </div>

                <div *ngIf="userAssign === true" class="form-group">
                    <label>Users</label>
<!--                    <div *ngFor="let group of groups">-->
<!--                        <span class="option-name" (click)="handleGroupSelection(group)"-->
<!--                              [ngClass]="{'selected-option': selectedGroup?.id === group.id}">-->
<!--                            {{ prettyGroupName(group.name) }}-->
<!--                        </span>-->
<!--                        <div *ngIf="groupUsersMap.get(group.id)?.length">-->
<!--                            <input class="selectall-checkbox" type="checkbox" (change)="toggleSelectAll(group, $event)">Select-->
<!--                            all-->
<!--                        </div>-->
                        <div *ngFor="let user of users" class="option-item">
                            <input class="user-checkbox"
                                   type="checkbox"
                                   [value]="user"

                                   (change)="toggleUserSelection(user, $event)"
                            />
                            <div class="option-name" (click)="handleUserSelection(user)"
                                 [ngClass]="{'selected-option': selectedUser?.id === user.id}">
                                {{ user.real_name }}
                            </div>
                        </div>
<!--                    </div>-->
                </div>

                <div *ngIf="userAssign === false" class="form-group">
                    <label>Groups</label>
                    <div *ngFor="let group of groups" class="group-item">
                        <input class="group-checkbox"
                               type="checkbox"
                               [value]="group"
                               (change)="toggleGroupSelection(group, $event)"
                        />
                        <span class="option-name" (click)="handleGroupSelection(group)"
                              [ngClass]="{'selected-option': selectedGroup?.id === group.id}">
                            {{ prettyGroupName(group.name) }}
                        </span>
                        <div class="group-users" *ngIf="selectedGroup === group">
                            <div *ngFor="let user of users">
                                <div class="user-name">{{ user.real_name }}</div>
                            </div>
                        </div>
                    </div>
                </div>
                
                <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true"
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
        <ng-template #noPermissionView>
            <p>Access denied for students.</p>
        </ng-template>
    `,
    styleUrls: ["./badge-giver.component.scss"],
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
                await this.badgeService.getUsersFromGroup(group.name)
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
                    await this.badgeService.getUserAndPersonalGroup(user.name);

                await this.badgeService
                    .withdrawSelectedBadge(
                        pGroup["1"].id,
                        this.selectedBadge?.id,
                        Users.getCurrent().id,
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
                        Users.getCurrent().id,
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
    emptyTable(taulukko: any[]) {
        while (taulukko.length > 0) {
            taulukko.pop();
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
