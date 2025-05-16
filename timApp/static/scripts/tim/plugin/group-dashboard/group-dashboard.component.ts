import type {OnInit} from "@angular/core";
import {Component, Input, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {manageglobals} from "tim/util/globals";
import {NameChangerModule} from "tim/plugin/group-dashboard/name-changer.component";
import type {IBadge} from "tim/gamification/badge/badge.interface";
import {BadgeModule} from "tim/gamification/badge/badge.component";
import {GroupService} from "tim/plugin/group-dashboard/group.service";

@Component({
    selector: "tim-group-dashboard",
    template: `
        <ng-container>
            <div class="tim-dashboard">
    <h1 class="name-header">
    {{ displayName }}'s <ng-container i18n>dashboard</ng-container>
    <span *ngIf="nameJustUpdated" class="name-updated-icon">✔️</span>
</h1>

    <div class="dashboard-section">
        <h2 i18n class="section-title">Group details</h2>
        <h3>{{displayName}}'s <ng-container i18n>badges</ng-container></h3>
        <div class="group-badge-area">
            <div i18n *ngIf="groupBadges.length === 0">No group badges yet.</div>
    <span *ngFor="let badge of groupBadges" class="badge">
        <tim-badge class
            [title]="badge.title"
            [color]="badge.color"
            [shape]="badge.shape"
            [image]="badge.image"
            [description]="badge.description"
            [message]="badge.message">
        </tim-badge>
    </span>
</div>
        <h3 i18n>Statistics</h3>
<div class="stat-summary">
    <p><ng-container i18n>Total members: </ng-container><strong>{{ totalMembers }}</strong></p>
    <p><ng-container i18n>Total badges (group + user): </ng-container><strong>{{ totalBadges }}</strong></p>
</div>

<div class="stat-visuals">
    <p><em>Graphs TBA</em></p>
</div>
    </div>

    <div class="dashboard-section">
        <h2 i18n class="section-title">Members</h2>
        <div class="member-list">
            <div class="member-card" *ngFor="let member of members">
                <div class="member-info">
                    <span class="member-name">{{ member.real_name }}</span>
                    <div class="member-badge-area">
                        <span *ngFor="let badge of member.badges" class="badge">
                            <tim-badge
                                title="{{badge.title}}"
                                color="{{badge.color}}"
                                shape="{{badge.shape}}"
                                [image]="badge.image"
                                description="{{badge.description}}"
                                message="{{badge.message}}">
                            </tim-badge>
                        </span>
                    </div>
                </div>
            </div>
        </div>
    </div>
                </div>
</ng-container>
`,
    styleUrls: ["./group-dashboard.component.scss"],
})
export class GroupDashboardComponent implements OnInit {
    constructor(
        private groupService: GroupService,
        private badgeService: BadgeService
    ) {}

    @Input() group!: string;
    displayName: string | undefined;
    groupId: number | undefined;
    contextGroup: string | undefined;
    members: any[] = [];
    title: string | undefined;
    currentUserName: string | undefined;
    canViewAllBadges: boolean = false;
    groupBadges: IBadge[] = [];
    nameJustUpdated = false;
    totalMembers: number = 0;
    totalBadges: number = 0;

    /**
     * Triggers loading of group related data if group is provided in the user interface
     */
    ngOnInit() {
        if (this.group) {
            this.loadData();
        }
    }

    /**
     * Loads necessary data for group dashboard, including:
     * - context group
     * - group's display name and id
     * - members of the group
     * - badges for individual members of the group and group's common badges
     */
    async loadData() {
        this.contextGroup = this.groupService.getContextGroup(this.group);
        this.currentUserName = manageglobals().current_user.name;
        await this.getGroupName();
        await this.fetchMembers();
        await this.fetchUserBadges();
        await this.fetchGroupBadges();
    }

    /**
     * Fetches data for current group, including:
     * - pretty name (description)
     * - group id
     * - permissions for badge viewing
     * TODO: do badge viewing rights entirely in backend
     * @returns group data
     */
    async getGroupName() {
        const fetchedGroup = await this.groupService.getCurrentGroup(
            this.group
        );
        if (fetchedGroup) {
            this.displayName = fetchedGroup.description || "";
            this.groupId = fetchedGroup.id;
        }

        this.canViewAllBadges = fetchedGroup.edit_access || false;
    }

    /**
     * Fetches a list of users belonging to current group,
     * updates member view and member count in user interface
     */
    async fetchMembers() {
        const members = await this.groupService.getUsersFromGroup(this.group);

        if (members) {
            this.members = members;
            this.totalMembers = members.length;
        }
    }

    /**
     * Fetches personal badges for each group member.
     * Currently, a regular member of the group can only see his own badges.
     * An admin or teacher can see every member's personal badges.
     * Uses member's personal group IDs to query badges.
     * Counts total badges.
     */
    async fetchUserBadges() {
        let badgeCount = 0;
        const badgePromises = this.members.map(async (user) => {
            const isCurrentUser = user.name === this.currentUserName;

            if (!this.canViewAllBadges && !isCurrentUser) {
                return;
            }

            const personalGroup =
                await this.groupService.getUserAndPersonalGroup(user.name);

            if (personalGroup[1].id) {
                const badges = await this.badgeService.getBadges(
                    personalGroup[1].id,
                    this.contextGroup!
                );

                if (badges.length > 0) {
                    user.badges = badges;
                    badgeCount += badges.length;
                }
            } else {
                return;
            }
        });

        await Promise.all(badgePromises);
        this.totalBadges += badgeCount;
    }

    /**
     * Fetches badges that are assigned directly to the group itself.
     * Updates the group's badge list and adds to the total badge count.
     */
    async fetchGroupBadges() {
        const groupBadges = await this.badgeService.getBadges(
            this.groupId!,
            this.contextGroup!
        );
        if (groupBadges) {
            this.groupBadges = groupBadges;
            this.totalBadges += groupBadges.length;
        } else {
            return;
        }
    }
}

@NgModule({
    declarations: [GroupDashboardComponent],
    exports: [GroupDashboardComponent],
    imports: [CommonModule, NameChangerModule, BadgeModule],
})
export class GroupDashboardModule {}
