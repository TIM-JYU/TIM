import {Component, Input, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {manageglobals} from "tim/util/globals";
import {IFolder, IFullDocument} from "tim/item/IItem";
import {GroupNameModule} from "tim/plugin/group-dashboard/group-name.component";
import {IBadge, IUser} from "tim/gamification/badge/badge.interface";
import {BadgeModule} from "tim/gamification/badge/badge.component";
import {GroupService} from "tim/plugin/group-dashboard/group.service";
import {PointService} from "tim/plugin/group-dashboard/point.service";
import {Subscription} from "rxjs";
import {UserService} from "tim/user/userService";

@Component({
    selector: "tim-group-dashboard",
    template: `
        <ng-container>
            <div class="tim-dashboard">
    <h1 class="name-header">
    {{ displayName }}'s dashboard
    <span *ngIf="nameJustUpdated" class="name-updated-icon">✔️</span>
</h1>

    <div class="dashboard-section">
        <h2 class="section-title">Group details</h2>
        <h3>{{displayName}}'s badges</h3>
        <div class="group-badges">
            <div *ngIf="groupBadges.length === 0">No group badges yet.</div>
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
        <h3>Statistics</h3>
<div class="stat-summary">
    <p><strong>Total members:</strong> {{ totalMembers }}</p>
    <p><strong>Total badges (group + user):</strong> {{ totalBadges }}</p>
    <p><strong>Total points: {{totalPoints}}</strong></p>
</div>

<div class="stat-visuals">
    <p><em>Graphs TBA</em></p>
</div>
    </div>

    <div class="dashboard-section">
        <h2 class="section-title">Members</h2>
        <div class="member-list">
            <div class="member-card" *ngFor="let member of members">
                <div class="member-info">
                    <span class="member-name">{{ member.real_name }}</span>
                    <div class="badges">
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
        private badgeService: BadgeService,
        private pointService: PointService
    ) {}

    @Input() group!: string;
    private item: IFullDocument | IFolder | undefined;
    private pointSub?: Subscription;
    totalPoints: number = 0;
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

    //TODO: scrollbar ja skaalaus, kun badgeja on paljon
    //TODO: group badgejen noutaminen ei-jäsenille
    ngOnInit() {
        if (this.group) {
            this.loadData();
        }
    }

    async loadData() {
        this.contextGroup = this.groupService.getContextGroup(this.group);
        this.currentUserName = manageglobals().current_user.name;
        await this.getGroupName();
        await this.fetchMembers();
        await this.fetchUserBadges();
        await this.fetchGroupBadges();
        this.refreshTotalPoints();

        this.item = manageglobals().curr_item;
        this.pointSub = this.pointService.pointsUpdated$.subscribe(() => {
            this.refreshTotalPoints();
        });
    }

    async getGroupName() {
        const fetchedGroup = await this.groupService.getCurrentGroup(
            this.group
        );
        if (fetchedGroup) {
            this.displayName = fetchedGroup.description || "";
            this.groupId = fetchedGroup.id;
        }

        this.canViewAllBadges =
            fetchedGroup.isTeacher || fetchedGroup.isAdmin || false;
    }

    async fetchMembers() {
        const members = await this.groupService.getUsersFromGroup(this.group);

        if (members) {
            this.members = members;
            this.totalMembers = members.length;
        }
    }

    async fetchUserBadges() {
        let badgeCount = 0;
        const badgePromises = this.members.map(async (user) => {
            const isCurrentUser = user.name === this.currentUserName;

            if (!this.canViewAllBadges && !isCurrentUser) {
                return;
            }

            try {
                const personalGroup =
                    await this.groupService.getUserAndPersonalGroup(user.name);

                if (personalGroup[1].id) {
                    const badges = await this.badgeService.getUserBadges(
                        personalGroup[1].id,
                        this.contextGroup!
                    );

                    if (badges.length > 0) {
                        user.badges = badges;
                        badgeCount += badges.length;
                    }
                } else {
                    console.error(
                        `No personal group ID found for ${user.name}`
                    );
                }
            } catch (error) {
                console.error(`Error fetching badges for ${user.name}:`, error);
            }
        });

        await Promise.all(badgePromises);
        this.totalBadges += badgeCount;
    }

    async fetchGroupBadges() {
        try {
            const groupBadges = await this.badgeService.getUserBadges(
                this.groupId!,
                this.contextGroup!
            );
            if (groupBadges) {
                this.groupBadges = groupBadges;
                this.totalBadges += groupBadges.length;
            }
        } catch (error) {
            console.error("Error fetching group mutual badges:", error);
        }
    }

    private refreshTotalPoints() {
        this.totalPoints = this.pointService.getTotalPoints(this.group);
    }

    ngOnDestroy(): void {
        this.pointSub?.unsubscribe();
    }
}

@NgModule({
    declarations: [GroupDashboardComponent],
    exports: [GroupDashboardComponent],
    imports: [CommonModule, GroupNameModule, BadgeModule],
})
export class GroupDashboardModule {}
