import type {OnInit} from "@angular/core";
import {Component, Input, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {manageglobals} from "tim/util/globals";
import {NameChangerModule} from "tim/plugin/group-dashboard/name-changer.component";
import type {IErrorAlert} from "tim/gamification/badge/badge.interface";
import type {IBadge} from "tim/gamification/badge/badge.interface";
import {BadgeModule} from "tim/gamification/badge/badge.component";
import {GroupService} from "tim/plugin/group-dashboard/group.service";
import {to2, toPromise} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import {PurifyModule} from "tim/util/purify.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import type {IGroup, IUser} from "tim/user/IUser";
import type {BadgeGroupInfo} from "tim/plugin/group-dashboard/group.service";

// FIXME: temp interfaces, get rid of these
export interface IBadgeUser extends IUser {
    badges: IBadge[];
}

@Component({
    selector: "tim-group-dashboard",
    template: `
        <ng-container>
            <div class="tim-dashboard">
                
    <h1 class="name-header">
        <span *ngIf="displayName">{{ displayName }}'s </span><ng-container i18n>dashboard</ng-container>
    <span *ngIf="nameJustUpdated" class="name-updated-icon">✔️</span>
</h1>
                
                <!-- Show alert if group is not found -->
        <div *ngIf="alerts.length > 0">
            <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type"
                [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                <div [innerHTML]="alert.msg | purify"></div>
            </tim-alert>
        </div>
    <div *ngIf="displayName && alerts.length === 0">
    <div class="dashboard-section">
        <h2 i18n class="section-title">Group details</h2>
        <h3>{{displayName}}'s <ng-container i18n>badges</ng-container></h3>
        <div class="group-badge-area">
            <div i18n *ngIf="groupBadges.length === 0">No group badges yet.</div>
            <ng-container *ngIf="groupBadges.length > 0">
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
            </ng-container>
</div>
        <h3 i18n>Statistics</h3>
<div class="stat-summary">
    <p><ng-container i18n>Total members: </ng-container><strong>{{ this.members.length }}</strong></p>
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
            </div>    
</ng-container>
`,
    styleUrls: ["./group-dashboard.component.scss"],
})
export class GroupDashboardComponent implements OnInit {
    constructor(
        private groupService: GroupService,
        protected badgeService: BadgeService,
        private http: HttpClient
    ) {}

    @Input() group!: string;
    displayName: string | undefined;
    groupId: number | undefined;
    contextGroup: string | undefined;
    members: IBadgeUser[] = [];
    title: string | undefined;
    currentUserName: string | undefined;
    canViewAllBadges: boolean = false;
    groupBadges: IBadge[] = [];
    nameJustUpdated = false;
    totalBadges: number = 0;
    alerts: Array<IErrorAlert> = [];

    /**
     * Triggers loading of group related data if group is provided in the user interface
     */
    ngOnInit() {
        if (this.group) {
            // FIXME: Since the group dashboard is aimed at the group members, there is no reason
            //  to extract the super-group from the sub-group name.
            // this.contextGroup = this.groupService.getContextGroup(this.group);
            this.contextGroup = this.group;
            this.currentUserName = manageglobals().current_user.name;

            this.getGroupName().then((_) => {
                this.getMembers(); // .then((r) => r);
                // .then((__) => {
                //     this.getUserBadges();
                // });
                // this.getUserBadges().then((r) => r);
                this.fetchGroupBadges(); // .then((r) => r);
            });
        }
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
        const response = await toPromise(
            this.http.get<BadgeGroupInfo>(`/groups/groupinfo/${this.group}`)
        );

        // FIXME: why are we using BadgeService to show the error?
        if (!response.ok) {
            this.badgeService.showError(
                this.alerts,
                {
                    data: {
                        error: response.result.error.error,
                    },
                },
                "danger"
            );
            return;
        }
        const groupInfo = response.result;

        if (groupInfo !== undefined) {
            this.displayName = groupInfo.description || "";
            this.groupId = groupInfo.id;
        } else {
            console.error(
                `Invalid group information for group '${this.group}': group id is undefined.`
            );
        }
    }

    /**
     * Fetches a list of users belonging to current group,
     * updates member view and member count in user interface
     */
    async getMembers() {
        let members: IUser[] = [];

        const response = await to2(
            this.groupService.getUsersFromGroup(this.group)
        );
        if (response.ok) {
            members = response.result;
        }

        this.members = [];
        for (const m of members) {
            const u: IBadgeUser = {
                id: m.id,
                name: m.name,
                real_name: m.real_name,
                email: m.email,
                student_id: m.student_id,
                badges: await this.getBadgesForUser(m.name),
            };
            this.members.push(u);
        }
    }

    /**
     * Get badges for a specific user if
     *  - the specified user is the current user (users can always view their own badges)
     *  - the current user has at least teacher rights to the context group that the specified user belongs to
     * @param user_id
     */
    async getBadgesForUser(username: string): Promise<IBadge[]> {
        let badges: IBadge[] = [];

        // No access checks needed here, there are done on the server when getting badges from db
        // const teacherRight = await this.groupService.queryTeacherRightsToGroup(
        //     this.groupId!
        // );
        // const current_user = genericglobals().current_user;
        // if (current_user.name == username || teacherRight) {
        const personal_group_query = await this.groupService.getPersonalGroup(
            username
        );
        let personal_group: IGroup | undefined;
        if (personal_group_query.ok) {
            personal_group = personal_group_query.result;
            badges = await this.badgeService.getBadges(
                personal_group.id,
                this.group
            );
        }
        // }

        return badges;
    }

    /**
     * Fetches personal badges for each group member.
     * Currently, a regular member of the group can only see his own badges.
     * An admin or teacher can see every member's personal badges.
     * Uses member's personal group IDs to query badges.
     * Counts total badges.
     */
    async getUserBadges() {
        let badgeCount = 0;
        const badgePromises = this.members.map(async (user) => {
            // const isCurrentUser = user.name === this.currentUserName;
            // if (!this.canViewAllBadges && !isCurrentUser) {
            //     return;
            // }

            let personalGroup: IGroup | undefined;
            this.groupService.getPersonalGroup(user.name).then((response) => {
                if (response.ok) {
                    personalGroup = response.result;
                } else {
                    personalGroup = undefined;
                }
            });

            if (personalGroup != undefined) {
                if (personalGroup.id != undefined) {
                    const badges = await this.badgeService.getBadges(
                        personalGroup.id,
                        this.contextGroup!
                    );

                    if (badges.length > 0) {
                        user.badges = badges;
                        badgeCount += badges.length;
                    }
                } else {
                    console.error(
                        "group-dashboard.component.ts: getUserBadges():: personal group id was undefined."
                    );
                    return;
                }
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
        if (this.groupId != undefined) {
            const groupBadges = await this.badgeService.getBadges(
                this.groupId,
                this.contextGroup!
            );
            if (groupBadges) {
                this.groupBadges = groupBadges;
                this.totalBadges += groupBadges.length;
            } else {
                console.error(
                    "group-dashboard.component.ts: fetchGroupBadges():: group id was undefined."
                );
                return;
            }
        }
    }

    protected readonly alert = alert;
}

@NgModule({
    declarations: [GroupDashboardComponent],
    exports: [GroupDashboardComponent],
    imports: [
        CommonModule,
        NameChangerModule,
        BadgeModule,
        PurifyModule,
        TimUtilityModule,
    ],
})
export class GroupDashboardModule {}
