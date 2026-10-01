import type {OnInit} from "@angular/core";
import {Component, Input, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {genericglobals} from "tim/util/globals";
import {NameChangerModule} from "tim/plugin/group-dashboard/name-changer.component";
import type {IErrorAlert} from "tim/gamification/badge/badge.interface";
import type {IBadgeAward} from "tim/gamification/badge/badge.interface";
import {BadgeModule} from "tim/gamification/badge/badge.component";
import {GroupService} from "tim/plugin/group-dashboard/group.service";
import {toPromise} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import {PurifyModule} from "tim/util/purify.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import type {IGroup, IUser} from "tim/user/IUser";
import type {BadgeGroupInfo} from "tim/plugin/group-dashboard/group.service";

// FIXME: temp interfaces, get rid of these
export interface IBadgeUser extends IUser {
    badges: IBadgeAward[];
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
                                        [title]="badge.template.title"
                                        [color]="badge.template.color"
                                        [shape]="badge.template.shape"
                                        [image]="badge.template.image"
                                        [description]="badge.template.description"
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
                    </div>

                    <div class="dashboard-section">
                        <h2 i18n class="section-title">Members</h2>
                        <div class="member-list">
                            <div class="member-card" *ngFor="let member of this.members">
                                <div class="member-info">
                                    <span class="member-name">{{ member.real_name }}</span>
                                    <div class="member-badge-area">
                                        <span *ngFor="let badge of member.badges" class="badge">
                                            <tim-badge class="member-badge"
                                                title="{{badge.template.title}}"
                                                color="{{badge.template.color}}"
                                                shape="{{badge.template.shape}}"
                                                [image]="badge.template.image"
                                                description="{{badge.template.description}}"
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
    groupBadges: IBadgeAward[] = [];
    nameJustUpdated = false;
    totalBadges: number = 0;
    alerts: Array<IErrorAlert> = [];

    /**
     * Triggers loading of group related data if group is provided in the user interface.
     *
     * The group information is loaded first, because everything after it needs the
     * context group that it resolves.
     */
    async ngOnInit() {
        if (!this.group) {
            return;
        }
        this.currentUserName = genericglobals().current_user.name;

        if (!(await this.getGroupInfo())) {
            return;
        }
        this.members = await this.getMembers();
        const memberBadges = await Promise.all(
            this.members.map((m) => this.getBadgesForUser(m.name))
        );
        this.members.forEach((m, i) => (m.badges = memberBadges[i]));
        this.groupBadges = await this.fetchGroupBadges();
        this.totalBadges =
            this.groupBadges.length +
            memberBadges.reduce((total, badges) => total + badges.length, 0);
    }

    /**
     * Fetches data for the current group: its pretty name, its id, and the group that
     * its badges are scoped to.
     *
     * @returns whether the group could be read; an error alert is shown if it could not
     */
    async getGroupInfo(): Promise<boolean> {
        const response = await toPromise(
            this.http.get<BadgeGroupInfo>(
                `/groups/groupinfo/${encodeURIComponent(this.group)}`
            )
        );
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
            return false;
        }
        const groupInfo = response.result;
        // A group without an admin document has no pretty name; fall back to the
        // internal name, since the whole dashboard is hidden without one.
        this.displayName = groupInfo.description ?? groupInfo.name;
        this.groupId = groupInfo.id;
        // Badges belong to a course, so a subgroup's badges live in its parent group.
        // This used to be guessed by cutting the name at the first "-", which was wrong
        // for a top-level group whose name contains a dash and for any subgroup not
        // named after its parent.
        this.contextGroup = groupInfo.parent_group ?? groupInfo.name;
        return true;
    }

    /**
     * Fetches a list of users belonging to current group
     */
    async getMembers(): Promise<IBadgeUser[]> {
        const users: IUser[] = await this.groupService.getUsersFromGroup(
            this.group
        );
        const members: IBadgeUser[] = [];
        for (const m of users) {
            const u: IBadgeUser = {
                id: m.id,
                name: m.name,
                real_name: m.real_name,
                email: m.email,
                student_id: m.student_id,
                badges: [],
            };
            members.push(u);
        }
        return members;
    }

    /**
     * Get badges for a specific user
     * @param username
     */
    async getBadgesForUser(username: string): Promise<IBadgeAward[]> {
        let badges: IBadgeAward[] = [];
        const personal_group_query = await this.groupService.getPersonalGroup(
            username
        );
        let personal_group: IGroup | undefined;
        if (personal_group_query.ok) {
            personal_group = personal_group_query.result;
            badges = await this.badgeService.getBadges(
                personal_group.id,
                this.contextGroup!
            );
        }
        return badges;
    }

    /**
     * Fetches badges that are assigned directly to the group itself.
     * Updates the group's badge list and adds to the total badge count.
     */
    async fetchGroupBadges(): Promise<IBadgeAward[]> {
        let groupBadges: IBadgeAward[] = [];
        if (this.groupId != undefined) {
            const result = await this.badgeService.getBadges(
                this.groupId,
                this.contextGroup!
            );
            if (result.length > 0) {
                groupBadges = result;
            }
        }
        return groupBadges;
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
