import {Component, Input, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {manageglobals} from "tim/util/globals";
import {IFolder, IFullDocument} from "tim/item/IItem";
import {GroupNameModule} from "tim/plugin/group-dashboard/group-name.component";
import {IBadge, IUser} from "tim/gamification/badge/badge.interface";
import {BadgeModule} from "tim/gamification/badge/badge.component";
import {GroupService} from "tim/plugin/group-dashboard/group.service";

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
        <ul>
            <li>Graphs</li>
            <li>Points</li>
            <li>Totals</li>
        </ul>
    </div>

    <div class="dashboard-section">
        <h2 class="section-title">Members</h2>
        <div class="member-list">
            <div class="member-card" *ngFor="let member of members">
                <div class="member-info">
                    <span class="member-name">{{ member.name }}</span>
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

    <div class="dashboard-section">
        <h2 class="section-title">Settings</h2>
        <b>Change group name:</b>
        <tim-group-name
            *ngIf="group"
            [group]="group"
            (contextGroupChange)="onContextGroupChange($event)"
            (groupNameChange)="onGroupNameChange($event)">
        </tim-group-name>
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
    private item: IFullDocument | IFolder | undefined;
    displayName: string | undefined;
    groupId: number | undefined;
    contextGroup: string | undefined;
    members: any[] = [];
    title: string | undefined;
    groupBadges: IBadge[] = [];
    nameJustUpdated = false;

    //TODO: total badges in group details
    //TODO: total members in group
    async ngOnInit(): Promise<void> {
        if (this.group) {
            await this.getGroupName();
            await this.fetchMembers();
            await this.fetchUserBadges();
            await this.fetchGroupBadges();
        }
        this.item = manageglobals().curr_item;
    }

    async getGroupName() {
        const fetchedGroup = await this.groupService.getCurrentGroup(
            this.group
        );
        if (fetchedGroup) {
            this.displayName = fetchedGroup.description || "";
            this.groupId = fetchedGroup.id;
        }
    }

    async fetchMembers() {
        const members = await this.groupService.getUsersFromGroup(this.group);

        if (members) {
            this.members = members;
        }
    }

    async fetchUserBadges() {
        const badgePromises = this.members.map(async (user) => {
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
    }

    async fetchGroupBadges() {
        try {
            const groupBadges = await this.badgeService.getUserBadges(
                this.groupId!,
                this.contextGroup!
            );
            if (groupBadges) {
                this.groupBadges = groupBadges;
            }
        } catch (error) {
            console.error("Error fetching group mutual badges:", error);
        }
    }

    onContextGroupChange(context: string) {
        this.contextGroup = context;
    }

    onGroupNameChange(newName: string) {
        this.displayName = newName;
        this.nameJustUpdated = true;

        setTimeout(() => {
            this.nameJustUpdated = false;
        }, 1500);
    }
}

@NgModule({
    declarations: [GroupDashboardComponent],
    exports: [GroupDashboardComponent],
    imports: [CommonModule, GroupNameModule, BadgeModule],
})
export class GroupDashboardModule {}
