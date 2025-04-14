import {Component, Input, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {IManageGlobals, manageglobals} from "tim/util/globals";
import {IFolder, IFullDocument} from "tim/item/IItem";
import {GroupNameModule} from "tim/gamification/badge/group-name.component";

@Component({
    selector: "tim-group-dashboard",
    template: `
        <ng-container>
    <h1>{{ displayName }}'s Dashboard</h1>

    <div class="member-list">
        <div class="member-card" *ngFor="let member of members">
            <div class="member-info">
                <span class="member-name">{{ member.name }}</span>

                <div class="badges">
                    <span *ngFor="let badge of member.badges" class="badge">
                        🏅 {{ badge.name }}
                    </span>
                </div>
            </div>
        </div>
    </div>
    <div class="settings">
        <h2>Settings</h2>
        <h3>Change group name:</h3>
        <tim-group-name [group]="group"></tim-group-name>
    </div>            
</ng-container>`,
    styleUrls: ["./group-dashboard.component.scss"],
})
export class GroupDashboardComponent implements OnInit {
    constructor(private badgeService: BadgeService) {}

    @Input() group!: string;
    private item: IFullDocument | IFolder | undefined;
    displayName: string | undefined;
    groupId: number | undefined;
    members: any[] = [];
    title: string | undefined;
    ngOnInit(): void {
        if (this.group) {
            this.getGroupName();
            this.fetchMembers();
        }
        this.item = manageglobals().curr_item;
    }

    async getGroupName() {
        const fetchedGroup = await this.badgeService.getCurrentGroup(
            this.group
        );
        if (fetchedGroup) {
            this.displayName = fetchedGroup.description || "";
        }
    }

    async fetchMembers() {
        const members = await this.badgeService.getUsersFromGroup(this.group);

        if (members) {
            console.log("Jäsenet: ", members);
            this.members = members;
        }
    }
}

@NgModule({
    declarations: [GroupDashboardComponent],
    exports: [GroupDashboardComponent],
    imports: [CommonModule, GroupNameModule],
})
export class GroupDashboardModule {}
