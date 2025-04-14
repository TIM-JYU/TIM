import {Component, Input, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {IManageGlobals, manageglobals} from "tim/util/globals";
import {IFolder, IFullDocument} from "tim/item/IItem";

@Component({
    selector: "tim-group-dashboard",
    template: `
        <ng-container>
            <h1>{{ this.displayName }}'s Dashboard</h1>
        </ng-container>`,
    styleUrls: ["./group-dashboard.component.scss"],
})
export class GroupDashboardComponent implements OnInit {
    private item: IFullDocument | IFolder | undefined;
    constructor(private badgeService: BadgeService) {}

    @Input() group!: string;
    displayName: string | undefined;
    groupId: number | undefined;
    title: string | undefined;
    ngOnInit(): void {
        if (this.group) {
            this.getGroupName();
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
}

@NgModule({
    declarations: [GroupDashboardComponent],
    exports: [GroupDashboardComponent],
    imports: [CommonModule],
})
export class GroupDashboardModule {}
