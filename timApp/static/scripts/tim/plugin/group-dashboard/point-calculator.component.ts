import {Component, Input, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {GroupService} from "./group.service";
import {BadgeService} from "../../gamification/badge/badge.service";
import {GroupNameModule} from "./group-name.component";
import {BadgeModule} from "../../gamification/badge/badge.component";

@Component({
    selector: "tim-point-calculator",
    template: `
    <ng-container>
        <h1>Groups</h1>
        <div class="point-calculator">
            <!-- Fetch all subgroups, create name label and points field for every group -->
        </div>
    </ng-container>`,
    styleUrls: ["./point-calculator.component.scss"],
})
export class PointCalculatorComponent implements OnInit {
    constructor(
        private groupService: GroupService,
        private badgeService: BadgeService
    ) {}

    @Input() group!: string;

    async ngOnInit() {
        await this.fetchSubGroups();
    }

    async fetchSubGroups() {
        if (this.group) {
            const groups = await this.groupService.getSubGroups(this.group);
            if (groups) {
                console.log(groups);
            }
        }
    }
}

@NgModule({
    declarations: [PointCalculatorComponent],
    exports: [PointCalculatorComponent],
    imports: [CommonModule, GroupNameModule, BadgeModule],
})
export class PointCalculatorModule {}
