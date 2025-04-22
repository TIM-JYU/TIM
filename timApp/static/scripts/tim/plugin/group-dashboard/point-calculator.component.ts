import {Component, Input, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {GroupService} from "./group.service";
import {BadgeService} from "../../gamification/badge/badge.service";
import {GroupNameModule} from "./group-name.component";
import {BadgeModule} from "../../gamification/badge/badge.component";
import {FormsModule} from "@angular/forms";

@Component({
    selector: "tim-point-calculator",
    template: `
    <ng-container>
        <h1 class="heading-with-info">
    Groups
    <span class="info-icon" title="Displaying subgroups of main group (context group) {{this.group!}}">?</span>
  </h1>
        <div class="point-calculator">
            <div *ngFor="let subgroup of subGroups" class="group-entry">
      <label class="group-label">{{ subgroup.name }}</label>
      <input
        type="number"
        [(ngModel)]="subgroup.points"
        placeholder="Enter points"
      />
    </div>
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
    subGroups: {name: string; points?: number}[] = [];

    async ngOnInit() {
        await this.fetchSubGroups();
    }

    async fetchSubGroups() {
        if (this.group) {
            const groups = await this.groupService.getSubGroups(this.group);
            if (groups) {
                //console.log(groups);

                this.subGroups = groups.map((g: any) => ({
                    name: typeof g === "string" ? g : g.name,
                    points: 0,
                }));
            }
        }
    }
}

@NgModule({
    declarations: [PointCalculatorComponent],
    exports: [PointCalculatorComponent],
    imports: [CommonModule, GroupNameModule, BadgeModule, FormsModule],
})
export class PointCalculatorModule {}
