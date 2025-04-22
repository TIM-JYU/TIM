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
            <button (click)="savePoints()">Save changes</button>
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
                this.subGroups = groups.map((g: any) => ({
                    name: typeof g === "string" ? g : g.name,
                    points: 0,
                }));
            }
            for (const group of this.subGroups) {
                const prettyGroup = await this.groupService.getCurrentGroup(
                    group.name
                );
                if (prettyGroup) {
                    console.log(prettyGroup);
                    group.name = prettyGroup.description || group.name;
                }
            }

            const saved = localStorage.getItem(`points-${this.group}`);
            if (saved) {
                const savedPoints = JSON.parse(saved);
                this.subGroups.forEach((group) => {
                    if (savedPoints[group.name] != null) {
                        group.points = savedPoints[group.name];
                    }
                });
            }
        }
    }

    savePoints() {
        const pointsMap = this.subGroups.reduce((acc, group) => {
            acc[group.name] = group.points || 0;
            return acc;
        }, {} as Record<string, number>);

        localStorage.setItem(`points-${this.group}`, JSON.stringify(pointsMap));
        console.log("Points saved to localStorage:", pointsMap);
    }
}

@NgModule({
    declarations: [PointCalculatorComponent],
    exports: [PointCalculatorComponent],
    imports: [CommonModule, GroupNameModule, BadgeModule, FormsModule],
})
export class PointCalculatorModule {}
