import {Component, Input, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {GroupService} from "./group.service";
import {GroupNameModule} from "./group-name.component";
import {BadgeModule} from "tim/gamification/badge/badge.component";
import {FormsModule} from "@angular/forms";
import {PointService} from "tim/plugin/group-dashboard/point.service";
import {showConfirm} from "tim/ui/showConfirmDialog";

@Component({
    selector: "tim-point-calculator",
    template: `
    <ng-container>
        <h1 class="heading-with-info">
    Groups of {{this.group}}
    <span class="info-icon" title="Displaying subgroups of context group {{this.group!}}">?</span>
  </h1>
        <div class="point-calculator">
            <div *ngFor="let subgroup of subGroups" class="group-entry">
      <label class="group-label" [title]="subgroup.name">{{ subgroup.description }}</label>
      <input
        type="number"
        [(ngModel)]="subgroup.points"
        placeholder="Enter points"
        [ngClass]="{'flashing': modifiedGroups.has(subgroup.id ?? 0)}"
      />
    </div>
            <div class="buttons">
            <button class="save" (click)="savePoints()">Save changes</button>
            <button class="reset" (click)="resetPoints()">Reset points</button>
                </div>
        </div>
    </ng-container>`,
    styleUrls: ["./point-calculator.component.scss"],
})
export class PointCalculatorComponent implements OnInit {
    constructor(
        private groupService: GroupService,
        private pointService: PointService
    ) {}

    @Input() group!: string;
    subGroups: {
        name: string;
        points?: number;
        id?: number;
        description?: string;
        previousPoints?: number;
    }[] = [];
    modifiedGroups: Set<number> = new Set();

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
                    previousPoints: 0,
                }));
            }
            for (const group of this.subGroups) {
                const prettyGroup = await this.groupService.getCurrentGroup(
                    group.name
                );
                if (prettyGroup) {
                    group.description = prettyGroup.description || group.name;
                    group.id = prettyGroup.id;

                    const savedPoints = this.pointService.getPoints(group.name);

                    if (savedPoints && group.id) {
                        group.points = savedPoints[group.id];
                        group.previousPoints = savedPoints[group.id];
                    }
                }
            }
        }
    }

    savePoints() {
        this.subGroups.forEach((group) => {
            if (group.id != null && group.name) {
                const points = group.points || 0;

                if (group.previousPoints !== points) {
                    this.pointService.setPoints(group.name, {
                        [group.id]: points,
                    });
                    this.modifiedGroups.add(group.id);
                    this.flashField(group.id);
                    group.previousPoints = points;
                }
            }
        });
    }

    flashField(groupId: number) {
        setTimeout(() => {
            this.modifiedGroups.delete(groupId);
        }, 2000);
    }

    async resetPoints() {
        const confirmed = await showConfirm(
            "Confirm Reset",
            "Are you sure you want to reset all points? This cannot be undone."
        );

        if (!confirmed) {
            return;
        }

        this.subGroups.forEach((group) => {
            if (group.id != null) {
                group.points = 0;
                group.previousPoints = 0;
            }
        });

        const pointsMap: Record<number, number> = {};
        this.subGroups.forEach((group) => {
            if (group.id) {
                pointsMap[group.id] = 0;
            }
        });
        this.pointService.setPoints(this.group, pointsMap);
    }
}

@NgModule({
    declarations: [PointCalculatorComponent],
    exports: [PointCalculatorComponent],
    imports: [CommonModule, GroupNameModule, BadgeModule, FormsModule],
})
export class PointCalculatorModule {}
