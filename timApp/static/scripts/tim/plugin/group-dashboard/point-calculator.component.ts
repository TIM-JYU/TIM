import {Component, Input, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {GroupService} from "./group.service";
import {BadgeService} from "../../gamification/badge/badge.service";
import {GroupNameModule} from "./group-name.component";
import {BadgeModule} from "../../gamification/badge/badge.component";
import {FormsModule} from "@angular/forms";
import {PointService} from "tim/plugin/group-dashboard/point.service";

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
        private badgeService: BadgeService,
        private pointService: PointService
    ) {}

    @Input() group!: string;
    subGroups: {name: string; points?: number; id?: number}[] = [];

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
                    group.name = prettyGroup.description || group.name;
                    group.id = prettyGroup.id;
                }
            }

            const savedPoints = this.pointService.getPoints(this.group);
            if (savedPoints) {
                this.subGroups.forEach((group) => {
                    if (group.id != null && savedPoints[group.id] != null) {
                        group.points = savedPoints[group.id];
                    }
                });
            }
        }
    }
    //TODO: väläytä pistekenttää esim. vihreänä - indikoi tallentamista
    // tuo pisteet servicen kautta group dashboardiin näkyviin
    savePoints() {
        const pointsMap: Record<number, number> = {};
        this.subGroups.forEach((group) => {
            if (group.id != null) {
                pointsMap[group.id] = group.points || 0;
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
